{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}


module Iohk
(
  checkPayload
  , choosePayload
  , compactPayload
  , compactPayloadCutoffTime
  , fullCompactPayload
  , runIOHK
  , WeightedSum(..)
  , Payload(..)
  , __remoteTable
  )
where
import Debug.Trace (trace)

import            Control.Concurrent
import            Data.Binary
import qualified  Data.List as L
import            Data.Monoid
import            Data.Typeable
import            Control.Distributed.Process
import            Control.Distributed.Process.Closure
import            Control.Distributed.Process.Internal.Types (LocalNode)
import            Control.Distributed.Process.Node (runProcess)
import            Control.Monad (forM_)
import qualified  Data.Set as S
import            GHC.Generics (Generic)
import            System.IO
import            System.Random

import GHC.Exts (sortWith)

import Config

remotable []

-- | Message created an Agent
type Msg t = (t, Float)

-- | Message notifying the beginning of the grace period.
data Grace = Grace
  deriving (Typeable, Generic, Show)

instance Binary Grace

-- | Weighted sum of a list of messages.
data WeightedSum t = WeightedSum {
  nbMgs :: Int              -- ^ Number of messages ( == index of the last message).
  , sum :: Float            -- ^ Weighted sum for these 'nbMgs' messages.
  , timeLastMsg :: t        -- ^ Time of the last message in this weight sum.
  }
  deriving (Eq, Show, Typeable, Generic)

instance (Binary t) => Binary (WeightedSum t)

-- | Payloads are sent between agents. A Payload is supposed to carry all the information
-- that has been generated in the system.
-- The Payload can be compacted if its size (number of messages) becomes to large.--
-- However, compaction implies a risk of loosing some messages.
data Payload t = Payload {
  weightedSum :: WeightedSum t          -- ^ The weighted sum of the messages previous those in 'msgs'.
  , messages :: [Msg t]               -- ^ The messages in time ascending order.
  }
  deriving (Eq, Show, Typeable, Generic)
instance (Binary t) => Binary (Payload t)


-- | Check the Payload invariant. Fail if invariants are not respected.
checkPayload :: Payload Int -> Payload Int
checkPayload p =
  if payloadInvariant p
  then p
  else trace (show p) $ error "invariant violated"

-- | Verify the Payload invariant.
payloadInvariant :: Payload Int -> Bool
payloadInvariant (Payload (WeightedSum n wsum t) msgs) =
  (b1 && b2 && b3 && b4 && b5)
  where
  -- messages are after the messages in the weighted sum.
  b1 =  case msgs of
        [] -> True
        ((t0, _) : _) -> t < t0
  -- messages are sorted in time ascending order
  b2 = sortWith fst msgs == msgs
  -- number of messages in the weighted sum is non negative
  b3 = n >= 0
  -- weigthed sum is non negative
  b4 = wsum >= 0
  -- times are unique
  b5 = let times = map fst msgs in mkUniq times == times

-- | Determine whether the Payload must be compacted.
shoudCompactPayload :: Payload t -> Int -> Bool
shoudCompactPayload (Payload _ msgs) len =
  len <= L.length msgs

-- | Compact a Payload, making sure that the number of remaing messages is less than 'len'.
compactPayload :: Payload Int -> Int -> Payload Int
compactPayload (Payload wsum msgs) len =
  checkPayload $ Payload wsum' after
  where
  (before, after) = L.splitAt len msgs
  wsum' = computeWeightedSum wsum before


-- | Compact a payload base on a cuttoff time.
compactPayloadCutoffTime :: Payload Int -> Int -> Payload Int
compactPayloadCutoffTime (Payload wsum msgs) cutoff =
  checkPayload $ Payload wsum' after
  where
  (before, after) = L.partition (\(t, _) -> t <= cutoff) msgs
  wsum' = computeWeightedSum wsum before

-- | Compact a payload using all messages.
fullCompactPayload :: Payload Int -> Payload Int
fullCompactPayload (Payload wsum msgs) =
  checkPayload $ Payload wsum' []
  where
  wsum' = computeWeightedSum wsum msgs

-- | Compute the weighted sum.
computeWeightedSum :: WeightedSum t -> [Msg t] -> WeightedSum t
computeWeightedSum (WeightedSum n0 s0 t0) msgs =
  go n0 s0 t0 msgs
  where
  go n s t [] = WeightedSum n s t
  go n s _ ((t, v) : rest) = go (n+1) (s + fromIntegral (n+1) * v) t rest


-- | Add a new message to a Payload. This can fail if the message time is
  -- smaller than the time of the last message in the weighted sum.
addMsg :: Ord t => (t, Float) -> Payload t -> Maybe (Payload t)
addMsg (t0, _) (Payload (WeightedSum _ _ t) _) | t0 <= t = Nothing
addMsg (t0, v0) (Payload wsum msgs) =
  Just (Payload wsum (mergeMsgs msgs [(t0, v0)]))


-- | Merge 2 lists of messages, preserving the order.
mergeMsgs :: (Ord t, Ord v) =>
  [(t, v)] -> [(t, v)] -> [(t, v)]
mergeMsgs msgs1 msgs2 = fst $ mergeMsgsWithMod msgs1 msgs2


-- | Merge 2 lists of messages, preserving the order, and determine which one (or both) has the most information.
mergeMsgsWithMod :: (Ord t, Ord v) =>
  [(t, v)] -> [(t, v)] -> ([(t, v)], InfoSource)
mergeMsgsWithMod = go
  where
  go [] [] = ([], None)
  go msg1s [] = (msg1s, Me)
  go [] msg2s = (msg2s, Other)
  go ((t1, v1) : rest1) msgs2@((t2, _) : _) | t1 < t2 =
    let (rest, infoSrc) = go rest1 msgs2
    in ((t1, v1) : rest, Me <> infoSrc)
  go msgs1@((t1, _) : _) ((t2, v2) : rest2) | t2 < t1 =
    let (rest, infoSrc) =  go msgs1 rest2
    in ((t2, v2) : rest, Other <> infoSrc)
  go ((t1, v1) : rest1) ((_, v2) : rest2)  =
    let (rest, infoSrc) = go rest1 rest2
    in ((t1, max v1 v2) : rest, Both <> infoSrc)


-- | Choose (or merge if necessary) 2 payload and determine which one (or both) has the most information.
choosePayload :: Payload Int -> Payload Int -> (Payload Int, InfoSource)
choosePayload (Payload wsum1@(WeightedSum _ s1 t1) msgs1)
              p2@(Payload (WeightedSum _ _ t2) _)         | t2 <= t1 =
  (checkPayload $ Payload wsum msgs, infoSrc <> infoSrc')
  where
  (Payload wsum3@(WeightedSum _ s3 _) msgs3) = compactPayloadCutoffTime p2 t1
  (wsum, infoSrc) =
    if s1 >= s3 then (wsum1, Me) else (wsum3, Other) -- we keept the highest weighed sum to conform to the requirement.
  (msgs, infoSrc') = mergeMsgsWithMod msgs1 msgs3

choosePayload p1 p2 =
  let (payload', infoSrc) = choosePayload p2 p1
  in (payload', reverseInfoSource infoSrc)

data InfoSource = Me | Other | Both | None

reverseInfoSource :: InfoSource -> InfoSource
reverseInfoSource Me = Other
reverseInfoSource Other = Me
reverseInfoSource Both = Both
reverseInfoSource None = None

instance Monoid InfoSource where
  mempty = None
  mappend None m = m
  mappend m None = m
  mappend Both _ = Both
  mappend _ Both = Both
  mappend Me Other = Both
  mappend Other Me = Both
  mappend Me Me = Me
  mappend Other Other = Me


-- | Start the IOHL Agent.
runIOHK ::
  Config
  -> LocalNode
  -> [NodeId]
  -> IO ()
runIOHK cfg@(MkConfig{..}) node otherNodeIds = do
  runProcess node start
  where
  start = do
    selfPid <- getSelfPid
    register "iohkAgent" selfPid
    _ <- spawnLocal (makeMsgSenderProcess cfg selfPid)
    _ <- spawnLocal (makeTimerProcess cfg selfPid)
    let payload = Payload (WeightedSum 0 0.0 0) [] :: Payload Int
    go selfPid (1::Int) payload
  go selfPid n payload = do
    receiveWait
      [ match $ \(t, v) -> do
          case addMsg (t, v) payload of
            Nothing -> do
              liftIO $ hPrint stderr (n, "Cannot add message", t, v, weightedSum payload)
              go selfPid n payload
            Just payload1 -> do
              liftIO $ hPrint stderr (n, "Accept message ", t, v)

              payload2 <-
                case shoudCompactPayload payload1 cfCompactingMaxLen of
                  True -> do
                    let p@(Payload s _) = compactPayload payload1 cfCompactingMaxLen
                    liftIO $ hPrint stderr (n, "Compacted payload", s)
                    return p
                  False -> return payload1
              -- new information is inserted, notify everyone.
              forM_ otherNodeIds (\other -> nsendRemote other "iohkAgent" (payload2, selfPid))
              go selfPid (n+1) payload2

        , match $ \((otherPayload, senderPid)::(Payload Int, ProcessId)) -> do
            liftIO $ hPrint stderr (n, "Received payload message")
            let (newPayload, infoSrc) = choosePayload payload otherPayload


            case infoSrc of
              None -> goInGrace n newPayload  -- no new information, do not notify
              Me -> do
                -- I am up to date, the sender does not add any information.
                -- However the sender has less information than me so let it know
                liftIO $ hPrint stderr (n, "I have more information, notifying the sender")
                send senderPid (newPayload, selfPid)
                go selfPid (n+1) newPayload
              Other -> do
                -- The sender has more information than I.
                -- No need to notify anyone since the sender already did it
                liftIO $ hPrint stderr (n, "The sender has more information")
                go selfPid (n+1) newPayload
              Both -> do
                -- The sender and I have complementary information
                -- Compacting & Notify everyone
                liftIO $ hPrint stderr (n, "Combine information and share")
                compactedNewPayload <-
                    case shoudCompactPayload newPayload cfCompactingMaxLen of
                      True -> do
                        let p@(Payload s _) = compactPayload newPayload cfCompactingMaxLen
                        liftIO $ hPrint stderr (n, "Compacted payload", s)
                        return p
                      False -> return newPayload
                forM_ otherNodeIds (\other -> nsendRemote other "iohkAgent" compactedNewPayload)
                go selfPid (n+1) compactedNewPayload
        , match $ \Grace -> do
            liftIO $ hPrint stderr ("Entering grace period")
            -- Forward Grace message. Uncomment to see what happens.
            -- forM_ otherNodeIds (\other -> nsendRemote other "iohkAgent" Grace)
            goInGrace n payload
      ]


  goInGrace n payload = do
    receiveWait
      [ match $ \(_ :: Msg Int) -> do
          liftIO $ hPrint stderr ("Ignoring message because in grace period")
          goInGrace n payload

        , match $ \((otherPayload, _)::(Payload Int, ProcessId)) -> do
            let (newPayload, _) = choosePayload payload otherPayload
            goInGrace (n+1) newPayload

        , match $ \() -> do
            liftIO $ hPutStrLn stderr "Received Stop Message"
            -- Forward Stop message. Uncomment to see what happens.
            -- forM_ otherNodeIds (\other -> nsendRemote other "iohkAgent" ())
            let (Payload (WeightedSum nbMsgs wsum _) _) = fullCompactPayload payload
            liftIO $ hPutStrLn stderr ("Terminated")
            liftIO $ hPrint stderr (nbMsgs, wsum)
            return ()
      ]

-- | Generate messages : time and value.
genMsgs :: Int -> Int -> StdGen -> ([(Int, Float)], StdGen)
genMsgs nbMsg tmax gen =
  (sortWith fst msgs, gen2)
  where
  (times, gen1) = L.foldl' procTimes ([], gen) [(1::Int) .. nbMsg]
  (msgs, gen2) = L.foldl' procMsgs ([], gen1) (mkUniq times)
  procTimes (acc, g) _ = let
    (t, g') = randomR (0, tmax) g
    in (t:acc, g')
  procMsgs (acc, g) t = let
    (v, g') = randomR (0.0, 1.0) g
    in ((t, v):acc, g')

mkUniq :: [Int] -> [Int]
mkUniq = S.toList . S.fromList


-- | Process that sends pre-generated messages to the IOHK Agent.
makeMsgSenderProcess :: Config -> ProcessId -> Process ()
makeMsgSenderProcess (MkConfig {..}) iohkAgentPid = do
  go (1::Int) 0 msgs
  where
  go n t0 ((t,v) : rest) = do
    liftIO $ threadDelay (t-t0)
    send iohkAgentPid (t, v)
    go (n+1)t rest
  go _ _ [] = return ()

  gen = mkStdGen cfSeed
  (msgs, _) = genMsgs cfNbMsgs cfSubmissionPeriod gen

-- | Process that manages the submission and grace periods.
makeTimerProcess :: Config -> ProcessId -> Process ()
makeTimerProcess (MkConfig {..}) iohkAgentPid = do

  liftIO $ threadDelay cfSubmissionPeriod
  send iohkAgentPid Grace

  liftIO $ threadDelay cfGracePeriod
  send iohkAgentPid ()

  return ()



