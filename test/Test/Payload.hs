

module Test.Payload
(
  tests
)
where

import            Debug.Trace (trace)
import qualified  Data.Set as S
import qualified  Data.List as L
import            Test.Framework
import            Test.Framework.Providers.QuickCheck2 (testProperty)
import            Test.QuickCheck
import            Iohk

import GHC.Exts (sortWith)

tests :: [Test]
tests = [

  testProperty "testFullCompactPayloadProp" testFullCompactPayloadProp
  , testProperty "testChooseMeProp" testChooseMeProp
  , testProperty "testChooseBothProp" testChooseBothProp
  , testProperty "testChooseOtherProp" testChooseOtherProp
  , testProperty "testChooseNoneProp" testChooseNoneProp

  , testProperty "testCompactPayload" testCompactPayloadProp
  , testProperty "testMultipleCompactPayloadProp" testMultipleCompactPayloadProp
  , testProperty "testMultipleCompactPayloadCutoffTimeProp" testMultipleCompactPayloadCutoffTimeProp
  ]

payloadGen :: Int -> Gen (Payload Int)
payloadGen n = do
  nbMsgs <- choose (10, 10+n)
  tmax <- choose (1000000, 10000000)
  times <- vectorOf nbMsgs $ choose (0, tmax)
  msgs <- mapM (\t -> choose (0.0, 1.0) >>= \v -> return (t, v)) $ mkUniq times
  return $ Payload (WeightedSum 0 0.0 0) $ sortWith fst msgs
  where
  mkUniq :: [Int] -> [Int]
  mkUniq = S.toList . S.fromList



testChooseNoneProp :: Property
testChooseNoneProp =
  forAll (payloadGen 1000) testChooseMe


testChooseNone :: Payload Int -> Bool
testChooseNone payload@(Payload s msgs)  =
    case (choosePayload payload payload) of
      (_, None) -> True
      _ -> False

testChooseMeProp :: Property
testChooseMeProp =
  forAll (payloadGen 1000) testChooseMe


testChooseMe :: Payload Int -> Property
testChooseMe payload@(Payload s msgs)  =
  L.length msgs > 10 ==>
    case (choosePayload payload payload2) of
      (_, Me) -> True
      _ -> False
  where
  n = L.length msgs `div` 2
  payload2 = Payload s $ L.take n msgs


testChooseOtherProp :: Property
testChooseOtherProp =
  forAll (payloadGen 10) testChooseOther


testChooseOther :: Payload Int -> Property
testChooseOther payload@(Payload s msgs)  =
  L.length msgs > 10 ==>
--    case (mergeMsgsWithMod msgs2 msgs ) of
    case (choosePayload payload2 payload ) of
      (_, Other) -> True
      (_, x) -> trace (show ("res ", x)) False
  where
  n = L.length msgs `div` 2
  msgs2 = L.take n msgs
  payload2 = Payload s msgs2


testChooseBothProp :: Property
testChooseBothProp =
  forAll ((,) <$> (payloadGen 1000) <*> (payloadGen 1000)) testChooseBoth

testChooseBoth :: (Payload Int, Payload Int) -> Bool
testChooseBoth (p1, p2) =
  case (choosePayload p1 p2) of
    (_, Both) -> True
    _ -> False

fullCompactPayloadRef :: (Num t, Eq t) => Payload t -> Payload t
fullCompactPayloadRef (Payload (WeightedSum 0 0.0 0) msgs) =
  Payload (WeightedSum n s t) []
  where
  (n, s, t) = L.foldl' proc (0, 0.0, 0) msgs
  proc (n', s', _) (t', v') = (n'+1, s' + fromIntegral (n'+1) * v', t')
fullCompactPayloadRef _ = error "Not Zero Sum"

testCompactPayloadProp :: Property
testCompactPayloadProp =
  forAll (payloadGen 20) testCompactPayload

testCompactPayload :: Payload Int -> Bool
testCompactPayload p@(Payload _ msgs) =
  L.all id actuals
  where
  actuals = L.map (\n -> (fullCompactPayload $ compactPayload p n) == expected) [0 .. L.length msgs]
  expected = fullCompactPayloadRef p


testFullCompactPayloadProp :: Property
testFullCompactPayloadProp =
  forAll (payloadGen 20) testFullCompactPayload

testFullCompactPayload :: Payload Int -> Bool
testFullCompactPayload p@(Payload _ _) =
  actual == expected
  where
  actual = fullCompactPayload p
  expected = fullCompactPayloadRef p


testMultipleCompactPayloadProp :: Property
testMultipleCompactPayloadProp =
  forAll ((,) <$> (payloadGen 1000) <*> (vectorOf 220 $ choose (0, 10))) testMultipleCompactPayload

testMultipleCompactPayload :: (Payload Int, [Int]) -> Bool
testMultipleCompactPayload (p@(Payload _ msgs), cutoffs) =
  case (comparePayload actual expected) of
    True -> True
    False ->  trace (show (actual, expected, L.length msgs)) $ False
  where
  actual = fullCompactPayload $ L.foldl' compactPayload p cutoffs
  expected = fullCompactPayloadRef p



testMultipleCompactPayloadCutoffTimeProp :: Property
testMultipleCompactPayloadCutoffTimeProp =
  forAll ((,) <$> (payloadGen 1000) <*> (vectorOf 220 $ choose (0, 10))) testMultipleCompactPayloadCutoffTime


testMultipleCompactPayloadCutoffTime :: (Payload Int, [Int]) -> Bool
testMultipleCompactPayloadCutoffTime (p@(Payload _ msgs), cutoffs) =
  case (comparePayload actual expected) of
    True -> True
    False ->  trace (show (actual, expected, L.length msgs)) $ False
  where
  proc p'@(Payload _ []) _ = p'
  proc p'@(Payload _ msgs') n =
    case L.take n msgs' of
    [] -> p'
    (cutoff, _):_ -> compactPayloadCutoffTime p' cutoff
  actual = fullCompactPayload $ L.foldl' proc p cutoffs
  expected = fullCompactPayloadRef p

comparePayload :: Eq a => Payload a -> Payload a -> Bool
comparePayload (Payload (WeightedSum n1 s1 t1) msgs1) (Payload (WeightedSum n2 s2 t2) msgs2) =
  n1 == n2 && t1 == t2 && msgs1 == msgs2 && abs (s1-s2) < 1.0




