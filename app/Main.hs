{-# LANGUAGE RecordWildCards #-}

import Data.ByteString.Char8 (pack)
import Control.Distributed.Process
import Control.Distributed.Process.Internal.Types (LocalNode)
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode)
import Network.Socket (HostName, ServiceName)
import Network.Transport (EndPointAddress (..))
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import System.IO

import Config
import Iohk

rtable :: RemoteTable
rtable = __remoteTable initRemoteTable

defNodes :: [([Char], [Char], Int, Int)]
defNodes = [
  -- (host, port, seed, nb messsages)
  -- Can be overridden by command line arguments
    ("127.0.0.1", "5000", 11, 50)
  , ("127.0.0.1", "5001", 12, 50)
  , ("127.0.0.1", "5002", 13, 50)
  , ("127.0.0.1", "5003", 14, 50)
  , ("127.0.0.1", "5004", 15, 50)
  ]



main :: IO ()
main = do
  options <- parseCliArgs
  (node, host, port, seed, nbMsgs, otherNodes) <- startNode
  let config = makeConfig host port seed nbMsgs options
  hPutStrLn stderr "Config"
  hPrint stderr config
  hPutStrLn stderr "---------------------"
  runIOHK config node otherNodes



-- | using the predefined node list, find one that has not been started yet.
-- Return the node and nodeIds of the other agents.
startNode :: IO (LocalNode, HostName, ServiceName, Int, Int, [NodeId])
startNode = do
  (node, host, port, seed, nbMsg, others) <- go [] defNodes
  return (node, host, port, seed, nbMsg, map mkNodeId others)
  where
  mkNodeId (host, port) =
    let addr = EndPointAddress (pack (host ++":"++port++":0"))
    in NodeId addr
  go _ [] = fail "error"
  go busy ((host, port, seed, nbMsg) : rest) = do
    r <- createTransport host port defaultTCPParameters
    case r of
      Right t -> do
        node <- newLocalNode t rtable
        return (node, host, port, seed, nbMsg, busy ++ map (\(h, p, _, _) -> (h, p)) rest)
      Left _ -> go ((host, port) : busy) rest




