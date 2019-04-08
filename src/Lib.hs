module Lib
    ( appEntry
    ) where

import qualified Control.Exception as E
import Network.Socket
import Control.Concurrent
import Control.Monad

import Brick.BChan (BChan, newBChan, writeBChan)

import Pipes
import qualified Pipes.Prelude as P
import Pipes.Network.TCP (fromSocket, toSocket)
import Pipes.Binary (DecodingError(..), decode, encode)
import Pipes.Parse (parsed)

import Chat

appEntry :: IO ()
appEntry = do
  opts <- execParser fullOpts
  eventChan <- newBChan 10
  upChan <- newChan
  addr <- resolve (host opts) (port opts)
  sock <- open addr
  network <- forkFinally (talk eventChan upChan sock) (const $ close sock)
  let s = initState $ writeChan upChan
  brickMain s eventChan
  killThread network
  where
    resolve host port = do
        let hints = defaultHints { addrSocketType = Stream }
        addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
        return addr
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

talk :: BChan Message -> Chan ClientMsg -> Socket -> IO ()
talk eventChan upChan sock = do

  down <- forkIO $ do
    (e, _) <- runEffect $ do
      let bytesReciever = fromSocket sock 4096
          decoder = parsed decode bytesReciever
      decoder >-> P.mapM_ (writeBChan eventChan . Right)
    writeBChan eventChan $ Left e

  runEffect $ lift (readChan upChan) >~ for cat encode >-> toSocket sock
  killThread down
