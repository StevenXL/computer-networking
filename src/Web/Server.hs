{-# LANGUAGE ScopedTypeVariables #-}

module Web.Server where

import           Control.Concurrent        (forkFinally)
import qualified Control.Exception         as E
import           Control.Monad             (forever, void)
import           Data.Text.Encoding        (decodeUtf8)
import           Network.Socket            (AddrInfo, AddrInfoFlag (AI_PASSIVE),
                                            Socket, SocketType (Stream), accept,
                                            addrAddress, addrFamily, addrFlags,
                                            addrProtocol, addrSocketType, bind,
                                            close, defaultHints, getAddrInfo,
                                            listen, socket)
import           Network.Socket.ByteString (recv, sendAll)
import           Web.Request               (Request, parseRawRequest)
import           Web.Response (Response, toByteString, response400)

main :: IO ()
main = do
  addr <- resolve "3000"
  E.bracket (open addr) close use
  where resolve :: String -> IO AddrInfo
        resolve port = do
          let hints = defaultHints { addrSocketType = Stream, addrFlags = [AI_PASSIVE] }
          addr:_ <- getAddrInfo (Just hints) Nothing (Just port)
          return addr
        open :: AddrInfo -> IO Socket
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
          bind sock (addrAddress addr)
          listen sock 10
          return sock
        use :: Socket -> IO b
        use sock = forever $ do
                (conn, peer) <- accept sock
                void $ forkFinally (handleConn conn) (const $ close conn)
        handleConn :: Socket -> IO ()
        handleConn conn = do
          rawRequest <- recv conn 4096
          let mRequest = parseRawRequest (decodeUtf8 rawRequest)
          maybe (onUnparseableRequest conn) (onParseableRequest conn) mRequest
        onUnparseableRequest :: Socket -> IO ()
        onUnparseableRequest conn = do
          sendAll conn (toByteString response400)
        onParseableRequest :: Socket -> Request -> IO ()
        onParseableRequest = undefined
