module TcpEcho.Server where

import qualified Control.Exception as E
import Control.Concurrent (forkFinally)
import Control.Monad (void, forever)
import Network.Socket (AddrInfo(..), close, defaultHints, SocketType(..), socket, AddrInfoFlag(..), bind, listen, setSocketOption, SocketOption(..), accept, getAddrInfo)
import Network.Socket.ByteString (recv, sendAll)
import Data.Time.Clock (getCurrentTime)
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  addr <- resolve "3000"
  E.bracket (open addr) close loop
  where resolve port = do
          let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream } -- we are going to use this information to create the socket
          addr:_ <- getAddrInfo (Just hints) Nothing (Just port) -- even though we do not want to open a socket to a remote server, we still need to use the DNS service
          return addr
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) -- create the socket
          setSocketOption sock ReuseAddr 1 -- I do not know what this does
          bind sock (addrAddress addr) -- Each socket has two endpoints, a remote and a local. Here, we are using bind to connect our socket to the local port 3000
          listen sock 10 -- listen for connections on this socket; It is NOT the same as recv. The latter is used by the client to receive the data send by the server
          return sock
        loop sock = forever $ do -- repeat the IO action forever
                      (conn, peer) <- accept sock -- this is another part of the socket API; notice that the socket must be bound and must be listening; conn is the new socket with a logical direct connection to the other host machine
                      putStrLn $ "Connection from " ++ show peer
                      void $ forkFinally (talk conn) (\_ -> close conn) -- do the work in a different thread, and no matter what close the connection
        talk conn = do
          clientMsg <- recv conn 4096
          currentTime <- getCurrentTime
          let msg = "Received msg: " ++ C.unpack clientMsg ++ " at " ++ show currentTime ++ "."
          sendAll conn (C.pack msg)
