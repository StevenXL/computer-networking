module TcpEcho.Client where

import qualified Control.Exception as E
import Network.Socket (AddrInfo(..), close, defaultHints, SocketType(Stream), getAddrInfo, socket, connect)
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString as B

main :: IO ()
main = do
  addr <- resolve "127.0.0.1" "3000"
  E.bracket (open addr) close talk
  where resolve host port = do
          let hints = defaultHints { addrSocketType = Stream }
          addr:_ <- getAddrInfo (Just hints) (Just host) (Just port) -- getAddrInfo must use DNS to get back the IP address of the server
          return addr
        open addr = do
          sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr) -- socket creates the socket from the address information; the socket doesn't take an address, because it is not connected
          connect sock (addrAddress addr) -- now that we have a socket, we have to connect that socket to the server's socket
          return sock
        talk sock = do
          sentence <- B.getLine
          sendAll sock sentence
          serverReply <- recv sock 4096
          B.putStr serverReply
