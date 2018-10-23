#!/usr/bin/env stack
{- stack
   --resolver lts-11.7
   --install-ghc
   runghc
   --package bytestring
   --package network
   --package data-default-class
   --package tls
   --
-}
--   -Wall -Werror

-- Issue with stack: Version 1.7.1
-- Git revision 681c800873816c022739ca7ed14755e85a579565 x86_64 hpack-0.28.2
-- the following flags after -- aren't read anymore and are just sent as extra
-- arguments which are caught by getArgs. Therefore, they are outcommented

--------------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main (main) where

--------------------------------------------------------------------------------

import           Control.Exception
  ( IOException
  , try
  )
import           Control.Concurrent
  ( forkIO
  )
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
  ( fromMaybe
  , listToMaybe
  )
import           Data.Default.Class
  ( def
  )
import           Network.Socket             hiding
  ( recv
  , send
  )
import qualified Network.TLS                as T
import qualified Network.TLS.Extra          as TE
import           System.Environment
  ( getArgs
  )

--------------------------------------------------------------------------------

tlsPort
  :: IO PortNumber

recv
  :: T.Context
  -> IO (Either IOException BS.ByteString)
  
send
  :: T.Context
  -> [ BS.ByteString ]
  -> IO (Either IOException ())

pong
  :: T.Context
  -> IO ()

spawn
  :: (Socket, SockAddr)
  -> T.Credentials
  -> IO ()

loop
  :: Socket
  -> Either String T.Credential
  -> IO ()

server
  :: IO ()

main
  :: IO ()

--------------------------------------------------------------------------------

main =
  server

--------------------------------------------------------------------------------

tlsPort =
  getArgs >>= pure . fromMaybe 8443 . listToMaybe . (map read) 

recv ctx =
  try $ T.recvData ctx

send ctx bs =
  try $ T.sendData ctx $ L8.fromChunks $ bs

pong ctx =
  do
    res <- recv ctx
    case Right "ping" == res of
      False -> T.contextClose ctx
      True  ->
        do
          req <- send ctx $ [ "pong" ]
          case Right () == req of
            False -> T.contextClose ctx
            True  -> pong ctx

spawn (sock, _) creds =
  do
    ctx <- T.contextNew sock $ para creds
    ___ <- T.handshake  ctx
    pong ctx
  where
    para x509 =
      def
      { T.serverWantClientCert = False
      , T.serverShared         = shared
      , T.serverSupported      = supported
      }
      where
        shared =
          def
          { T.sharedCredentials = x509
          }
        supported =
          def
          { T.supportedVersions = [ T.TLS12 ]
          , T.supportedCiphers  = ciphers
          }
        ciphers =
          [ TE.cipher_AES128_SHA1
          , TE.cipher_AES256_SHA1
          , TE.cipher_RC4_128_MD5
          , TE.cipher_RC4_128_SHA1
          ]

loop sock (Right creds) =
  do
    conn <- accept $ sock
    putStrLn $ ("Connected to: " ++) $ show $ snd $ conn
    ____ <- forkIO $ spawn conn $ T.Credentials [creds]
    loop sock $ Right creds
loop ____ (Left msg) =
  putStrLn $ msg 

server =
  do
    port <- tlsPort
    x509 <- T.credentialLoadX509 "../tls/localhost.crt" "../tls/localhost.key"
    -- x509 <- T.credentialLoadX509 "/Users/tim.siwula/Documents/Projects/haskell-p2p/tls/localhost.crt" "/Users/tim.siwula/Documents/Projects/haskell-p2p/tls/localhost.key"

    sock <- socket AF_INET Stream 0
    ____ <- setSocketOption sock ReuseAddr 1
    ____ <- bind sock $ SockAddrInet port iNADDR_ANY
    ____ <- listen sock 256

    putStrLn $ "Listening on port " ++ show port

    loop sock x509
