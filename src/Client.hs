#!/usr/bin/env stack
{- stack
   --resolver lts-11.7
   --install-ghc
   runghc
   --package bytestring
   --package network
   --package time
   --package data-default-class
   --package tls
   --package x509
   --package x509-store
   --package x509-validation
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
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Maybe
  ( fromJust
  , fromMaybe
  , listToMaybe
  )
import           Data.Time
  ( defaultTimeLocale
  , formatTime
  , getCurrentTime
  )
import           Data.Word
  ( Word8
  )
import qualified Data.X509                  as X509
import           Data.X509.CertificateStore
  ( readCertificateStore
  )
import qualified Data.X509.Validation       as X509
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

iso8601
  :: IO String

recv
  :: T.Context
  -> IO (Either IOException BS.ByteString)
  
send
  :: T.Context
  -> [ BS.ByteString ]
  -> IO (Either IOException ())

ping
  :: T.Context
  -> IO ()

client
  :: IO ()

main
  :: IO ()

--------------------------------------------------------------------------------

main =
  client

--------------------------------------------------------------------------------

tlsPort =
  getArgs >>= pure . fromMaybe 8443 . listToMaybe . (map read) 

iso8601 =
  -- https://hackage.haskell.org/package/time-1.9.1/docs/Data-Time-Format.html
  getCurrentTime >>= pure . (formatTime defaultTimeLocale "%FT%T%0QZ")
          
recv ctx =
  try $ T.recvData ctx

send ctx bs =
  try $ T.sendData ctx $ L8.fromChunks $ bs

ping ctx = 
  do
    req <- send ctx [ "ping" ]
    case Right () ==  req of
      False -> T.contextClose ctx
      True  -> 
        do
          tsping <- iso8601
          putStrLn $ tsping ++ " | Client | Ping"
          res <- recv ctx
          case Right "pong" == res of
            False -> T.contextClose ctx
            True  ->
              do
                tspong <- iso8601
                putStrLn $ tspong ++ " | Server | Pong"
                ping ctx

client =
  do
    port <- tlsPort
    x509 <- cacs
    sock <- socket AF_INET Stream 0
    ____ <- connect sock $ SockAddrInet port (tupleToHostAddress host)

    putStrLn $ ("Connected to: " ++) $ name
    
    ctx <- T.contextNew sock $ para x509
    ___ <- T.handshake ctx
    
    ping ctx
      where
        cacs = readCertificateStore "../tls/root.ca.crt" >>= pure . fromJust
        host = (127, 0, 0, 1) :: (Word8, Word8, Word8, Word8)
        name = "localhost" :: HostName
        para x509 =
          ( T.defaultParamsClient name BS.empty )
          { T.clientSupported =
            def
            { T.supportedCiphers  = TE.ciphersuite_strong
            , T.supportedVersions = [ T.TLS12 ]
            }
          , T.clientShared =
            def
            { T.sharedCAStore = x509
            }
          , T.clientHooks = hook
          }
        hook =
          -- Disable checkLeafV3 when testing wit local created CAs
          -- github.com/vincenthz/hs-tls/issues/154#issuecomment-268083940
          def
          { T.onServerCertificate = leaf
          }
        leaf =
          X509.validate
          X509.HashSHA256
          X509.defaultHooks
          $ X509.defaultChecks { T.checkLeafV3 = False }
