{-|
Module : SwaggerPetstore.Client
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module SwaggerPetstore.Client where

import SwaggerPetstore.Model
import SwaggerPetstore.API
import SwaggerPetstore.MimeTypes

import qualified Control.Monad.IO.Class as P
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.Proxy as P (Proxy(..))
import Data.Function ((&))
import Data.Monoid ((<>))
import Data.Text (Text)
import GHC.Exts (IsString(..))
import Web.FormUrlEncoded as WH
import Web.HttpApiData as WH
import Control.Monad.Catch (MonadThrow)

import qualified Control.Monad.Logger as LG

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Text.Printf as T

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BCL
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Client as NH
import qualified Network.HTTP.Client.TLS as NH
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Types.Method as NH
import qualified Network.HTTP.Types as NH
import qualified Network.HTTP.Types.URI as NH

-- * Config

data SwaggerPetstoreConfig = SwaggerPetstoreConfig
  { configHost  :: BCL.ByteString
  , configUserAgent :: Text
  , configExecLoggingT :: ExecLoggingT
  , configFilterLoggingT :: LG.LogSource -> LG.LogLevel -> Bool
  }

instance Show SwaggerPetstoreConfig where
  show c =
    T.printf
      "{ configHost = %v, configUserAgent = %v, ..}"
      (show (configHost c))
      (show (configUserAgent c))

mkConfig :: SwaggerPetstoreConfig
mkConfig =
  SwaggerPetstoreConfig
  { configHost = "http://petstore.swagger.io/v2"
  , configUserAgent = "swagger-haskell-http-client/1.0.0"
  , configExecLoggingT = runNullLoggingT
  , configFilterLoggingT = infoLevelFilter
  }

withStdoutLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withStdoutLogging p = p { configExecLoggingT = LG.runStdoutLoggingT}

withStderrLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withStderrLogging p = p { configExecLoggingT = LG.runStderrLoggingT}

withNoLogging :: SwaggerPetstoreConfig -> SwaggerPetstoreConfig
withNoLogging p = p { configExecLoggingT = runNullLoggingT}

-- * Dispatch

data MimeResponse res =
  MimeResponse { mimeResponseHttp :: NH.Response BCL.ByteString
               , mimeResponseResult :: Either SwaggerPetstoreError res
               }
  deriving (Show)

-- | returns both the underlying http response and the parsed result ('MimeResponse')
dispatchReq
  :: (Produces req accept, MimeUnrender accept res)
  => NH.Manager -- ^ http-client Connection manager
  -> accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (MimeResponse res) -- ^ response
dispatchReq manager accept config request = do
  httpResponse <- dispatchReqLbs manager accept config request
  let parsedResult =
        case mimeUnrender' accept (NH.responseBody httpResponse) of
          Left s -> Left (SwaggerPetstoreError s httpResponse)
          Right r -> Right r
  return (MimeResponse httpResponse parsedResult)

-- | like 'dispatchReq', but only returns the parsed result
dispatchReqRes
  :: (Produces req accept, MimeUnrender accept res)
  => NH.Manager -- ^ http-client Connection manager
  -> accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (Either SwaggerPetstoreError res) -- ^ response
dispatchReqRes manager accept config request = do
    MimeResponse _ parsedResult <- dispatchReq manager accept config request
    return parsedResult

-- | like 'dispatchReq', but only returns the underlying http response
dispatchReqLbs
  :: (Produces req accept, MimeUnrender accept res)
  => NH.Manager -- ^ http-client Connection manager
  -> accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchReqLbs manager accept config request = do
  initReq <- _toInitRequest accept config request 
  dispatchInitLbsUnsafe manager config initReq

-- | like 'dispatchReqLbs', but does not validate the operation is a 'Producer' of the "accept" 'MimeType'.  (Useful if the server's response is undocumented)
dispatchReqLbsUnsafe
  :: MimeType accept 
  => NH.Manager -- ^ http-client Connection manager
  -> accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchReqLbsUnsafe manager accept config request = do
  initReq <- _toInitRequest accept config request 
  dispatchInitLbsUnsafe manager config initReq

-- | like 'dispatchReqLbsUnsafe', but does not add an "accept" header.
dispatchReqLbsUnsafeRaw
  :: NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchReqLbsUnsafeRaw manager config request = do
  initReq <- _toInitRequest MimeNoContent config request 
  dispatchInitLbsUnsafe manager config initReq

-- | dispatch an InitRequest
dispatchInitLbsUnsafe
  :: NH.Manager -- ^ http-client Connection manager
  -> SwaggerPetstoreConfig -- ^ config
  -> InitRequest req contentType res accept -- ^ init request
  -> IO (NH.Response BCL.ByteString) -- ^ response
dispatchInitLbsUnsafe manager _ (InitRequest req) = do
  NH.httpLbs req manager
  
-- * InitRequest

-- | wraps an http-client 'Request' with request/response type parameters
newtype InitRequest req contentType res accept = InitRequest
  { unInitRequest :: NH.Request
  } deriving (Show)

-- |  Build an http-client 'Request' record from the supplied config and request
_toInitRequest
  :: MimeType accept
  => accept -- ^ "accept" 'MimeType'
  -> SwaggerPetstoreConfig -- ^ config
  -> SwaggerPetstoreRequest req contentType res -- ^ request
  -> IO (InitRequest req contentType res accept) -- ^ initialized request
_toInitRequest accept config req0 = do
  parsedReq <- NH.parseRequest $ BCL.unpack $ BCL.append (configHost config) (BCL.concat (urlPath req0))
  let req1 = _addAcceptHeader req0 accept  
      reqHeaders = ("User-Agent", WH.toHeader (configUserAgent config)) : paramsHeaders (params req1)
      reqQuery = NH.renderQuery True (paramsQuery (params req1))
      pReq = parsedReq { NH.method = (rMethod req1)
                       , NH.requestHeaders = reqHeaders
                       , NH.queryString = reqQuery
                       }
  outReq <- case paramsBody (params req1) of
    ParamBodyNone -> pure (pReq { NH.requestBody = mempty })
    ParamBodyB bs -> pure (pReq { NH.requestBody = NH.RequestBodyBS bs })
    ParamBodyBL bl -> pure (pReq { NH.requestBody = NH.RequestBodyLBS bl })
    ParamBodyFormUrlEncoded form -> pure (pReq { NH.requestBody = NH.RequestBodyLBS (WH.urlEncodeForm form) })
    ParamBodyMultipartFormData parts -> NH.formDataBody parts pReq

  pure (InitRequest outReq)

-- | convenience method for modifying the underlying Request
modifyInitRequest :: InitRequest req contentType res accept -> (NH.Request -> NH.Request) -> InitRequest req contentType res accept 
modifyInitRequest (InitRequest req) f = InitRequest (f req)

-- | convenience method for modifying the underlying Request (monadic)
modifyInitRequestM :: Monad m => InitRequest req contentType res accept -> (NH.Request -> m NH.Request) -> m (InitRequest req contentType res accept)
modifyInitRequestM (InitRequest req) f = fmap InitRequest (f req)

-- * Error

data SwaggerPetstoreError =
  SwaggerPetstoreError {
    parseError   :: String
  , reponseError :: NH.Response BCL.ByteString
  } deriving (Eq, Show)

-- * Logging

-- | runs the logger
type ExecLoggingT = forall m. P.MonadIO m =>
                              forall a. LG.LoggingT m a -> m a

-- ** Null Logger

nullLogger :: LG.Loc -> LG.LogSource -> LG.LogLevel -> LG.LogStr -> IO ()
nullLogger _ _ _ _ = return ()

runNullLoggingT :: LG.LoggingT m a -> m a
runNullLoggingT = (`LG.runLoggingT` nullLogger)

-- ** Logging Filters

errorLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
errorLevelFilter = minLevelFilter LG.LevelError

infoLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
infoLevelFilter = minLevelFilter LG.LevelInfo

debugLevelFilter :: LG.LogSource -> LG.LogLevel -> Bool
debugLevelFilter = minLevelFilter LG.LevelDebug

minLevelFilter :: LG.LogLevel -> LG.LogSource -> LG.LogLevel -> Bool
minLevelFilter l _ l' = l' >= l
