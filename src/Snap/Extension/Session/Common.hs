{-|

  This module contains functionality common among multiple back-ends.

-}

module Snap.Extension.Session.Common where


import           Numeric
import           Random

import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as Map
import           Data.Time
import           System.Locale

import           Codec.Utils
import           Data.Digest.SHA512

import           Snap.Types
import           Snap.Extension.Session




------------------------------------------------------------------------------
-- | Insert an authenticity token against CSRF into the session.
setAuthenticityToken :: (MonadSnap m) => Session -> m Session
setAuthenticityToken s = 
  case Map.lookup "_csrf_token" s of
    Just _ -> return s
    Nothing -> do
      t <- liftIO randomToken
      return $ Map.insert "_csrf_token" t s


------------------------------------------------------------------------------
-- | Insert a timestamp into the session.
setTimeStamp :: (MonadSnap m) => Session -> m Session
setTimeStamp s = do
  t <- liftIO $ getCurrentTime
  let t' = B.pack $ formatTime defaultTimeLocale "%s" t
  return $ Map.insert "_snap_ts" t' s


------------------------------------------------------------------------------
-- | Validate session against timeout policy.
--
-- * If timeout is set to 'Nothing', never trigger a time-out.
-- * If no timestamp is present in session, immediately trigger time-out.
-- * Othwerwise, do a regular time-out check based on current time and
-- timestamp.
checkTimeout :: (MonadSnap m) => Maybe Int -> Session -> m Bool
checkTimeout Nothing _ = return False
checkTimeout (Just x) s = 
  let t0str = B.unpack `fmap` Map.lookup "_snap_ts" s
      t0 = t0str >>= parseTime defaultTimeLocale "%s"
      x' = fromIntegral x
  in do
      t1 <- liftIO getCurrentTime
      return $ case t0 of
        Nothing -> True -- Timeout not set in cookie, reject immediately
        Just (t0') -> t1 > addUTCTime (x' * 60) t0'


------------------------------------------------------------------------------
-- | Generates a random salt.
randomToken :: IO ByteString
randomToken = do
    chars <- sequence $ take 15 $ repeat $
        randomRIO (0::Int,15) >>= return . flip showHex ""
    return $ B.pack $ concat chars

