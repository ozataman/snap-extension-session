{-|

  This module provides an implementation of 'Snap.Extension.Session' using
  secure cookies shuttled back-and-forth between the web server and the user of
  your application.

  The resulting cookie contents will not be readable to the end-user. However,
  you should still never put critical information inside the session. Storing
  a user_id may be fine, but never put, say the remaining balance on an account
  in a session.

  Note that this method leaves your system open to replay, aka session
  hi-jacking attacks. To prevent this, consider always on SSL.

-}

module Snap.Extension.Session.CookieSession
  ( 
    module Snap.Extension.Session
  , CookieSessionState(..)
  , defCookieSessionState
  , HasCookieSessionState(..)
  , cookieSessionStateInitializer
  ) where

import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import qualified Data.Map as M
import           Data.Monoid

import           Data.Serialize
import           Snap.Extension
import           Snap.Types
import           Web.ClientSession

import           Snap.Extension.Session
import           Snap.Extension.Session.Common


------------------------------------------------------------------------------
-- | 
data CookieSessionState = CookieSessionState
  { csKey :: Key                    -- ^ Cookie encryption key
  , csKeyPath :: FilePath           -- ^ Where the encryption key is stored
  , csCookieName :: ByteString      -- ^ Cookie name for your app
  , csTimeout :: Maybe Int          -- ^ Timeout in minutes
  , csAuthToken :: Bool             -- ^ Keep authenticity token in session
  }


------------------------------------------------------------------------------
-- | 'defCookieSessionState' is a good starting point when initializing your
-- app. The default configuration is:
--
-- > csKeyPath = "site_key.txt"
-- > csCookieName = "snap-session"
-- > csTimeout = Just 30
-- > csAuthToken = True
defCookieSessionState :: CookieSessionState
defCookieSessionState = CookieSessionState 
                          { csKeyPath = "site_key.txt"
                          , csKey = ""
                          , csCookieName = "snap-session"
                          , csTimeout = Just 30
                          , csAuthToken = True }


------------------------------------------------------------------------------
-- |
class HasCookieSessionState s where

  ----------------------------------------------------------------------------
  -- | Getter to get 'CookieSessionState' from your app's state.
  getCookieSessionState :: s -> CookieSessionState


  ----------------------------------------------------------------------------
  -- | Setter to inject 'CookieSessionState' into your app's state.
  setCookieSessionState :: CookieSessionState -> s -> s



------------------------------------------------------------------------------
-- | Initializes the given 'CookieSessionState'. It will read the encryption
-- key if present, create one at random and save if missing.
cookieSessionStateInitializer 
  :: CookieSessionState
  -> Initializer CookieSessionState
cookieSessionStateInitializer cs = do
  st <- liftIO $ do
    k <- getKey (csKeyPath cs) 
    return $ cs { csKey = k }
  mkInitializer st


------------------------------------------------------------------------------
-- | Register CookieSessionState as an Extension.
instance InitializerState CookieSessionState where
  extensionId = const "Session/CookieSession"
  mkCleanup = const $ return ()
  mkReload = const $ return ()


------------------------------------------------------------------------------
-- |
instance HasCookieSessionState s => MonadSession (SnapExtend s) where

  ----------------------------------------------------------------------------
  -- | Serialize the session, inject into cookie, modify response.
  --
  setSession s = do
    cs <- asks getCookieSessionState
    s' <- setSessionParams s cs
    let val = encrypt (csKey cs) . encode $ s'
    let nc = Cookie (csCookieName cs) val Nothing Nothing (Just "/")
    modifyResponse $ addResponseCookie nc
    

  ----------------------------------------------------------------------------
  -- | Read the session from the cookie. If none is present, return empty map.
  getSession = do
    cs <- asks getCookieSessionState
    let key = csKey cs
    let cn = csCookieName cs
    rqCookie <- getCookie cn
    rspCookie <- getResponseCookie cn `fmap` getResponse
    let ck = rspCookie `mplus` rqCookie
    let val = fmap cookieValue ck >>= decrypt key >>= return . decode
    let val' = maybe M.empty (either decodeFail id) val
    to <- checkTimeout (csTimeout cs) val'
    return $ case to of
      True -> M.empty
      False -> val'
    where 
      decodeFail = const $ 
        error "Data.Serialize: Could not decode contents of the cookie session."

------------------------------------------------------------------------------
-- | Deal with setting security related parameters based on supplied
-- preferences.
setSessionParams :: (MonadSnap m) 
                 => Session 
                 -> CookieSessionState 
                 -> m Session
setSessionParams s cs = do
  s' <- case csAuthToken cs of
    True -> setAuthenticityToken s
    False -> return s
  s'' <- case csTimeout cs of
    Just _ -> setTimeStamp s'
    Nothing -> return s'
  return s''
