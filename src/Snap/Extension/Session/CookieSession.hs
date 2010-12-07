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
  ( CookieSessionState
  , HasCookieSessionState(..)
  , cookieStateInitializer
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


------------------------------------------------------------------------------
-- | 
data CookieSessionState = CookieSessionState
  { csKey :: Key 
  , csCookieName :: ByteString
  }


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
-- | Initializes the 'CookieSessionState' with the encryption key.
--
-- Provide a 'FilePath' to where the secure key can be found.
-- If the file is not found, a key will be generated and saved in its place.
cookieStateInitializer :: FilePath  -- ^ Path to key file.
                       -> ByteString  -- ^ Name of the session cookie.
                       -> Initializer CookieSessionState
cookieStateInitializer fp cn = do
  st <- liftIO $ do
    k <- getKey fp 
    return $ CookieSessionState k cn
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
  -- This cookie does not expire. 
  -- TODO: Implement expiration policy.
  setSession s = do
    cs <- asks getCookieSessionState
    let val = encrypt (csKey cs) . encode $ s
    let nc = Cookie (csCookieName cs) val Nothing Nothing (Just "/")
    modifyResponse $ addCookie nc
    

  ----------------------------------------------------------------------------
  -- | Read the session from the cookie. If none is present, return empty map.
  getSession = do
    cs <- asks getCookieSessionState
    ck <- getCookie (csCookieName cs)
    let val = fmap cookieValue ck >>= decrypt (csKey cs) >>= return . decode
    return $ maybe M.empty (either decodeFail id) val
    where 
      decodeFail = const $ 
        error "Data.Serialize: Could not decode contenst of the cookie session."
