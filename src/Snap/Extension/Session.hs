{-|
 
-}

module Snap.Extension.Session
  ( MonadSession(..)
  , Session
  ) where

import           Control.Monad
import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import qualified Data.Map as Map

import           Snap.Types


------------------------------------------------------------------------------
-- | Base session on the fast and capable Map library.
-- TODO: Does this have a large performance hit?
type Session = Map ByteString ByteString


------------------------------------------------------------------------------
-- | The 'MonadCookieSession' class. Minimum complete definition: 'getSession'
-- and 'setSession'.
class MonadSnap m => MonadSession m where

  ----------------------------------------------------------------------------
  -- | Function to get the session in your app's monad.
  --
  -- This will return a @Map ByteString ByteString@ data type, which you can
  -- then use freely to read/write values. 
  getSession :: m Session


  ----------------------------------------------------------------------------
  -- | Set the session in your app's monad.
  setSession :: Session -> m ()


  ------------------------------------------------------------------------------
  -- | Get a value associated with given key from the 'Session'.
  getFromSession :: ByteString -> m (Maybe ByteString)
  getFromSession k = Map.lookup k `liftM` getSession


  ------------------------------------------------------------------------------
  -- | Remove the given key from 'Session'
  deleteFromSession :: ByteString -> m ()
  deleteFromSession k = Map.delete k `liftM` getSession >>= setSession


  ------------------------------------------------------------------------------
  -- | Set a value in the 'Session'.
  setInSession :: ByteString 
               -> ByteString 
               -> m ()
  setInSession k v = Map.insert k v `liftM` getSession >>= setSession


  ----------------------------------------------------------------------------
  -- | Clear the active session. Uses 'setSession'.
  clearSession :: m ()
  clearSession = setSession Map.empty


  ----------------------------------------------------------------------------
  -- | Touch session to reset the timeout. You can chain a handler to call this
  -- in every authenticated route to keep prolonging the session with each
  -- request.
  touchSession :: m ()
  touchSession = getSession >>= setSession



