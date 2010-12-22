{-|
 
-}

module Snap.Extension.Session
  ( MonadSession(..)
  , Session

    -- * Higher Level Functions
  , getFromSession
  , setInSession
  , deleteFromSession
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
-- | The 'MonadCookieSession' class. Minimal complete definition:
class MonadSnap m => MonadSession m where

  ----------------------------------------------------------------------------
  -- | Function to get the session in your app's monad.
  --
  -- This will return a @Map ByteString ByteString@ data type, which you can
  -- then use freely to read/write values. Remember to commit it back with
  -- 'setSession' if you want to persist changes.
  getSession :: m Session


  ----------------------------------------------------------------------------
  -- | Function to set the session in your app's monad.
  setSession :: Session -> m ()


------------------------------------------------------------------------------
-- | Get a value associated with given key from the 'Session'.
getFromSession :: MonadSession m => ByteString -> m (Maybe ByteString)
getFromSession k = Map.lookup k `liftM` getSession


------------------------------------------------------------------------------
-- | Remove the given key from 'Session'
deleteFromSession :: MonadSession m => ByteString -> m ()
deleteFromSession k = Map.delete k `liftM` getSession >>= setSession


------------------------------------------------------------------------------
-- | Set a value in the 'Session'.
setInSession :: MonadSession m 
             => ByteString 
             -> ByteString 
             -> m ()
setInSession k v = Map.insert k v `liftM` getSession >>= setSession

