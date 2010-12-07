{-|
 
-}

module Snap.Extension.Session
  ( MonadSession(..)
  , Session
  ) where

import qualified Data.ByteString.Char8 as B
import           Data.ByteString (ByteString)
import           Data.Map (Map)

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


