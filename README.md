# Simple Session Management for Snap Framework Applications

This package is intended as a simple session management system for Snap
applications.


We define a type Session as

    type Session = Map ByteString ByteString

which gives us all the convenience and power of Haskell's standard Map library.

It is yet to be seen if this is effective and/or efficient - any feedback would
be appreciated.

## Usage Example

Let's add a user id to our session and persist it:

    import qualified Data.Map as M
    import           Snap.Extension.Session
    

    ...

    myHandler = do
      s <- getSession
      let newSession = M.insert "user_id" "1456" s
      setSession s
      render "pages/myPage"

The "user_id" field will now be available in this user's session until we
delete it or expire the session.

## Backends

There is currently a single back-end: Snap.Extension.Session.CookieSession. It
uses Data.Serialize to serialize the Session data type and Michael Snoyman's
Web.ClientSession to encrypt the cookie. 

Please see its Haddock documentation for more information.


## Other Backends

The idea would be to add various other back-ends as desired. Redis, MongoDB,
SQL-based databases, etc. should all be straightforward enough to implement.


# TODO

* Add convenience functions to clear the session.
* Increase security: Maybe add a random token into encrypted cookie (or
  something like that to the same effect) to reduce the likelihood of somebody
  guessing sessions with very little in them. 
