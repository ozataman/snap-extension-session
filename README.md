# Simple Session Store for Snap Framework Applications

## Requirements

This package currently requires the bleeding edge snap-core and snap-server (>
0.3) libraries, as it uses some cookie functionality that is due to be released
in a near release.

## Introduction

This package is intended as a simple session persistence framework for Snap
applications. For those familiar with Rails, the functionality is similar to 
    
    session[:user_id] = 1234
    session[:last_query] = "johnpollak"

The difference, however, is that we can't just store arbitrary data -types and
instead use only ByteStrings.


We define a type Session as

    type Session = Map ByteString ByteString

which gives us all the convenience and power of Haskell's standard Map library.

It is yet to be seen if this is effective and/or efficient - any feedback would
be appreciated.

## Usage Example

Let's assume we have somehow authenticated our user and would like to persist
the user's id in our session in a secure way: 

    import qualified Data.Map as M
    import           Snap.Extension.Session
    

    ...

    myHandler = do
      setInSession "user_id" "1456" -- that's all we have to do!
      render "pages/myPage"

The "user_id" field will now be available in this user's session until we
delete it or expire the session.

We can now retrieve it at any point with:
    
    myHandler2 = do
      uid <- getFromSession "user_id"
      render "pages/myPage2"


## Backends


### CookieSession

There is currently a single back-end: Snap.Extension.Session.CookieSession. It
uses Data.Serialize to serialize the Session data type and Michael Snoyman's
Web.ClientSession to encrypt the cookie. The cookie is encrypted, which means
it is fully secure and can't be read by the client/end-user.

Since this method has no need for a DB back-end, it works out of the box and is
pretty much the simplest session persistence to use. For those familiar, this
method is the Rails default as well.

Please see its Haddock documentation for more information.


## Other Backends

The idea would be to add various other back-ends as desired. Redis, MongoDB,
SQL-based databases, etc. should all be straightforward enough to implement. We
would just need a scheme to presist the session type in the respective
database.


# TODO

* Increase security: Maybe add a random token into encrypted cookie (or
  something like that to the same effect) to reduce the likelihood of somebody
  guessing sessions with very little in them. 
* Add convenience functions to clear session
* Add convenience functions to set session expiration time-out
