# Session Store Management for Snap Framework

## Requirements

This package currently requires the bleeding edge snap-core and snap-server (>
0.3) libraries, as it uses some cookie functionality that is due to be released
soon.

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

It is yet to be seen if this is effective and/or efficient in the long run but
has worked well so far.


## The Concept

User/session management has two basic levels (potentially more if you add
permissions/roles/etc.):

  - Making sure an established session between any user - authenticated or
  	otherwise - user and the server stays secure.
  - Authenticating users, which means having proof that a user is who she says
  	she is before we grant her some important priveleges in our applications.

This module tackles the first of these challenges. A separate Snap.Auth module
builds on the foundation here to provide the authentication-specific
functionality.

Both of these packages will be incorporated into Snap, likely starting with the
0.5 release.

## Usage Example

Let's assume we have an odd desire to persist our user's age in our
application's session store: 

    import qualified Data.Map as M
    import           Snap.Extension.Session
    

    ...

    myHandler = do
      setInSession "user_age" "32" -- that's all we have to do!
      render "pages/myPage"

The "user_age" session field will now be available in this user's session until
we delete it or expire the session.

We can now retrieve it at any point with:
    
    myHandler2 = do
      uage <- getFromSession "user_age"
      doSomethingWithUid uage
      render "pages/myPage2"


## Setting Up Your Application With extension-session

Let's setup the session functionality using the CookieSession backend.
    
    -- Define a field to hold the session state in your application state
    data ApplicationState = ApplicationState
      { appSessionSt :: CookieSessionState }

    -- Instantiate your app as a MonadSession
    instance HasCookieSessionState ApplicationState where
      getCookieSessionState = appSessionSt
      setCookieSessionState cs as = as { appSessionSt = cs }

    -- Add some simple initializer code
    appInit :: Initializer ApplicationState
    appInit = do
      cs <- cookieSessionStateInitializer $ defCookieSessionState
              { csKeyPath = "config/site-key.txt" 
              , csCookieName = "myapp-session" }
      return $ ApplicationState cs


And you are done. While you have to do this manually for now, we will in the
future have the @snap@ executable auto-generate some of this boiler plate
for you.



## Backends


### CookieSession

There is currently a single back-end: Snap.Extension.Session.CookieSession. It
uses Data.Serialize to serialize the Session data type and Michael Snoyman's
Web.ClientSession to encrypt the cookie. The cookie is encrypted, which means
it is fully secure and can't be read by the client/end-user.

Since this method has no need for a DB back-end, it works out of the box and is
pretty much the simplest session persistence back-end to use. For those
familiar, this method is the default behavior in Ruby on Rails as well.

Please see the Haddock documentation for more information.


## Other Backends

The idea would be to add various other back-ends as desired. Redis, MongoDB,
SQL-based databases, etc. should all be straightforward enough to implement. We
would just need a scheme to presist the session type in the respective
database.


# TODO/ROADMAP

## General Interface improvements
- Authenticity token support to prevent CSRF attacks 
- Timestamping option to limit replay attacks in time 
- Splices/handlers for easy CSRF protection token integration:
  - csrf_meta_tag for unobtrusive JS based binding to forms (like in Rails 3)
  - csrf_token_tag for a hidden field inside forms
  - verify_authenticity handler to be chained before your destructive handlers

## Planned Back-ends
- HDBC-based SQL back-ends once extension-hdbc is in place
- MongoDB backend


## Open Questions/Considerations
- Possibility of using JSON-like datatype for session store.
