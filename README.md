# Simple Session Management for Snap Framework Applications

This package is intended as a simple session management system for Snap
applications.


We define a type Session as

    type Session = Map ByteString ByteString

which gives us all the convenience and power of Haskell's standard Map library.

It is yet to be seen if this is effective and/or efficient - any feedback would
be appreciated.


## Backends

There is currently a single back-end: Snap.Extension.Session.CookieSession. It
uses Data.Serialize to serialize the Session data type and Michael Snoyman's
Web.ClientSession to encrypt the cookie. 

Please see its Haddock documentation for more information.


## Other Backends

The idea would be to add various other back-ends as desired. Redis, MongoDB,
SQL-based databases, etc. should all be straightforward enough to implement.
