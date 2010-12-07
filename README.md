Snap Extension for Simple Session Management
============================================

This package is intended as a simple session management system for Snap
applications.


We define a type Session type as

  type Session = Map ByteString ByteString

which gives us all the convenience and power of Haskell's standar Map library.

It is yet to be seen if this is effective and/or efficient - any feedback would
be appreciated.

