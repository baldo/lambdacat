LambdaCat - the Haskell online cat viewer
=========================================

Driven by the frustration modern browsers caused us we started to think about
how a browser should be.

Table of Contents
-----------------

1. Mission Statement
2. Release Notes
3. Installation
4. Configuration

1. Mission Statement
--------------------

To us, a browser should be fast, configurable, extendable, secure and easy to
use with keyboard only. The most important feature is the ability to
comfortably display online cat-content.

By extendable and configurable we mean that we like to easily change the
behaviour of the application in sourcecode. It should be possible to engage in
nearly every data flow by adopting the configuration.

With security, we mean that every communication and interaction with the net
should be controllable. Every communication should be deniable by default.

We like to use keyboard only applications since the mouse to keyboard movement
is time intensive and usually breaks the workflow. This is not acceptable.

Till now the mission is _not_ yet accomplished.

2. Release Notes
----------------

### 0.1.0 Haskell the Cat

This is the first code release. Its main purpose is to demonstrate our
software design. The API is currently not stable and is subject to change.

For now we only provide a Glade base UI (GladeUI). In the future we plan to
add further UIs which can be chosen in the configuration.

This release is _not_ for production use. Please be aware of that.

### 0.1.1 Cleanup Haskell the Cat 

Add some more informations to the cabal file. 

3. Installation
---------------

Lambdacat can be installed from [Hackage](http://hackage.haskell.org) using 
'cabal'. 

    > cabal update
    > cabal install lambdacat

You can also checkout the sources from github and compile them yourself.

After that the binary of lambdacat can be found in 

    ~/.cabal/bin

For everyday use you should include this path into your PATH enviroment
variable.

### Installing gtk2hs

If you have not installed the haskell gtk bindings you should do previous to 
the steps above:

    > cabal update
    > export PATH=$PATH:~/.cabal/bin
    > cabal install alex
    > cabal install happy
    > cabal install gtk2hs-buildtools

4. Configuration
----------------

A user configuration can be put in:

  ~/.config/lambdacat/lambdacat.hs

This file has to provide a 'main' function that then invokes the function
'lambdacat'.

A small example how this file might look like is the 'Main.hs'.
For further information please have a look at the haddock generated API
documentation.

Meow.

