LambdaCat - the Haskell online cat viewer
=========================================

Driven by the frustrating modern browsers cause. We started
to think about what a browser should be for us. 

Table of Contents
-----------------

1. Mission Statement
2. Release Notes
3. Configuration 

1. Mission Statement
--------------------

A browser to use should be fast, configurable, extendable , secure and
easily useable with keyboard only. 

By extendables and configurable, we mean that we like to change behaviour
of the application on source level easily. It should be possible
to engage in nearly every data flow. 

With security, we mean that every communication interaction with
the net should be controlable. Every communication should be 
deniable by default. 

We like to use keyboard only application, since the mouse -> keyboard 
movement is time intensive and usally breaks any workflow it is not
acceptable!

2. Release Notes 
----------------

### 0.1.0 Haskell the Cat
First code release. Its main purpose is to demonstrate our software
design to the peoaple. The API is currently not stable and will
change during the way to version 0.2. 

The used UI is the GladeUI, currently the ui can not be changed in the
configuration. 

This release is _not_ for production use. Please be aware of that. 

3. Configuration
---------------

A user related configuration can be created in 

  ~/.config/lambdacat/lambdacat.hs

a good example how this file should look like is the 'Main.hs'.
For more information look into the lambdacat API.
