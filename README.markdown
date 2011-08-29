WebRadar Document
=================

A template from which to create a WebRadar technical document.

Setting up
==========

* The following executables must the available on the `PATH`
  * java
  * rsync
  * wget 
  * xsltproc
  * aspell
  * xmllint
  * tar

* The following packages must be installed from Haskell hackage
  * FilePather
  * MissingH

* Docbook 4.5 catalog files must be installed

Ubuntu/Debian shortcut for setting up
-------------------------------------

Run this command to get started:
> `sudo apt-get install openjdk-6-jdk ghc6 cabal-install rsync wget xsltproc docbook-xml aspell libxml2-utils tar && cabal update && cabal install cabal-install && cabal install FilePather && cabal install MissingH`

