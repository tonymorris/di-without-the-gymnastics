Dependency Injection Without the Gymnastics
================


To get started, perform the following:

* Rename docbook-document.iml to yourproject.iml
* Edit .idea/modules.xml and change to yourproject.iml
* Edit bin/pdfx and change to yourproject
* Edit bin/allx and change to yourproject
* Edit .ghci and change to yourproject
* Edit Build.hs and change to yourproject
* Edit README.markdown for notes specific to your project.

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

