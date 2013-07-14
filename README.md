tesseractH
=============

Haskell binding to tesseract C API.

This project provides a low level binding to tesseract-ocr, an open source ocr library. Since version 3.0, tesseract-ocr now requires leptonica for image reading, and some internal data structures, so we also provide a minimal set bindings to a few functions from leptonica. We use the c2hs DSL to generate the bindings. 

Why this wrapper? Currently if you want to use tesseract you must use the command line interface. This can be slow and clunky if you want to perform ocr on a number of regions of interest. The goal is to accomplish the task without temp files and system calls. The tesseract and leptonica libraries have lots of other functionality, so the goal might expand depending on contributors' interests. I needed this for a project that uses Alberto Ruiz's easyVision project, but I think this library could be useful as an independent package. Any functionality that depends on easyVision will be added to that project as another module. 

System library requirements (as of May 2013):
tesseract-ocr 3.02.02 
leptonica 1.68

Installation (tested on Ubuntu 12.04):

1) First get leptonica-dev
$ sudo apt-get install leptonica-dev

2) The C API offered by tesseract in its current form is new as of v3.02.02.
Therefore compiling from source is required. It is fairly easy.

$ svn checkout http://tesseract-ocr.googlecode.com/svn/trunk/ tesseract-ocr-read-only
$ cd tesseract-ocr-read-only

3) Next follow the instructions in INSTALL.SVN (don't forget the last step!)

4) Install this package. Assuming you have git cloned this project...

$ cabal install

4) Run a test on a sample file. One is included. 

$ cd tesseractH
$ runhaskell test phototest.tif

You should see some output and are ready to hack.

The module TesseractH is intended to provide a haskellish interface.
The file TesseractH.CAPI.chs contains c2hs code required to generate the Tesseract.CAPI module.
One tip in using c2hs is to look in the dist directory to see the actual code generated. This can be helpful in chasing down problems.
