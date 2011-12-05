#!/bin/bash
runhaskell ./Setup.hs configure --enable-tests --ghc && runhaskell ./Setup.hs build
