{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Language.Atom.MSP430.Inline where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Atom

c = QuasiQuoter {
    quoteExp = \code -> [| action (\_ -> code) [] |]
 }
