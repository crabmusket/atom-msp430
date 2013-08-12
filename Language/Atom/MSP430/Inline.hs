{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module Language.Atom.MSP430.Inline where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Atom
import Data.String.Utils

c = QuasiQuoter {
    quoteExp = \code -> [| action (\_ -> code) [] |]
 }

asm = QuasiQuoter {
    quoteExp = \code -> [| action (\_ ->
        unlines $ map (\l -> "__asm__(\"" ++ (strip l) ++ "\");") $ lines $ code) [] |]
 }

