-- file: applicativeParsec.hs

module ApplicativeParsec
(
  module Control.Applicative
, module Text.ParserCombinators.Parsec
, initialPos
, module Text.ParserCombinators.Parsec.Error
)
    where

import Control.Applicative

-- Hide a few names that are provided by Applicative.
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>), ParseError, errorPos)

import Text.ParserCombinators.Parsec.Pos(initialPos)

import Text.ParserCombinators.Parsec.Error
