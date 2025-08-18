{-|
Module      : Language.Fountain.Tokenizer.L0Token
Description : Fountain level-zero tokens
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX

A level zero token, i.e., one requiring no context.
In our case, this also means /line-level/
tokens. New line sequences are treated as a token.

Multi-line spanning boneyards are eliminated at this level.
-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Fountain.Tokenizer.L0Token where


import Data.Text ( Text )


-- |  Level zero token
data L0Token  = L0Newline         -- ^  A line change between blocks of regular text
              | L0Boneyard !Text  -- ^  A /boneyard/, a form of multi-line comment
                                  --      that can span even blank lines.
                                  --      Certain document formats can store hidden text,
                                  --      as which some converters might regard this.
              | L0Note !Text      -- ^  A /note/, other possibly multi-line comment
                                  --      that is actually meant to be preserved.
                                  --      Unlike boneyards, it cannot span blank
                                  --      lines. Many document formats store them.
              | L0Spaces !Text    -- ^  Spaces between text and punctuation.
              | L0Content !Text   -- ^  Actual text in the script.
  deriving (Eq, Ord, Show)


-- |  Extracts the content of any given L0Token
l0TokenContent :: L0Token -> Text
l0TokenContent L0Newline      = "\n"
l0TokenContent (L0Boneyard t) = t
l0TokenContent (L0Note t)     = t
l0TokenContent (L0Spaces t)   = t
l0TokenContent (L0Content t)  = t


-- |  Level zero token stream: just a list of 'L0Token's.
--    Conveniently, /Megaparsec/ turns lists automatically
--    into 'Text.Megaparsec.Stream.Stream's as long as the base type is an
--    instance of 'Prelude.Ord'
type L0Stream = [L0Token]

