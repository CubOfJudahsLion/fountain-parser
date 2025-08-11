{-|
Module      : Language.Fountain.Tokenizer.L0Tokenizer
Description : Obtains level-zero tokens from @Text@ input
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX

The Level-0 tokenizer turns a 
-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Fountain.Tokenizer.L0Tokenizer
  where


import Data.Text (Text, pack)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

import Language.Fountain.Tokenizer.L0Token


-- |  A parser at the L0 token level
type L0Parser = Parsec Void Text L0Token

-- |  The error type returned by running @Text.Megaparsec.parse@.
type L0ErrorBundle = ParseErrorBundle Text Void


-- |  Consumes a newline
l0_newline :: L0Parser
l0_newline =  L0Newline <$
                choice  [ string "\r\n" -- string auto-backtracks in failure
                        , string "\r"
                        , string "\n"
                        ]

-- |  Parses a Boneyard Start mark ("@/*@")
l0_boneyardStart :: L0Parser
l0_boneyardStart = L0BoneyardStart <$ string "/*"

-- |  Parses a Boneyard End mark ("@*/@")
l0_boneyardEnd :: L0Parser
l0_boneyardEnd = L0BoneyardEnd <$ string "*/"

-- |  Parses a Note Start mark ("@[[@")
l0_noteStart :: L0Parser
l0_noteStart = L0NoteStart <$ string "[["

-- |  Parses a Note End mark ("@]]@")
l0_noteEnd :: L0Parser
l0_noteEnd = L0NoteEnd <$ string "]]"

-- |  Parses any other text that doesn't fall into the
--    previous categories. Normal text spanning a line
--    or less (might be cut before a newline by boneyards
--    or notes.)
l0_text :: L0Parser
l0_text = L0Text . pack <$> (some . try)
            (   noneOf ['[', ']', '/', '*', '\r', '\n']
            <|> char '/' <* notFollowedBy (char '*')
            <|> char '*' <* notFollowedBy (char '/')
            <|> char '[' <* notFollowedBy (char '[')
            <|> char ']' <* notFollowedBy (char ']')
            )

