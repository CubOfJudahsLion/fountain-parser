{-|
Module      : Language.Fountain.Tokenizer.L0Token
Description : Fountain level-zero tokens
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX

A level zero token, i.e., one requiring no context.
In our case, this also means /character-level/
tokens. New line sequences are treated as a token.
-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Fountain.Tokenizer.L0Token where


import Data.Text (Text)


-- |  Level zero token
data L0Token  = L0Newline       -- ^  A line change. @\\r\\n@, @\\r@ and @\\n@ are all interpreted as newlines
              | L0BoneyardStart -- ^  The @/*@ character pair, which starts a multi-line comment
              | L0BoneyardEnd   -- ^  The @*/@ character pair, which ends a mult-line comment
              | L0NoteStart     -- ^  The @[[@ character pair, which starts a note
              | L0NoteEnd       -- ^  The @]]@ character pair, which ends a note
              | L0Text Text     -- ^  Any text not fitting the preceding categories
  deriving (Eq, Ord, Show)


-- |  Level zero token stream: just a list of @L0Token@s.
--    Conveniently, /Megaparsec/ turns lists automatically
--    into @Stream@s as long as the base type is an
--    instance of @Ord@.
type L0Stream = [L0Token]


-- |  Turns @L0Token@s into literal text.
l0_toLiteral :: L0Token -> Text
l0_toLiteral t = case t of
  L0Newline       -> "\n"
  L0BoneyardStart -> "/*"
  L0BoneyardEnd   -> "*/"
  L0NoteStart     -> "[["
  L0NoteEnd       -> "]]"
  (L0Text txt)    -> txt

