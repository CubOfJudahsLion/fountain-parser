{-|
Module      : Language.Fountain.ParseTree
Description : Data structures and utilities for the parse tree.
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX
-}
module Language.Fountain.ParseTree where


-- |  Title entries for the parse tree. At this stage,
--    they're merely key: value pairs.
data TitleEntry =   MkTitleEntry
                    { key   :: String
                    , value :: String
                    }

data ScreenPlay =   MkScreenPlay
                    { titlePage :: [TitleEntry]
                    }

data TextPiece =    PlainTextPiece String
                    BoldTextPiece TextPiece
                    ItalicTextPiece TextPiece
                    UnderlineTextPiece TextPiece

data 
