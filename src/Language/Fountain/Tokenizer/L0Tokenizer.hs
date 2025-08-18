{-|
Module      : Language.Fountain.Tokenizer.L0Tokenizer
Description : Obtains level-zero tokens from @Text@ input
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX

The Level-0 tokenizer turns a character stream into a level-zero
token stream.
-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Language.Fountain.Tokenizer.L0Tokenizer where


import Control.Monad ( void )
import Data.Text ( Text )
import qualified Data.Text as T
import Data.Void ( Void )
import Text.Megaparsec hiding ( State )
import Text.Megaparsec.Char ( char, string )
import Language.Fountain.Tokenizer.L0Token
import Language.Fountain.Tokenizer.L0Token (L0Token(L0Spaces))


-- |  A parser with no transformer stack
type L0Parser = Parsec Void Text

-- |  The error type returned by running @Text.Megaparsec.parse@.
type L0ErrorBundle = ParseErrorBundle Text Void


--  List of newline sequences, to be later normalized.
newlineStrings :: [Text]
newlineStrings =  [ "\r\n"    -- Windows newline
                  , "\r"      -- Classic MacOS, OS-9 newline
                  , "\n\r"    -- RISC OS newline
                  , "\n"      -- POSIX newline
                  , "\v"      -- Vertical tab
                  , "\f"      -- Form-feed
                  , "\x0085"  -- Next Line
                  , "\x2028"  -- Line Separator
                  , "\x2029"  -- Paragraph Separator
                  ] 



--  Removes duplicates by ordering the array.
--  Written because the standard library version is ridiculously
--  slow, and it's ridiculous to import a whole package or
--  framework just for one function.
nub :: forall t. (Ord t, Show t) => [t] -> [t]
nub = nubRight id
  where
    --  Partitions a list by two predicates, ignoring elements
    --  that fulfill neither. Note that an element can be added
    --  to both results if it fulfills both.
    span2p :: Show t => (t -> Bool) -> (t -> Bool) -> [t] -> ([t], [t])
    span2p pLeft pRight = span2p' ([], [])
      where
        span2p' :: ([t], [t]) -> [t] -> ([t], [t])
        span2p' !finalTuple []                    = finalTuple
        span2p' (!accumLeft, !accumRight) (x:xs)  =
          let
            accumLeft'  = if pLeft x  then x:accumLeft  else accumLeft
            accumRight' = if pRight x then x:accumRight else accumRight
          in
            span2p' (accumLeft', accumRight') xs

    --  Splits the list in two by a mid-point pivot
    --  (removing duplicates), ordering the
    --  left one and joining it to the accumulator,
    --- recursively partitioning the right
    --  one until it's empty.
    nubRight :: ([t] -> [t]) -> [t] -> [t]
    nubRight f []     = f []
    nubRight f (x:xs) =
      let
        -- Our two predicates will ensure no duplicates between lists.
        -- as the pivot is not replicated in either.
        -- Recall that nub <$> only affects the second element of the tuple.
        (greater, lesser) = nub <$> span2p (> x) (< x) xs
      in
        nubRight (f . (lesser ++) . (x :)) greater


--   Characters from newline strings, made unique
newlineChars :: [Char]
newlineChars = nub $ T.unpack $ T.concat newlineStrings

--  Parses any newline sequence as string, producing a standard newline character as a result
strNewline :: L0Parser Text
strNewline = "\n" <$ choice (string <$> newlineStrings)

--  Parses a newline as a 'L0Token'
l0newline :: L0Parser L0Token
l0newline = L0Newline <$ strNewline


--  Parses a /Boneyard/ ("@/* ... */@"), which can go for multiple lines (even past blank ones.)
l0boneyard :: L0Parser L0Token
l0boneyard  =   L0Boneyard . T.pack
            <$> between
                  (string "/*")
                  (string "*/")
                  (many
                    (    noneOf ['*']
                    <|>  try (char '*' <* notFollowedBy (char '/'))))

-- Parses a /Note/ ("@[[ ... ]]@"), which can also be multiline. However, it can be stopped by
-- a double-newline
l0note :: L0Parser L0Token
l0note  =   L0Note . T.concat
        <$> between
              (string "[[")
              (string "]]")
              (many
                (choice [ T.singleton <$> noneOf (']' : newlineChars)
                        , T.singleton <$> try (char ']' <* notFollowedBy (char ']'))
                        , try (strNewline <* notFollowedBy strNewline)
                        ]))


data CodePoints = SingleChar !Char
                | CharRange !Char !Char
  deriving (Show)

parserForCodePoint :: CodePoints -> L0Parser Char
parserForCodePoint (SingleChar c)     = char c
parserForCodePoint (CharRange c1 c2)  = if c1 < c2
                                           then satisfy (\c -> c1 <= c && c <= c2)
                                           else satisfy (\c -> c2 <= c && c <= c1)

--  Creates a character list for a @CodePoints@ object
charListForCodepoint :: CodePoints -> [Char]
charListForCodepoint (SingleChar c)     = [c]
charListForCodepoint (CharRange c1 c2)  = if c1 < c2
                                            then [c1 .. c2]
                                            else [c2 .. c1]

--  A table with all space characters and their replacements.
--  Since the intent of a screenplay is to imitate a typewriter,
--  very tiny (or zero-width) spaces are removed and any others
--  turned into a regular space (@'\x20'@).
spacesAndReplacements :: [([CodePoints], Text)]
spacesAndReplacements = [  -- Zero-spacers
                          ( [ CharRange  '\x200A' '\x200D'  -- Hair space and zero-width spaces and [non]-joiners
                            , SingleChar '\x2060'           -- Word joiner
                            , SingleChar '\xFEFF'           -- Zero-width non-breaking space
                            ]
                          , T.empty                             -- These are replaced by empty text
                          )
                          -- One-spacers
                        , ( [ SingleChar ' '                -- Regular space
                            , SingleChar '\x00A0'           -- Non-breaking space
                            , CharRange  '\x2000' '\x2009'  -- Differently-sized (Em, En) spaces
                            , SingleChar '\x202F'           -- Narrow non-breaking space
                            , SingleChar '\x205F'           -- Medium Mathematical space
                            , SingleChar '\x3000'           -- Ideographic space
                            ]
                          , " "                                 -- These are replaced with a regular space
                          )
                          -- Four-spacers
                        , ( [ SingleChar '\t'               -- Tab character
                            ]
                          , T.replicate 4 " "                   -- Becomes four spaces as per the spec
                          )
                        ]

--  The list of space characters
spaceChars :: [Char]
spaceChars = concatMap (concatMap charListForCodepoint . fst) spacesAndReplacements

--  Create a parser for a list of codepoints
codePointSetReplacerParser :: ([CodePoints], Text) -> L0Parser Text
codePointSetReplacerParser (codepoints, replacement) =
  replacement <$ choice (parserForCodePoint <$> codepoints)

--  Create a parser for all lists of codepoints and their replacements
allCodePointsReplacerParser :: L0Parser Text
allCodePointsReplacerParser = choice (codePointSetReplacerParser <$> spacesAndReplacements)

--  Parses all consecutive spaces (including boneyards, which become empty strings)
l0spaces :: L0Parser L0Token
l0spaces = L0Spaces . T.concat <$> many allCodePointsReplacerParser


--  Parses everything else into content
strContent :: L0Parser Text
strContent = T.cons <$> anySingle <*> rest
  where
    rest :: L0Parser Text
    rest = 


--  Produces an 'L0Token' stream.
l0tokenStream = L0Stream
l0tokenStream =
  many (choice  [ l0spaces
                , l0newline
                , try l0boneyard
                , try l0note
                , 
                
