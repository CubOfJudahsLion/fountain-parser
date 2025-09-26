{-|
Module      : Language.Fountain.Preprocess
Description : Functions for preprocessing source text
Copyright   : (c) Alexander Feterman Naranjo, 2025
License     : BSD-3-Clause
Maintainer  : feterman@hotmail.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Language.Fountain.Preprocess ( PreprocessedContent(..), preprocessSource ) where


import Data.Text ( Text )
import Data.Text.Utils.Patterns ( pattern Empty, pattern (:<) )
import qualified Data.Text as T ( append, empty, pack, singleton )


-- |  A very basic set of tokens for classifying content.
data PreprocessedContent  = NewlineContent            -- ^  A newline
                          | InitialSpaceContent !Int  -- ^  A number of spaces at the beginning of a line
                          | TextContent !Text         -- ^  Actual text content
                          | BoneyardContent !Text     -- ^  A boneyard with its content
  deriving (Show)


-- |  Measures the number of places used by a character when expanded.
--    Portraits tabs as 4-places long, zero-width spaces as such, and
--    everything else occupying 1 place.
expandedCharSize :: Char -> Int
expandedCharSize '\t'                                 = 4
expandedCharSize ch | ch `elem` ['\x200A', '\x200B']  = 0
                    | otherwise                       = 1


-- |  Types of one- or two-character marks
data CharacterMark  = BeginBoneyardMark
                    | EndBoneyardMark
                    | NewlineMark
                    | SpaceMark !Char
                    | OtherCharMark !Char


-- |  Kinds of character-based identities found in preprocessing.
markToText :: CharacterMark -> Text
markToText BeginBoneyardMark                                    = "/*"
markToText EndBoneyardMark                                      = "*/"
markToText NewlineMark                                          = "\n"
markToText (SpaceMark '\t')                                     = T.pack $ replicate 4 ' '
markToText (SpaceMark ch) | ch `elem` ['\x200A', '\x200B']      = T.empty
                          | otherwise                           = " "
markToText (OtherCharMark o)                                    = T.singleton o


-- |  Adds the next character mark to the total and current content accumulaters
accumulateMark :: ([PreprocessedContent] -> [PreprocessedContent], PreprocessedContent) -- ^  Total accumulator (as difference list) and current content accumulator
               -> CharacterMark                                                         -- ^  Characters to accumulate
               -> ([PreprocessedContent] -> [PreprocessedContent], PreprocessedContent) -- ^  Returns: new accumulators after character(s) consumed
accumulateMark (!totalAccumFn, b@(BoneyardContent _))     EndBoneyardMark   = (totalAccumFn . (b :),              TextContent T.empty)
accumulateMark (!totalAccumFn, BoneyardContent b)         mark              = (totalAccumFn,                      BoneyardContent $ T.append b $ markToText mark)
accumulateMark (!totalAccumFn, content)                   BeginBoneyardMark = (totalAccumFn . (content :),        BoneyardContent T.empty)
accumulateMark (!totalAccumFn, content)                   NewlineMark       = (totalAccumFn . (content :),        NewlineContent)
accumulateMark (!totalAccumFn, NewlineContent)            (SpaceMark s)     = (totalAccumFn . (NewlineContent :), InitialSpaceContent $ expandedCharSize s)
accumulateMark (!totalAccumFn, NewlineContent)            mark              = (totalAccumFn . (NewlineContent :), TextContent $ markToText mark)
accumulateMark (!totalAccumFn, InitialSpaceContent n)     (SpaceMark s)     = (totalAccumFn,                      InitialSpaceContent $ n + expandedCharSize s)
accumulateMark (!totalAccumFn, i@(InitialSpaceContent _)) mark              = (totalAccumFn . (i :),              TextContent $ markToText mark)
accumulateMark (!totalAccumFn, TextContent tc)            mark              = (totalAccumFn,                      TextContent $ T.append tc $ markToText mark)


-- |  Optimizes the list of preprocessed items by removing zero-length ones
--    and concatenating successive texts.
optimizePreprocessed :: [PreprocessedContent] -> [PreprocessedContent]
optimizePreprocessed = foldr condense []
  where
    condense :: PreprocessedContent -> [PreprocessedContent] -> [PreprocessedContent]
    condense (InitialSpaceContent 0) accum                    = accum
    condense (TextContent Empty)     accum                    = accum
    condense (TextContent t1)        (TextContent t2 : rest)  = TextContent (T.append t1 t2) : rest
    condense (BoneyardContent Empty) accum                    = accum
    condense item                    accum                    = item : accum


-- |  Preprocess the text, normalizing spacing and newlines. Returns the text as a sequence of `PreprocessedContent` items.
--    The function appends and prepends a newline character to facilitate sliding-window inspections later.
preprocessSource :: Text -> [PreprocessedContent]
preprocessSource = optimizePreprocessed . preprocessSource' (id, NewlineContent)
  where
    --  Worker Function
    preprocessSource' :: ([PreprocessedContent] -> [PreprocessedContent], PreprocessedContent)  -- ^  Total (differential) and current-line accumulator
                      -> Text                                                                   -- ^  Remaining text to be processed
                      -> [PreprocessedContent]                                                  -- ^  Return value: a list of preprocessed content
    --  Trivial cases (at end of text.) Unclosed boneyards become text.
    preprocessSource' (totalAccumFn, BoneyardContent b) Empty = totalAccumFn [TextContent $ T.append "/*" b, NewlineContent]
    preprocessSource' (totalAccumFn, contentAccum) Empty      = totalAccumFn [contentAccum, NewlineContent]
    --  Beginning and end Boneyard markers
    preprocessSource' accums ('/'  :< '*'  :< rest)           = preprocessSource' (accumulateMark accums BeginBoneyardMark) rest
    preprocessSource' accums ('*'  :< '/'  :< rest)           = preprocessSource' (accumulateMark accums EndBoneyardMark) rest
    --  Any newlines move the line accumulator into the global accumulator and start an empty line accumulator.
    preprocessSource' accums ('\r' :< '\n' :< rest)           = preprocessSource' (accumulateMark accums NewlineMark) rest
    preprocessSource' accums ('\n' :< '\r' :< rest)           = preprocessSource' (accumulateMark accums NewlineMark) rest
    preprocessSource' accums (ch   :<         rest)
      --  We're interpreting vertical tabs and formfeed as newlines.  \x0085, \x2028 and \x2029 (Unicode for,
      --  respectively, next-line, line-separator and paragraph separator) are also intepreted as newlines.
      | ch `elem` ['\n', '\r',
                   '\v', '\f',
                   '\x0085', '\x2028', '\x2029']              = preprocessSource' (accumulateMark accums NewlineMark) rest
      --  The following spaces are replaced by a regular space(s): tab (4 spaces), %x00A0 (non-breaking), %x2000-2009
      --  (varying-width Em/En-based spaces), %x202F (narrow non-breaking), %x205F (mathematical middle-space), %x3000
      --  (Ideographic space.) Zero-width spaces (\x200B) and hair-width spaces (\x200A) are simply removed.
      | ch `elem` ['\t', ' ', '\x00A0'] ++
                  ['\x2000' .. '\x200B'] ++
                  ['\x202F', '\x205F', '\x3000']              = preprocessSource' (accumulateMark accums (SpaceMark ch))  rest
      --  Any other character is just appended to the line accumulator.
      | otherwise                                             = preprocessSource' (accumulateMark accums (OtherCharMark ch)) rest

