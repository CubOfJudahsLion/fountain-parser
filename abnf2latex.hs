#! /usr/bin/env runhaskell
{-
  ABNF2LATEX

  Copyright (c) 2025 Alexander Feterman Naranjo

  Redistribution and use in source and binary forms, with or without modification, are
  permitted provided that the following conditions are met:
  
  1. Redistributions of source code must retain the above copyright notice, this list of
  conditions and the following disclaimer.
  
  2. Redistributions in binary form must reproduce the above copyright notice, this list of
  conditions and the following disclaimer in the documentation and/or other materials
  provided with the distribution.
  
  3. Neither the name of the copyright holder nor the names of its contributors may be used to
  endorse or promote products derived from this software without specific prior written
  permission.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS “AS IS” AND ANY
  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
  COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
  TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 

  ----

  This is a script using GHC's builtin-in parser to pretty-print an ABNF file into a LaTeX
  fragment, following RFC 5234 (https://www.rfc-editor.org/rfc/rfc5234).

  Written just. One doesn't really have to parse something to pretty-print it in most cases.

  The RFC directly provides the parsing scheme. Nonetheless, the implementation remains
  much improvable: to begin with, because of size, this probably should be a project on its
  own, and a better parsing library could be used.

  The including document must use the following packages:
    - xcolor package with the dvipsnames option.
    - courier or any other package that allows bold teletype
-}

{-# OPTIONS_GHC -XGHC2021 #-}
{-# LANGUAGE ImplicitParams #-}
module Main (main) where

----------------------------------------
-- Imports
----------------------------------------

import Control.Monad ( void )
import Data.Char ( isAlpha, isDigit, isHexDigit, isPrint )
import Data.Foldable ( foldl' )
import Data.Kind ( Type )
import Data.List ( intercalate, singleton, uncons )
import Data.Maybe ( fromMaybe )
--import Data.Semigroup ( Max(..) )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.IO ( stdin, stdout, openFile, hClose, hGetContents', hPutStr, IOMode(ReadMode, WriteMode) )
import Text.ParserCombinators.ReadP   -- Good enough for the job.


----------------------------------------
--  Endomorphic monoidal functions
----------------------------------------

--  A wrapper for a conditional function.
newtype OptionalFunction m = OptionalFunction { getOptFn :: Maybe (m -> m) }

--  If an actual function is stored, it's run on its input and the result returned,
--  otherwise the input value is returned unchanged.
runOptional :: OptionalFunction m -> m -> m
runOptional = ($) . fromMaybe id . getOptFn

--  The Semigroup binop favors non-empty functions, composes them if both are present. 
instance Semigroup (OptionalFunction m) where
  (<>) (OptionalFunction Nothing) (OptionalFunction f)            = OptionalFunction f
  (<>) (OptionalFunction f) (OptionalFunction Nothing)            = OptionalFunction f
  (<>) (OptionalFunction (Just f1)) (OptionalFunction (Just f2))  = OptionalFunction (Just (f1 . f2))

--  The Monoid, of course, produces an absent function as mempty
instance Monoid (OptionalFunction m) where
  mempty = OptionalFunction Nothing


----------------------------------------
-- Parsing data structures
----------------------------------------

--  Numeric bases used in numeric strings
data NumValBase = Hex !Char
                | Dec !Char
                | Bin !Char
  deriving (Eq, Show)

--  Posible digit additions to a first digit group in a numeric string
data ExtraNumDigits = NoExtraDigits
                    | RangeDigits !String
                    | DigitChain ![String]

--  Specifies repetitions of an element. If omitted it means one rep.
data RepetitionBounds = RepsCount !String                         -- A specific number of reps 1*digit
                      | RepLimits !(Maybe String) !(Maybe String) -- One or both bounds, *digit "*" *digit
  deriving (Eq, Show)


--  Tree representation of the ABNF document (in terms of visual entities)
data ParseTree  = CharVal !String                           -- String as a double-quoted [multi]-character value
                | NumValSingle !NumValBase !String          -- String as a single number preceded by %[bdc]
                | NumValRange !NumValBase !String !String   -- String as a range (%[bdc] followed by two dash-separated numbers)
                | NumValSequence !NumValBase ![String]      -- String as a sequence (%[bdc] followed by multiple dot-separated numbers)
                | Prose !String                             -- A prose value (between angle brackets)
                | Rulename !String                          -- Name of a rule, started by an alpha and followed by dashes, alphas and digits
                | Newline !String                           -- ("\n" ["\r"] | "\r" ["\n"])
                | Whitespace !Char                          -- Any single whitespace character, " " or "\t"
                | Whitespaces !String                       -- A string of 2 or more whitespaces, with tabs replaced by a number of spaces
                | Printable !String                         -- A string of printable (non-whitespace, non-ln) characters with no inherent markup
                | Comment ![ParseTree]                      -- A comment (starts with ; and extends to the end of the line, including it)
                | Repetition !RepetitionBounds !ParseTree   -- Repetitions for any element
                | Concatenation ![ParseTree]                -- A concatenation of elements
                | AlternationOperator                       -- The alternation operator ("/")
                | Alternation ![ParseTree]                  -- A series of elements separated by alternation operators (contained therein)
                | Group ![ParseTree]                        -- A grouping of alternations or concatenations
                | Option ![ParseTree]                       -- An _optional_ grouping of alternations or concatenations
                | RuleDefinitionOperator !String            -- Rule definition operator ('=' or '/=')
                | RuleDefinition ![ParseTree]               -- Rule definition
                | Rule !ParseTree ![ParseTree]              -- A single definition rule
                | Rulelist ![ParseTree]                     -- A list of rules
  deriving (Eq, Show)


----------------------------------------
--  Whitespace coalescing
----------------------------------------

--  Structure for progressive space char accumulation
data SpaceAccum = NoSpaces
                | SingleSpace !Char
                | MultipleSpaces !String

-- Expands characters to singleton strings, or in the case of \t, into four characters
expandSpace :: Char -> String
expandSpace '\t'  = replicate 4 ' ' -- Hardcoded rule, for now: a tab equates four spaces
expandSpace c     = singleton c

-- Converts the space accumulator into an empty list or single-ParseTree list
spaceAccumToParseTrees :: SpaceAccum -> [ParseTree]
spaceAccumToParseTrees NoSpaces             = []
spaceAccumToParseTrees (SingleSpace s)      = [Whitespace s]
spaceAccumToParseTrees (MultipleSpaces ss)  = [Whitespaces ss]

--  Converts a parse tree into a space accumulator (possibly null one)
parseTreeToSpaceAccum :: ParseTree -> SpaceAccum
parseTreeToSpaceAccum (Whitespace '\t') = MultipleSpaces $ expandSpace '\t'
parseTreeToSpaceAccum (Whitespace w)    = SingleSpace w
parseTreeToSpaceAccum (Whitespaces ws)  = MultipleSpaces $ concatMap expandSpace ws
parseTreeToSpaceAccum _                 = NoSpaces

--  Accumulates the next parse tree into an existing space accumulator.
--  If it doesn't accumulate, it returns Left fwith 
spaceAccumulate :: SpaceAccum -> ParseTree -> (Maybe ParseTree, SpaceAccum)
spaceAccumulate NoSpaces tree                         =
  case parseTreeToSpaceAccum tree of
    NoSpaces  -> (Just tree, NoSpaces)
    accum     -> (Nothing, accum)    
spaceAccumulate (SingleSpace s) (Whitespace w)        = (Nothing, MultipleSpaces $ s : expandSpace w)
spaceAccumulate (SingleSpace s) (Whitespaces ws)      = (Nothing, MultipleSpaces $ s : concatMap expandSpace ws)
spaceAccumulate (MultipleSpaces ss) (Whitespace w)    = (Nothing, MultipleSpaces $ ss ++ expandSpace w)
spaceAccumulate (MultipleSpaces ss) (Whitespaces ws)  = (Nothing, MultipleSpaces $ ss ++ concatMap expandSpace ws)
spaceAccumulate spaceAccum tree                       = (Just tree, spaceAccum)

--  Collapses multiple spaces into a single entity
collapseSpaces :: [ParseTree] -> [ParseTree]
collapseSpaces = collapse' id NoSpaces
  where
    -- Working function (tail-recursive)
    collapse' :: ([ParseTree] -> [ParseTree]) -> SpaceAccum -> [ParseTree] -> [ParseTree]
    collapse' accumFn spaceAccum []           = accumFn $ spaceAccumToParseTrees spaceAccum
    collapse' accumFn spaceAccum trees@(t:ts) =
      let
        (sparedTree, newSpaceAccum) = spaceAccumulate spaceAccum t
      in
        case sparedTree of
          Nothing ->  collapse' accumFn newSpaceAccum ts
          Just t  ->  case newSpaceAccum of
                        NoSpaces  -> collapse' (accumFn . (t :)) newSpaceAccum ts
                        _         -> collapse' (accumFn . (spaceAccumToParseTrees newSpaceAccum ++)) NoSpaces trees


----------------------------------------
--  Parsing optimization
----------------------------------------

--  many and many1 produce mutiple possible parsings, which is
--  very wasteful. Here we implement equivalents that follow
--  the 'maximal munch' rule.
maxMany, maxMany1 :: ReadP m -> ReadP [m]
maxMany p = do
  parsed <- (singleton <$> p) <++ pure []
  if null parsed
    then pure []
    else (++) <$> pure parsed <*> maxMany p
  --(((++) . singleton <$> p) <++ pure []) <*> maxMany p
maxMany1 p = do
  parsed <- maxMany p
  if null parsed
    then pfail
    else pure parsed


----------------------------------------
--  Parser routines
----------------------------------------

--  Reads a 'char-val', a double-quited sring
charVal :: ReadP ParseTree
charVal = char '"' *> (CharVal <$> munch (\c -> isPrint c && c /= '"')) <* char '"'

--  Reads a 'num-val', a numerically depicted string, in hex, decimal or binary digits
numVal :: ReadP ParseTree
numVal = do
  void $ char '%'
  base <- choice  [ Hex <$> satisfy (`elem` ['x', 'X'])
                  , Dec <$> satisfy (`elem` ['d', 'D'])
                  , Bin <$> satisfy (`elem` ['b', 'B'])
                  ]
  let isBaseDigit = case base of
                      Hex _ -> isHexDigit
                      Dec _ -> isDigit
                      Bin _ -> isBinDigit
  firstDigitGroup <- munch1 isBaseDigit
  otherDigitGroups <- choice [ range isBaseDigit, chain isBaseDigit ] <++ pure NoExtraDigits
  pure $ case otherDigitGroups of
    NoExtraDigits                 -> NumValSingle base firstDigitGroup
    RangeDigits secondDigitGroup  -> NumValRange base firstDigitGroup secondDigitGroup
    DigitChain otherDigitGroups   -> NumValSequence base (firstDigitGroup : otherDigitGroups)
  where
    --  Queries whether a character is a binary digit
    isBinDigit :: Char -> Bool
    isBinDigit '0'  = True
    isBinDigit '1'  = True
    isBinDigit _    = False
    --
    --  Parses the dash an other digit group, completing a range
    range :: (Char -> Bool) -> ReadP ExtraNumDigits
    range test = char '-' *> (RangeDigits <$> munch1 test)
    --
    --  Parses a series of dots and following digits, making a collection of digit groups
    chain :: (Char -> Bool) -> ReadP ExtraNumDigits
    chain test = DigitChain <$> maxMany1 (char '.' *> munch1 test)

--  Reads 'prose-val', a value described in prose between angle brackets. No tabs.
proseVal :: ReadP ParseTree
proseVal = char '<' *> (Prose <$> munch (\c -> c /= '>' && isPrint c)) <* char '>'

--  Read a 'rulename', which starts with a letter and might be followed by more letters, digits and dashes
rulename :: ReadP ParseTree
rulename = Rulename <$> ((:) <$> satisfy isAlpha <*> munch (\c -> c == '-' || isDigit c || isAlpha c))

--  Tells whether a character is an allowed space character (space or tab) or not
isWhitespace :: Char -> Bool
isWhitespace '\t' = True
isWhitespace ' '  = True
isWhitespace _    = False

--  Reads a whitespace (a space or a tab) as a character
whitespaceS :: ReadP Char
whitespaceS = char ' ' +++ char '\t'

--  Reads a whitespace as a ParseTree
whitespace :: ReadP ParseTree
whitespace = Whitespace <$> whitespaceS

--  Reads a newline
newline :: ReadP ParseTree
newline =   Newline
        <$> choice  [ (:) <$> char '\n' <*> ((singleton <$> char '\r') <++ pure "")
                    , (:) <$> char '\r' <*> ((singleton <$> char '\n') <++ pure "")
                    ]

-- Reads a sequence of printable characters (non-whitespace and non-nl)
printable :: ReadP ParseTree
printable = Printable <$> munch1 (\c -> not (isWhitespace c) && isPrint c)

--  Reads a comment, newline included
comment :: ReadP [ParseTree]
comment = do
  void $ char ';'
  (:) <$> (Comment . collapseSpaces <$> maxMany (whitespace +++ printable))
      <*> (singleton <$> newline)

--  Reads a comment or a newline
commentOrNewline :: ReadP [ParseTree]
commentOrNewline = comment +++ (singleton <$> newline)

--  Comment or whitespace separating parts of a single definition
commentOrWhitespace :: ReadP [ParseTree]
commentOrWhitespace =   (singleton <$> whitespace)
                    +++ ((++) <$> commentOrNewline <*> (singleton <$> whitespace))

--  Multiple comments or whitespaces, mandatory or not into a single array
commentsOrWhitespaces :: Bool -> ReadP [ParseTree]
commentsOrWhitespaces mandatory =
  let
    multiplier = if mandatory then maxMany1 else maxMany
  in
    collapseSpaces . concat <$> multiplier commentOrWhitespace

--  A specification of repetitions
repeatBounds :: ReadP (Maybe RepetitionBounds)
repeatBounds = do
  maybeLower <- maybeDigits
  hasAsterisk <- (True <$ char '*') <++ pure False
  case (maybeLower, hasAsterisk) of
    (Nothing,    False) ->  pure $ Nothing
    (Just count, False) ->  pure $ Just $ RepsCount count
    (_,          True)  ->  do
                              maybeHigher <- maybeDigits
                              pure $ Just $ RepLimits maybeLower maybeHigher
  where
    --  Attempts to parse at least one digit, returning the result as a Maybe
    maybeDigits :: ReadP (Maybe String)
    maybeDigits = (Just <$> munch1 isDigit) <++ pure Nothing

--  Element
element :: ReadP ParseTree
element = choice [ rulename, group, optionGroup, charVal, numVal, proseVal ]

--  Repetition of an element
repetition :: ReadP ParseTree
repetition = do
  maybeReps  <- repeatBounds
  elemn <- element
  pure $ case maybeReps of
    Nothing   -> elemn
    Just reps -> Repetition reps elemn

--  A concatenation of elements
concatenation :: ReadP ParseTree
concatenation = do
  rep <- repetition
  moreReps <- concat <$> maxMany separatedRepetition
  pure $ if null moreReps
     then rep
     else Concatenation (rep : moreReps)
  where
    --  Gets another repetition but as a [ParseTree]
    repetitionSingleton :: ReadP [ParseTree]
    repetitionSingleton = singleton <$> repetition
    --
    --  Gets another repetition preceded by comments or whitespaces
    separatedRepetition :: ReadP [ParseTree]
    separatedRepetition = (++) <$> commentsOrWhitespaces True <*> repetitionSingleton

--  Parses a single alternation operator
alternationOperator :: ReadP ParseTree
alternationOperator = AlternationOperator <$ char '/'

--  An alternation of elements
alternation :: ReadP ParseTree
alternation = do
  firstConcatenation <- concatenation
  rest <- concat <$> maxMany otherConcatenation
  pure (if null rest
         then firstConcatenation
         else Alternation $ firstConcatenation : rest)
  where
    otherConcatenation :: ReadP [ParseTree]
    otherConcatenation = do
      separator1 <- commentsOrWhitespaces False
      op <- alternationOperator
      separator2 <- commentsOrWhitespaces False
      nextConcatenation <- singleton <$> concatenation
      pure $ separator1 ++ op : separator2 ++ nextConcatenation

--  An alternation optionally surrounded by comments or whitespaces
separatorBracketedAlternation :: Char -> Char -> ReadP [ParseTree]
separatorBracketedAlternation start end = do
  void $ char start
  separator1 <- commentsOrWhitespaces False
  alt <- alternation
  separator2 <- commentsOrWhitespaces False
  void $ char end
  pure $ separator1 ++ alt : separator2

--  A grouping of alternations or concatenations
group :: ReadP ParseTree
group = Group <$> separatorBracketedAlternation '(' ')'

--  An optional grouping of alternations or concatenations
optionGroup :: ReadP ParseTree
optionGroup = Option <$> separatorBracketedAlternation '[' ']'

-- An alternation of elements
elements :: ReadP [ParseTree]
elements = (:) <$> alternation <*> commentsOrWhitespaces False

--  Parses a rule definition (=) or definition extension (=/) operator
ruleDefinitionOperator :: ReadP ParseTree
ruleDefinitionOperator = RuleDefinitionOperator <$> string "=/" +++ string "="

--  Parses the rule def op and the surrounding comments/whitespaces
definedAs :: ReadP [ParseTree]
definedAs = do
  separator1 <- commentsOrWhitespaces False
  definitionOp <- ruleDefinitionOperator
  separator2 <- commentsOrWhitespaces False
  pure $ separator1 ++ definitionOp : separator2

--  Parses a single definition rule
rule :: ReadP ParseTree
rule = do
  lhs <- rulename
  definedAsElems <- definedAs
  rhsElems <- elements
  separator <- commentOrNewline
  pure $ Rule lhs (definedAsElems ++ rhsElems ++ separator)

--  Parses a set of rules (the entire file)
rulelist :: ReadP ParseTree
rulelist =    Rulelist . concat
         <$>  maxMany1
                 (   (singleton <$> rule)
                 +++ ((++) <$> commentsOrWhitespaces False <*> commentOrNewline))


----------------------------------------
--  Document Representations of elements
----------------------------------------

--  This is a class of elements that can be represented on a document
class DocumentRepresentable d where
  --  Produce a String from the representable.
  --  The Bool specifies where pretty-printing should be used or not
  docRep :: Bool -> d -> String

--  Automatic instance: any array of representables is representable itself
instance (DocumentRepresentable d) => DocumentRepresentable [d] where
  docRep prettyPrint ds = concatMap (docRep prettyPrint) ds


----------------------------------------
--  Prettyprinting
----------------------------------------

--  Revert the bases to their respective letter
baseLetter :: NumValBase -> Char
baseLetter (Hex c)  = c
baseLetter (Dec c)  = c
baseLetter (Bin c)  = c


--  For monoids, produces the given value if ?keep is true, mempty otherwise
condVal :: (Monoid m, ?keep :: Bool) => m -> m
condVal val = if ?keep then val else mempty


--  Repetition bound specifications have document representations
instance DocumentRepresentable RepetitionBounds where
  docRep prettyPrint bounds  =
    let
      (?\) :: Monoid m => m -> m
      (?\) = let ?keep = prettyPrint in condVal
    in
      (?\) "\\textcolor{RoyalBlue}{\\emph{" ++ docRep' bounds ++ (?\) "}}"
    where
      docRep' :: RepetitionBounds -> String
      docRep' (RepsCount count)       = count
      docRep' (RepLimits lower upper) = fromMaybe "" lower ++ "*" ++ fromMaybe "" upper

--  Returns parse trees to String form, whether for prettyprinting or as raw text.
instance DocumentRepresentable ParseTree where
  docRep prettyPrint tree =
    let
      --  The ?\ unary operator conditionally applies markup if pretty-printing
      (?\) :: Monoid m => m -> m
      (?\) = let ?keep = prettyPrint in condVal
      --  The ?$ unary operator conditionally converts the string to a LaTeX-escaped one if pretty-printing
      (?$) :: String -> String
      (?$) = runOptional $ (?\) stringAsLaTeX
    in
      case tree of
        (CharVal s)                       ->  (?\) "\\textcolor{BrickRed}{" ++ '"' : (?$) s ++ '"' : (?\) "}"
        (NumValSingle base digits)        ->  (?\) "\\textcolor{Brown}{\\textbf{\\" ++ '%' : (?\) "{}"
                                              ++ baseLetter base : (?\) "}" ++ digits ++ (?\) "}"
        (NumValRange base start end)      ->  (?\) "\\textcolor{Brown}{\\textbf{\\" ++ '%' : (?\) "{}"
                                              ++ baseLetter base : (?\) "}" ++ start ++ (?\) "\\textbf{" ++ '-' : (?\) "}" ++ end ++ (?\) "}"
        (NumValSequence base digitGroups) ->  (?\) "\\textcolor{Brown}{\\textbf{\\" ++ '%' : (?\) "{}"
                                              ++ baseLetter base : (?\) "}"
                                              ++ intercalate ((?\) "\\textbf{" ++ '.' : (?\) "}") digitGroups ++ (?\) "}"
        (Prose prose)                     ->  '<' : (?\) "\\emph{" ++ (?$) prose ++ (?\) "}" ++ ">"
        (Rulename name)                   ->  (?$) name
        (Whitespace w)                    ->  [w]
        (Whitespaces ws)                  ->  (?\) "\\mbox{" ++ fmap (const '~') ws ++ (?\) "}"
        (Printable s)                     ->  (?$) s
        (Comment tree)                    ->  (?\) "\\textcolor{Gray}{" ++ ';' : docRep True tree ++ (?\) "}"
        (Newline nl)                      ->  (?\) "\\\\" ++ nl
        (Repetition bounds element)       ->  docRep prettyPrint bounds ++ docRep prettyPrint element
        (Concatenation trees)             ->  docRep prettyPrint trees
        AlternationOperator               ->  (?\) "\\textbf{" ++ '/' : (?\) "}"
        (Alternation trees)               ->  docRep prettyPrint trees
        (Group trees)                     ->  (?\) "\\textbf{" ++ '(' : (?\) "}" ++ docRep prettyPrint trees ++ (?\) "\\textbf{" ++ ')' : (?\) "}"
        (Option trees)                    ->  (?\) "\\textbf{" ++ '[' : (?\) "}" ++ docRep prettyPrint trees ++ (?\) "\\textbf{" ++ ']' : (?\) "}"
        (RuleDefinitionOperator op)       ->  (?\) "\\textbf{" ++ op ++ (?\) "}"
        (RuleDefinition trees)            ->  docRep prettyPrint trees
        (Rule name trees)                 ->  (?\) "\\textbf{" ++ docRep prettyPrint name ++ (?\) "}" ++ docRep prettyPrint trees
        (Rulelist trees)                  ->  (?\) "{\\scriptsize\\ttfamily\n" ++ docRep prettyPrint trees ++ (?\) "}"
    where
      --  Converts a single character into a LaTeX escape sequence, if any
      charAsLaTeX :: Char -> String
      charAsLaTeX '<'              = "\\textless{}"
      charAsLaTeX '>'              = "\\textgreater{}"
      charAsLaTeX '~'              = "\\textasciitilde{}"
      charAsLaTeX '^'              = "\\textasciicircum{}"
      charAsLaTeX '\\'             = "\\textbackslash{}"
      charAsLaTeX '\''             = "\\'{ }"
      charAsLaTeX '`'              = "\\`{ }"
      charAsLaTeX c
        | c `elem` ('&':"%$#_{}") = '\\' : c : "{}"
        | otherwise               = [c]
      --
      --  Replaces all special LaTeX characters in string with equivalent literals
      stringAsLaTeX :: OptionalFunction String
      stringAsLaTeX = OptionalFunction (Just (concatMap charAsLaTeX))


--  Processes the file. If successful, LaTeX prettyprinting codes are returned.
--  Otherwise, Nothing is produced. The parser might produce multiple parsings,
--  so  we'll use the longest (last.)
processABNF :: String -> Maybe String
processABNF = fmap (docRep True . fst . fst) . uncons . reverse . readP_to_S rulelist


----------------------------------------
--  Main function and related
----------------------------------------

--  Classify the type of input or output handle
data InputHandleType  = StdIn | InFile
data OutputHandleType = StdOut | OutFile


main :: IO ()
main = do
  -- Input and output files may be passes as arguments.
  -- stdin and stdout are used in their absence
  args <- getArgs
  -- We obtain the input handle, keeping track of whether it's stdio or a file
  (inputType, inputHandle) <-
    if length args == 0
      then pure (StdIn, stdin)
      else do
        let filename = head args
        handle <- openFile filename ReadMode
        pure (InFile, handle)
  -- Read and close file
  contents <- hGetContents' inputHandle
  case inputType of
    StdIn   -> pure ()
    InFile  -> hClose inputHandle
  -- Parse and convert to pretty-printed text
  case processABNF contents of
    Nothing     -> die $ "Parsing error"
    Just parsed -> do
      -- Obtain output type and handle
      (outputType, outputHandle) <-
        if length args < 2
          then pure (StdOut, stdout)
          else do
            let filename = head $ tail args
            handle <- openFile filename WriteMode
            pure (OutFile, handle)
      -- write and close
      hPutStr outputHandle parsed
      case outputType of
        StdOut  -> pure ()
        OutFile -> hClose outputHandle

