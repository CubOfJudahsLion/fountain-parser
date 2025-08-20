{- A script using the Haskell builtin-in parser (it's enough for the job) to pretty-print
   an ABNF file into a LaTeX fragment, following RFC 5234 (https://www.rfc-editor.org/rfc/rfc5234). -}
module Main (main) where


import Data.Kind ( Type )
import Data.Semigroup ( Max(..) )
import System.Environment ( getArgs )
import System.Exit ( die )
import System.IO ( stdin, stdout, openFile, hClose, hGetContents', hPutStr, IOMode(ReadMode, WriteMode) )
import Text.ParserCombinators.ReadP   -- Good enough for the job.


--  Classifies the type of input handle
data InputHandleType  = StdIn | InFile

--  Classifies the type of output handle
data OutputHandleType = StdOut | OutFile


 --   Position inside a file, e.g., line and column.
newtype LineColumn i = LineColumn (i, i)
  deriving (Eq)

--  Lines more important than columns, of course.
instance (Ord i) => Ord (LineColumn i) where
  compare (LineColumn (l1, c1)) (LineColumn (l2, c2)) =
    case compare l1 l2 of
      EQ  -> compare c1 c2
      r   -> r

--  This is a wrapper for LineColumn. Since according to the Prelude,
--  Ord (LineColumn i) -> Ord (Maybe (LineColumn i)) -> Semigroup (Max (Maybe (LineColumn i)))
--  this allows the <> operator to pick the more advance
--  position in the file.
newtype FilePosition i = FilePosition (Max (Maybe (LineColumn i)))
  deriving (Ord, Semigroup, Functor)

--  A starting file position
initialFilePosition :: FilePosition Int
initialFilePosition = FilePosition (Max (Just (LineColumn (1, 1))))

--  We still want to read lines and columns
line, column :: FilePosition i -> Maybe i
line = fmap fst . getMax
column = fmap snd . getMax

--  We get a monoid nearly for free.
instance (Ord i) => Monoid (FilePosition i) where
  mempty = FilePosition (Max Nothing)

--  A parser that tracks its position throughout the file
data TrackingParser i d = TrackingParser  { position  :: FilePosition i
                                          , parser    :: ReadP d
                                          }

--  Produces a tracking parser 
emptyTracker :: TrackingParser i d
emptyTracker md = TrackingParser mempty (string "")

startTracker :: TrackingParser i d
startTracker md = TrackingParser

--  FilePos functor maps to the datum
instance Functor (TrackingParser i) where
  fmap f (FilePosition (Max maybe)) = FilePosition (Max (f <$> maybe))

--  A function to choose 

processABNF :: String -> Either String String
processABNF = Right . id


main :: IO ()
main = do
  args <- getArgs
  (inputType, inputHandle) <-
    if length args == 0
      then pure (StdIn, stdin)
      else do
        let filename = head args
        handle <- openFile filename ReadMode
        pure (InFile, handle)
  contents <- hGetContents' inputHandle
  case inputType of
    StdIn   -> pure ()
    InFile  -> hClose inputHandle
  case processABNF contents of
    Left err -> do
      die err 
    Right parsed -> do
      (outputType, outputHandle) <-
        if length args < 2
          then pure (StdOut, stdout)
          else do
            let filename = head $ tail args
            handle <- openFile filename WriteMode
            pure (OutFile, handle)
      hPutStr outputHandle parsed
      case outputType of
        StdOut  -> pure ()
        OutFile -> hClose outputHandle

