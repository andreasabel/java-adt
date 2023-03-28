module Options where

import Control.Monad

import System.Console.GetOpt
import System.Exit
import System.IO

-- | Entry point: parse command line arguments.
--   Print usage information and exit upon error.
parseCmdLine :: [String] -> IO (Options, String, Maybe String)
parseCmdLine argv = do
  let (os, ns, errs) = getOpt Permute optDescrs argv
  when (Help `elem` os) $ halt []
  case (ns, errs) of
    ([n]   , []) -> do
      (opts, dest) <- foldM (flip doFlag) (emptyOpts, Nothing) os
      return (opts, n, dest)
    ([_]    , _) -> halt errs
    ([]     , _) -> halt $ "no input file" : errs
    ((_:_:_), _) -> halt $ "too many input files (just one allowed)" : errs

-- | Options for @java-adt@ program.
data Options = Options
  { pubClasses     :: Bool
  , defaultVisitor :: Bool
  }

-- | Default options.
emptyOpts :: Options
emptyOpts = Options
  { pubClasses     = False  -- Don't output generated classes as @public@.
  , defaultVisitor = False  -- Don't add default visitors.
  }

-- | Helper data structure for @getOpt@.

data Flag
  = Public
  | Help
  | Visit
  | Output String
  deriving (Eq, Show)

optDescrs :: [OptDescr Flag]
optDescrs =
  [ Option ['h','?'] ["help"]   (NoArg Help)   "show usage information"
  , Option ['p']     ["public"] (NoArg Public) "produce public classes (many files)"
  , Option ['o']     ["output"] (ReqArg Output "FILE") "output file"
  , Option ['d']     ["defaultVisitor"] (NoArg Visit) "produce default visitor (Java 1.5)"
  ]

-- | Process a command line option.  Halt if contradictory.
doFlag :: Flag -> (Options, Maybe String) -> IO (Options, Maybe String)
doFlag Visit      (opts, s      ) = return (opts{ defaultVisitor = True }, s)
doFlag Public     (opts, Nothing) = return (opts{ pubClasses = True }, Nothing)
doFlag (Output s) (opts, Nothing)
  | not (pubClasses opts)         = return (opts, Just s)
doFlag _ _ = halt ["cannot use both -o and -p\n"]

-- | Print errors and usage information.
halt :: [String] -> IO a
halt errs = do
  hPutStrLn stderr $ concat errs ++ usageInfo header optDescrs
  exitFailure
  where header = "usage: java-adt [OPTION...] <inputFile>"
