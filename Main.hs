{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Monad
import qualified Data.Aeson           as A
import           Data.Aeson.TH
import           Data.Char
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Conduit as C
import           Options.Applicative  hiding (value)
import           System.Exit

-----------
-- TYPES --
-----------

data Args = Args { argNull     :: Bool
                 , argNone     :: Bool
                 , argVerbose  :: Bool
                 , argURL      :: String
                 , argTarget   :: String
                 , argOperator :: String
                 , argValue    :: Double
                 , argMinutes  :: Int }
          deriving (Show, Eq)

data Metric = Metric { metricTarget     :: String
                     , metricDatapoints :: [Datapoint] }
            deriving (Show, Eq)

data Datapoint = Datapoint (Maybe Double) Integer
               deriving (Show, Eq)

$( deriveJSON ((map toLower) . drop 6) ''Metric )
$( deriveJSON id                       ''Datapoint )

-----------
-- CHECK --
-----------

check :: Args -> IO ()
check args@(Args {..}) = do
  json <- graphiteQuery args
  when argVerbose $ putStrLn . show $ json
  checkMetrics args (A.decode json)

graphiteQuery :: Args -> IO L.ByteString
graphiteQuery args = C.simpleHttp (graphiteUrl args) >>= return

graphiteUrl:: Args -> String
graphiteUrl (Args{..}) =
  argURL ++ "/render/?target=" ++ argTarget
  ++ "&from=-" ++ show argMinutes ++ "min&format=json"

checkMetrics :: Args -> Maybe [Metric] -> IO ()
checkMetrics (Args {..}) (Just []) = do
  putStrLn $ "CRITICAL: no data " ++ argTarget
  exitWith $ ExitFailure 2
checkMetrics args@(Args {..}) (Just metrics) = do
  if argNull && any (any (not . hasValue) . metricDatapoints) metrics
    then do putStrLn $ "CRITICAL: Graphite values contain nulls " ++ argTarget
            exitWith $ ExitFailure 2
    else if all (checkValues args . values . metricDatapoints) metrics
         then do putStrLn $ "OK: Graphite values are within bounds"
                 exitSuccess
         else do putStrLn $ "CRITICAL: Graphite values are out of bounds " ++ argTarget
                 exitWith $ ExitFailure 2
  where
    hasValue :: Datapoint -> Bool
    hasValue (Datapoint (Just _) _) = True
    hasValue _                      = False
    values :: [Datapoint] -> [Double]
    values = map (\(Datapoint (Just v) _) -> v) . filter hasValue
checkMetrics (Args {..}) Nothing = do
  putStrLn $ "CRITICAL: no data " ++ argTarget
  exitWith $ ExitFailure 2

checkValues :: Args -> [Double] -> Bool
checkValues (Args {..}) = allOrNone (flip (operator argOperator) argValue)
  where
    allOrNone :: forall a. (a -> Bool) -> [a] -> Bool
    allOrNone = if argNone then none else all
    none :: forall a. (a -> Bool) -> [a] -> Bool
    none p xs = not (any p xs)

operator :: forall a. Ord a => [Char] -> a -> a -> Bool
operator "<"  = (<)
operator "<=" = (<=)
operator "==" = (==)
operator ">"  = (>)
operator ">=" = (>=)
operator _    = error "use <, <=, ==, > or >= for the operator"
                -- FIXME: use Parser with optparse-applicative

---------
-- CLI --
---------

main :: IO ()
main = execParser argsParserInfo >>= check
  where
    argsParserInfo :: ParserInfo Args
    argsParserInfo =
      info (helper <*> argsParser)
      ( fullDesc
        <> header   "CLI Check for Graphite"
        <> progDesc "Checks Graphite for out-of-bounds data" )
    argsParser :: Parser Args
    argsParser =
      Args
      <$> switch ( long "null"
                   <> help "Fire if we have null data for TARGET" )
      <*> switch ( long "none"
                   <> help "Fire only if none of the TARGET's values match" )
      <*> switch ( long "verbose"
                   <> help "Print out the data received from Graphite API" )
      <*> argument str ( metavar "URL"
                         <> help "Graphite API URL")
      <*> argument str ( metavar "TARGET"
                         <> help "Graphite Target Metric (pattern or name)")
      <*> argument str ( metavar "OPERATOR"
                         <> help "'<', '<=', '==', '>' or '>='")
      <*> argument auto ( metavar "VALUE"
                          <> help "Combined with OPERATOR to check TARGET's values")
      <*> argument auto ( metavar "MINUTES"
                          <> help "Number of previous minutes (from now) to check" )
