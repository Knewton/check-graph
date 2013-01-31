{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

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

data Args = Args { argURL      :: String
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
check args = graphiteQuery args >>= checkMetrics args . A.decode

graphiteQuery :: Args -> IO L.ByteString
graphiteQuery args = C.simpleHttp (graphiteUrl args) >>= return

graphiteUrl:: Args -> String
graphiteUrl (Args{..}) =
  argURL ++ "/render/?target=" ++ argTarget
  ++ "&from=-" ++ show argMinutes ++ "min&format=json"

checkMetrics :: Args -> Maybe [Metric] -> IO ()
checkMetrics args@(Args {..}) (Just metrics) = do
  if all (checkValues args . values . metricDatapoints) metrics
    then do putStrLn $ "OK: Graphite values that are present are OK"
            exitSuccess
    else do putStrLn $ "CRITICAL: " ++ argTarget
            exitFailure
checkMetrics Args {..} Nothing = do
  putStrLn $ "CRITICAL: no data " ++ argTarget
  exitFailure

checkValues :: Args -> [Double] -> Bool
checkValues (Args {..}) = all (flip (operator argOperator) argValue)

values :: [Datapoint] -> [Double]
values = map (\(Datapoint (Just v) _) -> v) . filter noData

noData :: Datapoint -> Bool
noData (Datapoint Nothing _) = False
noData _                     = True

---------
-- CLI --
---------

main :: IO ()
main = execParser argsParserInfo >>= check

argsParserInfo :: ParserInfo Args
argsParserInfo =
  info (helper <*> argsParser)
  ( fullDesc
    <> header   "CLI Check for Graphite"
    <> progDesc "Checks Graphite for out-of-bounds data" )

argsParser :: Parser Args
argsParser =
  Args
  <$> argument str  ( metavar "URL" )
  <*> argument str  ( metavar "TARGET" )
  <*> argument str  ( metavar "OPERATOR" )
  <*> argument auto ( metavar "VALUE" )
  <*> argument auto ( metavar "MINUTES" )

operator :: forall a. Ord a => [Char] -> a -> a -> Bool
operator "<"  = (<)
operator "<=" = (<=)
operator "==" = (==)
operator ">"  = (>)
operator ">=" = (>=)
operator _    = error "FIXME: use Parser with optparse-applicative"
