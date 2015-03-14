{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Exception.Lifted
import qualified Data.Aeson                as A
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as L
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Network.HTTP.Client
import           Options.Applicative
import           System.Exit
import           System.Locale

-----------
-- TYPES --
-----------

data Args = Args { argErrNoData :: Bool
                 , argFallback  :: String
                 , argTimeout   :: Int
                 , argURL       :: String
                 , argTarget    :: String
                 , argOperator  :: String
                 , argValue     :: Double
                 , argMinutes   :: Int }
          deriving (Show, Eq)

data Metric = Metric { metricTarget     :: String
                     , metricDatapoints :: [Datapoint] }
            deriving (Show, Eq)

data Datapoint = Datapoint (Maybe Double) Integer
               deriving (Show, Eq)
data Val = Val Double String
               deriving (Show, Eq)
-- JSON
$( deriveJSON defaultOptions { fieldLabelModifier = (map toLower) . drop 6 } ''Metric )
$( deriveJSON defaultOptions ''Datapoint )

-----------
-- CHECK --
-----------

check :: Args -> IO ()
check args = graphiteQuery args >>= checkMetrics args . A.decode

graphiteQuery :: Args -> IO L.ByteString
graphiteQuery args@(Args{..}) =
  let query url = withManager defaultManagerSettings $ \ mgr -> do
        req <- parseUrl (graphiteUrl url args)
        httpLbs (req { responseTimeout = Just (argTimeout * 100000) }) mgr
          >>= return . responseBody
      oops :: SomeException -> IO L.ByteString
      oops _ = query argFallback
  in handle oops $ query argURL

graphiteUrl:: String -> Args -> String
graphiteUrl url Args{..} =
  url ++ "/render/?target=" ++ argTarget
    ++ "&from=-" ++ show argMinutes ++ "min&format=json"

checkMetrics :: Args -> Maybe [Metric] -> IO ()
checkMetrics args@(Args{..}) Nothing = errNoData args
checkMetrics args@(Args{..}) (Just []) | argErrNoData = errNoData args
checkMetrics _args@(Args{..}) (Just []) = do
  putStrLn "OK: There's no data but flag --err-no-data is not set"
checkMetrics args@(Args{..}) (Just metrics) = do
  let realData = map (values . metricDatapoints) metrics
      maxDatapointListSize =
        foldl (\acc xs -> if length xs > acc
                          then length xs
                          else acc) 0 realData
  case filter (badMetricMatch args) metrics of
    [] -> do
      putStrLn $ "OK: " ++ show maxDatapointListSize
        ++ " present datapoints are " ++ argOperator
        ++ " " ++ show argValue ++ show metrics
        ++ " (most recent datapoint is " ++ show (last . last $ realData) ++ ")"
      exitSuccess
    badMetrics -> do
      putStrLn $ "CRITICAL: " ++ show argTarget ++ " has bad values:\n  "
        ++ intercalate "," (map show (map prettyPoints badMetrics))
      exitWith $ ExitFailure 2

errNoData :: forall a. Args -> IO a
errNoData Args{..} = do
  putStrLn $ "CRITICAL: no data " ++ argTarget
  exitWith $ ExitFailure 2

badMetricMatch :: Args -> Metric -> Bool
badMetricMatch args = not . checkValues args . values . metricDatapoints

checkValues :: Args -> [Double] -> Bool
checkValues Args{..} = all (flip (operator argOperator) argValue)

values :: [Datapoint] -> [Double]
values = map (\(Datapoint (Just v) _) -> v) . filter noData

prettyPoints :: Metric -> [Val]
prettyPoints Metric { metricTarget = _a , metricDatapoints = datapoints } = pretty datapoints

pretty :: [Datapoint] -> [Val]
pretty = map (\(Datapoint (Just v) t) ->
    Val v (timeToStr t) )
      . filter noData

timeToStr :: Integer -> String
timeToStr t = formatTime defaultTimeLocale "%F %TZ" (posixSecondsToUTCTime $ fromIntegral t)

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
  <$> switch ( short 'e'
               <> long "err-no-data"
               <> help "Error on empty data set from Graphite" )
  <*> strOption ( short 'f'
                  <> long "fallback"
                  <> metavar "URL"
                  <> value "http://grafana-api.knewton.net:8888"
                  <> help "Fallback URL for Graphite metrics" )
  <*> option auto ( short 't'
                    <> long "timeout"
                    <> metavar "SECONDS"
                    <> value 10
                    <> help "Seconds we'll wait for Graphite to respond" )
  <*> argument str ( metavar "URL"
                     <> help "Base URL for Graphite metrics" )
  <*> argument str ( metavar "TARGET"
                     <> help "Target metric to retrieve" )
  <*> argument str ( metavar "OPERATOR"
                     <> help "<, <=, ==, > or >=" )
  <*> argument auto ( metavar "VALUE"
                      <> help "Threshold value used in evaluation" )
  <*> argument auto ( metavar "MINUTES"
                      <> help "Window the proposition should be true" )

operator :: forall a. Ord a => [Char] -> a -> a -> Bool
operator "<"  = (<)
operator "<=" = (<=)
operator "==" = (==)
operator ">"  = (>)
operator ">=" = (>=)
operator _    = error "The operator should be one of <, <=, ==, > or >="
