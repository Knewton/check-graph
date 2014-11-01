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
import           Network.HTTP.Client
import           Options.Applicative
import           System.Exit

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

-- JSON
$( deriveJSON defaultOptions { fieldLabelModifier = (map toLower) . drop 6 } ''Metric )
$( deriveJSON defaultOptions ''Datapoint )

-----------
-- CHECK --
-----------

check :: Args -> IO ()
check args = graphiteQuery args >>= checkMetrics args . A.decode

graphiteQuery :: Args -> IO L.ByteString
graphiteQuery args@(Args {..}) =
  let query url = withManager defaultManagerSettings $ \ mgr -> do
        req <- parseUrl (graphiteUrl url args)
        httpLbs (req { responseTimeout = Just (argTimeout * 100000) }) mgr
          >>= return . responseBody
      oops :: SomeException -> IO L.ByteString
      oops _ = query argFallback
  in handle oops $ query argURL

graphiteUrl:: String -> Args -> String
graphiteUrl url (Args{..}) =
  url ++ "/render/?target=" ++ argTarget
  ++ "&from=-" ++ show argMinutes ++ "min&format=json"

checkMetrics :: Args -> Maybe [Metric] -> IO ()
checkMetrics args@(Args {..}) (Just metrics) = do
  case filter (badMetricMatch args) metrics of
    []         -> do
      putStrLn $ "OK: Graphite values that are present are OK"
      exitSuccess
    badMetrics -> do
      putStrLn $ "CRITICAL: " ++
        intercalate " " (map (L.toString . A.encode) badMetrics)
      exitWith $ ExitFailure 2
checkMetrics Args {..} Nothing = do
  putStrLn $ "CRITICAL: no data " ++ argTarget
  exitWith $ ExitFailure 2

badMetricMatch :: Args -> Metric -> Bool
badMetricMatch args = not . checkValues args . values . metricDatapoints

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
  <$> switch ( short 'e'
               <> long "err-no-data"
               <> help "Error on empty data set from Graphite" )
  <*> strOption ( short 'f'
                  <> long "fallback"
                  <> metavar "URL"
                  <> value "http://grafana-api.knewton.net:8888" )
  <*> option auto ( short 't'
                    <> long "timeout"
                    <> metavar "SECONDS"
                    <> value 10 )
  <*> argument str ( metavar "URL" )
  <*> argument str ( metavar "TARGET" )
  <*> argument str ( metavar "OPERATOR" )
  <*> argument auto ( metavar "VALUE" )
  <*> argument auto ( metavar "MINUTES" )

operator :: forall a. Ord a => [Char] -> a -> a -> Bool
operator "<"  = (<)
operator "<=" = (<=)
operator "==" = (==)
operator ">"  = (>)
operator ">=" = (>=)
operator _    = error "The operator should be one of <, <=, ==, > or >="
