{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Acme.LookOfDisapproval
import           Control.Exception.Lifted
import           Control.Monad.IO.Class
import qualified Data.Aeson                as A
import           Data.Aeson.TH
import qualified Data.ByteString.Lazy      as L
import qualified Data.ByteString.Lazy.UTF8 as L
import           Data.Char
import           Data.List
import           Network.HTTP.Client
import qualified Network.HTTP.Conduit      as C
import           Network.HTTP.Types
import           Options.Applicative       hiding (value)
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
  let fallback = "http://grafana.knewton.net:8888"
      timeout = 10 * 100000 -- timeout in microseconds
      query url = withManager defaultManagerSettings $ \ mgr -> do
        req <- parseUrl (graphiteUrl url args)
        httpLbs (req { responseTimeout = Just timeout }) mgr
          >>= return . responseBody
      oops :: SomeException -> IO L.ByteString
      oops e = query fallback
  in handle oops $ query argURL

graphiteUrl:: String -> Args -> String
graphiteUrl url (Args{..}) =
  url ++ "/render/?target=" ++ argTarget
  ++ "&from=-" ++ show argMinutes ++ "min&format=json"

checkMetrics :: Args -> Maybe [Metric] -> IO ()
checkMetrics args@(Args {..}) (Just metrics) | not (null metrics) = do
  case filter (badMetricMatch args) metrics of
    [] -> do
      putStrLn $ "OK: Graphite values that are present are OK"
      exitSuccess
    badMetrics -> do
      putStrLn $ "CRITICAL: " ++
        intercalate " " (map (L.toString . A.encode) badMetrics)
      exitWith $ ExitFailure 2
checkMetrics Args {..} _ = do
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
operator _    = ಠ_ಠ "The operator should be one of <, <=, ==, > or >="
