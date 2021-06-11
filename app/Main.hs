import qualified Connection.Output as Connection (runTests)
import qualified Bandwidth.Output as Bandwidth (runTests)

import Data.Semigroup ((<>))
import Options.Applicative

data Options = Options
  { mode :: String
  }

options :: Parser Options
options = Options
      <$> strOption
          ( long "mode"
         <> short 'm'
         <> value "all"
         <> metavar "MODE" )

main :: IO ()
main = handle =<< execParser opts
 where
   opts = info (options <**> helper)
     ( fullDesc
    <> progDesc "Indicate MODE (connection or bandwidth)" )

handle :: Options -> IO ()
handle (Options "connection") = Connection.runTests
handle (Options "bandwidth")  = Bandwidth.runTests
handle (Options "all")        = do
  Connection.runTests
  Bandwidth.runTests
  return ()
handle (Options _)            = error "no such option"
