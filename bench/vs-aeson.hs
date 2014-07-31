import Criterion.Main
import Data.Aeson (Value)
import Data.ByteString (ByteString)
import qualified Data.Aeson        as Aeson
import qualified Data.ByteString   as B
import qualified Data.JSON.Stream  as JS
import System.Environment (getArgs)
import System.FilePath (takeFileName)

type Label = String

aeson :: ByteString -> Maybe Value
aeson = Aeson.decodeStrict

aesonStricter :: ByteString -> Maybe Value
aesonStricter = Aeson.decodeStrict'

js :: ByteString -> Maybe Value
js = JS.decodeValue

benchOnBS :: Label -> ByteString -> Benchmark
benchOnBS label str =
	bgroup label
    [ bench "aeson"          $ nf aeson str
    , bench "aeson-stricter" $ nf aesonStricter str
    , bench "json-streams"   $ nf js str
    ]

main :: IO ()
main = do
  users20     <- file "users_20.json"
  users200    <- file "users_200.json"
  users2000   <- file "users_2000.json"
  users20000  <- file "users_20000.json"
  users200000 <- file "users_200000.json"

  defaultMain
    [ bgroup "users"
        [ benchOnBS "20"     users20
        , benchOnBS "200"    users200
        , benchOnBS "2000"   users2000
        , benchOnBS "20000"  users20000
        , benchOnBS "200000" users200000
        ]
    ]

file :: FilePath -> IO ByteString
file fp = B.readFile $ "bench/json_files/" ++ fp
