import Data.JSON.Stream
import qualified Data.ByteString as B
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file] -> go file
    _      -> usage

usage :: IO ()
usage = putStrLn "Usage: js-profile <filepath>"

go :: FilePath -> IO ()
go fp = do
  str <- B.readFile fp
  result (putStrLn $ "Couldn't parse content of " ++ fp ++ " as JSON")
         (\v str -> do
            putStrLn $ "Parsed: " ++ show v
            putStrLn $ "Left unconsumed: " ++ show str)
         (runParser parseValue str)
