
import Data.Int

main :: IO ()
main = do
  let x = 123
      q = 7
  helloC x q
  putStrLn $ "Hello from Haskell! " ++ (show x) ++ " = " ++ (show $ x `mod` q) ++ " mod " ++ (show q)

foreign import ccall unsafe "helloC" helloC :: Int64 -> Int64 -> IO ()