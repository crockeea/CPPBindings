
import Crypto.Lol.Factored
import Crypto.Lol.Cyclotomic.Tensor.CTensor
import Crypto.Lol.Cyclotomic.Tensor

import Data.Int

main :: IO ()
main = print $ show $ (scalarPow 2 :: CT F3 Int64)