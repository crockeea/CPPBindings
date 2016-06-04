
module HLibWithC
( addHInt, addHDouble ) where

import Data.Int

addHInt :: Int64 -> IO Int64
addHInt = addCInt

addHDouble :: Double -> IO Double
addHDouble = addCDouble

foreign import ccall unsafe "addCInt" addCInt :: Int64 -> IO Int64
foreign import ccall unsafe "addCDouble" addCDouble :: Double -> IO Double
