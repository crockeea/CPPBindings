
import HLibWithCPP

main :: IO ()
main = do
  let x = 1
      y = 2.0
  x' <- addHInt x
  y' <- addHDouble y
  if (x' == x+1) && (y' == y+1.0)
  then print "SUCCESS"
  else print "FAIL"