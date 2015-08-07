import Data.List (sort)

readInt :: String -> Int
readInt = read

next c = show (((readInt c) + 1) `mod` 10)
cycs2 s = concat [next [c] | c <- s]
cycs1 = sort . cycs2
takeUntilDuplicate = foldr (\x r -> x : takeWhile (/= x) r) []
grow x = x ++ grow [cycs1 (last x)]
cycles x = (takeUntilDuplicate . grow) [sort x]

main = do
  print (cycles "714")
  -- ["147","258","369","047","158","269","037","148","259","036"]

