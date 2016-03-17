

data Coord  = Coord {xCoord :: Int , yCoord:: Int} deriving (Show)




mySudoku :: [[Int]]
mySudoku =  [[2, 5, 0, 0],   --square (0,0) square (1,0)
             [0, 1, 0, 0],
             [4, 0, 7, 0],  --square (0,2) square (1,2)
             [0, 0, 5, 2]]

getColumnsAll :: [[Int]] -> [[Int]]
getColumnsAll xss =
   let dim = length $ head xss
   in [(getColumn xss i) | i <- [0..(dim -1)]]

getColumns :: [[Int]] ->  Int -> Int -> [[Int]]
getColumns xss firstCol lastCol =
  let dim = length $ head xss
  in [(getColumn xss i) | i <- [firstCol..(lastCol)]]

getColumn :: [[Int]] -> Int -> [Int]
getColumn xss i = [ row !! i | row <- xss]


getSquare :: [[Int]] -> Coord -> [[Int]]
getSquare xss (Coord x y) =
  let dim = length $ head xss --4
      littleDim =  (floor . sqrt . fromIntegral) dim 
      rows = slice xss y (y+ littleDim - 1)
      r = getColumns rows x  (x+(littleDim-1))
  in  r



slice :: [a] -> Int -> Int -> [a]
slice xs indexStart indexStop = take (indexStop - indexStart + 1)  (drop indexStart xs)

origin :: Coord
origin = Coord 0 0

f = getColumn mySudoku 2
main :: IO ()
main = print ("ciao" ++
            show f ++
            show (slice [1..10] 3 7) ++
            show (getSquare mySudoku origin) ++
            show (getColumnsAll mySudoku))
