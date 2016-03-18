import Data.List

data Coord  = Coord {xCoord :: Int , yCoord:: Int} deriving (Show)


--  2 4 5, 6 0 1, 9 0 0

mySudoku :: [[Int]]
mySudoku =  [[2, 5, 0, 0],   --square (0,0) square (1,0)
             [0, 1, 0, 0],
             [4, 0, 7, 0],  --square (0,2) square (1,2)
             [0, 0, 5, 2]]

goodRows :: [[Int]] -> Bool
goodRows xs  = all noDups xs

goodColumns :: [[Int]] -> Bool
goodColumns xss =
  let cols = getColumnsAll xss
  in  goodRows cols

goodSquares :: [[Int]] -> Bool
goodSquares xss =
  let squares = getSquaresAll xss
  in  goodRows squares

goodSudoku :: [[Int]] -> Bool
goodSudoku xss = goodRows xss &&  goodColumns xss &&  goodSquares xss


getColumnsAll :: [[Int]] -> [[Int]]
getColumnsAll xss =
   let dim = length $ head xss
   in [getColumn xss i | i <- [0..(dim -1)]]


getColumns :: [[Int]] ->  Int -> Int -> [[Int]]
getColumns xss firstCol lastCol = [getColumn xss i | i <- [firstCol..lastCol]]

getColumn :: [[Int]] -> Int -> [Int]
getColumn xss i = [ row !! i | row <- xss]


getSquare :: [[Int]] -> Coord -> [Int]
getSquare xss (Coord x y) =
  let dim = length $ head xss --4
      littleDim =  (floor . sqrt . fromIntegral) dim
      rows = slice xss y (y+ littleDim - 1)
      columns =  getColumns rows x  (x+(littleDim-1))
  in  concat columns


getSquaresAll xss =
  let dim = length $ head xss --4
      littleDim =  (floor . sqrt . fromIntegral) dim
  in [getSquare xss (Coord x y) | x <- take littleDim [0,littleDim..], y <- take littleDim [0, littleDim..]]


slice :: [a] -> Int -> Int -> [a]
slice xs indexStart indexStop = take (indexStop - indexStart + 1)  (drop indexStart xs)

noDups :: [Int] -> Bool
noDups xs =
  let xsNoZeroes = filter (/= 0) xs
  in  xsNoZeroes == nub xsNoZeroes

origin :: Coord
origin = Coord 0 0

f = getColumn mySudoku 2
main :: IO ()
main = print ("ciao" ++
            show f ++
            show (slice [1..10] 3 7) ++
            show (getSquare mySudoku origin) ++
            show (noDups [1]) ++
            show (goodSudoku mySudoku) ++
            show (getColumnsAll mySudoku))
