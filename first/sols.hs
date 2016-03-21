import Data.List
import Debug.Trace (traceShow)

foo a b c = traceShow ("input a: " ++ show a) (a + b + c)
main' = print (foo 1 2 3)


data Coord  = Coord {xCoord :: Int , yCoord:: Int}
instance Show Coord where
  show (Coord x y ) = "(" ++ show x ++ ", " ++ show y  ++ ")"

data Action = Action Int Coord

instance Show Action where
  show (Action num (Coord x y )) = " (" ++ show x ++ ", " ++ show y ++ ") = "  ++ show num


data Solution = Solution [[Int]]
data Outcome = Outcome [[Int]]

instance Show Outcome where
  show (Outcome xss) = concatMap (\row -> "\n" ++ show row ) xss

instance Show Solution where
  show (Solution xss) = concatMap (\row -> "\n" ++ show row ) xss

markAsSolution :: [[Int]] -> Solution
markAsSolution xss = Solution xss

myMove = Action 1 (Coord 0 0)

type CubeState = [[Int]]
data ActionScheduled = AS (Coord, Int)

data Q = Q [(CubeState, ActionScheduled)]

solutions :: [[Int]] -> Q -> [Solution]
solutions = undefined


solutions3 :: [[Int]] ->  [Solution]
solutions3 xss
  | isSolution xss = [ Solution xss]
  | noZeroes xss = []
  | otherwise =
      let outcomes = allOutcomes xss
          pureOutcomes = (map  stripOutcome outcomes)
          firstSolutions = filter isSolution pureOutcomes
          secondSolutions = concatMap solutions3 pureOutcomes
          totalSolutions = (map Solution firstSolutions) ++ secondSolutions
      in  totalSolutions

solutions' :: [[Int]] -> [[[Int]]] -> [Solution]
solutions' xss toVisitOutcomes
  | isSolution xss  =  [ Solution xss]
  | noZeroes xss  = []
  | otherwise =
       let childs = allOutcomes' xss
       in
        if  (not . null) childs then
           let
            out1 = head childs
            toTryOuts = tail childs
           in    solutions' out1 (toTryOuts ++ toVisitOutcomes)
        else
          solutions' (head toVisitOutcomes) (tail toVisitOutcomes)

solutions2 :: [[Int]] -> [Action] -> [Solution]
solutions2 xss toTryActions'
  | isSolution xss  =  [ Solution xss]
  | noZeroes xss  = traceShow xss []
  | otherwise =
       let childs = allPossibleActions xss
       in
        if  (not . null) childs then
           let
            action1 = head childs
            out1 = outcome xss action1
            toTryActions = tail childs
           in    solutions2 xss (toTryActions' ++ toTryActions )
        else
          let a1 = head toTryActions'
              outcome1 = outcome xss a1
          in solutions2 xss (tail toTryActions')






test0 = let x = solutions' myT0 [] in (x, length x)
test1 = let x = solutions' myT1 [] in (x, length x)
test2 = let x = solutions' myT2 [] in (x, length x)
test3 = let x = solutions' myT3 [] in (x, length x)
test4 = let x = solutions' myT4 [] in (x, length x)
test5 = let x = solutions' myT5 [] in (x, length x)
test6 = let x = solutions' myT6 [] in (x, length x)
test7 = let x = solutions' myT7 [] in (x, length x)
test8 = let x = solutions' myT8 [] in (x, length x)
test9 = let x = solutions' myT9 [] in (x, length x)
test10 = let x = solutions' myT10 [] in (x, length x)
test11 = let x = solutions' myT11 [] in (x, length x)
test12 = let x = solutions' myT12 [] in (x, length x)
test30 = let x = solutions' myT30 [] in (x, length x)

test40 = let x = solutions2 myT4 [] in (x, length x)
test411 = let x = solutions2 myT11 [] in (x, length x)
test430 = let x = solutions2 myT30 [] in (x, length x)
test431 = let x = solutions2 myT31 [] in (x, length x)
test432 = let x = solutions2 myT32 [] in (x, length x)
test433 = let x = solutions2 myT33 [] in (x, length x)
test434 = let x = solutions2 myT34 [] in (x, length x)


stripOutcome :: Outcome -> [[Int]]
stripOutcome (Outcome xss) = xss

allOutcomes :: [[Int]] -> [Outcome]
allOutcomes xss = [ Outcome (outcome xss action) | c <- allFreeCoords xss, action <- possibleActions xss c]

allOutcomes' :: [[Int]] -> [[[Int]]]
allOutcomes' xss = [  outcome xss action | c <- allFreeCoords xss, action <- possibleActions xss c]

possibleActions :: [[Int]] -> Coord -> [Action]
possibleActions xss (Coord x y) =
  let dim = getDim xss
  in [Action n (Coord x y) | n <- [1..dim], n `elem` freeNumbers xss x y]


allPossibleActions :: [[Int]] -> [Action]
allPossibleActions xss = concat [ possibleActions xss c | c <- allFreeCoords xss]

outcome :: [[Int]] -> Action -> [[Int]]
outcome xss (Action num (Coord x y)) =
  let row = xss !! y
      row' = replaceElement row x num
  in replaceElement xss y row'

allFreeCoords :: [[Int]] -> [Coord]
allFreeCoords xss =
  let dim = getDim xss
      lastIndex = (dim-1)
  in  [Coord x y | x <- [0..lastIndex], y <- [0..lastIndex], ((xss !! y) !! x) == 0]



noZeroes :: [[Int]] -> Bool
noZeroes yss = 0 `notElem`  concat yss

isSolution :: [[Int]] -> Bool
isSolution xss = goodSudoku xss && noZeroes xss


freeNumbers :: [[Int]] ->  Int -> Int -> [Int]
freeNumbers xss x y =
    let rowY = xss !! y
        element = rowY !! x
    in  freeNums' element
    where
      freeNums' element
        | element /= 0 = []
        | element == 0 =
          let coordSquare = coordSquareOfElem xss x y
          in intersectionMany [ numbersInRow xss y, numbersInColumn xss x, numbersInSquare xss coordSquare]


coordSquareOfElem :: [[Int]] -> Int -> Int -> Coord
coordSquareOfElem xss x y  =
   let
       littleDim = getLittleDim xss::Int
       x' = (x `div` littleDim) * littleDim
       y' = y `div` littleDim * littleDim
   in Coord x' y'

numbersInRow :: [[Int]] -> Int -> [Int]
numbersInRow xss y = let dim = getDim xss in [n | n <- [1..dim],  n `notElem` (xss !! y)    ]

numbersInColumn :: [[Int]] -> Int -> [Int]
numbersInColumn xss x = let dim = getDim xss in [n | n <- [1..dim],  n `notElem` getColumn xss x    ]

numbersInSquare :: [[Int]] -> Coord -> [Int]
numbersInSquare xss (Coord x y)  = let dim = getDim xss in [n | n <- [1..dim],  n `notElem` (getSquare xss (Coord x y))   ]



intersection xs ys = filter (\x -> elem x ys) xs

intersectionMany []  =  []
intersectionMany [xs]  =  xs
intersectionMany (xs:xss) = intersection xs  (intersectionMany xss) -- foldr intersection [] xss



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

getDim :: [[Int]] -> Int
getDim xss = length $ head xss --4

getLittleDim :: [[Int]] -> Int
getLittleDim xss =
  let dim = length $ head xss --4
      littleDim =  (floor . sqrt . fromIntegral) dim
  in littleDim

getSquare :: [[Int]] -> Coord -> [Int]
getSquare xss (Coord x y) =
  let littleDim = getLittleDim xss
      rows = slice xss y (y+ littleDim - 1)
      columns =  getColumns rows x  (x+(littleDim-1))
  in  concat columns

getSquaresAll :: [[Int]] -> [[Int]]
getSquaresAll xss =
  let dim = length $ head xss --4
      littleDim =  (floor . sqrt . fromIntegral) dim
  in [getSquare xss (Coord x y) | x <- take littleDim [0,littleDim..], y <- take littleDim [0, littleDim..]]


slice :: [a] -> Int -> Int -> [a]
slice xs indexStart indexStop = take (indexStop - indexStart + 1)  (drop indexStart xs)

replaceElement :: [a] -> Int -> a -> [a]
replaceElement xs index newValue   =  take index xs ++ [newValue] ++ drop (index+1) xs

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

            --  2 4 5, 6 0 1, 9 0 0

mySudoku :: [[Int]]
mySudoku =  [[3, 0, 0, 2],   --square (0,0) square (1,0)
             [0, 4, 1, 0],
             [0, 3, 2, 0],  --square (0,2) square (1,2)
             [4, 0, 0, 1]]

mySudoku' :: [[Int]]
mySudoku' =  [[3, 0, 0, 2],   --square (0,0) square (1,0)
              [0, 4, 0, 0],
              [0, 0, 2, 0],  --square (0,2) square (1,2)
              [0, 0, 0, 0]]



myEasySudoku :: [[Int]]
myEasySudoku = [ [0, 1, 2, 4],
                 [4, 2, 3, 0],
                 [1, 0, 4, 2],
                 [2, 4, 1, 0]
                 ]

myT0 :: [[Int]]
myT0 = [  [3, 1, 2, 4],
          [4, 2, 3, 1],
          [1, 3, 4, 2],
          [2, 4, 1, 0]
       ]

myT1 :: [[Int]]
myT1 = [  [0, 1, 2, 4],
          [4, 2, 3, 1],
          [1, 3, 4, 2],
          [2, 4, 1, 0]
       ]

myT2 :: [[Int]]
myT2 = [  [0, 1, 2, 4],
          [4, 2, 3, 1],
          [1, 0, 4, 2],
          [2, 4, 1, 0]
       ]

myT3 :: [[Int]]
myT3 = [  [0, 1, 2, 4],
          [4, 2, 0, 1],
          [1, 0, 4, 2],
          [2, 4, 1, 0]
       ]

myT4 :: [[Int]]
myT4 = [  [0, 1, 2, 0],
          [4, 2, 0, 1],
          [1, 0, 4, 2],
          [2, 4, 1, 0]
       ]

myT5 :: [[Int]]
myT5 = [  [0, 1, 2, 0],
          [0, 2, 0, 1],
          [1, 0, 4, 2],
          [2, 4, 1, 0]
       ]

myT6 :: [[Int]]
myT6 = [  [0, 1, 2, 0],
          [0, 2, 0, 1],
          [1, 0, 4, 0],
          [2, 4, 1, 0]
       ]

myT7 :: [[Int]]
myT7 = [  [0, 1, 2, 0],
          [0, 2, 0, 1],
          [1, 0, 4, 0],
          [0, 4, 1, 0]
       ]

myT8 :: [[Int]]
myT8 = [  [0, 1, 2, 0],
          [0, 2, 0, 1],
          [1, 0, 4, 0],
          [0, 0, 1, 0]
       ]
myT9 :: [[Int]]
myT9 = [  [0, 1, 0, 0],
          [0, 2, 0, 1],
          [1, 0, 4, 0],
          [0, 0, 1, 0]
       ]

myT10 :: [[Int]]
myT10 = [ [0, 1, 0, 0],
          [0, 0, 0, 1],
          [1, 0, 4, 0],
          [0, 0, 1, 0]
       ]

myT11 :: [[Int]]
myT11 = [ [0, 1, 0, 0],
          [0, 0, 0, 1],
          [1, 0, 0, 0],
          [0, 0, 1, 0]
       ]

myT12 :: [[Int]]
myT12 = [ [0, 1, 0, 0],
          [0, 0, 0, 1],
          [1, 0, 0, 0],
          [0, 0, 0, 0]
       ]
myT30 :: [[Int]]
myT30 = [
          [0,    0,    9,    4,    0,    0,    8,    5,    0],
          [5,    0,    0,    7,    0,    0,    4,    0,    0],
          [2,    8,    0,    1,    0,    0,    0,    0,    0],
          [0,    9,    5,    0,    0,    1,    0,    0,    0],
          [0,    1,    0,    0,    6,    0,    0,    4,    0],
          [0,    0,    0,    9,    0,    0,    5,    7,    0],
          [0,    0,    0,    0,    0,    9,    0,    6,    4],
          [0,    0,    7,    0,    0,    5,    0,    0,    9],
          [0,    4,    8,    0,    0,    7,    3,    0,    0]
      ]

myT31 :: [[Int]]
myT31 = [
          [5,6,8,3,1,9,4,7,2],
          [7,9,1,4,5,2,3,6,8],
          [2,3,4,6,7,8,5,9,1],
          [6,1,9,5,8,4,7,2,3],
          [3,2,5,7,9,1,6,8,4],
          [4,8,7,2,3,6,9,1,5],
          [1,4,3,8,6,7,2,5,9],
          [8,5,6,9,2,3,1,4,7],
          [9,7,2,1,4,5,8,3,0]
      ]

myT32 :: [[Int]]
myT32 = [
          [0,0,8,3,1,9,4,0,2],
          [0,0,1,4,5,2,3,0,0],
          [2,3,4,6,7,8,5,9,1],
          [6,1,9,5,8,4,7,2,3],
          [3,2,5,7,9,1,6,8,4],
          [4,8,7,2,3,6,9,1,5],
          [1,4,3,8,6,7,2,5,9],
          [8,5,6,9,2,3,1,4,7],
          [9,7,2,1,4,5,8,3,0]
      ]
myT33 :: [[Int]]
myT33 = [
          [0,0,8,3,1,9,4,0,2],
          [0,0,1,4,5,2,3,0,0],
          [2,3,4,6,0,0,5,9,1],
          [6,1,9,0,0,4,0,0,0],
          [3,2,5,7,9,1,0,0,0],
          [4,0,0,2,0,0,9,1,5],
          [1,4,3,8,6,7,2,5,9],
          [8,5,6,9,2,3,1,4,7],
          [9,7,2,1,4,5,8,3,0]
      ]

myT34 :: [[Int]]
myT34 = [
          [0,0,8,3,1,9,4,0,2],
          [0,0,1,4,5,2,3,0,0],
          [2,3,4,6,0,0,5,9,1],
          [6,1,9,0,0,4,0,0,0],
          [0,2,5,0,9,0,0,0,0],
          [4,0,0,2,0,0,9,1,5],
          [1,4,3,0,0,0,0,5,9],
          [0,0,6,9,0,3,1,0,0],
          [9,7,2,1,0,5,8,0,0]
      ]

mySolvedSudoku :: [[Int]]
mySolvedSudoku = [ [3, 1, 2, 4],
                   [4, 2, 3, 1],
                   [1, 3, 4, 2],
                   [2, 4, 1, 3]
                 ]

myBadSudoky :: [[Int]]
myBadSudoky =    [ [4, 1, 2, 4],
                   [4, 2, 3, 1],
                   [1, 3, 4, 2],
                   [2, 4, 1, 3]
                 ]
