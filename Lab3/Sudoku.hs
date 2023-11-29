module Sudoku where

import Test.QuickCheck
--import Data.List(replicate)
import Data.Char(isDigit, digitToInt)
import Data.List
import Data.Maybe

------------------------------------------------------------------------------

-- Ä1
-- | Representation of a Digit type that makes the implementation safer
--TODO 
--data Digit = 

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just


-- * A1

-- | allBlankSudoku is a sudoku with just blanks
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

allFilledSudoku :: Sudoku
allFilledSudoku = Sudoku (replicate 9 (replicate 9 $ Just 1))
-- * A2

-- | isSudoku sud checks if sud is really a valid representation of a sudoku,
--   i.e. is there 9 elements in the sudoku and are all of those nine elements long.
isSudoku :: Sudoku -> Bool
isSudoku s = (length (rows s) == 9)
           && checkRowsLengths s                                      
           && checkValidEntries s checkCell
           where 
                checkRowsLengths s = all ((== 9) . length) (rows s)

-- | Checks whether a cell is a valid cell
checkCell :: Maybe Int -> Bool
checkCell e = case e of
          Just e  -> e <= 9 && e > 0
          Nothing -> True

-- | Checks whether all cells in a sudoku satesfies the function applied
checkValidEntries :: Sudoku -> (Maybe Int -> Bool) -> Bool
checkValidEntries s func = all (all func) (rows s)
-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = checkValidEntries s isJust


------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr (unlines (map (map maybeToChar) (rows s)))
            where
              maybeToChar n = case n of
                              Just n -> head (show n)
                              Nothing -> '.'

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = 
                    do
                      s <- readFile filePath
                      let sudoku = parseSudokuFile (lines s)
                      if isSudoku sudoku
                        then return sudoku
                      else error "file does not contain a valid Sudoku"
                      return sudoku

-- | parseSudokuFile takes the rows of a Sudoku string, and parses it to the row type and builds a Sudoku from it.
-- TODO: This can be shortend to a substantialy smaller size using line finction probably.. 
parseSudokuFile :: [String] -> Sudoku
parseSudokuFile s = Sudoku $ map buildRow s
              where
                buildRow :: [Char] -> [Cell]
                buildRow s = buildRowHelper s []
                buildRowHelper :: String -> [Cell] -> [Cell]
                buildRowHelper (s:ss) l | isDigit s = buildRowHelper ss (Just (digitToInt s) : l)
                                        | s == '.'  = buildRowHelper ss (Nothing : l)
                                        | s == '\n' = reverse l
                buildRowHelper _ l                  = reverse l


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen Cell
cell = frequency [(9, return Nothing),(1, rNum)]
        where
            rNum = do
                  n <- choose(1, 9)
                  return $ Just n


-- * C2

-- | an instance for generating Arbitrary Sudokusx  
instance Arbitrary Sudoku where
  arbitrary = do
                sud <- vectorOf 9 $ vectorOf 9 cell
                return $ Sudoku sud
 
-- * C3
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1
-- | Checks whether a Block is a valid sudoku block.I.e. all blocks have nine cells and no Just Ints are duplicated.
isOkayBlock :: Block -> Bool
isOkayBlock b = all checkCell b 
              && (length b == 9) 
              && (length justs == length (nub justs))
            where
              justs = filter isJust b


-- * D2

-- | Create all blocks blocks from the sudoku
blocks :: Sudoku -> [Block]
blocks (Sudoku sudoku) = cSquares ++ cRows ++ cColumns
      where
        cSquares = getSquares sudoku
        getSquares [] = []
        getSquares sud = [concatMap (take 3) (take 3 sud), 
                          concatMap (take 3 . drop 3) (take 3 sud),
                          concatMap (drop 6) (take 3 sud)]
                           ++ getSquares (drop 3 sud)
        cRows    = sudoku
        cColumns = transpose sudoku

-- | Checks that the amount blocks are of the correct amount
prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = (length bs == 27)
                          && all (\x -> length x == 9) bs
                  where 
                    bs = blocks sudoku

-- * D3
-- | Checks whether a given sudoku satisfies all the rules of Sudoku? 
isOkay :: Sudoku -> Bool
isOkay s = isSudoku s && all isOkayBlock (blocks s)


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1
-- | Finds all blank cells in a given sudoku and returns thier positions
blanks :: Sudoku -> [Pos]
blanks (Sudoku sudoku) = blanks' (concat sudoku) 0
    where 
      blanks' [] _         = []
      blanks' (c:cs) i | isNothing c = 
                       (i `div` 9, i `mod` 9) : blanks' cs (i+1)
                               | otherwise    = blanks' cs (i+1)

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = length  (blanks allBlankSudoku) == 81


-- * E2
-- that, given a list, and a tuple containing an index in the list and a new value, 
-- updates the given list with the new value at the given index. Examples:
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) [] _ = []
(!!=) (x:xs) (0, a)                     = a : xs
(!!=) (x:xs) (n, a) | n > length xs = error "Index out of bounds"
                    | otherwise         = x : (xs !!= (n-1, a))



-- Checks the expected properties: 
-- Input and output lists have the same length, 
-- The element at the index in the output list is indeed replaced by the new value.
prop_bangBangEquals_correct :: [Int] -> (Int,Int) -> Bool
prop_bangBangEquals_correct xs (n, a) = prop_bangBangEquals_sameLength xs (n', a) 
                                     && prop_bangBangEquals_indexChanged xs (n', a)
  where 
    n' = min (abs n) (length xs - 1)
    prop_bangBangEquals_sameLength xs (i, y)   = length xs == length (xs !!= (i, y))
    prop_bangBangEquals_indexChanged [] _      = True
    prop_bangBangEquals_indexChanged xs (i, y) = (xs !!= (i, y)) !! i == y


-- * E3
-- given a Sudoku, a position, and a new cell value, 
-- updates the given Sudoku at the given position with the new value
update :: Sudoku -> Pos -> Cell -> Sudoku
update (Sudoku sudoku) (x,y) c = Sudoku (sudoku !!= (x, (sudoku !! x) !!= (y, c)))

-- Checks that the updated position really has gotten the new value.
prop_update_updated :: Sudoku -> Pos -> Cell -> Bool
prop_update_updated (Sudoku sudoku) (x,y) c = 
          rows (update (Sudoku sudoku) (x',y') c) !! x' !! y' == c
  where x' = min (abs x) 8
        y' = min (abs y) 8


------------------------------------------------------------------------------

-- * F1
-- | solves the given sudoku using a simple backtracking algorithm.
solve :: Sudoku -> Maybe Sudoku
solve s = case solve' s (blanks s) of
            [] -> Nothing
            (s:_) -> Just s
  where
    solve' :: Sudoku -> [Pos] ->  [Sudoku]
    solve' s [] = [s] 
    solve' s (p:ps) = concat [solve' n ps | 
            n <- map (update s p . Just) [1..9], isOkay n] 

  
-- * F2
-- | reads a sudoku from a file, solves the sudoku, and prints the result on the screen.
readAndSolve :: FilePath -> IO ()
readAndSolve f = do
            s <- readSudoku f
            let solved = fromJust $ solve s 
            printSudoku solved

-- * F3
-- Checks whether the first sudoku is a solution of the second sudoku
isSolutionOf :: Sudoku -> Sudoku -> Bool
isSolutionOf (Sudoku solved) (Sudoku sud) = isSolutionOf' (concat solved) (concat sud) 
                                            && (length $ blanks (Sudoku solved)) == 0
      where
        isSolutionOf' :: [Cell] -> [Cell] -> Bool
        isSolutionOf' [] []                         = True
        isSolutionOf' (s:ss) (c:cs) | s == c       = isSolutionOf' ss cs
                                    | isNothing c  = isSolutionOf' ss cs
                                    | otherwise    = False

-- * F4
-- Checks that the solution is a valid sudoku and that it is a solution of the original sudoku.
prop_SolveSound :: Sudoku -> Bool  
prop_SolveSound sudoku = case solve sudoku of
                            Nothing -> True
                            Just s  -> isSolutionOf s sudoku

-- | Does the same as quickCheck, except that it only tests 30 tests.
fewerChecks :: Testable prop => prop -> IO ()
fewerChecks = quickCheckWith stdArgs{ maxSuccess = 30 }