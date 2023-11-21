module Sudoku where

import Test.QuickCheck
--import Data.List(replicate)
import Data.Char(isDigit, digitToInt)
import Data.List(nub)

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

example2 :: Sudoku
example2 =
    Sudoku
      [ [j 3,j 3,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
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

-- | isSudoku sud checks if sud is really a valid representation of a sudoku, i.e. is there 9 elements in the sudoku and are all of those nine elements long.
-- puzzle
isSudoku :: Sudoku -> Bool
isSudoku s = ((length $ rows s) == 9) 
           && checkRowsLength s                                      
           && checkValidEntries s checkElement
           where 
                checkRowsLength   :: Sudoku -> Bool
                checkRowsLength   s = and (map (\n -> n == 9) (map length (rows s)))
checkElement :: Maybe Int -> Bool
checkElement e = case e of
          Just e  -> (e <= 9 && e >= 0)
          Nothing -> True

checkValidEntries :: Sudoku -> (Maybe Int -> Bool) -> Bool
checkValidEntries s func = and (map rowCheck (rows s))
                where
                    rowCheck :: Row -> Bool
                    rowCheck r  = and (map func r)
-- * A3

-- | isFilled sud checks if sud is completely filled in,
-- i.e. there are no blanks
isFilled :: Sudoku -> Bool
isFilled s = checkValidEntries s checkElement
          where
            checkElement e = case e of
                             Just e -> True
                             Nothing -> False

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen

--Should change all elements to n or ., then print each row, then 
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr (combineRows (map createChars (rows s)))
            where
              combineRows :: [[Char]] -> String
              combineRows (r:rs) = r ++ ['\n'] ++ combineRows rs
              combineRows []     = []
              createChars :: [Maybe Int] -> [Char]
              createChars r = map monadToChar r
              monadToChar :: Maybe Int -> Char
              monadToChar n = case n of
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
                      if (isSudoku sudoku)
                        then return sudoku
                      else error("file does not contain a valid Sudoku")
                      return sudoku

parseSudokuFile :: [String] -> Sudoku
parseSudokuFile s = Sudoku $ map buildRow s
              where

                buildRow :: [Char] -> [Cell]
                buildRow s = buildRowHelper s []
                buildRowHelper :: String -> [Cell] -> [Cell]
                buildRowHelper (s:ss) l | isDigit s = buildRowHelper ss $ (Just (digitToInt s)):l
                                        | s == '.'  = buildRowHelper ss $ (Nothing):l
                                        | s == '\n' = reverse l
                buildRowHelper _ l                 = reverse la


------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9, elements[Nothing]),(1, rNum)]
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

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku sud = isSudoku sud
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock b = (and $ map checkElement b) 
              && (length b == 9) 
              && (length justs == length (nub justs))
            where
              justs = filter (/= Nothing) b


-- * D2

blocks :: Sudoku -> [Block]
blocks (Sudoku sudoku) = cSquares ++ cRows ++ cColumns
      where
        cSquares = getSquares sudoku
        getSquares [] = []
        getSquares (r1:r2:r3:rs) =  [(take 3 r1) ++ (take 3 r2) ++ (take 3 r3)
                               , (take 3 (drop 3 r1)) ++ (take 3 (drop 3 r2))  ++ (take 3 (drop 3 r3)) 
                               , (drop 6 r1) ++ (drop 6 r2) ++ (drop 6 r3)] 
                               ++ getSquares rs -- TODO: GÖR DENNA SNYGG (Som skepparn 😉)
        cRows    = sudoku
        cColumns = getCols 0 sudoku
        getCols 8 sud = map (!! 8) sud:[]
        getCols n sud = map (!! n) sud: getCols (n+1) sud 

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = ((length bs) == 27)
                           && (and $ map (\x -> length x == 9) bs)
                           where 
                            bs = blocks sudoku

-- * D3

isOkay :: Sudoku -> Bool
isOkay s = (isSudoku s) && (and $ map isOkayBlock $ blocks s) -- 👍


---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
