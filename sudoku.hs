import Data.List
import Data.Char

data Sudoku = Sudoku [[Maybe Int]]
       deriving (Show)

rows :: Sudoku -> [[Maybe Int]]
rows (Sudoku rs) = rs

example :: Sudoku
example =
  Sudoku
    [ [Just 3,Just 6,Nothing,Nothing,Just 7,Just 1,Just 2,Nothing,Nothing]
    , [Nothing,Just 5,Nothing,Nothing,Nothing,Nothing,Just 1,Just 8,Nothing]
    , [Nothing,Nothing,Just 9,Just 2,Nothing,Just 4,Just 7,Nothing,Nothing]
    , [Nothing,Nothing,Nothing,Nothing,Just 1,Just 3,Nothing,Just 2,Just 8]
    , [Just 4,Nothing,Nothing,Just 5,Nothing,Just 2,Nothing,Nothing,Just 9]
    , [Just 2,Just 7,Nothing,Just 4,Just 6,Nothing,Nothing,Nothing,Nothing]
    , [Nothing,Nothing,Just 5,Just 3,Nothing,Just 8,Just 9,Nothing,Nothing]
    , [Nothing,Just 8,Just 3,Nothing,Nothing,Nothing,Nothing,Just 6,Nothing]
    , [Nothing,Nothing,Just 7,Just 6,Just 9,Nothing,Nothing,Just 4,Just 3]
    ]

completedSudoku :: Sudoku
completedSudoku =
    Sudoku 
      [ [Just 7,Just 6,Just 3,Just 4,Just 2,Just 1,Just 5,Just 9,Just 8]
      , [Just 8,Just 1,Just 2,Just 9,Just 7,Just 5,Just 4,Just 6,Just 3]
      , [Just 9,Just 5,Just 4,Just 3,Just 8,Just 6,Just 7,Just 2,Just 1]
      , [Just 4,Just 9,Just 5,Just 2,Just 6,Just 3,Just 1,Just 8,Just 7]
      , [Just 6,Just 8,Just 1,Just 5,Just 4,Just 7,Just 9,Just 3,Just 2]
      , [Just 2,Just 3,Just 7,Just 8,Just 1,Just 9,Just 6,Just 5,Just 4]
      , [Just 1,Just 2,Just 6,Just 7,Just 5,Just 8,Just 3,Just 4,Just 9]
      , [Just 5,Just 4,Just 9,Just 1,Just 3,Just 2,Just 8,Just 7,Just 6]
      , [Just 3,Just 7,Just 8,Just 6,Just 9,Just 4,Just 2,Just 1,Just 5]
      ]

--Task A1
allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku (replicate 9 (replicate 9 Nothing))

--Task A2
isSudoku :: Sudoku -> Bool
isSudoku s     = and [a,b,c]
    where a = countRows s 
          b = countColumns s 
          c = checkNumbers s

countRows :: Sudoku -> Bool
countRows s = length (rows s) == 9

countColumns :: Sudoku -> Bool
countColumns s = length (transpose (rows s)) == 9

checkNumbers :: Sudoku -> Bool
checkNumbers s | length (rows s) == 9 = helper s 0 True
               | otherwise = False


helper :: Sudoku -> Int -> Bool -> Bool
helper s 10 _    = True
helper s i False = False
helper s i _ = helper s (i+1) (checkNumbers' ((rows s) !! i) 0 True)

checkNumbers' :: [Maybe Int] -> Int -> Bool -> Bool
checkNumbers' s 10 _    = True
checkNumbers' s _ False = False
checkNumbers' s i _     | length s == 9 
                        = checkNumbers' s (i+1) (checkNumbers'' (s !! i))
                        | otherwise = False

checkNumbers'' :: Maybe Int -> Bool
checkNumbers'' Nothing = True
checkNumbers'' i       = i >= Just 1 && i <= Just 9

--Task A3
isSolved :: Sudoku -> Bool
isSolved s = isSolved' s 0 True

isSolved' :: Sudoku -> Int -> Bool -> Bool
isSolved' s 10 _    = True
isSolved' s i False = False
isSolved' s i _     = isSolved' s (i+1) (isSolved'' ((rows s) !! i))

isSolved'' :: [Maybe Int] -> Bool
isSolved'' l = not (any (Nothing==) l)


--Task B1
printSudoku :: Sudoku -> IO ()
printSudoku s = putStrLn (helper [] 0)
    where helper c 9 = c
          helper c i = helper (c ++ (transcribe ((rows s) !! i) [] 0) ++ "\n") (i+1)

transcribe :: [Maybe Int] -> [Char] -> Int -> String
transcribe _ c 9 = c
--transcribe a c i = transcribe a (c ++ [(symbol (a !! i))]) (i+1)
transcribe a c i = transcribe a (c ++ [(symbol (a !! i))] ++ " ") (i+1) --with spaces; prettier



{-transcribe a c i | i == 0 = transcribe a ("| " ++ c ++ [(symbol (a !! i))] ++ " ") (i+1)
                 | i `mod` 3 == 2 = transcribe a (c ++ [(symbol (a !! i))] ++ " | ") (i+1)
                 | otherwise = transcribe a (c ++ [(symbol (a !! i))] ++ " ") (i+1)-}


symbol :: Maybe Int -> Char
symbol Nothing  = '.'
symbol (Just i) = (intToDigit i)

--Task B2
readSudoku :: FilePath -> IO Sudoku
readSudoku fileName = do
    text <- readFile fileName
    return $ Sudoku [a++b++c++d++e++f++g++h++i]
        where a = readSudoku' (text !! 0)
              b = readSudoku' (text !! 1)
              c = readSudoku' (text !! 2)
              d = readSudoku' (text !! 3)
              e = readSudoku' (text !! 4)
              f = readSudoku' (text !! 5)
              g = readSudoku' (text !! 6)
              h = readSudoku' (text !! 7)
              i = readSudoku' (text !! 8)

readSudoku' :: String -> [Maybe Int]
readSudoku' s = [a,b,c,d,e,f,g,h,i]
        where a = readSudoku'' (s !! 0)
              b = readSudoku'' (s !! 1)
              c = readSudoku'' (s !! 2)
              d = readSudoku'' (s !! 3)
              e = readSudoku'' (s !! 4)
              f = readSudoku'' (s !! 5)
              g = readSudoku'' (s !! 6)
              h = readSudoku'' (s !! 7)
              i = readSudoku'' (s !! 8)

readSudoku'' :: Char -> Maybe Int
readSudoku'' c | c == '.'  = Nothing
               | otherwise = (Just (digitToInt c))

--abc :: FilePath -> Maybe Int
--abc s | ((readFile s) !! 0) == '.' = (Nothing)

{-abc :: FilePath -> Sudoku
abc s = readSudoku

readSudoku :: String -> Sudoku
readSudoku s = [[a]++[b]++[c]++[d]++[e]++[f]++[g]++[h]++[i]]
    where a = readSudoku' (makeString s)
          b = readSudoku' (makeString s)
          c = readSudoku' (makeString s)
          d = readSudoku' (makeString s)
          e = readSudoku' (makeString s)
          f = readSudoku' (makeString s)
          g = readSudoku' (makeString s)
          h = readSudoku' (makeString s)
          i = readSudoku' (makeString s)

abcd :: [Maybe Int] -> 

readSudoku' :: String -> [Maybe Int]
readSudoku' s = [a,b,c,d,e,f,g,h,i]
    where a = readSudoku'' (s !! 0)
          b = readSudoku'' (s !! 1)
          c = readSudoku'' (s !! 2)
          d = readSudoku'' (s !! 3)
          e = readSudoku'' (s !! 4)
          f = readSudoku'' (s !! 5)
          g = readSudoku'' (s !! 6)
          h = readSudoku'' (s !! 7)
          i = readSudoku'' (s !! 8)

readSudoku'' :: Char -> Maybe Int
readSudoku'' c | c == '.'  = Nothing
               | otherwise = (Just (digitToInt c))-}

--test :: FilePath -> IO String
--test s = readFile s








