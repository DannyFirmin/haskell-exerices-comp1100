module Main where

import Data.Char
import Data.List
import System.IO

import System.CPUTime
import Text.Printf

-- Modify these import statements as required.
import SetsWithLists

-- import SetsWithTrees
time :: IO t -> IO t
time function = do
  start <- getCPUTime
  value <- function
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10 ^ (12 :: Int))
  _ <- printf "\tComputation time: %0.3f sec\n" (diff :: Double)
  return value

readTextFileAsWords :: String -> IO [String]
readTextFileAsWords filename = do
  handle <- openFile filename ReadMode
  contents <- hGetContents handle
  let linesInFile = lines contents
  let wordsInFile =
        concatMap
          (words . filter (\c -> isAlpha c || c == ' ' || c == '\'' || c == '-'))
          linesInFile
  let lcWordsInFile = map (map toLower) wordsInFile
  return lcWordsInFile

buildSmallListsOfSize
  :: Ord a
  => Int -> [a] -> [[a]]
buildSmallListsOfSize size list =
  case list of
    [] -> []
    xs -> first : buildSmallListsOfSize size rest
      where (first, rest) = splitAt size xs

main :: IO ()
main
  -- Say hello.
 = do
  putStrLn "\n\t\tDown The Rabbit Hole!\n"
  -- Parse the initial texts.
  putStrLn "- Reading in Alice's Adventures In Wonderland."
  aliceWords <- readTextFileAsWords "alice-in-wonderland.txt"
  putStrLn "- Reading in Through The Looking Glass."
  glassWords <- readTextFileAsWords "through-the-looking-glass.txt"
  putStr "\n"
  -- Create sets using addElement
  putStrLn "- Creating single set for AAIW by folding addElement over input."
  let aliceSet = foldr addElement emptySet aliceWords
  let aliceSetSize = setSize aliceSet :: Integer
  putStr "\tWords in AAIW: "
  time $ print aliceSetSize
  putStrLn "- Creating single set for TTLG by folding addElement over input."
  let glassSet = foldr addElement emptySet glassWords
  let glassSetSize = setSize glassSet :: Integer
  putStr "\tWords in TTLG: "
  time $ print glassSetSize
  putStr "\n"
  -- Create sets using addElement in order
  putStrLn
    "- Creating single set for AAIW by folding addElement over sorted input."
  let aliceSetSorted = foldr addElement emptySet (sort aliceWords)
  let aliceSetSizeSorted = setSize aliceSetSorted :: Integer
  putStr "\tWords in AAIW: "
  time $ print aliceSetSizeSorted
  putStrLn
    "- Creating single set for TTLG by folding addElement over sorted input."
  let glassSetSorted = foldr addElement emptySet (sort glassWords)
  let glassSetSizeSorted = setSize glassSetSorted :: Integer
  putStr "\tWords in TTLG: "
  time $ print glassSetSizeSorted
  putStr "\n"
  -- Create sets by progressive unioning of small sets
  putStrLn
    "- Creating single set for AAIW folding set union over sublists of size 300."
  let aliceSmallLists = buildSmallListsOfSize 300 aliceWords
  let aliceSmallSets = map (foldr addElement emptySet) aliceSmallLists
  let aliceSet2 = foldr setUnion emptySet aliceSmallSets
  putStr "\tWords in AAIW: "
  time $ print (setSize aliceSet2 :: Integer)
  putStrLn
    "- Creating single set for TTLG folding set union over sublists of size 300."
  let glassSmallLists = buildSmallListsOfSize 300 glassWords
  let glassSmallSets = map (foldr addElement emptySet) glassSmallLists
  let glassSet2 = foldr setUnion emptySet glassSmallSets
  putStr "\tWords in AAIW: "
  time $ print (setSize glassSet2 :: Integer)
  putStr "\n"
  -- Return both sets to the empty set by removing elements individually
  putStrLn "- Removing all elements from AAIW set, one by one."
  let aliceBackToEmpty = foldr removeElement aliceSet (reverse aliceWords)
  let aliceSetSizeEmpty = setSize aliceBackToEmpty :: Integer
  putStr "\tWords in AAIW after all inputs removed (should be 0): "
  time $ print aliceSetSizeEmpty
  putStrLn "- Removing all elements from TTLG set, one by one."
  let glassBackToEmpty = foldr removeElement glassSet (reverse glassWords)
  let glassSetSizeEmptyOneByOne = setSize glassBackToEmpty :: Integer
  putStr "\tWords in TTLG after all inputs removed (should be 0): "
  time $ print glassSetSizeEmptyOneByOne
  putStr "\n"
  -- Return both sets to empty by taking the progressive (and left-associative) set
  -- difference of the smaller sets created above.
  putStrLn
    "- Removing all elements from AAIW set, by repeatedly taking set difference."
  let aliceEmptyDifference = foldl setDifference aliceSet aliceSmallSets
  let aliceSetSizeEmptyDifference = setSize aliceEmptyDifference :: Integer
  putStr "\tWords in AAIW after all inputs removed (should be 0): "
  time $ print aliceSetSizeEmptyDifference
  putStrLn
    "- Removing all elements from TTLG set, by repeatedly taking set difference."
  let glassEmptyDifference = foldl setDifference glassSet glassSmallSets
  let glassSetSizeEmptyDifference = setSize glassEmptyDifference :: Integer
  putStr "\tWords in TTLG after all inputs removed (should be 0): "
  time $ print glassSetSizeEmptyDifference
  putStr "\n"
  -- Calculate all words in both AAIW and TTLG, the hard way using contains
  putStrLn "- Working out how many words appear in both texts, using contains."
  let bothLists = nub $ aliceWords ++ glassWords
  putStr "\tDistinct words in either AAIW or TTLG: "
  print (length bothLists)
  let wordsInBoth =
        [ word
        | word <- bothLists
        , aliceSet `containsElement` word
        , glassSet `containsElement` word
        ]
  let sharedSize = length wordsInBoth
  putStr "\tWords in both AAIW and TTLG: "
  time $ print sharedSize
  putStr "\n"
  putStrLn
    "- Working out how many words appear in both texts, using set intersection."
  putStr "\tDistinct words in either AAIW or TTLG: "
  print ((setSize :: Set a -> Integer) $ aliceSet `setUnion` glassSet)
  let sharedSizeIntersect =
        (setSize :: Set a -> Integer) $ aliceSet `setIntersection` glassSet
  putStr "\tWords in both AAIW and TTLG: "
  time $ print sharedSizeIntersect
  putStr "\n"
  putStrLn "- Checking whether each small set is equal to any other small set."
  let combinedSmallSets = aliceSmallSets ++ glassSmallSets
  let equalityChecks =
        [ (set1, set2)
        | set1 <- combinedSmallSets
        , set2 <- combinedSmallSets
        , set1 `setEquals` set2
        ]
  putStr "\tNumber of combinations of equal sets: "
  time $ print (length equalityChecks)
  putStr "\n"
