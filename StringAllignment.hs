--Exercise 1:


-- In that case, we do not need to check different scores.
-- We only need to maximize length. Thus we will replace score with length.



--Exercise 2:


scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore (x:xs) [] = scoreSpace + similarityScore xs []
similarityScore [] (y:ys) = scoreSpace + similarityScore [] ys
similarityScore (x:xs) (y:ys) = maximum [(similarityScore xs ys) + score(x, y),
                                        (similarityScore xs (y:ys)) + score(x, '-'),
                                        (similarityScore (x:xs) ys) + score('-', y)]

score:: (Char, Char) -> Int
score(x,'-') =  scoreSpace
score('-',y) = scoreSpace
score(x,y)
  |x == y = scoreMatch
  |x /= y = scoreMismatch

--Add the elements as heads in the list of tuples
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

--The "value" of an element is defined by a function supplied as a parameter.
--The result is a list of all maximum elements.
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy _ [] = error "maxima by an empty list"
maximaBy valueFcn xs = [x | x <- xs, valueFcn x == maximum (map valueFcn xs)]

type AlignmentType = (String,String)

--Returns a list of all optimal alignments between string1 and string2.
optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys)
optAlignments (x:xs) (y:ys) = maximaBy stringScore ((attachHeads x y (optAlignments xs ys))
                                                 ++ (attachHeads x '-' (optAlignments xs (y:ys)))
                                                 ++ (attachHeads '-' y (optAlignments (x:xs) ys)))

--The score of two strings
stringScore:: (String, String) -> Int
stringScore ([], []) = 0
stringScore (x:xs, []) = scoreSpace + stringScore(xs, [])
stringScore ([], y:ys) = scoreSpace + stringScore([], ys)
stringScore (x:xs, y:ys)
  |x == y     = scoreMatch + stringScore (xs, ys)
  |otherwise  = scoreMismatch + stringScore (xs, ys)

outputOutAlignments:: String -> String -> IO ()
outputOutAlignments x y = sequence_ (map printTuple (optAlignments x y))

printTuple :: (String, String) -> IO ()
printTuple (x,y) = putStr ("\n" ++ x ++ "\n" ++ y ++ "\n")



--Exercise 3


similarityScore2 :: String -> String -> Int
similarityScore2 string1 string2 =  mcsSimilarityScore string1 string2

mcsSimilarityScore :: Eq a => [a] -> [a] -> Int
mcsSimilarityScore xs ys = mcsSim (length xs) (length ys)
  where
    mcsSim i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> Int
    mcsEntry x 0 = x * scoreSpace
    mcsEntry 0 y = y * scoreSpace
    mcsEntry i j
      | x == y    = scoreMatch + mcsSim (i-1) (j-1)
      | otherwise = maximum [(mcsSim i (j-1) + scoreSpace),
                            (mcsSim (i-1) j + scoreSpace),
                            (mcsSim (i-1) (j-1) + scoreMismatch)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 string1 string2 = snd (mcsOptAlignments string1 string1)

mcsOptAlignments :: String -> String -> (Int, [AlignmentType])
mcsOptAlignments xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]

    mcsEntry :: Int -> Int -> (Int, [AlignmentType])
    mcsEntry 0 0 = (0, [("","")])
    mcsEntry i 0 = (i * scoreSpace, attachTails (xs!!(i-1)) '-' (snd $ mcsLen (i-1) 0))
    mcsEntry 0 j = (j * scoreSpace, attachTails '-' (ys!!(j-1)) (snd $ mcsLen 0 (j-1)))
    mcsEntry i j = tupleFromList $ filter ((==maxInt).fst) listTuple
        where
          diag = mcsLen (i-1) (j-1)
          down = mcsLen i (j-1)
          right = mcsLen (i-1) j
          diagScore = if x==y then scoreMatch else scoreMismatch
          --Max score
          maxInt = maximum [((fst diag) + diagScore),
                            ((fst down) + scoreSpace),
                            ((fst right) + scoreSpace)]
          --List of all tuples
          listTuple = [(((fst diag) + diagScore), (attachTails x y (snd diag)))] ++
                    [(((fst down) + scoreSpace), (attachTails '-' y (snd down)))] ++
                    [(((fst right) + scoreSpace), attachTails x '-' (snd right))]
          x = xs!!(i-1)
          y = ys!!(j-1)


--Convert a list of tuples to a concatinated tuple
tupleFromList :: [(Int, [AlignmentType])] -> (Int, [AlignmentType])
tupleFromList xs = ((fst $ head xs), (concatMap snd xs))

--Add the elements as tails in the list of tuples
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs ++ [h1],ys ++ [h2]) | (xs,ys) <- aList]
