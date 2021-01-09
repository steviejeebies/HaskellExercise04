{- butrfeld Andrew Butterfield -}
module Ex04 where

name, idno, username :: String
name      =  "Rowe, Stephen"  -- replace with your name
idno      =  "14319662"    -- replace with your student id
username  =  "rowes"   -- replace with your TCD username

declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes -------------------------------------------------------------------

-- do not change anything in this section !


-- a binary tree datatype, honestly!
data BinTree k d
  = Branch (BinTree k d) (BinTree k d) k d
  | Leaf k d
  | Empty
  deriving (Eq, Show)


-- Part 1 : Tree Insert -------------------------------

-- Implement:
ins :: Ord k => k -> d -> BinTree k d -> BinTree k d
ins k d (Empty) = Leaf k d
ins kIn dIn (Leaf k d)
  | k == kIn    = Leaf k dIn
  | otherwise   = ins kIn dIn (Branch Empty Empty k d)
ins kIn dIn (Branch left right k d)
  | k == kIn    = Branch left right k dIn
  | k < kIn     = Branch left (ins kIn dIn right) k d
  | otherwise   = Branch (ins kIn dIn left) right k d

-- Part 2 : Tree Lookup -------------------------------

-- Implement:
lkp :: (Monad m, Ord k) => BinTree k d -> k -> m d
lkp (Empty) _ = fail "no elements in tree"
lkp (Leaf k d) kIn
  | k == kIn    = return d
  | otherwise   = fail "not found in tree"
lkp (Branch left right k d) kIn
  | k == kIn    = return d
  | k < kIn     = lkp right kIn
  | otherwise   = lkp left kIn

-- Part 3 : Tail-Recursive Statistics

{-
   It is possible to compute BOTH average and standard deviation
   in one pass along a list of data items by summing both the data
   and the square of the data.
-}
twobirdsonestone :: Double -> Double -> Int -> (Double, Double)
twobirdsonestone listsum sumofsquares len
 = (average,sqrt variance)
 where
   nd = fromInteger $ toInteger len
   average = listsum / nd
   variance = sumofsquares / nd - average * average

{-
  The following function takes a list of numbers  (Double)
  and returns a triple containing
   the length of the list (Int)
   the sum of the numbers (Double)
   the sum of the squares of the numbers (Double)

   You will need to update the definitions of init1, init2 and init3 here.
-}
getLengthAndSums :: [Double] -> (Int,Double,Double)
getLengthAndSums ds = getLASs init1 init2 init3 ds
init1 = 0 
init2 = 0
init3 = 0

{-
  Implement the following tail-recursive  helper function
-}
getLASs :: Int -> Double -> Double -> [Double] -> (Int,Double,Double)
getLASs init1 init2 init3 ds = foldl (\(len, sum, sumSqr) x -> (1+len, x+sum, (x^2)+sumSqr)) (init1, init2, init3) ds  

-- Final Hint: how would you use a while loop to do this?
--   (assuming that the [Double] was an array of double)
