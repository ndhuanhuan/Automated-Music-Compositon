module Algo where
import Data.Ratio
import Data.List hiding (transpose)
import Data.Char(toLower, toUpper)
import Data.Random.Normal
import System.Random
import Dist
import System.IO
import qualified Data.MarkovChain as M
import Basic
----------------------cascade recursion algorithm-------------------------
--a function to recursively apply transformations for some number of times
--It takes in two functions and a integer and a music,
-- apply these two functions recursively for the specified number of times. 
cas ::  (Music a -> Music a) -> (Music a -> Music a) -> Int 
        -> Music a -> Music a
cas f g 0 m  = rest 0
cas f g n m  = m :=: g (cas f g (n-1) (f m))







-------------------------Self-similar Music-------------------------------
data Cluster  = Cluster AbsNote [Cluster]                -- a tree-like data type
type AbsNote    = (Duration,AbsolutePitch)               -- we used absolute pitch  in creating structure

--it used to generates an infinitely deep cluster
selfSimilar      :: [AbsNote] -> Cluster
selfSimilar a  = 
    let mkCluster note = Cluster note (map (mkCluster . noteProd note) a)
    in Cluster (0,0) (map mkCluster a)

--a simple way to generate new note		
--combine two pitches, and decrease duration 
--(note:duration is generally less than 1, so product of two duration become smaller.) 	
noteProd                  :: AbsNote -> AbsNote -> AbsNote
noteProd (d0,p0) (d1,p1)  = (d0*d1,p0+p1)

--skim notes in nth level 
--
fringe                       :: Int -> Cluster -> [AbsNote]
fringe 0 (Cluster note cls)  = [note]
fringe n (Cluster note cls)  = concatMap (fringe (n-1)) cls

--a converter
simToMusic     :: [AbsNote] -> Music Pitch
simToMusic  x  = line ( map absDuToPitch x)
--converter
absDuToPitch         :: (Duration,AbsolutePitch) -> Music Pitch
absDuToPitch (d,ap)  = note d (pitch ap)

--function that combine what we defined all together.
--inputs: initial pattern, a level, transpose number, and tempo scaling ratio.
finalSS pat n tr te = 
   transpose tr ( tempo te ( simToMusic ( fringe n ( selfSimilar pat))))




   
   


   
   
--------------------------------Sorting algorithm based composing------------------------
--insert sort algorithm
insert' :: (Ord a) => a -> [a] -> [a]
insert' x xs = takeWhile ((<=)x) xs ++ [x] ++ dropWhile ((<=)x) xs

--it record every procedure insert sort made and merge results into a list
insertMusic :: (Ord a) => [a] -> [a]
insertMusic []=[]
insertMusic [x]=[x]
insertMusic (x:xs:xxs) = (insert' x (xs:xxs))++(insertMusic(insert' x (xs:xxs)))     

--convert a sequence(list) of numbers into music pitch
insertGen :: Duration-> [Int] -> Int -> Music Pitch
insertGen d a n= intToMusicPitches d (take n (insertMusic a))


--merge sort algorithm
merge' :: (Ord a) => [a] -> [a] -> [a]
merge' [] b = b
merge' a [] = a
merge' a@(x:xs) b@(y:ys)
     |(x<=y) = [x]++(merge' xs b)
     |otherwise = [y] ++ (merge' a ys)

	  
--it record every procedure merge sort made and merge results into a list
msortMusic :: (Ord a)=> [a] -> [a]
msortMusic [] = []
msortMusic [x] = [x]
msortMusic xs = (merge' xx yy)++(msortMusic (tail (merge' xx yy)))
       where
            k =(length xs) `div` 2
            xx =take k xs
            yy =drop k xs

--convert a sequence(list) of numbers into music pitch			
msortGen' :: Duration-> [Int] ->Int-> Music Pitch	
msortGen' d a n= intToMusicPitches d (take n (msortMusic a))

--select sort algorithm
isSorted::[Int]->Bool
isSorted [] = True
isSorted xs = ( h == (minimum xs)) && isSorted (delete h xs)
            where h = head xs

--it record every procedure select sort made and merge results into a list			
selectSortMusic::[Int]->[Int]
selectSortMusic xs  | not(isSorted xs)  = (minimum xs) : (delete m xs) ++ selectSortMusic (delete m xs) 
               | otherwise  = []
                where m = minimum xs

--convert a sequence(list) of numbers into music pitch
selectSortGen :: Duration-> [Int] -> Music Pitch	
selectSortGen d a = intToMusicPitches d (selectSortMusic a)
  

  
  
  
  
  
---------------------Random music based on distributions------------------------------

--random number generator
sGen :: StdGen
sGen = mkStdGen 45

--convert float point to absolute pitch
toAbsP1    :: Float -> AbsolutePitch
toAbsP1 x  = round (40*x + 30)

--convert absolute pitch into a note
mkNote1  :: AbsolutePitch -> Music Pitch
mkNote1  = note tn . pitch

--convert a sequence of absolute pitch into a melody
mkLine1        :: [AbsolutePitch] -> Music Pitch
mkLine1 rands  = line (take 32 (map mkNote1 rands))

--interpret a floating-point number as an interval
toAbsP2     :: Float -> AbsolutePitch
toAbsP2 x   = round (5*x)

--uses scanl to generate a sum representing melody line
mkLine2 :: AbsolutePitch -> [AbsolutePitch] -> Music Pitch
mkLine2 start rands = 
   line (take 64 (map mkNote1 (scanl (+) start rands)))
-----------------------------------------------------------------------
-- uniform distribution
m1' :: Music Pitch
m1' = mkLine1 (randomRs (30,70) sGen)

-- linear distribution
m2' :: Music Pitch
m2' =  let rs1 = randSeq linear sGen
      in mkLine1 (map toAbsP1 rs1)

-- exponential distribution
m3'      :: Float -> Music Pitch
m3' a  =  let rs1 = randSeq (exponential a) sGen
           in mkLine1 (map toAbsP1 rs1)

-- Normal distribution
m4'      :: Float -> Float -> Music Pitch
m4' c b=  let rs1 = randSeq (normal' (c,b)) sGen
          in mkLine2 50 (map toAbsP2 rs1)







  
----------------------------------Markov Chain------------------------------------
--MarkovChain 
markovChain    ps   n = mkLine3 (M.run n ps 0 (mkStdGen 42))
markovChain'   pss  n = mkLine3 (concat (M.runMulti  n pss 0 (mkStdGen 42)))

-- music-making functions
mkNote3     :: Pitch -> Music Pitch
mkNote3     = note tn

mkLine3     :: [Pitch] -> Music Pitch
mkLine3 ps  = line (take 64 (map mkNote3 ps))













------------------------------Random Permutation Composition------------------------------------
--calculate difference between first element and last element of list
diff::[Int]->Int
diff [] = 0
diff x  = abs (subtract (head x) (last x))

--quick sort algorithm
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

--insert "diff" of list 
addHead :: [Int] -> [Int]
addHead x= (diff x):x

sortGT ::(Ord a) => [a] -> [a] -> Ordering
sortGT (x:xs:xxs) (y:ys:yys)
    |x<y = GT
    |x>y = LT
    |x==y = compare xs ys

--sort lists by first element of list
qSortHead :: [[Int]]->[[Int]]
qSortHead x = sortBy sortGT (sortedPerm x)

--add head to every element in the list
sortedPerm :: [[Int]]->[[Int]]
sortedPerm []= []
sortedPerm x= map addHead x

--it applies permutation to a given list and sort lists by "diff"
finalPermu ::[Int] ->[Int]
finalPermu x= concat (map tail (qSortHead(permutations x)))

--convert a sequence(list) of numbers into music pitch
finalPermuGen:: Duration ->[Int] ->Int ->Music Pitch
finalPermuGen d a n= intToMusicPitches d (take n (finalPermu a))







----------------------------Euclid's algorithm composing------------------------------------------
--Modified greatest common divisor algorithm
selfGCD :: Integral f => f -> f -> [f]  
selfGCD a 0= [a]
selfGCD a b= [a]++(selfGCD b (a `mod` b))

--convert a sequence(list) of numbers into music pitch
selfGCDGen :: Duration-> Int ->Int-> Music Pitch	
selfGCDGen d a b = intToMusicPitches d (selfGCD a b)




------------------------------Chinese Restaurant composing algorithm------------------------------------
--This is the modified version of conventional Chinese restaurant processes
--The algorithm takes in an integer number and produce a random partition of n elements set [1...n]
--One may imagine this process as a sitting pattern in a restaurant where each element represents a customer,
--and each partition represents a group of customer sitting at the same table.
prob::[[Int]] -> Double
prob x = 1.0 /(fromIntegral (length x))
             
joinTable :: StdGen -> Int -> [[Int]] -> [[Int]]
joinTable _ n [] = [[n]]
joinTable g n (x:xs)  =
                       let (p, g') = randomR(0.0, 1.0) g
                           y = prob (x:xs)
                       in
                          if  p > y then (x++[n]):xs 
                          else  x:(joinTable g' n xs) 

chiRest :: StdGen -> Int -> [[Int]]
chiRest _ 0 = []
chiRest _ 1 = [[1]]
chiRest g n = joinTable g n (chiRest g' (n-1))
           where (_, g') = next g

chiRestGen :: Duration-> StdGen ->Int-> Music Pitch
chiRestGen d g n = 	intToMusicPitches d (concat (chiRest g n))	