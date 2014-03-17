module Dist where
import System.Random

------------------------linear distribution----------------------------------------
--We used a tricky way to create a linear distributed generator. 
--We used two uniformed random number generators, and we always take 
--smaller value that these two generator generated.
--probability density function is  f(x) = 2(1-x)  (0 <= x <= 1)
--you can modify the generator by adding more randomR to change gradient.
linear :: (RandomGen g, Floating a, Random a, Ord a) => g -> (a,g)
linear g0 = 
    let (r1, g1) = randomR (0, 1) g0
        (r2, g2) = randomR (0, 1) g1
    in (min r1 r2, g2)


------------------------exponential distribution----------------------------------------
--it will generate an exponentially distributed random numbers
--when given a parameter lambda.The probability density function is
--f(x) = lambda e^(-lambda * x)
exponential :: (RandomGen g, Floating a, Random a, Eq a) => 
            a                                              --  horizontal spread of the function.
         -> g                                              --  random number generator.
         -> (a,g)
exponential lambda g0 = (-log r1 / lambda, g1)
    where (r1, g1) = elimSame 0 random g0



------------------------normal distribution----------------------------------------	
--normal distribution, we directly used package normaldistribution.
--http://hackage.haskell.org/package/normaldistribution-1.1.0.1/docs/Data-Random-Normal.html




-----------------------useful functions--------------------------------------------
--when given a function and a random number generator,
--it will generate an infinite list of random values
randSeq :: (RandomGen g, Random a) => 
         (g -> (a,g)) -> g -> [a]
randSeq f x = xs : randSeq f x' 
    where (xs,x') = f x

--it used to avoid generating same random number

elimSame :: (Random a, Eq a, RandomGen g) => a -> (g -> (a,g)) -> g -> (a,g)
elimSame x f g = if r == x then elimSame x f g' else (r,g')
    where (r,g') = f g