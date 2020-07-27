module Logic.Maths 
       (activationFn,
        activationFn',
        quadratic',
        quadratic,
        vectorLength,
        boxMuller) where

relu :: (Floating a, Ord a) => a -> a
relu = max 0

relu' :: (Floating a, Ord a) => a -> a
relu' x | x < 0     = 0
        | otherwise = 1

sigmoid :: (Floating a, Ord a) => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- (d/dx) sig(x) = sig(x)(1 - sig(x)) :shrug:
sigmoid' :: (Floating a, Ord a) => a -> a
sigmoid' x =
  let s   = sigmoid x in
      s * (1 - s)

activationFn :: (Floating a, Ord a) => a -> a
activationFn = sigmoid

activationFn' :: (Floating a, Ord a) => a -> a
activationFn' = sigmoid'

-- This is an implementation of the Box-Muller transform i think. I dunno if this plus
-- the impure part is actually what makes it up :shrug: 
boxMuller ::  (RealFloat a) => a -> a -> a
boxMuller x1 x2 = noNaN $ sqrt (-2 * log x1) * cos (2 * pi * x2)
    where noNaN x = if isNaN x || isInfinite x then 1.0 else x

-- This will be squared after which makes sqrt pointless, but it is what it is :actualshrug:
vectorLength :: Floating a => [a] -> a
vectorLength = sqrt . sum . map (**2)

-- Activations here refer to the output of the brain
quadratic :: Floating a => [a] -> [a] -> a
quadratic activations = (**2) . vectorLength . zipWith (-) activations

-- We use the derivative of the cost function to see how a particular
-- node's activation changes in comparison to some desired output. This is why
-- we're only interested in one activation value and one Y; in the code, these
-- will be compared with the Y activation belonging to some "ideal" execution of the network
-- i.e. one that gives the desired output.
quadratic' :: Floating a => a -> a -> a
quadratic' activation y = activation - y

-- For tests: 
-- vectorLength [-4, -12, 2] ==> 12.806248
-- quadratic [2, -5, 4] [6, 7, 2] ==> 164