import Data.Function
import Data.Functor.Product
import Control.Arrow
import Control.Monad

data Dice = Dice {
  offset :: Int,
  maxValue :: Int,
  probs  :: [Double]
}

-- Not point-free, but only used for display.
instance Show Dice where
  show (Dice offset maxValue probs) = show offset ++ ", " ++ show maxValue ++ ", " ++ show probs

-- zipWithIndex with offset index
offsetIndex :: Int -> [b] -> [(Int, b)]
offsetIndex =
  let offsetList = flip map [0..] . (+) in
    zip . offsetList

-- fma a (b,c) = a + b*c
fma :: (Int, Double) -> Double -> Double
fma = (+) . uncurry (*) . first fromIntegral

valueProb :: Dice -> [(Int, Double)]
valueProb = offsetIndex . offset <*> probs

-- Using <*>  (S comb) to apply argument to both replicate and fromIntegral.
makeDie :: Int -> Dice
makeDie =
  let
    getProbs = replicate <*> (1/) . fromIntegral
  in
    Dice 1 <*> getProbs

diceMean :: Dice -> Double
diceMean = foldl (flip fma) 0 . valueProb

convSingle :: Dice -> Dice -> Int -> Double
convSingle d1 d2 n
  | n > (maxValue d1 + maxValue d2) `div` 2 + 1 =
    let m = offset d1 + maxValue d1 + offset d2 + maxValue d2 - n in
      convSingle d1 d2 m
  | otherwise =
    let
      offsetPair = curry $ join (***) offset
      maximumIndex :: Dice -> Dice -> Int -> Int
      maximumIndex = curry $ subtract . uncurry (+) . uncurry offsetPair
      --maxIndex = 1 + n - offset d1 - offset d2
      maxIndex = 1 + maximumIndex d1 d2 n
      relevantProbs = flip $ flip take . probs
      relProbs = relevantProbs maxIndex

      -- sumList is list to sum in convolution
      partial = flip $ zipWith (*) . relProbs 
      sumList = partial . reverse . relProbs
    in
      sum $ sumList d1 d2

convolution :: Dice -> Dice -> Dice
convolution =
  let
    getNewMin = uncurry $ flip ((+) . offset) . offset
    getNewMax = uncurry $ flip ((+) . maxValue) . maxValue

    curriedSize = (-) . getNewMax <*> getNewMin

    -- createList length start
    createList = flip (fmap . (+)) . (`take` [0..])
    getNewRange = createList . (+1) . curriedSize <*> getNewMin

    mappedRange = flip map . getNewRange

    newProbs = mappedRange <*> uncurry convSingle
    diceWithMinMax = (Dice . getNewMin) <*> getNewMax
  in
    curry $ diceWithMinMax <*> newProbs

test :: Int -> Int -> Dice
test a b =
  let
    d1 = makeDie a
    d2 = makeDie b
  in
    convolution d1 d2