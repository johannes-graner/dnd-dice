import Data.Function
import Data.Functor.Product
import Control.Arrow
import Control.Monad

data Dice = Dice {
  offset :: Int,
  maxValue :: Int,
  probs  :: [Double]
}

instance Show Dice where
  show (Dice offset maxValue probs) = show offset ++ ", " ++ show maxValue ++ ", " ++ show probs

offsetIndex :: Int -> [b] -> [(Int, b)]
offsetIndex offset = zip [offset..]

-- fma a (b,c) = a + b*c
fma :: (Int, Double) -> Double -> Double
fma = (+) . uncurry (*) . first fromIntegral

valueProb :: Dice -> [(Int, Double)]
valueProb = offsetIndex . offset <*> probs

-- Using <*>  (S comb) to apply argument to both replicate and fromIntegral.
-- Using join (W comb) to apply argument twice.
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
      maxIndex = n - offset d1 - offset d2
      probs1 = take (maxIndex+1) $ probs d1
      probs2 = take (maxIndex+1) $ probs d2
    in
      sum $ zipWith (*) probs1 $ reverse probs2

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