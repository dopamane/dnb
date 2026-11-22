module Sound.DNB (kickwav, kick) where

import Data.Binary.Get
import Data.Binary.Put
import Data.Int
import Data.List
import Data.Rap

kickwav :: Rap
kickwav = [Stanza "RIFF" "WAVE"
  [ Verse "fmt " $ runPut $ do
    putWord16le 1
    putWord16le 1
    putWord32le 44100
    putWord32le $ 44100 * 2
    putWord16le 2
    putWord16le 16
    putWord16le 0
  , Verse "data" $ runPut $ foldMap (putInt16le . norm) kick]]

kick :: [Double]
kick = take 22050 $
  zipWith (*) (iterate (* 0.999995) 0.95) $ map (sin . (* (2 * pi))) $ scanl' acc 0 fs
  where
    acc phi fn = wrap $ phi + fn / 44100
    fs = take 1300 (iterate (subtract 0.113) 200) ++ repeat 50

wrap :: Double -> Double
wrap d
  | d > 1 = 0
  | otherwise = d

norm :: Double -> Int16
norm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)
