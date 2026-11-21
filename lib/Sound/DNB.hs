module Sound.DNB (kickwav, kick) where

import Data.Binary.Get
import Data.Binary.Put
import Data.Int
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
  , Verse "data" $ runPut $ foldMap (putInt16le . norm) kick]]

kick :: [Double]
kick = zipWith (*) (iterate (* 0.93) 1)
  [sin (2 * pi * f * i / 44100) | (i, f) <- zip [0 .. 22050 - 1] fs]
  where
    fs = take 1323 (iterate (subtract 0.113) 200) ++ repeat 50

norm :: Double -> Int16
norm d
  | d > 0 = fromIntegral $ min (maxb :: Integer) $ round (d * maxb)
  | d < 0 = fromIntegral $ max (minb :: Integer) $ round (d * minb)
  | otherwise = 0

maxb :: Num a => a
maxb = fromIntegral (maxBound :: Int16)

minb :: Num a => a
minb = fromIntegral (minBound :: Int16)
