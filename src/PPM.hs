{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module PPM (PPM (PPM), encodeP3, ppm) where

import Color (RGB, RGBInt, rgbInt)
import Vec (R3 (R3))

data PPM = PPM {ppmWidth :: Int, ppmHeight :: Int, ppmMaxP :: Int, ppmPixels :: [RGBInt]}

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

encodeP3 :: PPM -> Text
encodeP3 PPM {..} =
  let header = ["P3", unwords [show ppmWidth, show ppmHeight], show ppmMaxP]
      rows =
        [ unwords [show c | R3 r g b <- row, c <- [r, g, b]]
          | row <- chunksOf ppmWidth ppmPixels
        ]
   in unlines (header <> rows)

ppm :: Int -> Int -> Int -> [RGB] -> PPM
ppm width height maxP pixels = PPM width height maxP $ fmap (rgbInt maxP) pixels
