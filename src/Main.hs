{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import safe Relude

type RGB = (Double, Double, Double)

data PPM = PPM {ppmWidth :: Int, ppmHeight :: Int, ppmMaxP :: Int, ppmRows :: [[RGB]]}

encodeP3 :: PPM -> Text
encodeP3 PPM {..} =
  let header = ["P3", unwords [show ppmWidth, show ppmHeight], show ppmMaxP]
      rows =
        [ unwords [show x | (r, g, b) <- row, (x :: Int) <- fmap floor [r, g, b]]
          | row <- toList ppmRows
        ]
   in unlines (header <> rows)

example :: PPM
example =
  let width = 300
      height = 300
      maxP = 255
   in PPM
        width
        height
        maxP
        [ [ ( fromIntegral r / fromIntegral height * fromIntegral maxP,
              fromIntegral c / fromIntegral width * fromIntegral maxP,
              fromIntegral maxP
            )
            | c <- [1 .. width]
          ]
          | r <- [1 .. height]
        ]

main :: IO ()
main = putTextLn $ encodeP3 example
