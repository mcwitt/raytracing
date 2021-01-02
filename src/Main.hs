{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import safe Relude
import safe System.IO (hPutStr)
import safe Text.Printf (printf)

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

render :: IO PPM
render =
  let width = 300
      height = 300
      maxP = 255
      rows = mapM (renderRow maxP width height) [1 .. height]
   in PPM width height maxP <$> rows
  where
    renderRow maxP w h r = do
      hPutStr stderr $ printf "\rProgress: %d/%d" r h
      mapM (renderPixel maxP w h r) [1 .. w]
    renderPixel maxP w h r c =
      pure
        ( fromIntegral r / fromIntegral h * fromIntegral maxP,
          fromIntegral c / fromIntegral w * fromIntegral maxP,
          fromIntegral maxP
        )

main :: IO ()
main = do
  image <- render
  putTextLn $ encodeP3 image
