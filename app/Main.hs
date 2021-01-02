{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import safe Color (RGBInt, rgbInt)
import safe Relude
import safe System.IO (hPutStr)
import safe Text.Printf (printf)

data PPM = PPM {ppmWidth :: Int, ppmHeight :: Int, ppmMaxP :: Int, ppmRows :: [[RGBInt]]}

encodeP3 :: PPM -> Text
encodeP3 PPM {..} =
  let header = ["P3", unwords [show ppmWidth, show ppmHeight], show ppmMaxP]
      rows =
        [ unwords [show c | (r, g, b) <- row, c <- [r, g, b]]
          | row <- toList ppmRows
        ]
   in unlines (header <> rows)

render :: IO PPM
render =
  let width = 300
      height = 300
      cmax = 255
      rows = mapM (renderRow cmax width height) [1 .. height]
   in PPM width height cmax <$> rows
  where
    renderRow cmax w h r = do
      hPutStr stderr $ printf "\rProgress: %d/%d" r h
      mapM (renderPixel cmax w h r) [1 .. w]
    renderPixel cmax w h r c =
      pure $
        rgbInt
          cmax
          ( fromIntegral r / fromIntegral h,
            fromIntegral c / fromIntegral w,
            0.5
          )

main :: IO ()
main = do
  image <- render
  putTextLn $ encodeP3 image
