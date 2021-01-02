{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module PPM (PPM (PPM), encodeP3) where

import Color (RGBInt)
import Relude
import Vec (R3 (R3))

data PPM = PPM {ppmWidth :: Int, ppmHeight :: Int, ppmMaxP :: Int, ppmRows :: [[RGBInt]]}

encodeP3 :: PPM -> Text
encodeP3 PPM {..} =
  let header = ["P3", unwords [show ppmWidth, show ppmHeight], show ppmMaxP]
      rows =
        [ unwords [show c | R3 r g b <- row, c <- [r, g, b]]
          | row <- toList ppmRows
        ]
   in unlines (header <> rows)
