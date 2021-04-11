{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PPM (StreamingPPM (..), p3Lines) where

import Color (RGB, rgbInt)
import Streaming (Of, Stream)
import Streaming.Prelude qualified as S
import Vec (R3 (R3))

data StreamingPPM m = StreamingPPM
  { ppmWidth :: Int,
    ppmHeight :: Int,
    maxP :: Int,
    ppmRows :: Stream (Of [RGB]) m ()
  }

p3Lines :: StreamingPPM IO -> Stream (Of String) IO ()
p3Lines StreamingPPM {..} = do
  S.yield "P3"
  S.yield $ unwords [show ppmWidth, show ppmHeight]
  S.yield $ show maxP
  S.map mkLine ppmRows
  where
    mkLine row =
      unwords
        [ show c
          | pixel <- row,
            let R3 r g b = rgbInt maxP pixel,
            c <- [r, g, b]
        ]
