{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PPM (StreamingPPM (..), p3Lines) where

import Color (RGB, rgbInt)
import Pipes (Pipe, Producer, await, yield, (>->))
import Pipes.Prelude qualified as P
import Vec (R3 (R3))

data StreamingPPM m = StreamingPPM
  { ppmWidth :: Int,
    ppmHeight :: Int,
    maxP :: Int,
    ppmStream :: Producer RGB m ()
  }

chunksOf :: Monad m => Int -> Pipe a [a] m ()
chunksOf n = replicateM n await >>= yield >> chunksOf n

p3Lines :: StreamingPPM IO -> Producer Text IO ()
p3Lines StreamingPPM {..} = do
  yield "P3"
  yield $ unwords [show ppmWidth, show ppmHeight]
  yield $ show maxP
  ppmStream >-> rows
  where
    rows :: Pipe RGB Text IO ()
    rows =
      chunksOf ppmWidth
        >-> P.map
          ( \row ->
              unwords
                [ show c
                  | pixel <- row,
                    let R3 r g b = rgbInt maxP pixel,
                    c <- [r, g, b]
                ]
          )
