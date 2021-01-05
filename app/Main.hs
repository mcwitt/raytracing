module Main (main) where

import safe Lib
  ( defaultImageConfig,
    defaultViewportConfig,
    render,
  )
import safe PPM (encodeP3)

main :: IO ()
main = do
  image <- render defaultImageConfig (defaultViewportConfig defaultImageConfig)
  putTextLn $ encodeP3 image
