{-# LANGUAGE Safe #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import safe Lib
import safe PPM (encodeP3)
import Relude

main :: IO ()
main = do
  image <- render defaultImageConfig (defaultViewportConfig defaultImageConfig)
  putTextLn $ encodeP3 image
