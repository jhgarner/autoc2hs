{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Lib

main :: IO ()
main = someFunc

$generate
