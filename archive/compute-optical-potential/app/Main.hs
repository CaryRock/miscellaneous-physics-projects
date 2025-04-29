-- This program will:
--      take a filePath from the command line
--      parse that file into a list of elements to compute V, W_sc, and W_abas for
--      compute V, W_sc, and W_abs
--      print out the result
module Main (main) where

-- The library "Lib.hs" defining the module, "Lib", in src/ (to wit: src/Lib.hs)
import Lib

main :: IO ()
main = computeOpticalPotential
