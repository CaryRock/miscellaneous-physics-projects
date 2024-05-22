{-# LANGUAGE ParallelListComp #-}
module TestRead where

import Data.List.Split
import Data.List (intercalate)
import Data.Map (Map)
import Data.Typeable
import Data.Foldable

-- The below creates a dictionary (or is supposed to - Map only takes two arguments)
-- I want lists returned
--mapify :: [String] -> Map Double Double Double Double
--mapify = foldr (\s m -> let (lmbda:roi0:roi1:roi2) = (splitWhen (== ',') s)
--                            l = read lmbda :: Double
--                            r0 = read roi0 :: Double
--                            r1 = read roi1 :: Double
--                            r2 = read roi2 :: Double
--                        in insertWith (+) l r0 r1 r2 m) empty


lmbda :: Read a => Int -> [Char] -> a
lmbda col line = read $ (splitOn "," line) !! col

-- This will actually return the permutations; I want it to only return the tuples
-- of the same index
--zip4 :: [a] -> [b] -> [c] -> [d] -> [e]
--zip4 as bs cs ds = [(a, b, c, d) | a <- as, b <- bs, c <- cs, d <- ds]
list_to_string :: Show a => [a] -> String
list_to_string = unwords . map show

--list_to_csv_string :: Show a => [a] -> [[a]] -> String
--list_to_csv_string ss = intercalate "," (list_to_string $ ss)
-- intercalate would work on effs or ls below since those are [String], but 
-- This works:
-- putStrLn $ intercalate "," (map show (map (lmbda 0) $ effs :: [Double]))

-- To print out the columns nicely
--4plePrint :: (a, a, a, a) -> String
fourplePrint (p, q, r, s) = (show p) ++ "\t" ++ (show q) ++ "\t" ++ (show r) ++ "\t" ++ (show s)

main :: IO ()
main = do
    efficiencies <- readFile "2-40_AA_efficiency.txt"
    let effs = drop 2 (lines efficiencies)  -- Because the first two lines are comments
        ls = map (lmbda 0) $ effs :: [Double]
        r0s= map (lmbda 1) $ effs :: [Double]
        r1s= map (lmbda 2) $ effs :: [Double]
        r2s= map (lmbda 3) $ effs :: [Double]
        combo = [(a, b, c, d) | a <- ls
                              | b <- r0s
                              | c <- r1s
                              | d <- r2s]

    --print ls
    --print r0s
    --print r1s
    --print r2s
    --print (typeOf ls)
    --print combo
    forM_ (putStrLn . fourplePrint) combo

    --print ("print: " ++ (show $ [1, 2, 3, 4]))
    --putStrLn ("putStrLn: " ++ (show $ [1, 2, 3, 4]))
    --print ("print: " ++ (list_to_string $ [1, 2, 3, 4]))
    --putStrLn ("putStrLn: " ++ (list_to_string $ [1, 2, 3, 4]))
    --print ("print: " ++ (list_to_csv_string [1, 2, 3, 4]))
    --putStrLn ("putStrLn: " ++ (list_to_csv_string [1, 2, 3, 4]))
