module Main where

import Lib

main :: IO ()
main = do
        putStrLn (renderHeap maxHeapWithSmallRoot)
        putStrLn "---"
        putStrLn (renderHeap (maxHeapify maxHeapWithSmallRoot))
