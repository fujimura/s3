module Main where

import S3

main :: IO ()
main = do
    xs <- s3get "https://johnsmith.s3.amazonaws.com" listAllBucketHandler
