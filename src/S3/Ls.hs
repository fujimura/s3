{-# LANGUAGE GADTs #-}
module S3.Ls
  ( S3Object(..)
  , ls
  , ls'
  , runIO
  , runMock
  ) where

import           Control.Applicative    ((<*))
import           Control.Monad.Free
import           Control.Monad.Identity
import           System.Exit
import qualified System.Process
import           Text.Parsec
import           Text.Parsec.String

newtype S3Object = S3Object (String, String, String) deriving (Eq,Show)

data Action a = ReadProcessWithExitCode FilePath [String] String ((ExitCode, String, String) -> a)

instance Functor Action where
    f `fmap` (ReadProcessWithExitCode cmd args input r) = ReadProcessWithExitCode cmd args input (f . r)

readProcessWithExitCode :: FilePath -> [String] -> String -> Free Action (ExitCode, String, String)
readProcessWithExitCode cmd args input  = liftF $ ReadProcessWithExitCode cmd args input id

runIO :: Free Action a -> IO a
runIO (Pure a) = return a
runIO (Free a) = case a of
    ReadProcessWithExitCode cmd args input f -> System.Process.readProcessWithExitCode cmd args input >>= runIO . f

runMock :: a -> Free Action a -> Identity a
runMock a _ = return a

ls :: String -> IO [S3Object]
ls = ls' runIO

ls' :: (Monad m) => (Free Action (ExitCode, String, String) -> m (ExitCode, String, String))
       -> String -> m [S3Object]
ls' runner bucketName = do
  (_,ok,_) <- runner $ readProcessWithExitCode "s3cmd" ["ls", "s3://" ++ bucketName] []
  if ok == ""
    then error $ "No bucket found" ++ " " ++ "(" ++ bucketName ++ ")"
    else return $ parseResult ok

parseResult :: String -> [S3Object]
parseResult str = case parse result "ERROR" str of
                Left l   -> error $ show l
                Right xs -> xs

result :: Parser [S3Object]
result = many line <* eof

line :: Parser S3Object
line = do
    date <- manyTill anyChar (try (string "   "))
    size <- manyTill anyChar (try (string "   "))
    string "s3://"
    name <- manyTill anyChar (try (char '\n'))
    return $ S3Object (date, size, name)
