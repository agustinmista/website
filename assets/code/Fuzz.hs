{-# LANGUAGE OverloadedStrings #-}
module Fuzz where

import Data.String
import Control.Monad
import System.Exit
import Data.ByteString
import System.Process.ByteString
import Test.QuickCheck
import Test.QuickCheck.Monadic

data Exp 
  = Const Double
  | Add Exp Exp
  | Sub Exp Exp
  | Mul Exp Exp
  | Div Exp Exp
  | Par Exp
  deriving Show

encode :: Exp -> ByteString
encode (Const n)   = fromString (show n)
encode (Add e1 e2) = encode e1 <> "+" <> encode e2
encode (Sub e1 e2) = encode e1 <> "-" <> encode e2
encode (Mul e1 e2) = encode e1 <> "*" <> encode e2
encode (Div e1 e2) = encode e1 <> "/" <> encode e2
encode (Par e) = "(" <> encode e <> ")"

instance Arbitrary Exp where
  arbitrary = sized gen 
    where
      gen 0 = Const . abs <$> arbitrary
      gen n = oneof
        [ Const . abs <$> arbitrary
        , Add <$> gen (n-1) <*> gen (n-1)
        , Sub <$> gen (n-1) <*> gen (n-1)
        , Mul <$> gen (n-1) <*> gen (n-1)
        , Div <$> gen (n-1) <*> gen (n-1)
        , Par <$> gen (n-1)
        ]

bc :: ByteString -> IO ExitCode
bc bs = do 
  (exitCode, _, _) <- readProcessWithExitCode "bc" [] bs
  return exitCode

zzuf :: ByteString -> IO ByteString
zzuf bs = do
  (_, stdout, _) <- readProcessWithExitCode "zzuf" ["--ratio", "0.1"] bs
  return stdout

prop_fuzz_bc :: Exp -> Property 
prop_fuzz_bc exp = monadicIO $ do
  exitCode <- run $ bc <=< zzuf $ encode exp
  assert (exitCode == ExitSuccess)

encodeInts :: [Int] -> ByteString
encodeInts = fromString . unlines . fmap show 

shell cmd stdin = do 
  (exitCode, _, _) <- readProcessWithExitCode cmd [] stdin
  return exitCode

prop_run_stdin :: Arbitrary t => String -> (t -> ByteString) -> t -> Property
prop_run_stdin target encode testcase = monadicIO $ do
  exitCode <- run $ shell target $ encode testcase
  assert (exitCode == ExitSuccess)
  where
    
    zzuf bs = do
      (_, stdout, _) <- readProcessWithExitCode "zzuf" [] bs
      return stdout
