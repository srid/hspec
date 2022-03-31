module Main where

import           Prelude ()
import           Helper

import           Test.Hspec.Meta
import           System.SetEnv
import           System.Environment
import           Data.List
import qualified All



protectEnv :: IO a -> IO a
protectEnv action = do
  env <- getEnvironment
  prepareEnv env *> action <* restoreEnv env

prepareEnv :: [(String, String)] -> IO ()
prepareEnv env = do
  mapM_ (unset . fst) env
  setEnv "IGNORE_DOT_HSPEC" "yes"
  where
    unset :: String -> IO ()
    unset name = when ("HSPEC_" `isPrefixOf` name) (unsetEnv name)

restoreEnv :: [(String, String)] -> IO ()
restoreEnv env = do
  new <- getEnvironment
  let
    modified = env \\ new
    added = map fst new \\ map fst env
  mapM_ (uncurry setEnv) modified
  mapM_ unsetEnv added

spec :: Spec
spec = aroundAll_ protectEnv All.spec

main :: IO ()
main = hspec spec
