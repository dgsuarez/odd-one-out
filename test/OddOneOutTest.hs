import OddOneOut

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Data.Maybe

main :: IO ()
main = defaultMain
       [ 
        testGroup "remove suffix" [
          testProperty "when suffix" propRemoveSuffix,
          testProperty "when not suffix" propCantRemoveSuffix
        ],
        testGroup "path with extension" [
          testProperty "base from path" propMakePathWithExtension,
          testProperty "bases from path" propPathsWithExtension
        ]
       ] 


propRemoveSuffix :: String -> String -> Property
propRemoveSuffix base extension = not (null base) ==> not (null extension) ==> (removeSuffix extension $ base ++ extension) == (Just base)

propCantRemoveSuffix :: String -> String -> Property
propCantRemoveSuffix base extension = not (null base) ==> not (null extension) ==> (base /= extension) ==> (removeSuffix extension $ extension ++ base) == Nothing

propMakePathWithExtension :: String -> String -> Property
propMakePathWithExtension b e = not (null b) ==> not (null e) ==> (b /= e) ==> (fmap base $ makePathWithExtension (b ++ e) e) == (Just b)

propPathsWithExtension :: [String] -> String -> Property
propPathsWithExtension bs e = not (null bs) ==> not (null e) ==> map base (pathsWithExtension paths e) == bs
  where paths = map (\x -> x ++ e) bs

propFilesFromExtension :: [String] -> [String] -> Property
propFilesFromExtension bs es = not (null bs) ==> not (null es) ==> length (concatMap id (filesPerExtension bs es)) == (length combined)
  where combined = concatMap (\b -> map (b++) es) bs
