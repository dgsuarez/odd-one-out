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
        ]
       ] 


propRemoveSuffix :: String -> String -> Property
propRemoveSuffix base extension = not (null base) ==> not (null extension) ==> (removeSuffix extension $ base ++ extension) == (Just base)

propCantRemoveSuffix :: String -> String -> Property
propCantRemoveSuffix base extension = not (null base) ==> not (null extension) ==> (base /= extension) ==> (removeSuffix extension $ extension ++ base) == Nothing
