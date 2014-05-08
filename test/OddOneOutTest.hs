import OddOneOut

import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = defaultMainWithOpts
       [ 
        testProperty "empty" propEmpty
       ] mempty

emptyEL :: [ExtensionList]
emptyEL = []

propEmpty :: [String] -> [String] -> Property
propEmpty xs ys = (null xs) ==> (null ys) ==> groupIntoExtensionLists ys xs == emptyEL
