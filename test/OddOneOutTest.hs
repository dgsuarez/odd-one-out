import OddOneOut

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

main :: IO ()
main = defaultMain
       [ 
        testGroup "groupIntoExtensionLists" [
          testProperty "all empty" propEmpty,
          testProperty "empty extensions" propEmptyExtensions,
          testProperty "set ext" propSetExtensions,
          testProperty "same or less files" propFilterFiles
        ]
       ] 

emptyEL :: [ExtensionList]
emptyEL = []

propEmpty = groupIntoExtensionLists [] [] == emptyEL

propEmptyExtensions xs = groupIntoExtensionLists xs [] == emptyEL

propSetExtensions :: [String] -> [[String]] -> Property
propSetExtensions files extension_groups = not (null extension_groups) ==> map extension_group (groupIntoExtensionLists files extension_groups) == extension_groups

propFilterFiles :: [String] -> [String] -> Property
propFilterFiles fs extensions = not (null fs) ==> not (null extensions) ==> (length . files) (makeExtensionList fs extensions) <= (length fs)

