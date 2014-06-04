import OddOneOut

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck

import Debug.Trace

main :: IO ()
main = defaultMain
       [ 
        testGroup "remove suffix" [
          testProperty "when suffix" propRemoveSuffix,
          testProperty "when not suffix" propCantRemoveSuffix
        ],
        testGroup "path with extension" [
          testProperty "base from path" propMakePathWithExtension,
          testProperty "bases from path" propPathsWithExtension,
          testCase "generate all files" testOnePathWithExtension,
          testCase "generate all files" testMultiplePathsMultipleExtensions
        ],
        testGroup "odd one out" [
          testCase "all there" testNotOneOdd,
          testCase "one odd" testOneOdd,
          testCase "no extensions" testNoExtensions,
          testCase "all empty" testAllEmpty,
          testCase "all odd" testAllOdd,
          testCase "multiple extensions" testMultipleExtensionGroups,
          testCase "multiple misses" testMultipleMissesSameBase
        ]
       ] 

nonEmptyStrings es =  not (null es) && ((> 0) . length $ es) && (all ((> 0) . length) es)

propRemoveSuffix :: String -> String -> Property
propRemoveSuffix base extension = not (null base) ==> not (null extension) ==> (removeSuffix extension $ base ++ extension) == (Just base)

propCantRemoveSuffix :: String -> String -> Property
propCantRemoveSuffix base extension = not (null base) ==> not (null extension) ==> (base /= extension) ==> (removeSuffix extension $ extension ++ base) == Nothing

propMakePathWithExtension :: String -> String -> Property
propMakePathWithExtension b e = not (null b) ==> not (null e) ==> (b /= e) ==> (fmap base $ makePathWithExtension (b ++ e) e) == (Just b)

propPathsWithExtension :: [String] -> String -> Property
propPathsWithExtension bs e = nonEmptyStrings bs ==> not (null e) && ((> 0) . length $ e) ==> map base (pathsWithExtensions paths [e]) == bs
  where paths = map (\x -> x ++ e) bs

testOnePathWithExtension = filesPerExtensions ["asdf.xa", "asdfasdf.xb"] [["xa"]] @?= [[PathWithExtension {base="asdf.", extension="xa"}]]

testMultiplePathsMultipleExtensions = filesPerExtensions ["ad.xb", "asdf.xa", "ju.xb"] [["xa"], ["xb"]] @?= res
  where res = [[PathWithExtension {base="asdf.", extension="xa"}],
               [PathWithExtension {base="ad.", extension="xb"}, PathWithExtension {base="ju.", extension="xb"}]]

testNotOneOdd = oddOneOut ["a.b", "c.b", "a.a", "c.a"] [["b"], ["a"]] @?= []

testOneOdd = oddOneOut ["a.b", "c.b", "a.a"] [["b"],["a"]] @?= ["c.b"]

testNoExtensions = oddOneOut ["a.b", "a.a", "b.a"] [] @?= []

testAllOdd = oddOneOut ["a.b", "c.b", "a.a"] [["b"],["a"],["z"]] @?= ["a.b", "c.b"]

testAllEmpty = oddOneOut [] [] @?= []

testMultipleExtensionGroups = oddOneOut ["a.b", "b.c", "b.a", "a.j"] [["b", "c"], ["j"]] @?= ["b.c"]

testMultipleMissesSameBase = oddOneOut ["a.b", "a.c", "b.z"] [["b", "c"], ["z"]] @?= ["a.b", "a.c"]

