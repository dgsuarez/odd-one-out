module OddOneOut where

import Data.List
import Data.Maybe

data PathWithExtension = PathWithExtension {
  base :: String,
  extension :: String
} deriving (Eq, Show)

oddOneOut :: [String] -> [[String]] -> [String]
oddOneOut paths extensions = map asFullPath $ onlyInFirstBy base $ filesPerExtensions paths extensions

onlyInFirstBy :: Eq b => (a -> b) -> [[a]] -> [a]
onlyInFirstBy _ [] = []
onlyInFirstBy extractor (x:xs) = filter (\e -> any (not . (elemBy extractor e)) xs) x

elemBy :: Eq b => (a -> b) -> a -> [a] -> Bool
elemBy extractor p ps = (extractor p) `elem` (map extractor ps)

asFullPath :: PathWithExtension -> String
asFullPath p = (base p) ++ (extension p)

makePathWithExtension :: String -> String -> Maybe PathWithExtension
makePathWithExtension path ext = fmap (\base -> PathWithExtension {base=base, extension=ext}) $ removeSuffix ext path

pathsFromExtensions :: String -> [String] -> [Maybe PathWithExtension]
pathsFromExtensions path extensions = map (makePathWithExtension path) extensions

removeSuffix :: Eq a => [a] -> [a] -> Maybe [a]
removeSuffix suffix full = if isSuffixOf suffix full 
                           then Just $ take ((length full) - (length suffix)) full 
                           else Nothing

pathsWithExtensions :: [String] -> [String] -> [PathWithExtension]
pathsWithExtensions paths exts = map fromJust $ filter isJust $ concatMap (\path -> pathsFromExtensions path exts) paths

filesPerExtensions :: [String] -> [[String]] -> [[PathWithExtension]]
filesPerExtensions paths extensions = map (pathsWithExtensions paths) extensions

