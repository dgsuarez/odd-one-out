module OddOneOut where

import Data.List
import Data.Maybe

data PathWithExtension = PathWithExtension {
  base :: String,
  extension :: String
} deriving (Eq, Show)

oddOneOut :: [String] -> [String] -> [String]
oddOneOut paths extensions = map asFullPath $ onlyInFirstBy base $ filesPerExtension paths extensions

onlyInFirstBy :: Eq b => (a -> b) -> [[a]] -> [a]
onlyInFirstBy _ [] = []
onlyInFirstBy extractor (x:xs) = filter (\e -> any (not . (elemBy extractor e)) xs) x

elemBy :: Eq b => (a -> b) -> a -> [a] -> Bool
elemBy extractor p ps = (extractor p) `elem` (map extractor ps)

asFullPath :: PathWithExtension -> String
asFullPath p = (base p) ++ (extension p)

makePathWithExtension :: String -> String -> Maybe PathWithExtension
makePathWithExtension path ext = fmap (\base -> PathWithExtension {base=base, extension=ext}) $ removeSuffix ext path

removeSuffix :: Eq a => [a] -> [a] -> Maybe [a]
removeSuffix suffix full = if isSuffixOf suffix full 
                           then Just $ take ((length full) - (length suffix)) full 
                           else Nothing

pathsWithExtension :: [String] -> String -> [PathWithExtension]
pathsWithExtension paths ext = map fromJust $ filter isJust $ map (\path -> makePathWithExtension path ext) paths

filesPerExtension :: [String] -> [String] -> [[PathWithExtension]]
filesPerExtension paths extensions = map (pathsWithExtension paths) extensions

