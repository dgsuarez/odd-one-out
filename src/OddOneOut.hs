module OddOneOut where

import Data.List
import Data.Maybe

data PathWithExtension = PathWithExtension {
  base :: String,
  extension :: String
} deriving (Eq, Show)

oddOneOut :: [String] -> [String] -> [String]
oddOneOut paths extensions = map asFullPath $ foldl1 filterNonPresent (filesPerExtension paths extensions)

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

filterNonPresent :: [PathWithExtension] -> [PathWithExtension] -> [PathWithExtension]
filterNonPresent acc current = filter (\x -> (base x) `notElem` currentBases) acc
  where currentBases = map base current

