module OddOneOut (
  ExtensionList,
  groupIntoExtensionLists,
  extension_group,
  files,
  makeExtensionList
) where

import Data.List

data ExtensionList = ExtensionList {
  files :: [String],
  extension_group :: [String]
} deriving (Eq, Show)


groupIntoExtensionLists :: [String] -> [[String]] -> [ExtensionList]
groupIntoExtensionLists files extension_groups = map (makeExtensionList files) extension_groups

makeExtensionList :: [String] -> [String] -> ExtensionList
makeExtensionList files extension_group = ExtensionList {extension_group = extension_group, files = (filterByExtension files $ head extension_group)}

filterByExtension ::  [String] -> String -> [String]
filterByExtension files ext = filter (isSuffixOf ext) files
