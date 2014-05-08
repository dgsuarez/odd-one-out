module OddOneOut (
  {-filterExtensionLists,-}
  groupIntoExtensionLists,
  ExtensionList,
  extension_group,
  files,
  makeExtensionList
) where


data ExtensionList = ExtensionList {
  files :: [String],
  extension_group :: [String]
} deriving (Eq, Show)

filterExtensionLists :: [ExtensionList] -> ExtensionList
filterExtensionLists = undefined

groupIntoExtensionLists :: [String] -> [[String]] -> [ExtensionList]
groupIntoExtensionLists files extension_groups = map (makeExtensionList files) extension_groups

makeExtensionList :: [String] -> [String] -> ExtensionList
makeExtensionList files extension_group = ExtensionList {extension_group = extension_group, files = files}
