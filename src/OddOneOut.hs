module OddOneOut (
  {-filterExtensionLists,-}
  groupIntoExtensionLists,
  ExtensionList
) where


data ExtensionList = ExtensionList {
  files :: [String],
  extension :: String
} deriving (Eq, Show)

filterExtensionLists :: [ExtensionList] -> ExtensionList
filterExtensionLists = undefined

groupIntoExtensionLists :: [String] -> [String] -> [ExtensionList]
groupIntoExtensionLists files extensions = map (\ext -> undefined) extensions
