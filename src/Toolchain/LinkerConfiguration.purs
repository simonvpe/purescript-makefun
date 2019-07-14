module Toolchain.LinkerConfiguration (LinkerConfiguration(..)) where

import Node.Path (FilePath)

data LinkerConfiguration
  = LinkLibrary String
  | Entry String
  | DynamicLinker String
  | LibraryDirectory FilePath
  | LRaw (Array String)
  | NoLinkerConfiguration
