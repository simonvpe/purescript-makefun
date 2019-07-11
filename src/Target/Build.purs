module Target.Build (build) where

import Control.Monad.Except.Trans (ExceptT, mapExceptT)
import Data.Either(Either)
import Data.Either.Map (mapRight)
import Effect.Aff (Aff)
import Node.Path (FilePath, concat)
import Prelude (Unit, bind, map, pure, ($), (<$>), (>>=), (>>>))
import Target (Target, compilerConfig, linkerConfig, name)
import Target.Object (Object, loadObjects, needsRebuild, objectPath, sourcePath)
import Toolchain (Toolchain)
import Toolchain.Compiler as Compiler
import Toolchain.Linker as Linker

-- |
-- | Compile the sources of a target, generating the object files.
-- |
build :: Toolchain -> FilePath -> Int -> Target -> ExceptT Error Aff FilePath
build tc builddir nofThreads target =
  do
    objs <- loadObjects builddir target []
    _ <- makeCompileSpec objs >>= Compiler.parCompileN compiler nofThreads
    mapExceptT (unitToFilePath output) $ Linker.link tc (linkerConfig target) (linkSpec output objs)
  where compiler = Compiler.mkCompiler tc $ compilerConfig target
        output = concat [builddir, name target]

compileSpec :: Object -> Compiler.CompileSpec
compileSpec obj = Compiler.CompileSpec { input: sourcePath obj, output: objectPath obj }

makeCompileSpec :: Array Object -> ExceptT Error Aff (Array Compiler.CompileSpec)
makeCompileSpec objects = needsRebuild objects >>= map compileSpec >>> pure

unitToFilePath :: FilePath -> Aff (Either String Unit) -> Aff (Either String FilePath)
unitToFilePath path aff = aff >>= mapRight (\_ -> path) >>> pure

linkSpec :: FilePath -> Array Object -> Linker.LinkSpec
linkSpec output objs = Linker.LinkSpec { inputs: objectPath <$> objs, output: output}

type Error = String
