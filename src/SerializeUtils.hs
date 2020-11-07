module SerializeUtils where

import Data.ByteString as BS
import Data.Serialize
import Run
import System.Directory

serializePath :: FilePath
serializePath = "./serialized/"

makePath :: String -> String
makePath name = serializePath ++ name ++ ".serialized"

encodeRun :: Run a => a -> IO ()
encodeRun run = do
  createDirectoryIfMissing True serializePath
  BS.writeFile (makePath $ getName run) $ encode run

decodeRun :: Run a => String -> IO a
decodeRun name = do
  bytes <- BS.readFile (makePath name)
  case decode bytes of
    Left _ -> error "could not find given filepath"
    Right decoded -> return decoded


