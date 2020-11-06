module SerializeUtils where

import Data.ByteString as BS
import Data.Serialize
import System.Directory

serializePath :: FilePath
serializePath = "./serialized/"

makePath :: String -> String
makePath name = serializePath ++ name ++ ".serialized"

encodeRun :: Serialize a => a -> String -> IO ()
encodeRun run name = do
  createDirectoryIfMissing True serializePath
  BS.writeFile (makePath name) $ encode run

decodeRun :: Serialize a => String -> IO a
decodeRun name = do
  bytes <- BS.readFile (makePath name)
  case decode bytes of
    Left _ -> error "could not find given filepath"
    Right decoded -> return decoded
