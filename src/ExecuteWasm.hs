{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables#-}

module ExecuteWasm where

import           Language.JavaScript.Inline
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString               as BS
import           WasmGenerator

executeModule :: BS.ByteString -> IO (Aeson Double)
executeModule bytes = do
  session <- newSession defaultConfig
  let program = LBS.fromStrict bytes
  (e :: Aeson Double) <- eval
    session
    [block|
  const typedArray = new Uint8Array($program);
  return WebAssembly.instantiate(typedArray).then(result => {
    return result.instance.exports.main();
  }).catch(e => {
    return Infinity;
  });
  |]
  pure e

testExec :: IO [Aeson Double]
testExec = do
  testList <- test
  let bytesList = map snd testList
  mapM executeModule bytesList
