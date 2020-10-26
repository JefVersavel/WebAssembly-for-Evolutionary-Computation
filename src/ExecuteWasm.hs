{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ExecuteWasm where

import           Control.Exception
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Language.JavaScript.Inline

executeModule :: BS.ByteString -> IO Double
executeModule bytes =
  bracket (newSession defaultConfig) killSession $ \session -> do
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
    pure $ unAeson e
