{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module ExecuteWasm where

import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline

executeModule :: BS.ByteString -> IO Double
executeModule bytes =
  withSession defaultConfig $ \session -> do
    let program = LBS.fromStrict bytes
    (e :: Aeson Double) <-
      eval
        session
        [js|
  const typedArray = new Uint8Array($program);
  return WebAssembly.instantiate(typedArray).then(result => {
    return result.instance.exports.main();
  }).catch(e => {
    return Infinity;
  });
  |]
    evaluate $ unAeson e
