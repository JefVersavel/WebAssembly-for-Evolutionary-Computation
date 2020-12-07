{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module ExecuteWasm where

import Control.Exception
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Language.JavaScript.Inline

executeModule :: BS.ByteString -> Double -> IO Double
executeModule bytes global =
  withSession defaultConfig $ \session -> do
    let val = Aeson global
    let program = LBS.fromStrict bytes
    (e :: Aeson Double) <-
      eval
        session
        [js|
  const typedArray = new Uint8Array($program);
  const third = new WebAssembly.Global({
    value: 'f64',
    mutable: false 
  }, $val);
  let importObject = {
    second : {third}
  };  
  return WebAssembly.instantiate(typedArray,importObject).then(result => {
    return result.instance.exports.main();
  }).catch(e => {
    return Infinity;
  });
  |]
    evaluate $ unAeson e

executionTest = do
  bytes <- BS.readFile "./src/wasm/p1.wasm"
  executeModule bytes 5
