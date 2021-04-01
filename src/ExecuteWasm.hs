{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module ExecuteWasm where

import Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import GHC.Generics
import Language.JavaScript.Inline

executeModule :: BS.ByteString -> [Double] -> Double -> IO (Maybe Output)
executeModule bytes params int =
  withSession defaultConfig $ \session -> do
    print $ length params
    let val1 = Aeson $ head params
    let val2 = Aeson $ params !! 1
    let val3 = Aeson $ params !! 2
    let val4 = Aeson $ params !! 3
    let val5 = Aeson $ last params
    let internalValue = Aeson int
    let program = LBS.fromStrict bytes
    (jsn :: EncodedJSON) <-
      eval
        session
        [js|
  const typedArray = new Uint8Array($program);
  const param0 = new WebAssembly.Global({
      value: 'f64',
      mutable: false
  }, $val1);
  const param1 = new WebAssembly.Global({
      value: 'f64',
      mutable: true
  }, $val2);
  const param2 = new WebAssembly.Global({
      value: 'f64',
      mutable: true
  }, $val3);
  const param3 = new WebAssembly.Global({
      value: 'f64',
      mutable: true
  }, $val4);
  const param4 = new WebAssembly.Global({
      value: 'f64',
      mutable: true
  }, $val5);

  const state = new WebAssembly.Global({
      value: 'f64',
      mutable: true
  }, $internalValue)

  mod = x => Math.round(x) % 2

  let importObject = {
      external: { param0, param1, param2, param3, param4 },
      internal: { state },
      importFunctions: { mod }
  };
  return WebAssembly.instantiate(typedArray,importObject).then(result => {
    let json = {}
    json.outcome = result.instance.exports.main()
    json.internal = result.instance.exports.state.value
    return json
  }).catch(e => {
    return 65468151541;
  });
  |]
    return (A.decode $ unEncodedJSON jsn :: Maybe Output)

data Output = Output {outcome :: Double, internal :: Double}
  deriving (Show, Generic)

instance FromJSON Output

instance ToJSON Output

executionTest = do
  bytes <- BS.readFile "./src/wasm/p1.wasm"
  executeModule bytes [1, 1, 1, 1, 1] 4

inlineTest = do
  session <- newSession defaultConfig
  (i :: Aeson Int) <-
    eval
      session
      [js|
    const storage = require('node-persist');
    const a = 4;
    await storage.setItem('a', a);
    return a;
    |]
  print i
  (i' :: Aeson Int) <-
    eval
      session
      [js| 
    const storage = require('node-persist');
    return await storage.getItem('a');
    |]
  print i'
  print "ok"
