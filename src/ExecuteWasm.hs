{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

module ExecuteWasm where

import Language.JavaScript.Inline
import WasmGenerator
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign as F
import System.IO.Unsafe

storableToLBS :: Storable a => a -> LBS.ByteString
storableToLBS a =
  unsafeDupablePerformIO $
    fmap LBS.fromStrict $
      BS.create (sizeOf a) $ \p ->
        poke (castPtr p) a

storableFromLBS :: forall a. Storable a => LBS.ByteString -> IO a
storableFromLBS s = BS.unsafeUseAsCStringLen (LBS.toStrict s) $ \(p, len) ->
  if True
    then peek (castPtr p)
    else fail "Language.JavaScript.Inline.Examples.Utils.storableFromLBS"

newtype F64 = F64 Double
  deriving (Show, Storable) via Double

instance ToJS F64 where
  toJS x = [expr| $buf.readDoubleLE() |] where buf = storableToLBS x

instance FromJS F64 where
  rawJSType _ = RawBuffer
  toRawJSType _ =
    [expr| x => { const buf = Buffer.allocUnsafe(8); buf.writeDoubleLE(x); return buf; } |]
  fromJS _ = storableFromLBS

testExec :: IO JSVal
testExec = do
  ebList <- test
  session <- newSession defaultConfig
  eval session [expr| const l = 1;]


importNew :: Session -> IO JSVal
importNew _session = eval _session [expr| {} |]

wasmCompile :: Session -> LBS.ByteString -> IO JSVal
wasmCompile _session _module_buf =
  eval _session [expr| WebAssembly.compile($_module_buf) |]

wasmInstantiate :: Session -> JSVal -> JSVal -> IO JSVal
wasmInstantiate _session _module _import_obj =
  eval _session [expr| WebAssembly.instantiate($_module, $_import_obj) |]

exportGet :: Import f => Session -> JSVal -> String -> IO f
exportGet _session _instance (Aeson -> _export_name) = do
  _export_js_func <- eval _session [expr| $_instance.exports[$_export_name] |]
  pure $ importJSFunc _session _export_js_func
