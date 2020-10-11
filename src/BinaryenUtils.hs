{-# LANGUAGE ScopedTypeVariables #-}

module BinaryenUtils
  ( serializeModule
  )
where

import qualified Binaryen.Module               as Binaryen
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Unsafe        as BS
import           Foreign
import           Foreign.C

serializeModule :: Binaryen.Module -> IO BS.ByteString
serializeModule m = alloca $ \(buf_p :: Ptr (Ptr ())) ->
  alloca $ \(len_p :: Ptr CSize) -> alloca $ \(src_map_p :: Ptr (Ptr CChar)) ->
    do
      Binaryen.allocateAndWriteMut m nullPtr buf_p len_p src_map_p
      buf <- peek buf_p
      len <- peek len_p
      BS.unsafePackMallocCStringLen (castPtr buf, fromIntegral len)
