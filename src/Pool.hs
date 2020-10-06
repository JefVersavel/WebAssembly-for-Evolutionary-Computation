module Pool
    ( module Foreign.Marshal.Pool,
        pooledNewByteString0,
    )
where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import Foreign
import Foreign.C
import Foreign.Marshal.Pool

pooledNewByteString0 :: Pool -> BS.ByteString -> IO CString
pooledNewByteString0 p bs
    | BS.null bs = pure nullPtr
    | otherwise =
        BS.unsafeUseAsCStringLen
        bs
        ( \(src, len) -> do
            dst <- pooledMallocBytes p (succ len)
            copyBytes dst src len
            pokeByteOff dst len (0 :: Word8)
            pure dst
        )
