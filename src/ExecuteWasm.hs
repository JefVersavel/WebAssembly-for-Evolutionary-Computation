module ExecuteWasm where

import Language.JavaScript.Inline
import Data.String (IsString(fromString))

testExec = newSession defaultConfig
