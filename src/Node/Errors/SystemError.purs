module Node.Errors.SystemError
  ( SystemError
  , toError
  , address
  , code
  , dest
  , errno
  , getSystemErrorName
  , info
  , message
  , path
  , port
  , syscall
  ) where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect.Exception (Error)
import Foreign (Foreign)
import Unsafe.Coerce (unsafeCoerce)

foreign import data SystemError :: Type

toError :: SystemError -> Error
toError = unsafeCoerce

instance Show SystemError where
  show = unsafeCoerce

foreign import getField :: forall a. Fn2 String SystemError a

foreign import getNullableField :: forall a. Fn2 String SystemError (Nullable a)

address :: SystemError -> Maybe String
address = toMaybe <<< runFn2 getNullableField "address"

code :: SystemError -> String
code = runFn2 getField "code"

dest :: SystemError -> Maybe String
dest = toMaybe <<< runFn2 getNullableField "dest"

-- | Use `getSystemErrorName` to convert Int into a string name
errno :: SystemError -> Int
errno = runFn2 getField "errno"

-- | Returns the string name for a numeric error code that comes from a Node.js API. 
-- | The mapping between error codes and error names is platform-dependent. 
foreign import getSystemErrorName :: Int -> String

info :: SystemError -> Foreign
info = runFn2 getField "info"

message :: SystemError -> String
message = runFn2 getField "message"

path :: SystemError -> Maybe String
path = toMaybe <<< runFn2 getNullableField "path"

port :: SystemError -> Maybe Int
port = toMaybe <<< runFn2 getNullableField "port"

syscall :: SystemError -> String
syscall = runFn2 getField "syscall"
