module Node.OS
  ( eol
  , Arch(..)
  , arch
  , constants
  , CPU
  , cpus
  , devNull
  , Endianness(..)
  , endianness
  , toNodeEndianness
  , freemem
  , getCurrentProcessPriority
  , getPriority
  , homedir
  , hostname
  , LoadAvg
  , loadavg
  , IpVersion(..)
  , machine
  , NetworkInterface
  , networkInterfaces
  , release
  , setCurrentProcessPriority
  , setPriority
  , tmpdir
  , totalmem
  , type_
  , uptime
  , UserInfo
  , userInfo
  , userInfo'
  , userInfoBuffer
  , userInfoSE
  , userInfoSE'
  , userInfoBufferSE
  , version
  ) where

import Prelude

import Control.Alternative (guard)
import Data.Array (unsafeIndex)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Data.Posix (Pid)
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Exception (Error, try)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Node.Buffer.Immutable (ImmutableBuffer)
import Node.Encoding (Encoding(..), encodingToNode)
import Node.Errors.SystemError (SystemError)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

-- | The operating system-specific end-of-line marker.
-- |  `\n` on POSIX
-- | `\r\n` on Windows
foreign import eol :: String

-- | Possible values are 'arm', 'arm64', 'ia32', 'mips', 'mipsel', 'ppc', 'ppc64', 's390', 
-- | 's390x', 'x32', and 'x64'.
foreign import archImpl :: Effect String

data Arch
  = Arm
  | Arm64
  | Ia32
  | MIPS
  | MIPSEL
  | PPC
  | PPC64
  | S390
  | S390X
  | X32
  | X64
  | UnknownArch String

derive instance Eq Arch
instance Show Arch where
  show = case _ of
    Arm -> "Arm"
    Arm64 -> "Arm64"
    Ia32 -> "Ia32"
    MIPS -> "MIPS"
    MIPSEL -> "MIPSEL"
    PPC -> "PPC"
    PPC64 -> "PPC64"
    S390 -> "S390"
    S390X -> "S390X"
    X32 -> "X32"
    X64 -> "X64"
    UnknownArch s -> "(UnknownArch " <> show s <> ")"

-- | Returns the operating system CPU architecture for which the Node.js binary was compiled.
arch :: Effect Arch
arch = archImpl <#> case _ of
  "arm" -> Arm
  "arm64" -> Arm64
  "ia32" -> Ia32
  "mips" -> MIPS
  "mipsel" -> MIPSEL
  "ppc" -> PPC
  "ppc64" -> PPC64
  "s390" -> S390
  "s390x" -> S390X
  "x32" -> X32
  "x64" -> X64
  x -> UnknownArch x

-- | Contains commonly used operating system-specific constants for error codes, process signals, and so on.
-- | - `constants.signals`
-- | - `constants.errno`
-- | - `constants.dlopen`
-- | - `constants.priority`
-- | - `constants.UV_UDP_REUSEADDR`
foreign import constants :: Object Foreign

type CPU =
  { model :: String
  , speed :: Int
  , times ::
      { user :: Milliseconds
      , nice :: Milliseconds
      , sys :: Milliseconds
      , idle :: Milliseconds
      , irq :: Milliseconds
      }
  }

-- | Returns an array of objects containing information about each logical CPU core.
foreign import cpus :: Effect (Array CPU)

-- | The platform-specific file path of the null device.
-- | `\\.\nul` on Windows
-- | `/dev/null` on POSIX
foreign import devNull :: String

foreign import endiannessImpl :: Effect String

data Endianness
  = LittleEndian
  | BigEndian

derive instance eqEndianness :: Eq Endianness

instance showEndianness :: Show Endianness where
  show = case _ of
    LittleEndian -> "LittleEndian"
    BigEndian -> "BigEndian"

-- | Identifies the endianness of the CPU for which the Node.js binary was compiled.
endianness :: Effect Endianness
endianness = do
  end <- endiannessImpl
  pure case end of
    "BE" -> BigEndian
    "LE" -> LittleEndian
    x -> unsafeCrashWith $ "Impossible endian value: " <> show x

toNodeEndianness :: Endianness -> String
toNodeEndianness = case _ of
  BigEndian -> "BE"
  LittleEndian -> "LE"

-- | Returns the amount of free system memory in bytes as an integer.
foreign import freemem :: Effect Int

-- Effect Int
-- EffectFn1 Pid Int
foreign import getPriorityImpl :: forall f. f Int

-- | Returns the scheduling priority for the current process.
getCurrentProcessPriority :: Effect Int
getCurrentProcessPriority = getPriorityImpl

-- | Returns the scheduling priority for the process specified by pid.
getPriority :: Pid -> Effect Int
getPriority = runEffectFn1 (getPriorityImpl :: EffectFn1 Pid Int)

-- | Returns the string path of the current user's home directory.
foreign import homedir :: Effect String

-- | Returns the host name of the operating system as a string.
foreign import hostname :: Effect String

foreign import loadavgImpl :: Effect (Array Number)

-- | Each label indicates the number of minutes for the load average.
type LoadAvg =
  { one :: Number
  , five :: Number
  , fifteen :: Number
  }

-- | The load average is a measure of system activity calculated by the operating system
-- | and expressed as a fractional number.
-- | The load average is a Unix-specific concept.
-- | On Windows, the return value is always { one: 0, five: 0, fifteen: 0 }.
loadavg :: Effect LoadAvg
loadavg = loadavgImpl <#> \arr ->
  { one: unsafePartial $ unsafeIndex arr 0
  , five: unsafePartial $ unsafeIndex arr 1
  , fifteen: unsafePartial $ unsafeIndex arr 2
  }

data IpVersion
  = IPv4
  | IPv6
  | UnknownFamily String

derive instance Eq IpVersion

instance Show IpVersion where
  show = case _ of
    IPv4 -> "IPv4"
    IPv6 -> "IPv6"
    UnknownFamily x -> "(UnknownFamily " <> x <> ")"

foreign import machine :: Effect String

foreign import networkInterfacesImpl
  :: Effect
       ( Object
           ( Array
               { address :: String
               , netmask :: String
               , family :: String
               , mac :: String
               , internal :: Boolean
               , scopeId :: Nullable Int
               , cidr :: Nullable String
               }
           )
       )

-- | - address - The assigned IPv4 or IPv6 address
-- | - netmask - The IPv4 or IPv6 network mask
-- | - mac - The MAC address of the network interface
-- | - internal - true if the network interface is a loopback or similar interface that is not remotely accessible; otherwise false
-- | 
-- | Conditional fields:
-- | - cidr - The assigned IPv4 or IPv6 address with the routing prefix in CIDR notation. If the netmask is invalid, this property is set to null.
-- | - scopeid - The numeric IPv6 scope ID (only specified when family is IPv6)

type NetworkInterface =
  { address :: String
  , netmask :: String
  , family :: IpVersion
  , mac :: String
  , internal :: Boolean
  , scopeId :: Maybe Int
  , cidr :: Maybe String
  }

-- | Returns an object containing network interfaces that have been assigned a network address.
-- | Each key on the returned object identifies a network interface. 
-- | The associated value is an array of objects that each describe an assigned network address.
networkInterfaces :: Effect (Object (Array NetworkInterface))
networkInterfaces = map (map (map f)) networkInterfacesImpl
  where
  f iface =
    { address: iface.address
    , netmask: iface.netmask
    , family: case iface.family of
        "IPv4" -> IPv4
        "IPv6" -> IPv6
        x -> UnknownFamily x
    , mac: iface.mac
    , internal: iface.internal
    , cidr: toMaybe iface.cidr
    , scopeId: do
        guard (iface.family == "IPv6")
        toMaybe iface.scopeId
    }

-- | Returns the operating system as a string.
foreign import release :: Effect String

-- EffectFn1 Int Unit
-- EffectFn2 Pid Int Unit
foreign import setPriorityImpl :: forall f. f Unit

clampPriority :: Int -> Int
clampPriority = clamp (-20) 19

-- | See `setPriority`.
setCurrentProcessPriority :: Int -> Effect Unit
setCurrentProcessPriority i = runEffectFn1 (setPriorityImpl :: EffectFn1 Int Unit) $ clampPriority i

-- | Attempts to set the scheduling priority for the process specified by pid. 
-- | Note: all inputs are clamped to a valid value (i.e. `-20` = high priority and `19` = low priority). 
-- |
-- | Due to differences between Unix priority levels and Windows priority classes, 
-- | priority is mapped to one of six priority constants in `os.constants.priority`. 
-- | When retrieving a process priority level, this range mapping may cause the return value to be slightly different 
-- | on Windows. To avoid confusion, set priority to one of the priority constants.
-- |
-- | On Windows, setting priority to `PRIORITY_HIGHEST` requires elevated user privileges. 
-- | Otherwise the set priority will be silently reduced to `PRIORITY_HIGH`.
setPriority :: Pid -> Int -> Effect Unit
setPriority pid i = runEffectFn2 (setPriorityImpl :: EffectFn2 Pid Int Unit) pid $ clampPriority i

-- | Returns the operating system's default directory for temporary files as a string.
foreign import tmpdir :: Effect String

-- | Returns the total amount of system memory in bytes as an integer.
foreign import totalmem :: Effect Int

-- | Returns the operating system name as returned by `uname(3)`. 
-- | For example, it returns 'Linux' on Linux, 'Darwin' on macOS, and 'Windows_NT' on Windows.
foreign import type_ :: Effect String

-- | Returns the system uptime in number of seconds.
foreign import uptime :: Effect Number

-- | `a` is either `String` or `ImmutableBuffer`
type UserInfo a =
  { uid :: Int
  , gid :: Int
  , username :: a
  , homedir :: a
  , shell :: Maybe a
  }

-- | Returns information about the currently effective user. 
-- |
-- | - On POSIX platforms, this is typically a subset of the password file. 
-- | The returned object includes the username, uid, gid, shell, and homedir. 
-- | - On Windows, the uid and gid fields are -1, and shell is null.
-- | 
-- | Note: The value of `homedir` returned here is provided by the operating system. 
-- | This differs from the result of `os.homedir()`, 
-- | which queries environment variables for the home directory before falling back 
-- | to the operating system response.
-- |
-- | Note: This can throw a SystemError. If that's relevant to you, use `userInfoSE`
-- | and friends.
userInfo :: Effect (UserInfo String)
userInfo = userInfo' UTF8

userInfo' :: Encoding -> Effect (UserInfo String)
userInfo' enc = runUserInfo { encoding: encodingToNode enc }

userInfoBuffer :: Effect (UserInfo ImmutableBuffer)
userInfoBuffer = runUserInfo { encoding: "buffer" }

-- | A variant of `userInfo` that catches the thrown `SystemError`
-- | when `username` or `homedir` are undefined for the given user.
userInfoSE :: Effect (Either SystemError (UserInfo String))
userInfoSE = userInfoSE' UTF8

userInfoSE' :: Encoding -> Effect (Either SystemError (UserInfo String))
userInfoSE' enc = do
  map (lmap (unsafeCoerce :: Error -> SystemError)) $ try $ runUserInfo { encoding: encodingToNode enc }

userInfoBufferSE :: Effect (Either SystemError (UserInfo ImmutableBuffer))
userInfoBufferSE = do
  map (lmap (unsafeCoerce :: Error -> SystemError)) $ try $ runUserInfo { encoding: "buffer" }

-- Do not export as 'a' here can only be one of two things.
runUserInfo :: forall a. { encoding :: String } -> Effect (UserInfo a)
runUserInfo enc = runEffectFn1 userInfoImpl enc <#> \r ->
  { uid: r.uid
  , gid: r.gid
  , username: r.username
  , homedir: r.homedir
  , shell: toMaybe r.shell
  }

foreign import userInfoImpl
  :: forall a
   . EffectFn1 { encoding :: String }
       { uid :: Int
       , gid :: Int
       , username :: a
       , homedir :: a
       , shell :: Nullable a
       }

-- | Returns a string identifying the kernel version.
foreign import version :: Effect String
