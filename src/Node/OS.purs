module Node.OS
  ( NetworkInterface
  , CPU
  , eol
  , Arch(..), arch
  , cpus
  , Endianness(..), endianness
  , freemem
  , homedir
  , hostname
  , loadavg
  , networkInterfaces
  , Platform(..), platform
  , release
  , tmpdir
  , totalmem
  , ostype
  , uptime
  , userInfo, BufferEncoding(..)
  ) where

import Prelude

import Data.Array ((!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds, Seconds)
import Effect (Effect)
import Foreign.Object (Object, update)
import Node.Encoding (Encoding)
  
type NetworkInterface =
  { address :: String
  , netmask :: String
  , family :: String
  , mac :: String
  , internal :: Boolean
  }

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

loadavg :: Effect { one :: Number, five :: Number, fifteen :: Number }
loadavg = fromMaybe {one: 0.0, five: 0.0, fifteen: 0.0} <<< extract <$> loadavgImpl
  where
  extract xs = ado
    one <- xs !! 0
    five <- xs !! 1
    fifteen <- xs !! 2
    in {one, five, fifteen}

data Arch = X64 | ARM | IA32 | UnknownArch

derive instance eqArch :: Eq Arch

instance showArch :: Show Arch where
  show X64 = "X64"
  show ARM = "ARM"
  show IA32 = "IA32"
  show UnknownArch = "UnknownArch"

arch :: Effect Arch
arch = do
  a <- archImpl
  pure case a of
    "x64" -> X64
    "arm" -> ARM
    "ia32" -> IA32
    _ -> UnknownArch

data Endianness = LittleEndian | BigEndian | UnknownEndian

derive instance eqEndianness :: Eq Endianness

instance showEndianness :: Show Endianness where
  show LittleEndian = "LittleEndian"
  show BigEndian = "BigEndian"
  show UnknownEndian = "UnknownEndian"

endianness :: Effect Endianness
endianness = do
  e <- endiannessImpl
  pure case e of
    "BE" -> BigEndian
    "LE" -> LittleEndian
    _ -> UnknownEndian

data Platform = Darwin | FreeBSD | Linux | SunOS | Win32 | UnknownPlatform

derive instance eqPlatform :: Eq Platform

instance showPlatform :: Show Platform where
  show = case _ of
    Darwin -> "Darwin"
    FreeBSD -> "FreeBSD"
    Linux -> "Linux"
    SunOS -> "SunOS"
    Win32 -> "Win32"
    UnknownPlatform -> "UnknownPlatform"

platform :: Effect Platform
platform = do
  p <- platformImpl
  pure case p of
    "linux" -> Linux
    "darwin" -> Darwin
    "win32" -> Win32
    "freebsd" -> FreeBSD
    "sunos" -> SunOS
    _ -> UnknownPlatform

type UserInfo =
  { uid :: Int
  , gid :: Int
  , username :: String
  , homedir :: String
  , shell :: Maybe String
  }

userInfo :: Either Encoding BufferEncoding -> Effect UserInfo
userInfo enc = userInfoImpl (update <@> "shell") Nothing Just enc'
  where
  enc' = case enc of
    Left e -> {encoding: show e}
    _ -> {encoding: "buffer"}

data BufferEncoding = BufferEncoding

foreign import eol :: Char
foreign import archImpl :: Effect String
foreign import cpus :: Effect (Array CPU)
foreign import endiannessImpl :: Effect String
foreign import freemem :: Effect Number
foreign import homedir :: Effect String
foreign import hostname :: Effect String
foreign import loadavgImpl :: Effect (Array Number)
foreign import platformImpl :: Effect String
foreign import release :: Effect String
foreign import tmpdir :: Effect String
foreign import totalmem :: Effect Number
foreign import ostype :: Effect String
foreign import uptime :: Effect Seconds
foreign import networkInterfaces :: Effect (Object (Array NetworkInterface))
foreign import userInfoImpl 
  :: (∀ a. (a -> Maybe a) -> Object a -> Object a)
  -> (∀ a. Maybe a)
  -> (∀ a. a -> Maybe a)
  -> {encoding :: String}
  -> Effect UserInfo
