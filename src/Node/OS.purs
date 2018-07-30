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
  , userInfo
  ) where

import Prelude
     
import Data.Array ((!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Milliseconds, Seconds)
import Data.Map (Map, update) 
import Effect (Effect)
  
type NetworkInterface = { address :: String
                        , netmask :: String
                        , family :: String
                        , mac :: String
                        , internal :: Boolean
                        }
 
type CPU = { model :: String
           , speed :: Int
           , times :: { user :: Milliseconds
                      , nice :: Milliseconds
                      , sys :: Milliseconds
                      , idle :: Milliseconds
                      , irq :: Milliseconds } }

loadavg :: Effect { one :: Number, five :: Number, fifteen :: Number }
loadavg = pure <<< fromMaybe {one: 0.0, five: 0.0, fifteen: 0.0} <<< extract =<< loadavgImpl
  where
  extract xs = {one: _, five: _, fifteen: _} <$> xs !! 0 <*> xs !! 1 <*> xs !! 2

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
  show Darwin = "Darwin"
  show FreeBSD = "FreeBSD"
  show Linux = "Linux"
  show SunOS = "SunOS"
  show Win32 = "Win32"
  show UnknownPlatform = "UnknownPlatform"

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

userInfo :: {encoding :: String} -> Effect UserInfo
userInfo = userInfoImpl (flip update "shell") Nothing Just

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
foreign import networkInterfaces
  :: Effect (Map String (Array NetworkInterface))
foreign import userInfoImpl
  :: forall a 
   . ((a -> Maybe a) -> Map String a -> Map String a)
  -> Maybe a
  -> (a -> Maybe a)
  -> {encoding :: String}
  -> Effect UserInfo
