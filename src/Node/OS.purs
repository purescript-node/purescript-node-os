module Node.OS
  ( OS, NetworkInterface, CPU
  , eol
  , Arch(..), arch
  , cpus
  , Endianness(..), endianness
  , freemem
  , loadavg
  , homedir
  , hostname
  , Platform(..), platform
  , release
  , tmpdir
  , totalmem
  , ostype
  , uptime
  , networkInterfaces
  ) where

import Prelude

import Data.Array ((!!))
import Data.Maybe (fromMaybe)
import Data.StrMap (StrMap)
import Data.Time.Duration (Milliseconds, Seconds)

import Control.Monad.Eff (Eff, kind Effect)

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

foreign import data OS :: Effect 

foreign import eol :: Char

foreign import archImpl :: forall eff. Eff ( os :: OS | eff ) String

foreign import cpus :: forall eff. Eff ( os :: OS | eff ) (Array CPU)

foreign import endiannessImpl :: forall eff. Eff ( os :: OS | eff ) String

foreign import freemem :: forall eff. Eff ( os :: OS | eff ) Number

foreign import homedir :: forall eff. Eff ( os :: OS | eff ) String

foreign import hostname :: forall eff. Eff ( os :: OS | eff ) String

foreign import loadavgImpl :: forall eff. Eff ( os :: OS | eff ) (Array Number)

foreign import platformImpl :: forall eff. Eff ( os :: OS | eff ) String

foreign import release :: forall eff. Eff ( os :: OS | eff ) String

foreign import tmpdir :: forall eff. Eff ( os :: OS | eff ) String

foreign import totalmem :: forall eff. Eff ( os :: OS | eff ) Number

foreign import ostype :: forall eff. Eff ( os :: OS | eff ) String

foreign import uptime :: forall eff. Eff ( os :: OS | eff ) Seconds

foreign import networkInterfaces :: forall eff. Eff ( os :: OS | eff ) (StrMap (Array NetworkInterface))

loadavg :: forall eff. Eff ( os :: OS | eff ) { one :: Number, five :: Number, fifteen :: Number }
loadavg = pure <<< fromMaybe {one: 0.0, five: 0.0, fifteen: 0.0} <<< extract =<< loadavgImpl
  where
  extract xs = do
    one <- xs !! 0
    five <- xs !! 1
    fifteen <- xs !! 2
    pure {one, five, fifteen}

data Arch = X64 | ARM | IA32 | UnknownArch

derive instance eqArch :: Eq Arch

instance showArch :: Show Arch where
  show X64 = "X64"
  show ARM = "ARM"
  show IA32 = "IA32"
  show UnknownArch = "UnknownArch"

arch :: forall eff. Eff ( os :: OS | eff ) Arch
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

endianness :: forall eff. Eff ( os :: OS | eff ) Endianness
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

platform :: forall eff. Eff ( os :: OS | eff ) Platform
platform = do
  p <- platformImpl
  pure case p of
           "linux" -> Linux
           "darwin" -> Darwin
           "win32" -> Win32
           "freebsd" -> FreeBSD
           "sunos" -> SunOS
           _ -> UnknownPlatform
