module Test.Main where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.OS (arch, constants, cpus, devNull, endianness, eol, freemem, getCurrentProcessPriority, homedir, hostname, loadavg, machine, networkInterfaces, release, setCurrentProcessPriority, tmpdir, totalmem, type_, uptime, userInfoSE, version)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  log $ show eol
  logShow =<< arch
  log $ (unsafeCoerce :: _ -> String) constants
  traverse_ logShow =<< cpus
  log $ show devNull
  logShow =<< endianness
  logShow =<< freemem
  logShow =<< getCurrentProcessPriority
  log =<< homedir
  log =<< hostname
  logShow =<< loadavg
  logShow =<< machine
  logShow =<< networkInterfaces
  log =<< release
  setCurrentProcessPriority 30
  log =<< tmpdir
  logShow =<< totalmem
  log =<< type_
  logShow =<< uptime
  logShow =<< userInfoSE
  log =<< version
