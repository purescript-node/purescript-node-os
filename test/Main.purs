module Test.Main where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Console (log, logShow)
import Node.Encoding (Encoding(..))
import Node.OS (arch, freemem, userInfo)

main :: Effect Unit
main = do
  log "Username:"
  log <<< _.username =<< userInfo (Left UTF8)
  log "Freemem:"
  logShow =<< freemem
  log "Arch"
  logShow =<< arch
