-- Cipher.hs
module Ciper where

import Data.List (elemIndex)
import Data.Maybe (mapMaybe)

type Key = Int

alphabet = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "!@#$%^&*()_-+={}|[]:;'<>?,./" ++ "\"" ++ "\\"

shift length key pos = mod (pos + key) length

encode :: String -> Key -> String -> String
encode alphabet key clear =
  (map ((!!) alphabet) . map (shift l key) . mapMaybe (flip elemIndex alphabet)) clear where
      l = length (alphabet)

decode :: String -> Key -> String -> String
decode alphabet key jibberish =
  (map ((!!) alphabet) . map (shift l (l - key)) . mapMaybe (flip elemIndex alphabet)) jibberish where
      l = length (alphabet)
