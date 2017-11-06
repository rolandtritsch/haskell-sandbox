{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- Default.hs
module Default where

import qualified Data.Text as T

default (T.Text)

example :: T.Text
example = "foo"
