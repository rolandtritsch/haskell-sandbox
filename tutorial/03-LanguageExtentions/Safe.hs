{-# LANGUAGE Safe #-}
{-# LANGUAGE Trustworthy #-}

{-# LANGUAGE Safe #-}

-- Safe.hs
module Safe where

import Unsafe.Coerce
import System.IO.Unsafe

bad1 :: String
bad1 = unsafePerformIO getLine

bad2 :: a
bad2 = unsafeCoerce 3.14 ()
