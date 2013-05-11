{-# LANGUAGE RecordWildCards #-}
module TesseractH where

import Control.Arrow ((&&&))
import Control.Monad (forM)
--import Data.Bits (Bits((.|.), shiftL))
import Foreign.C.Types (CULong)
import Foreign.Ptr (Ptr, nullFunPtr)
import TesseractH.CAPI
--import Text.Print

version = tessVersion  