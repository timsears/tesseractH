{-# LANGUAGE RecordWildCards #-}
module TesseractH where

import Control.Arrow ((&&&))
import Control.Monad (forM)
--import Data.Bits (Bits((.|.), shiftL))
import Foreign.C.Types (CULong)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)
import Foreign (peek)
import TesseractH.CAPI
import Data.Default (Default (..))
import Control.Monad (liftM2)
import Data.Text (Text, pack)
--import Text.Print

version = tessVersion

data OtsuSettings = OtsuSettings
    { osTileWidth       :: Int
    , osTileHeight      :: Int
    , osSmoothX         :: Int
    , osSmoothY         :: Int
    , osScoreFract      :: Float
    } deriving (Show, Eq, Ord, Read)

instance Default OtsuSettings where def = OtsuSettings 100 100 1 1 0.1

type PIX_TH = PIX

withConnComp :: Connectivity -> PIX -> (BOXA -> PIXA -> IO a) -> IO a
withConnComp conn pix fxn = alloca $ \pixaPtr -> do
    boxa <- pixConnComp pix pixaPtr conn
    pixa <- peek pixaPtr
    fxn boxa pixa

-- | Note that the inner function uses `alloca` so the
--   ((PIX, PIX) -> IO a) function cannot return something that
--   uses the memory of the (PIX,PIX) tuple as that will be freed.
withOtsu
    :: OtsuSettings
    -> PIX
    -> Either ((PIX_TH, PIX) -> IO a) (PIX -> IO a)
    -> IO (Either Text a)
withOtsu os pix fxn = do
    -- in this case, we need the thresholds
    grayPix <- pixConvertRGBToGrayFast pix
    let otsu ptrPixTh ptrPixDest =
            pixOtsuAdaptiveThreshold grayPix
                                     (osTileWidth os) (osTileHeight os)
                                     (osSmoothX os) (osSmoothY os)
                                     (osScoreFract os)
                                     ptrPixTh ptrPixDest
    alloca $ \otsuRes -> do
        case fxn of
            Right fxnJustRes -> do
                res <- otsu nullPtr otsuRes
                case res of
                    0 -> peek otsuRes >>= fxnJustRes >>= return . Right
                    1 -> return $ Left $ pack "Error running otsu threshold on pix"
            Left fxnWithThres -> alloca $ \thresholds -> do
                res <- otsu thresholds otsuRes
                case res of
                    0 -> (liftM2 (,) (peek thresholds) (peek otsuRes)) >>= fxnWithThres >>= return . Right
                    1 -> return $ Left $ pack "Error running otsu threshold on pix"
