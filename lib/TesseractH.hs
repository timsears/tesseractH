{-# LANGUAGE RecordWildCards #-}
module TesseractH
    ( version
    , SauvolaSettings
    , Default
    , OtsuSettings
    , Default
    , PIX_TH
    , withConnComp
    , withOtsu
    , withSauvola
    ) where

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
import Data.Monoid ((<>))
--import Text.Print

version = tessVersion

data SauvolaSettings = SauvolaSettings
    { ssWindowSize      :: Int
    , ssFactor          :: Float
    , ssAddBorder       :: Int
    } deriving (Show, Eq, Ord, Read)

instance Default SauvolaSettings where def = SauvolaSettings 16 0.25 1

data OtsuSettings = OtsuSettings
    { osTileWidth       :: Int
    , osTileHeight      :: Int
    , osSmoothX         :: Int
    , osSmoothY         :: Int
    , osScoreFract      :: Float
    } deriving (Show, Eq, Ord, Read)

instance Default OtsuSettings where def = OtsuSettings 100 100 1 1 0.1

type PIX_TH = PIX

-- | Note that this uses L_CLONE access type so that for now any changes
--   to the pix and box list are a bad thing.  will move to taking
--   this as an arugment
withConnComp :: Connectivity -> PIX -> ( [(Box ,PIX) ] -> IO a) -> IO a
withConnComp conn pix fxn = alloca $ \pixaPtr -> do
    boxa <- pixConnComp pix pixaPtr conn
    pixa <- peek pixaPtr
    len <- boxaGetCount boxa
    boxPixList  <- flip mapM [0 .. len -1] $ \idx -> liftM2 (,)
        (boxaGetBox boxa idx L_CLONE >>= cBoxToBox)
        (pixaGetPix pixa idx L_CLONE)
    fxn boxPixList

-- | Note that the inner function uses `alloca` so the
--   ((PIX, PIX) -> IO a) function cannot return something that
--   uses the memory of the (PIX,PIX) tuple as that will be freed.
withOtsu
    :: OtsuSettings
    -> PIX
    -> Either ((PIX_TH, PIX) -> IO (Either Text a)) (PIX -> IO (Either Text a))
    -> IO (Either Text a)
withOtsu os pix fxn = do
    -- in this case, we need the thresholds
    depth <- pixGetDepth pix
    case depth of
        8 -> pixRemoveColormap pix REMOVE_CMAP_TO_GRAYSCALE >>= withGrayPix
        32 -> pixConvertRGBToGrayFast pix >>= withGrayPix
        _ -> return . Left . pack $ "Pix depth not 8 or 32. is: " <> show depth
  where
    withGrayPix grayPix = do
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
                        0 -> peek otsuRes >>= fxnJustRes
                        _ -> return $ Left $ pack "Error running otsu threshold on pix"
                Left fxnWithThres -> alloca $ \thresholds -> do
                    res <- otsu thresholds otsuRes
                    case res of
                        0 -> (liftM2 (,) (peek thresholds) (peek otsuRes)) >>= fxnWithThres
                        _ -> return $ Left $ pack "Error running otsu threshold on pix"

-- | Note that the inner function uses `alloca` so the
--   ((PIX, PIX) -> IO a) function cannot return something that
--   uses the memory of the (PIX,PIX) tuple as that will be freed.
withSauvola
    :: SauvolaSettings
    -> PIX
    -> (PIX -> IO (Either Text a))
    -> IO (Either Text a)
withSauvola settings pix fxn = do
    -- in this case, we need the thresholds
    depth <- pixGetDepth pix
    case depth of
        8 -> pixRemoveColormap pix REMOVE_CMAP_TO_GRAYSCALE >>= withGrayPix
        32 -> pixConvertRGBToGrayFast pix >>= withGrayPix
        _ -> return . Left . pack $ "Pix depth not 8 or 32. is: " <> show depth
  where
    withGrayPix grayPix = do
        pixWrite "/tmp/grayix.png" grayPix IFF_PNG -- _DEBUG
        alloca $ \sauvRes -> do
            res <- pixSauvolaBinarize grayPix
                                     (ssWindowSize settings)
                                     (ssFactor settings)
                                     (ssAddBorder settings)
                                     nullPtr nullPtr nullPtr sauvRes
            case res of
                0 -> peek sauvRes >>= \p -> pixWrite "/tmp/sr.png" p IFF_PNG >> fxn p
                _ -> return $ Left $ pack "Error running sauv threshold on pix"
