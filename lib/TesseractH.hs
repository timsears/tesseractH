{-# LANGUAGE RecordWildCards #-}
module TesseractH where

import Control.Arrow ((&&&))
import Control.Monad -- (forM)
import Control.Monad.Reader
--import Foreign.C.Types (CULong)
--import Foreign.Ptr (Ptr, nullFunPtr)
import System.IO.Unsafe
import TesseractH.CAPI
data TessRect =
  TessRect { rL :: Int, rT :: Int, rW :: Int, rH :: Int }

-- | This monad tracks the context of a tesseract session.
type Tess a = ReaderT TessBaseAPI IO a

version = tessVersion

tessContext :: FilePath -> IO TessBaseAPI
tessContext fn = do
  s <- tessBaseAPICreate 
  tessBaseAPIInit2 s "" "eng" OEM_DEFAULT 
  tessBaseAPISetPageSegMode s PSM_AUTO
  ptr <- pixRead fn 
  tessBaseAPISetImage2 s ptr
  return s

ocrRect :: TessRect -> Tess String
ocrRect TessRect{..} = do
  s <- ask 
  liftIO $ tessBaseAPISetRectangle s rL rT rW rH
  txt <- liftIO $ tessBaseAPIGetUTF8Text s
  return txt

ocrRects :: [TessRect] -> Tess [String]
ocrRects rs =  mapM ocrRect rs

ocrWhole :: Tess String
ocrWhole = do
  s <- ask
  lift $ tessBaseAPIGetUTF8Text s

runOCR :: FilePath -> IO String
runOCR fn = do
  s <- tessContext fn
  runReaderT ocrWhole s

runOCRRegions :: FilePath -> [TessRect] -> IO [String]
runOCRRegions fn rects = do
  s <- tessContext fn
  runReaderT (ocrRects rects) s
