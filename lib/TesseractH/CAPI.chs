{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module TesseractH.CAPI where

import qualified Data.Text as T
import Foreign
import Foreign.C
import Foreign.C.Types()
import Control.Monad (liftM4)
import Control.Monad (liftM4)
import Control.Applicative ((<$>))

#include <tesseract/capi.h>
-- #include <leptonica/environ.h>
-- #include <leptonica/pix.h>
#include <leptonica/allheaders.h>

-- | marshal an Enum (Haskell to C)
cIntFromEnum :: Enum a => a -> CInt
cIntFromEnum = fromIntegral . fromEnum

-- | unmarshal an Enum (C to Haskell)
cIntToEnum :: Enum a => CInt -> a
cIntToEnum = toEnum . fromIntegral

--------------------------------------------

-- ** Leptonica Functions

-- *** Box stuff`
-- data CBox = CBox

{# pointer *PIX #}
{# pointer *PIXA #}
{# pointer *BOX as BOX -> Box #}
{# pointer *BOXA  #}

type PIXHandle = Ptr PIX


{# fun boxaCreate as ^ {fromIntegral `Int'} -> `BOXA' id #}
{# fun boxaGetCount as ^ {id `BOXA'} -> `Int' fromIntegral #}

data Box = Box { x :: Int, y :: Int, w :: Int, h :: Int} deriving (Show, Eq, Ord, Read) 

cBoxW :: Ptr BOX -> IO Int
cBoxW b = fmap fromIntegral $ {#get BOX -> w#} b

cBoxH :: Ptr BOX -> IO Int
cBoxH b = fmap fromIntegral $ {#get BOX -> h#} b

cBoxX :: Ptr BOX -> IO Int
cBoxX b = fmap fromIntegral $ {#get BOX -> x#} b

cBoxY :: Ptr BOX -> IO Int
cBoxY b = fmap fromIntegral $ {#get BOX -> y#} b

cBoxToBox :: Ptr BOX -> IO Box
cBoxToBox c = liftM4 Box (cBoxX c) (cBoxY c) (cBoxW c) (cBoxH c)

-- | boxarray, index, access flag
{# fun boxaGetBox as ^ {id `BOXA', fromIntegral `Int', fromIntegral `Int'} -> `BOX' id #}

{# fun pixRead as ^ {`String'} -> `PIX' id #}

-- | second argument is size
{# fun pixReadMemPng as ^ {id `Ptr CUChar', fromIntegral `Int'} -> `PIX' id #}

-- | Cant get the args into haddock properly so here they are..
--
--   1. image data
--
--   1. size
--
--   1. flags
--
--   1. reduction (1, 2, 4, 8)
--
--   1. warnings
--
{# fun pixReadMemJpeg as ^
  { id `Ptr CUChar'    -- ^ image data
  , fromIntegral `Int' -- ^ size
  , fromIntegral `Int' -- ^ flags
  , fromIntegral `Int' -- ^ reduction (1, 2, 4, 8)
  , id `Ptr CInt'      -- ^ warnings
  , fromIntegral `Int' } -> `PIX' id #}

{# fun pixClone as ^ {id `PIX'} -> `PIX' id #}

{# fun pixConnComp as ^
  { id `PIX'
  , id `Ptr PIXA'
  , fromIntegral `Int'
  } -> `BOXA' id #}

{# fun pixConnCompBB as ^
  { id `PIX'
  , fromIntegral `Int'
  } -> `BOXA' id #}

toHandle' pix =  alloca (\h -> do 
                            poke h pix
                            return h)

toHandle pix = (\h -> do 
                   poke h pix
                   return h)               
               
{# fun pixCreate as ^ 
  { fromIntegral `Int' -- ^ width
  , fromIntegral `Int' -- ^ height
  , fromIntegral `Int' -- ^ depth
  } -> `PIX' id #}

-- | similar to passing &pix to pixDestroy 
--pixDestroyPIX pix = with pix pixDestroy

{# fun pixDestroy as ^ 
  { id `PIXHandle' 
  } -> `()' id #}

{# fun pixCreateNoInit as ^ 
  { fromIntegral `Int' -- ^ width
  , fromIntegral `Int' -- ^ height
  , fromIntegral `Int' -- ^ depth
  } -> `PIX' id #}

{# fun pixSetData as ^
  { id `PIX' -- ^ pix
  , id `Ptr CUInt' -- ^ data
  } -> `Int' fromIntegral #}

{# fun pixGetWpl as ^
  { id `PIX' -- ^ pix
  } -> `Int' fromIntegral #}

{# fun pixGetRefcount as pixGetRefCount
  { id `PIX' -- ^ pix
  } -> `Int' fromIntegral #}

{# fun pixGetInputFormat as ^
  { id `PIX' -- ^ pix
  } -> `Int' fromIntegral #}

{# fun pixSetWpl as ^
  { id `PIX' -- ^ pix
  ,  fromIntegral `Int'
  } -> `Int' fromIntegral #}

nullPointer = const nullPtr

{# fun pixConvert1To8 as ^ 
  { nullPointer `PIX'  -- ^ pixd -- null trigers new PIX 
  , id `PIX'  -- ^ pixs
  , fromIntegral `CChar' -- ^ val0 -- use 0
  , fromIntegral `CChar' -- ^ val1 -- 255
  } -> `PIX' id #}

{# fun pixEndianByteSwapNew as ^ 
  { id `PIX'
    } -> `PIX' id #}

{# fun pixEndianByteSwap as ^ 
  { id `PIX'
    } -> `Int' fromIntegral #}

{# fun pixWriteJpeg as ^
  { `String' -- ^ filename
  , id `PIX' -- ^ pix
  , fromIntegral `Int' -- ^ quality
  , fromIntegral `Int' -- ^ progressive
  } -> `Int' fromIntegral #}

{# fun pixWritePng as ^
  { `String' -- ^ filename
  , id `PIX' -- ^ pix
  , realToFrac `CFloat' -- ^ gamma
  } -> `Int' fromIntegral #}

peekInt c = fromIntegral <$> peek c
 
{# fun pixGetDimensions as ^
  { id `PIX' -- ^ pix
  , alloca- `Int' peekInt* -- ^ pw
  , alloca- `Int' peekInt* -- ^ ph
  , alloca- `Int' peekInt* -- ^ pd
  } -> `Int' fromIntegral #}

{# fun pixGetResolution as ^
  { id `PIX' -- ^ pix
  , alloca- `Int' peekInt* -- ^ pxres
  , alloca- `Int' peekInt* -- ^ pyres
  } -> `Int' fromIntegral #}

-- ** Image Processing

-- | args are:
--
--   1. pix
--
--   1. tile width
--
--   1. tile height
--
--   1. smooth x
--
--   1. smooth y
--
--   1. scorefract (typically 0.1)
--
--   1. thresholds.. can be null
--
--   1. destination pix
{# fun pixOtsuAdaptiveThreshold  as ^
  { id `PIX'
  , fromIntegral `Int'
  , fromIntegral `Int'
  , fromIntegral `Int'
  , fromIntegral `Int'
  , CFloat `Float'
  , id `Ptr PIX'
  , id `Ptr PIX'
  } -> `Int' fromIntegral #}
-- LEPT_DLL extern l_int32 pixOtsuAdaptiveThreshold ( PIX *pixs, l_int32 sx, l_int32 sy, l_int32 smoothx, l_int32 smoothy, l_float32 scorefract, PIX **ppixth, PIX **ppixd );


-- ** Tesseract
{# enum TessOcrEngineMode as ^ {} deriving (Show, Eq) #}
{# enum TessPageSegMode as ^ {} deriving (Show, Eq) #}
{# enum TessPageIteratorLevel as ^ {} deriving (Show, Eq) #}
{# enum TessPolyBlockType as ^ {} deriving (Show, Eq) #}
{# enum TessOrientation as ^ {} deriving (Show, Eq) #}
{# enum TessWritingDirection as ^ {} deriving (Show, Eq) #}
{# enum TessTextlineOrder as ^ {} deriving (Show, Eq) #}

{# fun pure TessVersion as ^ {} -> `String' #}

{# pointer *TessBaseAPI as ^ newtype #}

{# pointer *ETEXT_DESC as ETEXT_DESC newtype #}

{# fun TessBaseAPICreate as ^ {} -> `TessBaseAPI' id #}

{# fun TessBaseAPIClear as ^ {id `TessBaseAPI'} -> `()'  #}

{# fun TessBaseAPIDelete as ^ {id `TessBaseAPI'} -> `()'  #}

{# fun TessBaseAPIEnd as ^ {id `TessBaseAPI'} -> `()'  #}

{# fun TessBaseAPISetInputName as ^ 
{id `TessBaseAPI',
 `String' } -> `()' #}

{# fun TessBaseAPISetOutputName as ^ {id `TessBaseAPI',
                                     `String'} -> `()' #}

-- | baseAPI, datapath, langauge, mode
{# fun TessBaseAPIInit2 as ^
{ id `TessBaseAPI',
  `String', -- datapath
  `String', -- languate
  cIntFromEnum `TessOcrEngineMode'
  } -> `Int' fromIntegral #}

{# fun TessBaseAPISetPageSegMode as ^
{ id `TessBaseAPI',
  cIntFromEnum `TessPageSegMode'
  } -> `()' #}

-- | Args:
-- 1. image data
--
-- 1. width
--
-- 1. height
--
-- 1. bytes per pixel
--
-- 1. bytes per line
--
{# fun TessBaseAPISetImage as ^
{ id `TessBaseAPI',
  id `Ptr CUChar',    -- imagedata
  fromIntegral `Int', -- width
  fromIntegral `Int', -- height
  fromIntegral `Int', -- bytes per pixel
  fromIntegral `Int'  -- bytes per line
} -> `()' #}

{# fun TessBaseAPISetImage2 as ^
{ id `TessBaseAPI',
  id `PIX'    -- imagedata, struct from leptonica
} -> `()' #}

{# fun TessBaseAPISetRectangle as ^
{ id `TessBaseAPI',
  fromIntegral `Int', -- left
  fromIntegral `Int', -- top
  fromIntegral `Int', -- width
  fromIntegral `Int'  -- heigth
  } -> `()' #}

{# fun TessBaseAPIGetUTF8Text as ^
{ id `TessBaseAPI'
  } -> `String' #}


{# fun TessBaseAPIGetHOCRText as ^
{ id `TessBaseAPI',
  fromIntegral `Int' -- ^ page number
  } -> `String' #}

{# fun TessBaseAPISetSourceResolution as ^ 
{ id `TessBaseAPI',
  `Int'    -- ppi 
} -> `()' #}

{# fun TessBaseAPIMeanTextConf as ^ 
{ id `TessBaseAPI' }
-> `Int' #}

{# fun TessBaseAPIRecognize as ^
{ id `TessBaseAPI',
  id `ETEXT_DESC'
} -> `Int' fromIntegral #}


