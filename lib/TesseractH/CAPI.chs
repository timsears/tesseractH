{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module TesseractH.CAPI 

  ( BOXA
  , Box (..)
  , PIX
  , PIXA
  , PIX_IFF (..)
  , TessBaseAPI (..)
  , TessOcrEngineMode (..)
  , TessOrientation (..)
  , TessPageIteratorLevel (..)
  , TessPageSegMode (..)
  , TessPolyBlockType (..)
  , TessTextlineOrder (..)
  , TessWritingDirection (..)
  , boxaCreate
  , boxaGetBox
  , boxaGetCount
  , cBoxH
  , cBoxToBox
  , cBoxW
  , cBoxX
  , cBoxY
  , cIntFromEnum
  , cIntToEnum
  , pixClone
  , pixConnComp
  , pixConnCompBB
  , pixConvertRGBToGrayFast
  , pixGetDepth
  , pixIffToInt
  , pixOtsuAdaptiveThreshold
  , pixRead
  , pixReadMemJpeg
  , pixReadMemPng
  , pixWrite
  , pixaCreate
  , tessBaseAPICreate
  , tessBaseAPIDelete
  , tessBaseAPIGetHOCRText
  , tessBaseAPIGetUTF8Text
  , tessBaseAPIInit2
  , tessBaseAPISetImage
  , tessBaseAPISetImage2
  , tessBaseAPISetInputName
  , tessBaseAPISetOutputName
  , tessBaseAPISetPageSegMode
  , tessBaseAPISetRectangle
  , tessVersion
  )
where

import qualified Data.Text as T
import Foreign
import Foreign.C
import Control.Monad (liftM4)

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

-- *** Box stuff
-- data CBox = CBox

{# pointer *PIX #}
{# pointer *PIXA #}
{# pointer *BOX as BOX -> Box #}
{# pointer *BOXA  #}

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
{# fun boxaGetBox as ^ {id `BOXA', `Int', `Int'} -> `BOX' id #}

-- *** PIX stuff

-- | Numeric format flags.  This is quite brittle and may break in tesseract
--   versions after 3.02
data PIX_IFF  = IFF_UNKNOWN {- 0 -}  | IFF_BMP {- 1 -}       | IFF_JFIF_JPEG {- 2 -}
              | IFF_PNG {- 3 -}      | IFF_TIFF {- 4 -}      | IFF_TIFF_PACKBITS {- 5 -} 
              | IFF_TIFF_RLE {- 6 -} | IFF_TIFF_G3 {- 7 -}   | IFF_TIFF_G4 {- 8 -}
              | IFF_TIFF_LZW {- 9 -} | IFF_TIFF_ZIP {- 10 -} | IFF_PNM {- 11 -}
              | IFF_PS {- 12 -}      | IFF_GIF {- 13 -}      | IFF_JP2 {- 14 -}
              | IFF_WEBP {- 15 -}    | IFF_LPDF {- 16 -}     | IFF_DEFAULT {- 17 -}
              | IFF_SPIX {- 18 -}
              deriving (Show, Eq, Ord, Enum, Read) 

pixIffToInt :: PIX_IFF -> CInt; pixIffToInt = fromIntegral . fromEnum

-- | path, pix, format
{# fun pixWrite as ^ {`String', id `PIX', pixIffToInt `PIX_IFF'} -> `Int' #}
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
{# fun pixConvertRGBToGrayFast as ^ {id `PIX'} -> `PIX' id #}
{# fun pixGetDepth as ^ {id `PIX'} -> `Int' fromIntegral #}
{# fun pixaCreate as ^ {`Int'} -> `PIXA' id #}

data Connectivity = FourWay | EightWay deriving (Show, Eq, Read) 
connToInt :: Integral integral => Connectivity -> integral
connToInt FourWay = 4; connToInt _ = 8

{# fun pixConnComp as ^
  { id `PIX'
  , id `Ptr PIXA'
  , connToInt `Connectivity'
  } -> `BOXA' id #}

{# fun pixConnCompBB as ^ { id `PIX' , fromIntegral `Int' } -> `BOXA' id #}

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
{# fun TessBaseAPICreate as ^ {} -> `TessBaseAPI' id #}
{# fun TessBaseAPIDelete as ^ {id `TessBaseAPI'} -> `()'  #}
{# fun TessBaseAPISetInputName as ^ {id `TessBaseAPI',
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
-- 1. hieght
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

{# fun TessBaseAPIGetUTF8Text as ^ { id `TessBaseAPI' } -> `String' #}

-- | int is the page number
{# fun TessBaseAPIGetHOCRText as ^ { id `TessBaseAPI', fromIntegral `Int' } -> `String' #}

-- {# fun TesssBaseAPISetImage2 as ^
-- { id `TessBaseAPI',
--   id `PIX'
-- } -> `()'


{-

{#enum zbar_symbol_type_t as ZbarSymbolType {underscoreToCase} deriving (Show, Eq) #}
{#enum zbar_orientation_t as ZbarOrientation {underscoreToCase} deriving (Show, Eq) #}
{#enum zbar_error_t as ZbarError {underscoreToCase} deriving (Show, Eq) #}
{#enum zbar_config_t as ZbarConfig {underscoreToCase} deriving (Show, Eq, Bounded) #}
{#enum zbar_modifier_t as ZbarModifierConfig {underscoreToCase} deriving (Show,

{#fun zbar_image_scanner_create as ^ { } -> `ZbarImageScanner' id  #}

{#fun zbar_image_scanner_set_config as ^ {
    id `ZbarImageScanner',
    cIntFromEnum `ZbarSymbolType',
    cIntFromEnum `ZbarConfig',
    `Int'} -> `Int' #}

{#fun zbar_image_create as ^ { } -> `ZbarImage' id  #}

-- Todo -- Nicer type for second arg. Also, deal with 'zbar_fourcc'
{#fun zbar_image_set_format as ^ {
    id `ZbarImage',
    id `CULong' } -> `()' #}

{#fun zbar_image_set_size as ^ {
    id `ZbarImage',
    `Int',
    `Int' } -> `()' #}

--TODO How to pass data in through second arg?
{#fun zbar_image_set_data as ^ {
    id `ZbarImage',
    id `Ptr ()',
    `Int',
    id `FunPtr (ZbarImage -> IO())'} -> `()' #}

-- returns number of found barcodes
{#fun zbar_scan_image as ^ {
     id `ZbarImageScanner',
     id `ZbarImage'} -> `Int' #}

{#fun zbar_image_first_symbol as ^ {
    id `ZbarImage' } -> `ZbarSymbolT' id #}

{#fun zbar_symbol_next as ^ {
    id `ZbarSymbolT'} -> `ZbarSymbolT' id #}

{#fun zbar_symbol_get_type as ^ {
    id `ZbarSymbolT'} -> `ZbarSymbolType' cIntToEnum #}

{#fun zbar_symbol_get_data as ^ {
    id `ZbarSymbolT' } -> `String' #}

{#fun zbar_get_symbol_name as ^ {
    cIntFromEnum `ZbarSymbolType' } -> `String'  #}

{#fun zbar_image_destroy as ^ {
    id `ZbarImage' } -> `()' #}

{#fun zbar_image_scanner_destroy as ^ {
    id `ZbarImageScanner' } -> `()' #}

{#fun zbar_symbol_get_count as ^ {
    id `ZbarSymbolT' } -> `Int' #}

{#fun zbar_symbol_get_quality as ^ {
    id `ZbarSymbolT' } -> `Int'#}

{#fun zbar_symbol_get_loc_size as ^ {
    id `ZbarSymbolT' } -> `Int' #}

{#fun zbar_symbol_get_loc_x as ^ {
    id `ZbarSymbolT',
    `Int' } -> `Int' #}

{#fun zbar_symbol_get_loc_y as ^ {
    id `ZbarSymbolT',
    `Int' } -> `Int' #}

{#fun zbar_symbol_get_orientation as ^ {
    id `ZbarSymbolT'} -> `ZbarOrientation' cIntToEnum #}


{- defined in scanimage.c, NO LONGER NEEDED
{#fun get_data as ^ {
    `String',
    alloca - `CInt'  peek*,
    alloca - `CInt'  peek*,
    alloca - `Ptr ()' peek* } -> `()' id-   #}

{#fun scanimage as ^ {`String'} -> `Int'  #}

-}

-}


