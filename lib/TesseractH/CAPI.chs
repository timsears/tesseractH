{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module TesseractH.CAPI where

import qualified Data.Text as T
import Foreign
import Foreign.C
import Foreign.C.Types()

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

{# pointer *PIX #}
{# pointer *PIXA #}
{# pointer *BOX #}
{# pointer *BOXA  #}

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


--------------------------------------------
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

{# fun TessBaseAPIGetUTF8Text as ^
{ id `TessBaseAPI'
  } -> `String' #}


{# fun TessBaseAPIGetHOCRText as ^
{ id `TessBaseAPI',
  fromIntegral `Int' -- ^ page number
  } -> `String' #}



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


