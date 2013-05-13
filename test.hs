import TesseractH.CAPI
import Control.Applicative
import System.Environment
import TesseractH

main = main2

-- first version
main1 = do
  fn <- head <$> getArgs 
  putStrLn $ "Tesseract version " ++ tessVersion
  ap <- tessBaseAPICreate
  tessBaseAPIInit2 ap "" "eng" OEM_DEFAULT 
  tessBaseAPISetPageSegMode ap PSM_AUTO
  --let ptr = undefined
  ptr <- pixRead fn
  tessBaseAPISetImage2 ap ptr
  txt <- tessBaseAPIGetUTF8Text ap
  putStrLn $ "File: " ++ fn
  putStrLn $ "Result: " ++ txt

-- different rectangles
main2 = do
  let fn = "phototest.tif"
  putStrLn $ "Tesseract version " ++ tessVersion
  ap <- tessBaseAPICreate
  --tessBaseAPIInit2 ap "/usr/local/share" "eng" OEM_DEFAULT
  tessBaseAPIInit2 ap "" "eng" OEM_DEFAULT
  tessBaseAPISetPageSegMode ap PSM_AUTO
  ptr <- pixRead fn 
  tessBaseAPISetImage2 ap ptr
  tessBaseAPISetSourceResolution ap 200
  tessBaseAPISetRectangle ap 0 250 640 480 -- lower half
  txt <- tessBaseAPIGetUTF8Text ap
  putStrLn $ "File: " ++ fn
  putStrLn "-------------------------"
  putStrLn $ "Lower portion: \n" ++ txt
  putStrLn "-------------------------"
  tessBaseAPISetRectangle ap 0 0 640 480 
  txt <- tessBaseAPIGetUTF8Text ap
  putStrLn $ "Full Image Result: \n" ++ txt
  

