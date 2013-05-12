import TesseractH.CAPI
import Control.Applicative
import System.Environment
import TesseractH

main = main1 

-- first version
main1 = do
  fn <- head <$> getArgs 
  putStrLn $ "Tesseract version " ++ tessVersion
  ap <- tessBaseAPICreate
  tessBaseAPIInit2 ap "/usr/local/share" "eng" OEM_DEFAULT 
  tessBaseAPISetPageSegMode ap PSM_AUTO
  --let ptr = undefined
  ptr <- pixRead fn
  tessBaseAPISetImage2 ap ptr
  txt <- tessBaseAPIGetUTF8Text ap
  putStrLn $ "File: " ++ fn
  putStrLn $ "Result: " ++ txt
  