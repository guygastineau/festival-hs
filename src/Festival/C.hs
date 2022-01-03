{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Festival.C
  ( initialize
  , sayText
  , InitConf (..)
  ) where

import Foreign.C
--import Foreign.C.String

foreign import ccall "festival_c_initialize" initC :: CInt -> CInt -> IO ()
foreign import ccall "festival_c_say_text" sayTextC :: CString -> IO ()

data InitConf = InitConf { loadConf :: Bool, heapSize :: Int }
  deriving (Eq, Show)

initialize :: InitConf -> IO ()
initialize InitConf{..} = initC (if loadConf then 1 else 0) (toEnum heapSize)

sayText :: String -> IO ()
sayText = flip withCString sayTextC
