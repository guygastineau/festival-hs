{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Festival.C
  ( InitConf (..)
  , initialize
  , initLang
  , sayText
  , textToWave
  , ESTWave
  , newWave
  , sampleRate
  , setSampleRate
  , sampleCount
  , sample
  ) where

#include <festival-wrapper.h>
#include <EST-wrapper.h>

import Foreign
import Foreign.C
import Foreign.C.Types

import GHC.Natural (Natural)

data InitConf = InitConf { loadConf :: Bool, heapSize :: Int }
  deriving (Eq, Show)

initialize :: InitConf -> IO ()
initialize InitConf{..} = initC (if loadConf then 1 else 0) (toEnum heapSize)
foreign import ccall "festival_c_initialize" initC :: CInt -> CInt -> IO ()

initLang :: String -> IO ()
initLang = flip withCString cInitLang
foreign import ccall "festival_c_init_lang" cInitLang :: CString -> IO ()

sayText :: String -> IO ()
sayText = flip withCString sayTextC
foreign import ccall "festival_c_say_text" sayTextC :: CString -> IO ()

{#pointer *ESTWave foreign finalizer freeWave newtype #}

{#fun newWave as cNewWave { } -> `ESTWave' #}
{#fun pure sampleRate as cSampleRate { `ESTWave' } -> `CInt' #}
{#fun setSampleRate as cSetSampleRate { `ESTWave', `CInt' } -> `()' #}
{#fun pure sampleCount as cSampleCount { `ESTWave' } -> `CInt' #}

sampleRate :: ESTWave -> Natural
sampleRate = toEnum . fromIntegral . cSampleRate

setSampleRate :: Natural -> ESTWave -> IO ()
setSampleRate rate wave = cSetSampleRate wave . toEnum $ fromIntegral rate

sampleCount :: ESTWave -> Natural
sampleCount = toEnum . fromIntegral . cSampleCount

newWave :: Natural -> IO ESTWave
newWave rate = (fmap . const) <*> setSampleRate rate =<< cNewWave

{#fun festival_c_text_to_wave as cTextToWave { `CString', `ESTWave' } -> `CInt' #}
textToWave :: Int -> String -> IO ESTWave
textToWave sampleRate = flip withCString $ \text -> do
  wave <- cNewWave
  cSetSampleRate wave (toEnum sampleRate)
  return wave

-- The #fun construct breaks on `Ptr CShort`
foreign import ccall "EST-wrapper.h copySample"
  cCopySample :: CInt -> Ptr CShort -> Ptr ESTWave -> IO ()

waveGetForeign :: ESTWave -> ForeignPtr ESTWave
waveGetForeign (ESTWave p) = p

sample :: ESTWave -> IO (Ptr CShort)
sample wave = do
  let count = fromIntegral $ sampleCount wave
  allocaBytes count $ \buf -> withForeignPtr (waveGetForeign wave) $ \w ->
    cCopySample (toEnum count) buf w >> return buf
