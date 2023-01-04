{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Low Level bindings to @festival.h@.  This module requires thread-local
initialization before the useful function can be used.  If you want to
parallelize wave synthesis you should use `forkIO` from `Control.Concurrent`
for each _instance_ of festival.  Maybe you can cheat by initializing in one
thread and then cloning the environment for CoW work in a posix thread.
-}
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
import Foreign.ForeignPtr

import           GHC.Natural (Natural)
import           Data.Bifunctor (second)
import qualified Data.ByteString.Internal as BS

-- | A simple config type that is passed to initialization of festival.
data InitConf
  = InitConf
  { -- | Should Festival load the systemwide config files.
    -- You almost certainly want this to be `True`.
    loadConf :: Bool
    -- | Their documentation recommends around @240000@, but this segfaults
    -- for me on 64 bit arch linux.  I normally use @600000@
  , heapSize :: Int
  } deriving (Eq, Show)

-- | `initialize` must be called before usign any other festival functions.
initialize :: InitConf -> IO ()
initialize InitConf{..} = initC (if loadConf then 1 else 0) (toEnum heapSize)
foreign import ccall "festival_c_initialize" initC :: CInt -> CInt -> IO ()

-- | This should load a language file for festival.
initLang :: String -> IO ()
initLang = flip withCString cInitLang
foreign import ccall "festival_c_init_lang" cInitLang :: CString -> IO ()

-- | Say something! Direct Text-To-Speech. NB. This may take some time.
sayText :: String -> IO ()
sayText = flip withCString sayTextC
foreign import ccall "festival_c_say_text" sayTextC :: CString -> IO ()

-- | Foreign pointer to instance of C++ class `ESTWave` from the EST collection
-- of speech processing tools from Edinburgh.  It is a dependency of festival (c).
{#pointer *ESTWave foreign finalizer freeWave newtype #}

{#fun newWave as cNewWave { } -> `ESTWave' #}
{#fun pure sampleRate as cSampleRate { `ESTWave' } -> `CInt' #}
{#fun setSampleRate as cSetSampleRate { `ESTWave', `CInt' } -> `()' #}
{#fun pure sampleCount as cSampleCount { `ESTWave' } -> `CInt' #}

-- | Get the sample rate for some given wave.
sampleRate :: ESTWave -> Natural
sampleRate = toEnum . fromIntegral . cSampleRate

-- | Setting the sample rate before @cTextToWave@ gives the desired
-- rate to the sample (big wow).
setSampleRate :: Natural -> ESTWave -> IO ()
setSampleRate rate wave = cSetSampleRate wave . toEnum $ fromIntegral rate

-- | Number of 16bit samples in the `ESTWave`.
sampleCount :: ESTWave -> Natural
sampleCount = toEnum . fromIntegral . cSampleCount

-- | Create a new CPP ESTWave object with sample rate `rate`.
newWave :: Natural -> IO ESTWave
newWave rate = (fmap . const) <*> setSampleRate rate =<< cNewWave

{#fun festival_c_text_to_wave as cTextToWave { `CString', `ESTWave' } -> `CInt' #}
-- | Get the raw speech synthesis from some text given a sample rate.
textToWave :: Int -> String -> IO (Either String ESTWave)
textToWave sampleRate = flip withCString $ \text -> do
  wave <- cNewWave
  cSetSampleRate wave (toEnum sampleRate)
  ret <- cTextToWave text wave
  if ret == 0 then return $ Left "festival failed to produce wave from text"
    else return $ Right wave

-- The #fun construct breaks on `Ptr CShort`
foreign import ccall "EST-wrapper.h copySample"
  cCopySample :: CInt -> Ptr CShort -> Ptr ESTWave -> IO ()

waveGetForeign :: ESTWave -> ForeignPtr ESTWave
waveGetForeign (ESTWave p) = p

sampleForeign :: ESTWave -> IO (ForeignPtr CShort, Int)
sampleForeign wave = do
  let count = fromIntegral $ sampleCount wave
  dest <- mallocForeignPtrArray count
  withForeignPtr dest $ \buf ->
    withForeignPtr (waveGetForeign wave) $ \w ->
      cCopySample (toEnum count) buf w
  return (dest, count)

-- | Uses native endianness.
sample :: ESTWave -> IO BS.ByteString
sample wave = do
  (xs, n) <- sampleForeign wave
  return $ BS.fromForeignPtr (castForeignPtr xs) 0 (n * 2)
