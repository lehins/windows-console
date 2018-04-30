{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import Control.Monad
import System.IO as IO
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Storable

import System.Win32
import System.Win32.Console
import System.Win32.File
import System.Win32.Types
import Graphics.Win32.Misc

import Data.ByteString as S
import Data.ByteString.Char8 as S8
import Data.ByteString.Unsafe as S
import Data.Maybe

#if defined(mingw32_HOST_OS)
# if defined(i386_HOST_ARCH)
#  define WINDOWS_CCONV stdcall
# elif defined(x86_64_HOST_ARCH)
#  define WINDOWS_CCONV ccall
# else
#  error Unknown mingw32 arch
# endif
#endif


foreign import WINDOWS_CCONV unsafe "windows.h WriteConsoleW"
  c_WriteConsoleW :: HANDLE -> LPWSTR -> DWORD -> LPDWORD -> LPVOID -> IO INT



-- wStdPutStr8 :: ByteString -> IO ()
-- wStdPutStr8 str = do
--   hConsole <- wGetStdout
--   unsafeUseAsCStringLen str $ \(cstr, len) -> do
--     when (len < 0 || fromIntegral (maxBound :: CInt) < len) $
--       error $ "wStdPutStr: Length is out of bounds: " ++ show len
--     --wlen <- multiByteToWideChar 65001 0 cstr (fromIntegral len) nullPtr 0
--     fPtr <- mallocForeignPtrArray $ fromIntegral len
--     withForeignPtr fPtr $ \ ptr -> do
--       multiByteToWideChar 65001 0 cstr (fromIntegral len) ptr (fromIntegral len)
--       eCode <- c_WriteConsoleW hConsole ptr (fromIntegral len) nullPtr nullPtr
--       {-TODO: get what was written-}
--       print eCode


withUTF8Encoding :: IO c -> IO c
withUTF8Encoding action =
  bracket
    (do inCP <- getConsoleCP
        outCP <- getConsoleOutputCP
        inEnc <- hGetEncoding stdin
        outEnc <- hGetEncoding stdout
        errEnc <- hGetEncoding stderr
        setEncoding (65001, 65001, Just utf8, Just utf8, Just utf8)
        return (inCP, outCP, inEnc, outEnc, errEnc))
    setEncoding
    (const action)
  where
    setEncoding (inCP, outCP, inEnc, outEnc, errEnc) = do
      maybe (return ()) (hSetEncoding stdin) inEnc
      maybe (return ()) (hSetEncoding stdout) outEnc
      maybe (return ()) (hSetEncoding stderr) errEnc
      setConsoleCP inCP
      setConsoleOutputCP outCP


writeConsole :: HANDLE -> LPWSTR -> DWORD -> LPDWORD -> IO INT
writeConsole hConsoleOutput lpBuffer nNumberOfCharsToWrite lpNumberOfCharsWritten =
  doWrite lpBuffer nNumberOfCharsToWrite
  where
    doWrite buf toWrite = do
      poke lpNumberOfCharsWritten 0
      eCode <-
        c_WriteConsoleW hConsoleOutput buf toWrite lpNumberOfCharsWritten nullPtr
      if (eCode == 0)
        then do
          n <- peek lpNumberOfCharsWritten
          if (n /= 0 && n < toWrite)
            then do
              flushFileBuffers hConsoleOutput
              doWrite
                (plusPtr buf (fromIntegral n * sizeOf (undefined :: CWchar)))
                (toWrite - n)
            else return eCode
        else return eCode

withUtf8Handle :: Handle -> (Handle -> IO a) -> IO a
withUtf8Handle hdl action =
  bracket
    (do oldEnc <- hGetEncoding hdl
        hSetEncoding hdl utf8
        return $ fromMaybe localeEncoding oldEnc)
    (hSetEncoding hdl)
    (const (action hdl))

hwPutStr :: Handle -> String -> IO ()
hwPutStr hdl str = do
  eCode <-
    withCWStringLen str $ \(cstr, len) -> do
      when (len < 0 || fromIntegral (maxBound :: DWORD) < len) $
        error $ "hwPutStr: Length is out of bounds: " ++ show len
      lpNumberOfCharsWrittenForeignPtr <- mallocForeignPtr
      withHandleToHANDLE hdl $ \winHANDLE ->
        withForeignPtr lpNumberOfCharsWrittenForeignPtr $
        writeConsole winHANDLE cstr (fromIntegral len)
  when (eCode == 0) $ withUtf8Handle hdl (`IO.hPutStr` str)



main :: IO ()
main = do
  hwPutStr stdout "Алексей Кулешевич\n"
  -- setConsoleOutputCP oldCP
  -- withUTF8Encoding $ do
  -- wPutStr stdout "=лёха=\n"
  -- oldCP <- getConsoleCP
  -- setConsoleCP 65001
  -- setConsoleOutputCP 65001
  -- hSetEncoding stdin utf8
  -- hSetEncoding stdout utf8
  -- hSetEncoding stderr utf8
  -- -- wStdPutStr "-foo-"
  -- -- wStdPutStr "~bar\n~"
  -- wStdPutStr "=ха=\n"
  -- let a = S.pack [0xD0, 0x90, 0xD0, 0xBB, 0xD0, 0xB5, 0xD0, 0xBA, 0xD1, 0x81, 0xD0, 0xB5, 0xD0, 0xB9]
  -- -- wStdPutStr8 a
  -- Prelude.putStrLn "Кристина Кулешевич"
  -- Prelude.putStrLn "Christina Kuleshevich"
  -- S8.putStrLn a
  -- wStdPutStr8 a

  -- setConsoleOutputCP oldCP
