{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , ForeignFunctionInterface, LambdaCase, MultiParamTypeClasses #-}

-- | Uses the DevIL C library to read and write images from and to files.
--
-- The following example reads an image from a file, automatically determining
-- the image format, and then writes it back into a 'ByteString' as a greyscale
-- 'PNG' image.
--
-- @
-- Right io <- 'load' 'Autodetect' "image.jpg"
-- let grey = 'convert' io :: Grey
--     bs   = 'saveBS' 'PNG' grey
-- print bs
-- @
--
-- Please read our
-- <https://github.com/RaphaelJ/friday-devil/blob/master/README.md README> to
-- to get a detailed usage and some examples.
--
-- /Note:/ As the underlying DevIL library is *not* tread-safe, there is a
-- global lock which will prevent two load/save calls to be performed at the
-- same time.
-- The Haskell interface should be thread-safe but will not be able to benefit
-- from multiple processing cores.
module Vision.Image.Storage.DevIL (
    -- * Types & classes
      StorageImage (..), StorageError (..)
    , ImageType, LoadImageType, SaveImageType, SaveBSImageType
    -- * Functions
    , load, loadBS, save, saveBS
    -- * Images types
    , Autodetect (..), BLP (..), BMP (..), CUT (..), DCX (..), DDS (..)
    , DICOM (..), Doom (..), DoomFlat (..), DPX (..), EXR (..), FITS (..)
    , FTX (..), GIF (..), HDR (..), ICO (..), ICNS (..), IFF (..), IWI (..)
    , JASCPAL (..), JP2 (..), JPG (..), LIF (..), MDL (..), MNG (..), MP3 (..)
    , PCD (..), PCX (..), PIC (..), PIX (..), PNG (..), PNM (..), PSD (..)
    , PSP (..), PXR (..), RAW (..), ROT (..), SGI (..), SUN (..), Texture (..)
    , TGA (..), TIFF (..), TPL (..), UTX (..), VTF (..), WAL (..), WBMP (..)
    , XPM (..)
    ) where

import Control.Applicative ((<$>), (<*))
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.DeepSeq (NFData (..), deepseq)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (
      Error (..), ErrorT (..), runErrorT, throwError
    )
import Data.Convertible (Convertible (..), convert)
import Data.Int
import Data.Vector.Storable (unsafeFromForeignPtr0, unsafeWith)
import Data.Word
import Foreign.C.String (CString, withCString)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Alloc (alloca, mallocBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS

import Vision.Image.Class (nChannels)
import Vision.Image.Grey (Grey, GreyPixel)
import Vision.Image.RGBA (RGBA, RGBAPixel)
import Vision.Image.RGB (RGB, RGBPixel)
import Vision.Image.Type (Manifest (..), Delayed (..), delay)
import Vision.Primitive (Z (..), (:.) (..), ix2)

import Vision.Image.Storage.DevIL.ImageType

#include "IL/il.h"

data StorageImage = GreyStorage Grey | RGBAStorage RGBA | RGBStorage RGB
    deriving Show

data StorageError = FailedToInit      -- ^ Failed to initialise the library.
                  | FailedToOpenFile  -- ^ Failed to open the given file.
                  | FailedToLoad      -- ^ Failed to load the image, invalid
                                      -- format.
                  | FailedToHaskell   -- ^ Failed to convert the loaded image to
                                      -- its Haskell representation.
                  | FailedToDevil     -- ^ Failed to write the image content
                                      -- through the inner DevIL library.
                  | FailedToSave      -- ^ Failed to save the image.
                  | FileAlreadyExists -- ^ The file already exists.
                  | InvalidFile       -- ^ The file could not be loaded
                                      -- because of an invalid value.
                  | OutOfMemory       -- ^ Could not allocate memory for the new
                                      -- image data.
                  | UnknownFileType   -- ^ The file content or extension does
                                      -- not match any known image type.
                  | UnknownError (Maybe String)
    deriving (Eq)

-- Instances -------------------------------------------------------------------

instance Convertible StorageImage StorageImage where
    safeConvert = Right

instance Convertible (Manifest GreyPixel) StorageImage where
    safeConvert = Right . GreyStorage

instance Convertible (Manifest RGBAPixel) StorageImage where
    safeConvert = Right . RGBAStorage

instance Convertible (Manifest RGBPixel) StorageImage where
    safeConvert = Right . RGBStorage

instance Convertible StorageImage (Manifest GreyPixel) where
    safeConvert (GreyStorage img) = Right img
    safeConvert (RGBAStorage img) = Right $ convert img
    safeConvert (RGBStorage img)  = Right $ convert img

instance Convertible StorageImage (Manifest RGBAPixel) where
    safeConvert (GreyStorage img) = Right $ convert img
    safeConvert (RGBAStorage img) = Right img
    safeConvert (RGBStorage img)  = Right $ convert img

instance Convertible StorageImage (Manifest RGBPixel) where
    safeConvert (GreyStorage img) = Right $ convert img
    safeConvert (RGBAStorage img) = Right $ convert img
    safeConvert (RGBStorage img)  = Right img

instance Convertible StorageImage (Delayed GreyPixel) where
    safeConvert (GreyStorage img) = Right $ delay img
    safeConvert (RGBAStorage img) = Right $ convert img
    safeConvert (RGBStorage img)  = Right $ convert img

instance Convertible StorageImage (Delayed RGBAPixel) where
    safeConvert (GreyStorage img) = Right $ convert img
    safeConvert (RGBAStorage img) = Right $ delay img
    safeConvert (RGBStorage img)  = Right $ convert img

instance Convertible StorageImage (Delayed RGBPixel) where
    safeConvert (GreyStorage img) = Right $ convert img
    safeConvert (RGBAStorage img) = Right $ convert img
    safeConvert (RGBStorage img)  = Right $ delay img

instance NFData StorageImage where
    rnf !(GreyStorage img) = rnf img
    rnf !(RGBAStorage img) = rnf img
    rnf !(RGBStorage  img) = rnf img

instance Error StorageError where
    noMsg  = UnknownError Nothing
    strMsg = UnknownError . Just

instance Show StorageError where
    show FailedToInit      = "Failed to initialise the DevIL library."
    show FailedToOpenFile  = "Failed to open the given file."
    show FailedToLoad      = "Failed to load the image."
    show FailedToHaskell   =
        "Failed to convert the loaded image to its Haskell representation."
    show FailedToDevil     =
        "Failed to write the image content through the inner DevIL library."
    show FailedToSave      = "Failed to save the image."
    show FileAlreadyExists = "The file already exists."
    show InvalidFile       =
        "The file could not be loaded because of an invalid value."
    show OutOfMemory       = "Could not allocate memory for the new image data."
    show UnknownFileType   =
        "The file content or extension does not match any known image type."
    show (UnknownError (Just msg)) = msg
    show (UnknownError Nothing   ) = "Unknown error."

-- Functions -------------------------------------------------------------------

-- | Image types which can be loaded using 'load' and 'loadBS'.
class ImageType t => LoadImageType t

instance LoadImageType Autodetect
instance LoadImageType BLP
instance LoadImageType BMP
instance LoadImageType CUT
instance LoadImageType DCX
instance LoadImageType DDS
instance LoadImageType DICOM
instance LoadImageType Doom
instance LoadImageType DoomFlat
instance LoadImageType DPX
instance LoadImageType EXR
instance LoadImageType FITS
instance LoadImageType FTX
instance LoadImageType GIF
instance LoadImageType HDR
instance LoadImageType ICO
instance LoadImageType ICNS
instance LoadImageType IFF
instance LoadImageType IWI
instance LoadImageType JASCPAL
instance LoadImageType JP2
instance LoadImageType JPG
instance LoadImageType LIF
instance LoadImageType MDL
instance LoadImageType MNG
instance LoadImageType MP3
instance LoadImageType PCD
instance LoadImageType PCX
instance LoadImageType PIC
instance LoadImageType PIX
instance LoadImageType PNG
instance LoadImageType PNM
instance LoadImageType PSD
instance LoadImageType PSP
instance LoadImageType PXR
instance LoadImageType RAW
instance LoadImageType ROT
instance LoadImageType SGI
instance LoadImageType SUN
instance LoadImageType Texture
instance LoadImageType TGA
instance LoadImageType TIFF
instance LoadImageType TPL
instance LoadImageType UTX
instance LoadImageType VTF
instance LoadImageType WAL
instance LoadImageType WBMP
instance LoadImageType XPM

-- | Reads an image from a file.
--
-- If no image type is given, type will be determined automatically with the
-- file extension and the file headers.
load :: LoadImageType t => t -> FilePath
     -> IO (Either StorageError StorageImage)
load !t path =
    path `deepseq` (
        lockAndBind $ \name -> do
            ilLoad t path
            fromDevil name
    )

-- | Reads an image from a strict 'ByteString'.
--
-- If no image type is given, type will be determined automatically with the
-- file headers.
loadBS :: LoadImageType t => t -> BS.ByteString
       -> Either StorageError StorageImage
loadBS !t bs =
    bs `deepseq` (
        unsafePerformIO $
            lockAndBind $ \name -> do
                ilLoadL t bs
                fromDevil name
    )

-- | Image types which can be loaded using 'save'.
class ImageType t => SaveImageType t

instance SaveImageType Autodetect
instance SaveImageType BMP
instance SaveImageType CHEAD
instance SaveImageType DDS
instance SaveImageType EXR
instance SaveImageType HDR
instance SaveImageType JASCPAL
instance SaveImageType JP2
instance SaveImageType JPG
instance SaveImageType PNG
instance SaveImageType PNM
instance SaveImageType PSD
instance SaveImageType RAW
instance SaveImageType SGI
instance SaveImageType TGA
instance SaveImageType TIFF
instance SaveImageType VTF
instance SaveImageType WBMP
instance SaveImageType XPM

-- | Saves the image to the given file.
--
-- If no image type is given, the image type is determined by the filename
-- extension.
--
-- /Note:/ will fail if the file already exists.
save :: (SaveImageType t, Convertible i StorageImage)
     => t -> FilePath -> i -> IO (Maybe StorageError)
save !t path img =
    path `deepseq` storImg `deepseq` (do
        res <- lockAndBind $ \name -> do
            toDevil storImg
            ilSave t path
            ilDeleteImage name

        return $ case res of Right () -> Nothing
                             Left err -> Just err
    )
  where
    storImg = convert img

-- | Image types which can be loaded using 'saveBS'.
class ImageType t => SaveBSImageType t

instance SaveBSImageType BMP
instance SaveBSImageType CHEAD
instance SaveBSImageType DDS
instance SaveBSImageType EXR
instance SaveBSImageType HDR
instance SaveBSImageType JASCPAL
instance SaveBSImageType JP2
instance SaveBSImageType JPG
instance SaveBSImageType PNG
instance SaveBSImageType PNM
instance SaveBSImageType PSD
instance SaveBSImageType RAW
instance SaveBSImageType SGI
instance SaveBSImageType TGA
instance SaveBSImageType TIFF
instance SaveBSImageType VTF
instance SaveBSImageType WBMP
instance SaveBSImageType XPM

-- | Saves the image into a manifest vector from a strict 'ByteString'.
saveBS :: (SaveBSImageType t, Convertible i StorageImage)
       => t -> i -> Either StorageError BS.ByteString
saveBS !t img =
    storImg `deepseq` (
        unsafePerformIO $
            lockAndBind $ \name -> do
                toDevil storImg
                ilSaveL t <* ilDeleteImage name
    )
  where
    storImg = convert img

-- C wrappers and helpers ------------------------------------------------------

type StorageMonad = ErrorT StorageError IO

devilLock :: MVar ()
devilLock = unsafePerformIO $ newMVar ()
{-# NOINLINE devilLock #-}

-- | Uses a global lock ('devilLock') to prevent two threads to call the
-- library at the same time.
-- Make sure that action will not trigger another call to lockDevil due to lazy
-- evaluation.
lockDevil :: IO a -> IO a
lockDevil action = do
    takeMVar devilLock
    ret <- action
    putMVar devilLock ()
    return ret

-- | Locks the DevIL library, allocates a new image name and executes the given
-- action.
lockAndBind :: (ImageName -> StorageMonad a) -> IO (Either StorageError a)
lockAndBind action =
    lockDevil $
        runErrorT $ do
            ilInit
            name <- ilGenImageName
            ilBindImage name

            action name

type ILuint    = #type ILuint
type ILsizei   = #type ILsizei
type ILboolean = #type ILboolean
type ILenum    = #type ILenum
type ILint     = #type ILint
type ILubyte   = #type ILubyte

-- DevIL uses unsigned integers as names for each image in processing.
newtype ImageName = ImageName ILuint
    deriving (Show)

foreign import ccall unsafe "ilInit" ilInitC :: IO ()
foreign import ccall unsafe "ilGetError" ilGetErrorC :: IO ILenum
foreign import ccall unsafe "ilOriginFunc" ilOriginFuncC
    :: ILenum -> IO ILboolean
foreign import ccall unsafe "ilEnable" ilEnableC :: ILenum -> IO ILboolean

il_RGB, il_RGBA, il_LUMINANCE :: ILenum
il_RGB = (#const IL_RGB)
il_RGBA = (#const IL_RGBA)
il_LUMINANCE = (#const IL_LUMINANCE)

il_IMAGE_HEIGHT, il_IMAGE_WIDTH :: ILenum
il_IMAGE_FORMAT, il_IMAGE_TYPE :: ILenum
il_IMAGE_HEIGHT = (#const IL_IMAGE_HEIGHT)
il_IMAGE_WIDTH  = (#const IL_IMAGE_WIDTH)
il_IMAGE_FORMAT = (#const IL_IMAGE_FORMAT)
il_IMAGE_TYPE   = (#const IL_IMAGE_TYPE)

il_UNSIGNED_BYTE :: ILenum
il_UNSIGNED_BYTE = (#const IL_UNSIGNED_BYTE)

origin :: ILenum
origin = (#const IL_ORIGIN_UPPER_LEFT)

-- | Initialize the library.
ilInit :: StorageMonad ()
ilInit = do
    lift ilInitC

    -- By default, origin is undefined and depends on the image type
    _ <- ilOriginFuncC origin             <?> FailedToInit
    _ <- ilEnableC (#const IL_ORIGIN_SET) <?> FailedToInit
    return ()

foreign import ccall unsafe "ilGenImages" ilGenImagesC
  :: ILsizei -> Ptr ILuint -> IO ()

-- | Allocates a new image name.
ilGenImageName :: StorageMonad ImageName
ilGenImageName = lift $ do
    alloca $ \pName -> do
        ilGenImagesC 1 pName
        name <- peek pName
        return $! ImageName name

foreign import ccall unsafe "ilBindImage" ilBindImageC :: ILuint -> IO ()

-- | Sets the image name as the current image for processing.
ilBindImage :: ImageName -> StorageMonad ()
ilBindImage (ImageName name) = lift $ ilBindImageC name

foreign import ccall unsafe "ilLoad" ilLoadC :: ILenum -> CString
                                             -> IO ILboolean
foreign import ccall unsafe "ilLoadL" ilLoadLC :: ILenum -> CString -> ILuint
                                               -> IO ILboolean

ilLoad :: LoadImageType t => t -> FilePath -> StorageMonad ()
ilLoad t path = do
    _ <- withCString path (ilLoadC (toIlType t))
        <??> (\case (#const IL_COULD_NOT_OPEN_FILE) -> FailedToOpenFile
                    (#const IL_ILLEGAL_FILE_VALUE)  -> InvalidFile
                    (#const IL_INVALID_FILE_HEADER) -> InvalidFile
                    (#const IL_INVALID_VALUE)       -> InvalidFile
                    (#const IL_OUT_OF_MEMORY)       -> OutOfMemory
                    (#const IL_INVALID_EXTENSION)   -> UnknownFileType
                    _                               -> FailedToLoad)
    return ()

ilLoadL :: LoadImageType t => t -> BS.ByteString -> StorageMonad ()
ilLoadL t bs = do
    _ <- BS.unsafeUseAsCStringLen bs (\(ptr, len) ->
                                    ilLoadLC (toIlType t) ptr
                                             (fromIntegral len))
        <??> (\case (#const IL_COULD_NOT_OPEN_FILE) -> FailedToOpenFile
                    (#const IL_ILLEGAL_FILE_VALUE)  -> InvalidFile
                    (#const IL_INVALID_FILE_HEADER) -> InvalidFile
                    (#const IL_INVALID_VALUE)       -> InvalidFile
                    (#const IL_OUT_OF_MEMORY)       -> OutOfMemory
                    (#const IL_TYPE_UNKNOWN)        -> UnknownFileType
                    _                               -> FailedToLoad)
    return ()

foreign import ccall unsafe "ilGetInteger" ilGetIntegerC :: ILenum -> IO ILint
foreign import ccall unsafe "ilConvertImage" ilConvertImageC
    :: ILenum -> ILenum -> IO ILboolean
foreign import ccall unsafe "ilGetData" ilGetDataC :: IO (Ptr ILubyte)
foreign import ccall unsafe "ilDeleteImages" ilDeleteImagesC
    :: ILsizei -> Ptr ILuint -> IO ()

-- | Puts the current image inside a 'Vector'.
fromDevil :: ImageName -> StorageMonad StorageImage
fromDevil (ImageName name) = do
    format <- ilGetInteger il_IMAGE_FORMAT
    w      <- ilGetInteger il_IMAGE_WIDTH
    h      <- ilGetInteger il_IMAGE_HEIGHT
    let !size = ix2 h w

    case format of
        _ | format == il_RGB -> do
            convertChannels il_RGB
            RGBStorage <$> toManifest size
          | format == il_RGBA -> do
            convertChannels il_RGBA
            RGBAStorage <$> toManifest size
          | format == il_RGBA -> do
            convertChannels il_LUMINANCE
            GreyStorage <$> toManifest size
          | otherwise -> do -- Unsupported formats are converted to RGBA.
            ilConvertImage il_RGBA il_UNSIGNED_BYTE
            RGBAStorage <$> toManifest size
  where
    -- Converts the image to the given format if the pixel type isn't Word8.
    convertChannels destFormat = do
        pixelType <- ilGetInteger il_IMAGE_TYPE
        when (pixelType /= il_UNSIGNED_BYTE) $
            ilConvertImage destFormat il_UNSIGNED_BYTE

    -- Converts the C vector of unsigned bytes to a garbage collected 'Vector'
    -- inside a 'Manifest' image. ilDeleteImages will be called when the image
    -- will be garbage collected.
    toManifest size@(Z :. h :. w) = lift $ do
        pixels        <- castPtr <$> ilGetDataC
        managedPixels <- newForeignPtr pixels (with name (ilDeleteImagesC 1))
        return $! Manifest size (unsafeFromForeignPtr0 managedPixels (w * h))

    ilGetInteger mode = lift $ fromIntegral <$> ilGetIntegerC mode

    ilConvertImage format pixelType = do
        _ <- ilConvertImageC format pixelType <?> FailedToHaskell
        return ()

-- | Removes the image and any allocated memory.
ilDeleteImage :: ImageName -> StorageMonad ()
ilDeleteImage (ImageName name) = lift $ with name (ilDeleteImagesC 1)

foreign import ccall unsafe "ilRegisterOrigin" ilRegisterOriginC
    :: ILenum -- Origin
    -> IO ()

foreign import ccall unsafe "ilTexImage" ilTexImageC
    :: ILuint -> ILuint -> ILuint   -- w h depth
    -> ILubyte -> ILenum -> ILenum  -- numberOfChannels format type
    -> Ptr ()                       -- data (copy from this pointer)
    -> IO ILboolean

-- | Sets the current DevIL image to the vector's internal array.
toDevil :: StorageImage -> StorageMonad ()
toDevil storImg = do
    case storImg of GreyStorage img -> writeManifest img il_LUMINANCE
                    RGBAStorage img -> writeManifest img il_RGBA
                    RGBStorage  img -> writeManifest img il_RGB
  where
    writeManifest img@(Manifest (Z :. h :. w) vec) format = do
        _ <- (unsafeWith vec $ \p ->
                   ilTexImageC (fromIntegral w) (fromIntegral h) 1
                        (fromIntegral $ nChannels img)
                        format il_UNSIGNED_BYTE (castPtr p))
                <?> OutOfMemory
        lift $ ilRegisterOriginC origin

foreign import ccall unsafe "ilSave" ilSaveC
    :: ILenum -> CString -> IO ILboolean

-- | Saves the current image to a file.
ilSave :: SaveImageType t => t -> FilePath -> StorageMonad ()
ilSave t path = do
    _ <- withCString path (ilSaveC (toIlType t))
            <??> (\case (#const IL_COULD_NOT_OPEN_FILE) -> FailedToOpenFile
                        (#const IL_INVALID_EXTENSION)   -> UnknownFileType
                        (#const IL_FILE_ALREADY_EXISTS) -> FileAlreadyExists
                        _                               -> FailedToSave)
    return ()

foreign import ccall unsafe "ilSaveL" ilSaveLC
    :: ILenum -> Ptr () -> ILuint -> IO ILuint

-- | Saves the current image to a memory area.
ilSaveL :: SaveBSImageType t => t -> StorageMonad BS.ByteString
ilSaveL t = do
    -- ilSaveLC returns the number of bytes required to store the image when
    -- called with a NULL pointer and a size of 0.
    size <- ilSaveLC (toIlType t) nullPtr 0 <?> FailedToSave
    ptr  <- lift $ mallocBytes (fromIntegral size)

    _ <- ilSaveLC (toIlType t) ptr size
            <??> (\case (#const IL_OUT_OF_MEMORY) -> OutOfMemory
                        _                         -> FailedToSave)

    lift $ BS.unsafePackMallocCStringLen (castPtr ptr, fromIntegral size)

infix 0 <?>
-- | Wraps a breakable DevIL action (which returns 0 on failure) in the
-- 'StorageMonad'. Throws the given error in the monad if the action fails.
(<?>) :: Integral a => IO a -> StorageError -> StorageMonad a
action <?> err = action <??> (const err)

infix 0 <??>
-- | Wraps a breakable DevIL action (which returns 0 on failure) in the
-- 'StorageMonad'. On failure, throws the error given by the function when
-- called with the 'ilGetErrorC' error code.
(<??>) :: Integral a => IO a -> (ILenum -> StorageError) -> StorageMonad a
action <??> f = do
    res <- lift action
    when (res == 0) $ do
        err <- lift ilGetErrorC
        throwError (f err)
    return res
