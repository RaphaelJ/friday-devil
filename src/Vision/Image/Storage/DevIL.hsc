{-# LANGUAGE BangPatterns, FlexibleContexts, FlexibleInstances
           , ForeignFunctionInterface, MultiParamTypeClasses #-}

-- | Uses the DevIL C library to read and write images from and to files.
--
-- /Note:/ As the underlier DevIL library is *not* tread-safe, there is a global
-- lock which will prevent two load/save calls to be performed at the same time.
module Vision.Image.Storage.DevIL (
      ImageType (..), StorageImage (..), StorageError (..), load, loadBS, save
    ) where

import Control.Applicative ((<$>), (<*))
import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (Error (..), ErrorT, runErrorT, throwError)
import Data.Convertible (Convertible (..), convert)
import Data.Int
import Data.Vector.Storable (unsafeFromForeignPtr0, unsafeWith)
import Data.Word
import Foreign.C.String (CString, withCString)
import Foreign.Concurrent (newForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr)
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

data StorageImage = GreyStorage Grey | RGBAStorage RGBA | RGBStorage RGB

data ImageType = BMP | CUT
               | DDS         -- ^ DirectDraw Surface (.dds).
               | Doom        -- ^ Doom texture.
               | DoomFlat    -- ^ Doom flat texture (floor).
               | GIF | ICO | JPG
               | LIF         -- ^ Homeworld (.lif).
               | MNG | PCD | PCX | PIC | PNG
               | PNM         -- ^ Portable AnyMap (.pbm, .pgm or .ppm).
               | PSD | PSP | SGI | TGA | TIFF
               | RAW         -- Raw data with a 13-byte header.
    deriving (Eq, Show)

data StorageError = FailedToInit     -- ^ Failed to initialise the library.
                  | FailedToOpenFile -- ^ Failed to open the given file.
                  | InvalidType      -- ^ The file could not be loaded based
                                     -- on extension or header.
                  | OutOfMemory      -- ^ Could not allocate memory for the new
                                     -- image data.
                  | FailedToLoad     -- ^ Failed to load the image, invalid
                                     -- format.
                  | FailedToHaskell  -- ^ Failed to convert the loaded image to
                                     -- its Haskell representation.
                  | FailedToDevil    -- ^ Failed to write the image content
                                     -- through the inner DevIL library.
                  | FailedToSave     -- ^ Could not open the file for writing.
                  | UnknownError (Maybe String)
    deriving (Eq)

type StorageMonad = ErrorT StorageError IO

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

instance Error StorageError where
    noMsg  = UnknownError Nothing
    strMsg = UnknownError . Just

instance Show StorageError where
    show FailedToInit     = "Failed to initialise the DevIL library."
    show FailedToOpenFile = "Failed to open the given file."
    show InvalidType      =
        "The file could not be loaded based on extension or header."
    show OutOfMemory      = "Could not allocate memory for the new image data."
    show FailedToLoad     = "Failed to load the image."
    show FailedToHaskell  =
        "Failed to convert the loaded image to its Haskell representation."
    show FailedToDevil    =
        "Failed to write the image content through the inner DevIL library."
    show FailedToSave     = "Could not open the file for writing."
    show (UnknownError (Just msg)) = msg
    show (UnknownError Nothing   ) = "Unknown error."

-- | Reads an image into a manifest vector from a file.
--
-- If no image type is given, type will be determined automatically.
load :: Maybe ImageType -> FilePath -> IO (Either StorageError StorageImage)
load mType path =
    lockDevil $
        bindAndLoad $
            withCString path $ \cPath ->
                ilLoadC (toIlType mType) cPath

-- | Reads an image into a manifest vector from a strict 'ByteString'.
--
-- If no image type is given, type will be determined automatically.
-- TIFF images are not supported.
loadBS :: Maybe ImageType -> BS.ByteString
       -> IO (Either StorageError StorageImage)
loadBS (Just TIFF) _  = return $ Left FailedToLoad
loadBS mType       bs =
    lockDevil $
        bindAndLoad $
            BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
                ilLoadLC (toIlType mType) ptr (fromIntegral len)

-- | Saves the image to the given file.
--
-- /Note:/ The image type is determined by the filename extension.
-- Will fail if the file already exists.
save :: (Convertible i StorageImage) => FilePath -> i -> IO (Maybe StorageError)
save path img = lockDevil $ do
    res <- runErrorT $ do
        ilInit
        name <- ilGenImageName
        ilBindImage name

        toDevil $ convert img
        ilSaveImage path

        ilDeleteImage name

    return $ case res of Right () -> Nothing
                         Left err -> Just err

-- C wrappers and helpers ------------------------------------------------------

devilLock :: MVar ()
devilLock = unsafePerformIO $ newMVar ()
{-# NOINLINE devilLock #-}

-- | Uses a global lock ('devilLock') to prevent two threads to call the
-- library at the same time.
lockDevil :: IO a -> IO a
lockDevil action = do
    takeMVar devilLock
    ret <- action
    putMVar devilLock ()
    return ret

-- | Allocates a new image name, executes the given action to load the image
-- and then converts it into its Haskell representation.
bindAndLoad :: IO ILboolean ->  IO (Either StorageError StorageImage)
bindAndLoad action = runErrorT $ do
    ilInit
    name <- ilGenImageName
    ilBindImage name

    res <- lift action
    when (res == 0) $ do
        err <- lift ilGetErrorC
        throwError $ case err of
            (#const IL_COULD_NOT_OPEN_FILE) -> FailedToOpenFile
            (#const IL_INVALID_EXTENSION)   -> InvalidType
            (#const IL_INVALID_FILE_HEADER) -> InvalidType
            (#const IL_OUT_OF_MEMORY)       -> OutOfMemory
            _                               -> FailedToLoad

    fromDevil name

toIlType :: Maybe ImageType -> ILenum
toIlType (Just BMP)      = (#const IL_BMP)
toIlType (Just CUT)      = (#const IL_CUT)
toIlType (Just DDS)      = (#const IL_DDS)
toIlType (Just Doom)     = (#const IL_DOOM)
toIlType (Just DoomFlat) = (#const IL_DOOM_FLAT)
toIlType (Just GIF)      = (#const IL_GIF)
toIlType (Just ICO)      = (#const IL_ICO)
toIlType (Just JPG)      = (#const IL_JPG)
toIlType (Just LIF)      = (#const IL_LIF)
toIlType (Just MNG)      = (#const IL_MNG)
toIlType (Just PCD)      = (#const IL_PCD)
toIlType (Just PCX)      = (#const IL_PCX)
toIlType (Just PIC)      = (#const IL_PIC)
toIlType (Just PNG)      = (#const IL_PNG)
toIlType (Just PNM)      = (#const IL_PNM)
toIlType (Just PSD)      = (#const IL_PSD)
toIlType (Just PSP)      = (#const IL_PSP)
toIlType (Just SGI)      = (#const IL_SGI)
toIlType (Just TGA)      = (#const IL_TGA)
toIlType (Just TIFF)     = (#const IL_TIF)
toIlType (Just RAW)      = (#const IL_RAW)
toIlType Nothing         = (#const IL_TYPE_UNKNOWN)

#include "IL/il.h"

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
    ilOriginFuncC origin <?> FailedToInit
    ilEnableC (#const IL_ORIGIN_SET)            <?> FailedToInit

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
        ilConvertImageC format pixelType <?> FailedToHaskell

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
    writeManifest img@(Manifest (Z :. h :. w) vec) format =
        (unsafeWith vec $ \p ->
               ilTexImageC (fromIntegral w) (fromIntegral h) 1
                        (fromIntegral $ nChannels img)
                        format il_UNSIGNED_BYTE (castPtr p)
            <* ilRegisterOriginC origin
        ) <?> FailedToDevil

foreign import ccall unsafe "ilSaveImage" ilSaveImageC
    :: CString -> IO ILboolean

-- | Saves the current image.
ilSaveImage :: FilePath -> StorageMonad ()
ilSaveImage file = withCString file ilSaveImageC <?> FailedToSave

infix 0 <?>
-- | Wraps a breakable DevIL action (which returns 0 on failure) in the
-- 'StorageMonad'. Throws the given error in the monad if the action fails.
(<?>) :: IO ILboolean -> StorageError -> StorageMonad ()
action <?> err = do
    res <- lift action
    when (res == 0) $
        throwError err
