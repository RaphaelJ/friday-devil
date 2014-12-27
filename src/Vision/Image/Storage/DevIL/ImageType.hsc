-- | Defines image types which can be used with the DevIL API.
module Vision.Image.Storage.DevIL.ImageType where

import Data.Word

#include "IL/il.h"

class ImageType t where
    -- | Returns the DevIL constant associated with the image type.
    toIlType :: t -> #type ILenum

-- | Tries to determine automatically the image type with the header and/or the
-- file extension.
--
-- Raises an 'UnknownFileType' error if the detection failed.
data Autodetect = Autodetect deriving (Eq, Show)

data BLP = BLP deriving (Eq, Show)

data BMP = BMP deriving (Eq, Show)

data CHEAD = CHEAD deriving (Eq, Show)

data CUT = CUT deriving (Eq, Show)

data DCX = DCX deriving (Eq, Show)

-- | DirectDraw Surface (.dds).
data DDS = DDS deriving (Eq, Show)

data DICOM = DICOM deriving (Eq, Show)

-- | Doom texture.
data Doom = Doom deriving (Eq, Show)

-- | Doom flat texture (floor).
data DoomFlat = DoomFlat deriving (Eq, Show)

data DPX = DPX deriving (Eq, Show)

data EXR = EXR deriving (Eq, Show)

data FITS = FITS deriving (Eq, Show)

data FTX = FTX deriving (Eq, Show)

data GIF = GIF deriving (Eq, Show)

data HDR = HDR deriving (Eq, Show)

data ICO = ICO deriving (Eq, Show)

data ICNS = ICNS deriving (Eq, Show)

data IFF = IFF deriving (Eq, Show)

data IWI = IWI deriving (Eq, Show)

-- | Paint Shop Pro (Jasc) palette.
data JASCPAL = JASCPAL deriving (Eq, Show)

-- | JPEG 2000.
data JP2 = JP2 deriving (Eq, Show)

data JPG = JPG deriving (Eq, Show)

-- | Homeworld (.lif).
data LIF = LIF deriving (Eq, Show)

data MDL = MDL deriving (Eq, Show)

data MNG = MNG deriving (Eq, Show)

data MP3 = MP3 deriving (Eq, Show)

data PCD = PCD deriving (Eq, Show)

data PCX = PCX deriving (Eq, Show)

data PIC = PIC deriving (Eq, Show)

data PIX = PIX deriving (Eq, Show)

data PNG = PNG deriving (Eq, Show)

-- | Portable AnyMap (.pbm, .pgm or .ppm).
data PNM = PNM deriving (Eq, Show)

-- | Photoshop Document.
data PSD = PSD deriving (Eq, Show)

-- | Paint Shop Pro image.
data PSP = PSP deriving (Eq, Show)

data PXR = PXR deriving (Eq, Show)

-- | Raw data with a 13-byte header.
data RAW = RAW deriving (Eq, Show)

data ROT = ROT deriving (Eq, Show)

data SGI = SGI deriving (Eq, Show)

data SUN = SUN deriving (Eq, Show)

-- | Medieval II: Total War Texture (.texture) file.
data Texture = Texture deriving (Eq, Show)

data TGA = TGA deriving (Eq, Show)

data TIFF = TIFF deriving (Eq, Show)

data TPL = TPL deriving (Eq, Show)

data UTX = UTX deriving (Eq, Show)

data VTF = VTF deriving (Eq, Show)

data WAL = WAL deriving (Eq, Show)

data WBMP = WBMP deriving (Eq, Show)

data XPM = XPM deriving (Eq, Show)

-- Instances -------------------------------------------------------------------

instance ImageType Autodetect where
    toIlType Autodetect = (#const IL_TYPE_UNKNOWN)

instance ImageType BLP where
    toIlType BLP = (#const IL_BLP)

instance ImageType BMP where
    toIlType BMP = (#const IL_BMP)

instance ImageType CHEAD where
    toIlType CHEAD = (#const IL_CHEAD)

instance ImageType CUT where
    toIlType CUT = (#const IL_CUT)

instance ImageType DCX where
    toIlType DCX = (#const IL_DCX)

instance ImageType DDS where
    toIlType DDS = (#const IL_DDS)

instance ImageType DICOM where
    toIlType DICOM = (#const IL_DICOM)

instance ImageType Doom where
    toIlType Doom = (#const IL_DOOM)

instance ImageType DoomFlat where
    toIlType DoomFlat = (#const IL_DOOM_FLAT)

instance ImageType DPX where
    toIlType DPX = (#const IL_DPX)

instance ImageType EXR where
    toIlType EXR = (#const IL_EXR)

instance ImageType FITS where
    toIlType FITS = (#const IL_FITS)

instance ImageType FTX where
    toIlType FTX = (#const IL_FTX)

instance ImageType GIF where
    toIlType GIF = (#const IL_GIF)

instance ImageType HDR where
    toIlType HDR = (#const IL_HDR)

instance ImageType ICO where
    toIlType ICO = (#const IL_ICO)

instance ImageType ICNS where
    toIlType ICNS = (#const IL_ICNS)

instance ImageType IFF where
    toIlType IFF = (#const IL_IFF)

instance ImageType IWI where
    toIlType IWI = (#const IL_IWI)

instance ImageType JASCPAL where
    toIlType JASCPAL = (#const IL_JASC_PAL)

instance ImageType JP2 where
    toIlType JP2 = (#const IL_JP2)

instance ImageType JPG where
    toIlType JPG = (#const IL_JPG)

instance ImageType LIF where
    toIlType LIF = (#const IL_LIF)

instance ImageType MDL where
    toIlType MDL = (#const IL_MDL)

instance ImageType MNG where
    toIlType MNG = (#const IL_MNG)

instance ImageType MP3 where
    toIlType MP3 = (#const IL_MP3)

instance ImageType PCD where
    toIlType PCD = (#const IL_PCD)

instance ImageType PCX where
    toIlType PCX = (#const IL_PCX)

instance ImageType PIC where
    toIlType PIC = (#const IL_PIC)

instance ImageType PIX where
    toIlType PIX = (#const IL_PIX)

instance ImageType PNG where
    toIlType PNG = (#const IL_PNG)

instance ImageType PNM where
    toIlType PNM = (#const IL_PNM)

instance ImageType PSD where
    toIlType PSD = (#const IL_PSD)

instance ImageType PSP where
    toIlType PSP = (#const IL_PSP)

instance ImageType PXR where
    toIlType PXR = (#const IL_PXR)

instance ImageType RAW where
    toIlType RAW = (#const IL_RAW)

instance ImageType ROT where
    toIlType ROT = (#const IL_ROT)

instance ImageType SGI where
    toIlType SGI = (#const IL_SGI)

instance ImageType SUN where
    toIlType SUN = (#const IL_SUN)

instance ImageType Texture where
    toIlType Texture = (#const IL_TEXTURE)

instance ImageType TGA where
    toIlType TGA = (#const IL_TGA)

instance ImageType TIFF where
    toIlType TIFF = (#const IL_TIF)

instance ImageType TPL where
    toIlType TPL = (#const IL_TPL)

instance ImageType UTX where
    toIlType UTX = (#const IL_UTX)

instance ImageType VTF where
    toIlType VTF = (#const IL_VTF)

instance ImageType WAL where
    toIlType WAL = (#const IL_WAL)

instance ImageType WBMP where
    toIlType WBMP = (#const IL_WBMP)

instance ImageType XPM where
    toIlType XPM = (#const IL_XPM)
