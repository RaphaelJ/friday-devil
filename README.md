Provides bindings to the *[DevIL image library](http://openil.sourceforge.net/)*
to load and save *[friday](https://hackage.haskell.org/package/friday)* images.

*DevIL* is a fast image library written in C. The library must be installed on
the host system before usage.

Supports a wide variety of image formats, including BMP, JPG, PNG, GIF, ICO and
PSD.

# Quick tour

## Storage images

Images returned from `load` or passed to `save` must be `Convertile` from and to
`StorageImage`s. The `StorageImage` type is defined as:

```haskell
data StorageImage = GreyStorage Grey | RGBAStorage RGBA | RGBStorage RGB
```

The following example reads an image from a file, automatically determining
the image format, and then writes it back into a `ByteString` as a greyscale
*PNG* image.

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
main = do
    io <- load Autodetect "image.jpg"
    case io of
        Right (rgb :: RGB) -> -- Forces the RGB colorspace for the loaded image.
            let grey = convert rgb :: Grey
                bs   = saveBS PNG grey
            in print bs
        Left err           -> do
            putStrLn "Unable to load the image:"
            print err
```

See the *[friday-examples](https://github.com/RaphaelJ/friday-examples)*
package for a set of examples.
