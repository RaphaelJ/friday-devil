Provides bindings to the *[DevIL image library](http://openil.sourceforge.net/)*
to load and save *[friday](https://hackage.haskell.org/package/friday)* images.

*DevIL* is a fast image library written in C. The library must be installed on
the host system before usage.

Supports a wide variety of image formats, including BMP, JPG, PNG, GIF, ICO and
PSD.

# Quick tour

## Storage images

When you use the `load` function to read an image from a file, the returned
image is a `StorageImage`:

```haskell
data StorageImage = GreyStorage Grey | RGBAStorage RGBA | RGBStorage RGB
```

Before using an image loaded from a file, you need to convert it into an usable
representation such as RGB. You can do this convertion with the `convert` method
from the *convertible* package (re-exported by the *friday* package):

```haskell
toRGB :: StorageImage -> RGB
toRGB = convert
```

The following example reads an image from a file, automatically determining
the image format, and then writes it back into a `ByteString` as a greyscale
*PNG* image.

```haskell
Right io <- load Autodetect "image.jpg"
let grey = convert io :: Grey
    bs   = saveBS PNG grey
print bs
```

See the *[friday-examples](https://github.com/RaphaelJ/friday-examples)*
package for a set of examples.
