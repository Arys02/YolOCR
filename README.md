YolOCR
======

OCR, projet EPITA SPE

To compile, use ```make```
To execute, the syntax is : ```./YolOCR -i image_path [binarisation mode] [rotation mode] [-scale] [-adjust]```

For binarisation, different modes are:

* ```-global``` for global threshold detection.
* ```-local9``` for local threshold detection with 9x9 matrix.
* ```-local11``` for global threshold detection with 11x11 matrix.
* ```-bilinear``` for rotation using bilinear interpolation.
* ```-good``` for rotation using reverse matrix rotation.
* ```-bad``` for rotation using normal matrix rotation.
* ```-scale``` to scale image by 0.5.
* ```-adjust``` to adjust image to 30x30 centered.
