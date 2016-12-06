import pylab
import numpy

FILE = "pinhole1.smv"

with open(FILE, "rb") as f:
    f.seek(512)
    img = numpy.fromfile(f, dtype='uint16', sep="")
    print(img.shape)
    img.shape = ((4096, 4096))
    pylab.imshow(img, interpolate='nearest')
    pylab.show()
