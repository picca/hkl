""" -*- Mode: Python -*- """

import arrows, bravais

from FreeCAD import Base, Part
import FreeCADGui

# crystal lattice parameters

O = Base.Vector(0, 0, 0)
a = Base.Vector(1, 0, 0)
b = Base.Vector(.31,.95,0)
c = Base.Vector(.4,.5,0.76)
x = Base.Vector(2, 0, 0)
y = Base.Vector(0, 2, 0)
z = Base.Vector(0, 0, 2)

#declare camera_location = a/2+b/1.5-2*z;
#declare camera_look_at = a/2+b/1.5;

#orthonormal coordinates
Part.show(arrows.arrow(x)) #Grey
Part.show(arrows.arrow(y)) #Grey
Part.show(arrows.arrow(z)) #Grey

# the crystal
atomes = bravais.crystal_atomes(a, b, c, .1) # Blue
frame = bravais.crystal_frame(a, b, c, .005) # Gray

Part.show(atomes)
Part.show(frame)

"""
FreeCADGui.activeDocument().activeView().viewAxometric()
FreeCADGui.activeDocument().activeView().saveImage('crystal.png',800,600,'Current')
"""
