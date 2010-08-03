import arrows, bravais

from FreeCAD import Base, Part

#declare camera_location = 2 * (x+y+0.5*z);
#declare camera_look_at = <0,0,0>;

O = Base.Vector(0, 0, 0)
X = Base.Vector(1, 0, 0)
Y = Base.Vector(0, 1, 0)
Z = Base.Vector(0, 0, 1)

Part.show(Part.makePlane(5, 5, O, X)) #LightWood
Part.show(Part.makePlane(5, 5, O, Y)) #LightWood*.9
Part.show(Part.makePlane(5, 5, O, Z)) #LightWood*.8

b1 = X
b2 = Base.Vector(.31, .95, 0)
b3 = Base.Vector(.4, .5, 0.76)

a1 = b2.cross(b3)
a2 = b3.cross(b1)
a3 = b1.cross(b2)


# orthonormal coordinates
Part.show(arrows.arrow(Y)) #Gray
Part.show(arrows.arrow(Z)) #Gray

# reciprocal space
Part.show(arrows.arrow(b1)) #Blue
Part.show(arrows.arrow(b2)) #Red
Part.show(arrows.arrow(b3)) #Green

# real space
Part.show(arrows.arrow(a1)) #Cyan
Part.show(arrows.arrow(a2)) #Magenta
Part.show(arrows.arrow(a3)) #Yellow


Part.show(Part.makePlane(5, 5, O, b1)) #Blue
Part.show(Part.makePlane(5, 5, O, b2)) #Red
Part.show(Part.makePlane(5, 5, O, b3)) #Green
