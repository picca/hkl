import arrows, bravais

from FreeCAD import Base, Part

#declare camera_location = 2 * (x+y+0.5*z);
#declare camera_look_at = <0,0,0>;

O = Base.Vector(0, 0, 0)
X = Base.Vector(1, 0, 0)
Y = Base.Vector(0, 1, 0)
Z = Base.Vector(0, 0, 1)

Part.show(Part.makePlane(5, 5, Base.Vector(-.5, 0, 0), X)) #LightWood
Part.show(Part.makePlane(5, 5, Base.Vector(0, -.5, 0), Y)) #LightWood*.9
Part.show(Part.makePlane(5, 5, Base.Vector(0, 0, -.5), Z)) #LightWood*.8

v1 = Base.Vector(1, 0, 0)
v2 = Base.Vector(.31, .95, 0)
v3 = Base.Vector(.4, .5, 0.76)

Part.show(arrows.arrow(X)) # Grey
Part.show(arrows.arrow(Y)) # Grey
Part.show(arrows.arrow(Z)) # Grey

Part.show(arrows.repere(v1, v2, v3, True))
