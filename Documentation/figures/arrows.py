""" -*- Mode: Python -*- """

from FreeCAD import Base, Part
import math

def _arrow(begin, end):
    direction = end.sub(begin)
    length = direction.Length
    body_len = length * .9
    head_len = length * .1
    body_radius = length / 100.
    head_radius = length / 30.

    # compute the head_begin point
    tmp = Base.Vector(direction)
    tmp.multiply(.9)
    head_begin = begin.add(tmp)

    body = Part.makeCylinder(body_radius, body_len, begin, direction)
    head = Part.makeCone(head_radius, 0, head_len, head_begin, direction)
    return body.fuse(head)

def arrow(v):
    return _arrow(Base.Vector(0, 0, 0), v)

def _sector(p1, p0, p2, factor):
    v1 = p1.sub(p0)
    v2 = p2.sub(p0)
    radius = min(v1.Length, v2.Length) * factor
 
    ### compute the 3 point used for the arc part.
    # first
    pp1 = Base.Vector(v1)
    pp1.normalize()
    pp1.multiply(radius)

    # third
    pp2 = Base.Vector(v2)
    pp2.normalize()
    pp2.multiply(radius)

    #second is the middle of the
    pp0 = pp1.add(pp2)
    pp0.normalize()
    pp0.multiply(radius)

    arc = Part.Arc(pp1, pp0, pp2)
    arc.translate(p0)

    thikness = 0.005

    # compute the begin point
    vec = v2.cross(v1)
    vec.normalize()
    vec.multiply(thikness)
    begin = Base.Vector(vec)
    begin.multiply(-.5)
    line1 = Part.Line(pp1, p0)
    line2 = Part.Line(pp2, p0)
    shape = Part.Shape([line1, arc, line2])
    shape.translate(begin)

    wire = Part.Wire(shape.Edges)
    face = Part.Face(wire)
    
    return face.extrude(vec)

def sector(v1, v2, factor):
    return _sector(v1, Base.Vector(0, 0, 0), v2, factor)

def repere(v1, v2, v3, with_sector=False):
    shape = arrow(v1)
    shape = shape.fuse(arrow(v2))
    shape = shape.fuse(arrow(v3))
    if with_sector:
        shape = shape.fuse(sector(v1, v2, .3))
        shape = shape.fuse(sector(v1, v3, .4))
        shape = shape.fuse(sector(v2, v3, .5))
    return shape

def carrow(direction, r1, r2, angle, sens=1):
    # create the body along the z axis
    body = Part.makeTorus(r1, r2, Base.Vector(0, 0, 0), Base.Vector(0, 0, 1), 0, 360, angle)

    # put the head at the right place
    head_length = r1 * 30. / 180. * math.pi;
    head = Part.makeCone(r2* 2, 0, head_length, Base.Vector(r1, 0, 0), Base.Vector(0, 1, 0), 360)
    head.rotate((0, 0, 0), (0, 0, 1), angle)

    shape = body.fuse(head)

    #orientate the arrow in the right direction
    tmp = Base.Vector(0, 0, 1).cross(direction)
    if tmp.Length > 0.0001:
         shape.rotate((0, 0, 0),
                      (tmp.x, tmp.y, tmp.z),
                      Base.Vector(0, 0, 1).getAngle(direction) * 180. / math.pi)

    return shape

"""
a = FreeCAD.Vector(1, 0, 0)
b = FreeCAD.Vector(0, 1, 0)
c = FreeCAD.Vector(0, 0, 1)
rep = repere(a, b, c, True)
tmp = carrow(Base.Vector(1, 1, 1), 2, 0.1, 220)

for s in [rep, tmp]:
    Part.show(s)

import FreeCADGui as Gui
Gui.activeDocument().activeView().viewAxometric()
Gui.activeDocument().activeView().saveImage('/home/picca/Projets/hkl/Documentation/figures/toto.png',800,600,'Current')
"""