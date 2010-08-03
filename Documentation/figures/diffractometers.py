from FreeCAD import Base, Part
import math, arrows

def Axis(axis1, axis2, rank):
    dir1 = Base.Vector(axis1)
    if dir1.x >= 0 and dir1.y >= 0 and dir1.z >= 0:
        dir1.multiply(-1)
    dir2 = Base.Vector(axis2)
    if dir2.x >= 0 and dir2.y >= 0 and dir2.z >= 0:
        dir2.multiply(-1)
    radius = 1
    length = 1
    begin1 = Base.Vector(dir1)
    begin1.normalize()
    begin1.multiply(rank * (length + .1))
    a1 = Part.makeCylinder(radius, length, begin1, dir1)
    
    begin2 = Base.Vector(dir2)
    begin2.normalize()
    begin2.multiply(rank * (length + .1))
    a2 = Part.makeCylinder(radius, length, begin2, dir2)

    return a1.fuse(a2)

def diffractometer(axes):
    length = len(axes)
    shape = 1
    for i in range(length-1):
        print i
        ax1 = axes[length - i - 1]
        ax2 = axes[length - i - 2]
        rank = i + 1
        if not i:
            shape = Axis(ax1, ax2, rank)
        else:
            shape = shape.fuse(Axis(ax1, ax2, rank))
    return shape
  
"""
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

X = Base.Vector(1, 0, 0)
Y = Base.Vector(0, 1, 0)
Z = Base.Vector(0, 0, 1)
rep = arrows.repere(Y, Y, Z, False)

mu = Base.Vector(0, 0, 1)
komega = Base.Vector(0, -1, 0)
kappa = Base.Vector(0, -math.cos(50 / 180. * math.pi), -math.sin(50 / 180. * math.pi))
kphi = Base.Vector(0, -1, 0)
gamma = Base.Vector(0, 0, 1)
delta = Base.Vector(0, -1, 0)

dif = diffractometer([mu, komega, kappa, kphi])

for s in [rep, dif]:
    Part.show(s)

"""
import FreeCADGui as Gui
Gui.activeDocument().activeView().viewAxometric()
Gui.activeDocument().activeView().saveImage('/home/picca/Projets/hkl/Documentation/figures/toto.png',800,600,'Current')
"""
