from FreeCAD import Base, Part

def _cylinder(a, b, size):
  direction = b.sub(a)
  length = direction.Length
  return Part.makeCylinder(size, length, a, direction)

def atome(position, size):
  return Part.makeSphere(size, position)

def crystal_atomes(a, b, c, size):
  shape = atome(Base.Vector(0, 0, 0), size)
  shape = shape.fuse(atome(a, size))
  shape = shape.fuse(atome(b, size))
  shape = shape.fuse(atome(c, size))
  shape = shape.fuse(atome(a.add(b), size))
  shape = shape.fuse(atome(a.add(c), size))
  shape = shape.fuse(atome(b.add(c), size))
  shape = shape.fuse(atome(a.add(b).add(c), size))
  return shape

def crystal_frame(a, b, c, size):
  shape = _cylinder(Base.Vector(0, 0, 0), a, size)
  shape = shape.fuse(_cylinder(Base.Vector(0, 0, 0), b, size))
  shape = shape.fuse(_cylinder(Base.Vector(0, 0, 0), c, size))
  shape = shape.fuse(_cylinder(a, a.add(b), size))
  shape = shape.fuse(_cylinder(a, a.add(c), size))
  shape = shape.fuse(_cylinder(b, b.add(a), size))
  shape = shape.fuse(_cylinder(b, b.add(c), size))
  shape = shape.fuse(_cylinder(a.add(b), a.add(b).add(c), size))
  shape = shape.fuse(_cylinder(c, c.add(a), size))
  shape = shape.fuse(_cylinder(c, c.add(b), size))
  shape = shape.fuse(_cylinder(a.add(c), a.add(b).add(c), size))
  shape = shape.fuse(_cylinder(b.add(c), a.add(b).add(c), size))
  return shape
