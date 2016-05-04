import bpy
import numpy
import pyFAI
import math
import functools
import random
import bmesh


SCALE = 100

# Definition du détecteur Cirpad

class Cirpad(pyFAI.detectors.Detector): # (Detector):
    MAX_SHAPE = (11200, 120)
    IS_FLAT = False
    IS_CONTIGUOUS = False
    force_pixel = True
    uniform_pixel = False
    aliases = ["XCirpad"]
    MEDIUM_MODULE_SIZE = (560, 120)
    MODULE_SIZE = (80, 120)  # number of pixels per module (y, x)
    PIXEL_SIZE = (130e-6, 130e-6)
    DIFFERENT_PIXEL_SIZE = 2.5
    rot = [0,0,-6.74]

    def _calc_pixels_size(self,length, module_size, pixel_size):
        size = numpy.ones(length)
        n = (length // module_size)
        for i in range(1, n):
            size[i * module_size - 1] = self.DIFFERENT_PIXEL_SIZE
            size[i * module_size] = self.DIFFERENT_PIXEL_SIZE
        return pixel_size * size

    def passage(self,corners,rot): #,u)  # A définir plus précisément comment on obtient rot et u
        shape = corners.shape
        origine = corners[0][0][0,:]
        nmd = rotation(corners,rot)
        u = corners[shape[0]-1][0][1,:] - corners[0][0][0,:]
        u = u / numpy.linalg.norm(u)
        s = rotation(u,rot)
        s = s / numpy.linalg.norm(s)
        v = numpy.array([-u[1],u[0],u[2]])
        r = origine - nmd[0][0][0,:]
        w = (0.1e-3+0.24e-3+75.14e-3)*u + (0.8e-3)*v + (0.55e-3)*s + r
        nmd = translation(nmd,w)
        return(nmd)

    def get_pixel_corners(self):
        pixel_size1 = self._calc_pixels_size(self.MEDIUM_MODULE_SIZE[0], self.MODULE_SIZE[0], self.PIXEL_SIZE[0])
        pixel_size2 = (numpy.ones(self.MEDIUM_MODULE_SIZE[1]) * self.PIXEL_SIZE[1]).astype(numpy.float32)
        # half pixel offset
        pixel_center1 = pixel_size1 / 2.0  # half pixel offset
        pixel_center2 = pixel_size2 / 2.0
        # size of all preceeding pixels
        pixel_center1[1:] += numpy.cumsum(pixel_size1[:-1])
        pixel_center2[1:] += numpy.cumsum(pixel_size2[:-1])

        pixel_center1.shape = -1, 1
        pixel_center1.strides = pixel_center1.strides[0], 0

        pixel_center2.shape = 1, -1
        pixel_center2.strides = 0, pixel_center2.strides[1]

        pixel_size1.shape = -1, 1
        pixel_size1.strides = pixel_size1.strides[0], 0

        pixel_size2.shape = 1, -1
        pixel_size2.strides = 0, pixel_size2.strides[1]

        # On calcule la position du premier module
        corners = numpy.zeros((self.MEDIUM_MODULE_SIZE[0], self.MEDIUM_MODULE_SIZE[1], 4, 3), dtype=numpy.float32)
        corners[:, :, 0, 1] = pixel_center1 - pixel_size1 / 2.0
        corners[:, :, 0, 2] = pixel_center2 - pixel_size2 / 2.0
        corners[:, :, 1, 1] = pixel_center1 + pixel_size1 / 2.0
        corners[:, :, 1, 2] = pixel_center2 - pixel_size2 / 2.0
        corners[:, :, 2, 1] = pixel_center1 + pixel_size1 / 2.0
        corners[:, :, 2, 2] = pixel_center2 + pixel_size2 / 2.0
        corners[:, :, 3, 1] = pixel_center1 - pixel_size1 / 2.0
        corners[:, :, 3, 2] = pixel_center2 + pixel_size2 / 2.0

        n_corners = corners
        # Puis on calcule les coins pour les 19 modules restant
        for i in range(1, 20):
            n_corners = self.passage(n_corners, self.rot)
            corners = numpy.concatenate((corners, n_corners), axis=0) # A voir la disposition finale souhaité
        return corners

    # Pas fait encore
    def calc_cartesian_positions(self,d1 = None, d2 = None, center = True, use_cython = True):
        if (d1 is None) or d2 is None:
            d1 = pyFAI.utils.expand2d(numpy.arange(self.MAX_SHAPE[0]).astype(numpy.float32), self.MAX_SHAPE[1], False)
            d2 = pyFAI.utils.expand2d(numpy.arange(self.MAX_SHAPE[1]).astype(numpy.float32), self.MAX_SHAPE[0], True)
        corners = self.get_pixel_corners()
        if center:
            # avoid += It modifies in place and segfaults
            d1 = d1 + 0.5
            d2 = d2 + 0.5
        if  False and use_cython:
            p1, p2, p3 = bilinear.calc_cartesian_positions(d1.ravel(), d2.ravel(), corners, is_flat=False)
            p1.shape = d1.shape
            p2.shape = d2.shape
            p3.shape = d2.shape
        else: # A verifier
            i1 = d1.astype(int).clip(0, corners.shape[0] - 1)
            i2 = d2.astype(int).clip(0, corners.shape[1] - 1)
            delta1 = d1 - i1
            delta2 = d2 - i2
            pixels = corners[i1, i2]
            if pixels.ndim == 3:
                A0 = pixels[:, 0, 0]
                A1 = pixels[:, 0, 1]
                A2 = pixels[:, 0, 2]
                B0 = pixels[:, 1, 0]
                B1 = pixels[:, 1, 1]
                B2 = pixels[:, 1, 2]
                C0 = pixels[:, 2, 0]
                C1 = pixels[:, 2, 1]
                C2 = pixels[:, 2, 2]
                D0 = pixels[:, 3, 0]
                D1 = pixels[:, 3, 1]
                D2 = pixels[:, 3, 2]
            else:
                A0 = pixels[:, :, 0, 0]
                A1 = pixels[:, :, 0, 1]
                A2 = pixels[:, :, 0, 2]
                B0 = pixels[:, :, 1, 0]
                B1 = pixels[:, :, 1, 1]
                B2 = pixels[:, :, 1, 2]
                C0 = pixels[:, :, 2, 0]
                C1 = pixels[:, :, 2, 1]
                C2 = pixels[:, :, 2, 2]
                D0 = pixels[:, :, 3, 0]
                D1 = pixels[:, :, 3, 1]
                D2 = pixels[:, :, 3, 2]

            # points A and D are on the same dim1 (Y), they differ in dim2 (X)
            # points B and C are on the same dim1 (Y), they differ in dim2 (X)
            # points A and B are on the same dim2 (X), they differ in dim1 (Y)
            # points C and D are on the same dim2 (X), they differ in dim1 (
            p1 = A1 * (1.0 - delta1) * (1.0 - delta2) \
                 + B1 * delta1 * (1.0 - delta2) \
                 + C1 * delta1 * delta2 \
                 + D1 * (1.0 - delta1) * delta2
            p2 = A2 * (1.0 - delta1) * (1.0 - delta2) \
                 + B2 * delta1 * (1.0 - delta2) \
                 + C2 * delta1 * delta2 \
                 + D2 * (1.0 - delta1) * delta2
            p3 = A0 * (1.0 - delta1) * (1.0 - delta2) \
                 + B0 * delta1 * (1.0 - delta2) \
                 + C0 * delta1 * delta2 \
                 + D0 * (1.0 - delta1) * delta2
            # To ensure numerical consitency with cython procedure.
            p1 = p1.astype(numpy.float32)
            p2 = p2.astype(numpy.float32)
            p3 = p3.astype(numpy.float32)
        return p1, p2, p3

# Fonction utilisée pour définir le Cirpad mais je ne sais pas si je les met dans la classe ou pas car elles sont banals

def M(theta, u):
    """
    :param theta: the axis value in radian
    :type theta: float
    :param u: the axis vector [x, y, z]
    :type u: [float, float, float]
    :return: the rotation matrix
    :rtype: numpy.ndarray (3, 3)
    """
    c = math.cos(theta)
    one_minus_c = 1 - c
    s = math.sin(theta)
    return [[c + u[0]**2 * one_minus_c,
             u[0] * u[1] * one_minus_c - u[2] * s,
             u[0] * u[2] * one_minus_c + u[1] * s],
            [u[0] * u[1] * one_minus_c + u[2] * s,
             c + u[1]**2 * one_minus_c,
             u[1] * u[2] * one_minus_c - u[0] * s],
            [u[0] * u[2] * one_minus_c - u[1] * s,
             u[1] * u[2] * one_minus_c + u[0] * s,
             c + u[2]**2 * one_minus_c]]


def rotation(md,rot):
    shape = md.shape
    axe = numpy.array([[1,0,0],[0,1,0],[0,0,1]])    # A voir si ce n'est pas une entrée
    P = functools.reduce(numpy.dot, [M(numpy.radians(rot[i]),axe[i]) for i in range(len(rot))])
    try :
        nmd = numpy.transpose(numpy.reshape(numpy.tensordot(P,numpy.reshape(numpy.transpose(md),(3,shape[0]*shape[1]*4)), axes=1),(3,4,shape[1],shape[0])))
    except IndexError :
        nmd =  numpy.transpose(numpy.tensordot(P,numpy.transpose(md), axes=1))
    return(nmd)


def translation(md, u):
    nmd = md + u
    return(nmd)

# Change coordonée pour passer de celle de pyFAI à celle de l'affichage.

def inter2(array):
    ar = numpy.empty(numpy.shape(array))
    ar[0] = array[2]
    ar[1] = array[1]
    ar[2] = array[0]
    return(ar)

# Affiche points  coins pixels module
def print_corners(corners, name):
    shape = corners.shape
    vertices = []
    vertices = numpy.reshape(corners, (shape[0] * shape [1] * shape[2], 3))
    contour_mesh = bpy.data.meshes.new("contour_mesh")
    contour_mesh .from_pydata(vertices, [], [])
    contour_mesh .update()
    contour_objet = bpy.data.objects.new(name, contour_mesh )
    scene = bpy.context.scene
    contour_objet.active_material = mat1
    scene.objects.link(contour_objet)

    bpy.context.scene.objects.active = bpy.context.scene.objects[name]
    bpy.ops.object.mode_set(mode = 'EDIT')
    bpy.ops.mesh.remove_doubles()
    bpy.ops.object.mode_set(mode = 'OBJECT')



# Materiau classique utilisé
mat1 = bpy.data.materials.new('Mat1')
mat1.diffuse_color = (1,1,1)

mat2 = bpy.data.materials.new('Mat2')
mat2.diffuse_color = (0,1,0)


# Affiche points centres pixels
def print_centers(centers, name):
    shape = centers[0].shape
    m = numpy.zeros((3, shape[0], shape[1]))
    for i in range(3):
        if centers[i] is not None:
            m[i] = centers[i] * SCALE

    m = inter2(m)

    # Creation des vertices
    vertices = numpy.reshape(numpy.transpose(m), (shape [0] * shape[1], 3))

    # Creation du mesh à partir des vertices
    contour_mesh = bpy.data.meshes.new("contour_mesh")
    contour_mesh.from_pydata(vertices, [], [])
    contour_mesh.update()

    # Creation d'un object avec ce mesh
    contour_objet = bpy.data.objects.new(name, contour_mesh)

    # si pas de materiaux, le menu n'apparait pas ???
    contour_objet.active_material = mat2

    # ajout de l'objet à la scène pour qu'il devienne visible
    scene = bpy.context.scene
    scene.objects.link(contour_objet)


# Affiche le detecteur avec ses couleurs, pas tourner dans le bon sens pour l'instant
def print_pixel(corners, image):

    shape = corners.shape

    # Creer des vertices du detecteur
    vertices = numpy.reshape(corners * SCALE, (shape[0] * shape [1] * shape[2], 3))

    # creation de la list contenant les indices des vertices formant une face (un pixel)
    faces = [[4*i,4*i+1,4*i+2,4*i+3] for i in range(shape[0]*shape[1])]

    # creation du mesh avec ces vertices et ces faces.
    contour_mesh = bpy.data.meshes.new("contour_mesh")
    contour_mesh.from_pydata(vertices, [], faces)
    contour_mesh.update()

    # creation de l'object a partir du mesh
    contour_objet = bpy.data.objects.new('detector', contour_mesh )

    contour_objet.active_material = mat1

    # ajout de l'object dans la scene.
    scene = bpy.context.scene
    scene.objects.link(contour_objet)

    # Ajoute les couleurs
    # on cree un groupe de vertex par couleurs de la LUT
    # ici la lut est simple 0-255 (sur le rouge)
    max = int(image.max()) + 1
    MyVertices = [[] for i in range(max)]

    # On partour toute l'image et pour chaque intensité, on rajoute les index des
    # vertex de la list vertices formant une face dans le bon groupe de vertex.
    # TODO improve speed :)
    width = shape[1]

    # TODO voir comment utiliser plutot les faces pour ces couleurs.
    for i in range(shape[0]):
        i_width = i * width
        for j in range(shape[1]):
            i_width_j = 4 * (i_width + j)
            l = range(i_width_j, i_width_j + 4)
            MyVertices[int(image[i,j])].extend(l)
    bpy.context.scene.objects.active = bpy.context.scene.objects['detector']

    # Met les couleurs par groupe
    for k in range(max):

        name = "MyVertexGroup"+str(k)

         #Create a Vertex Group
        Group1 = bpy.context.object.vertex_groups.new(name)

        #Add the vertices to the vertex group
        #with Weight = 1.0 The Weight isn't
        #relevant in this case
        Group1.add(MyVertices[k], 1.0, 'ADD')

        #Select the vertex group
        bpy.ops.object.vertex_group_set_active(group= name)

        bpy.ops.object.material_slot_add()
        bpy.context.object.material_slots[bpy.context.object.material_slots.__len__() - 1].material = MakeMaterial_1(k,max)
        bpy.ops.object.mode_set(mode = 'EDIT')
        #Deselect all the vertices
        bpy.ops.mesh.select_all(action='DESELECT')
        #Select the vertices of the vertex group
        bpy.ops.object.vertex_group_select()
        #Assign the material on the selected vertices
        bpy.ops.object.material_slot_assign()
        bpy.ops.object.mode_set(mode = 'OBJECT')

    bpy.ops.object.mode_set(mode = 'EDIT')
    bpy.ops.mesh.remove_doubles()
    bpy.ops.object.mode_set(mode = 'OBJECT')


# Fonction servant a créé un materiau par couleur, dépend de l'intensité
def MakeMaterial_1(k, max):
    mat = bpy.data.materials.new("Mat2")
    mat.diffuse_shader = 'MINNAERT'
    mat.diffuse_color = (1, round(1-k/max,4), round(1-k/max,4))
    mat.darkness = 0.8
    return mat

detector = pyFAI.detectors.ImXPadS70()
corners = detector.get_pixel_corners()
centers = detector.calc_cartesian_positions()

shape = corners.shape[0:2]

image = numpy.random.randint(256, size=shape)

print_pixel(corners,image)
