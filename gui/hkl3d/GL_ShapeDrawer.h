/*
  Bullet Continuous Collision Detection and Physics Library
  Copyright (c) 2003-2006 Erwin Coumans  http://continuousphysics.com/Bullet/

  This software is provided 'as-is', without any express or implied warranty.
  In no event will the authors be held liable for any damages arising from the use of this software.
  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it freely,
  subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not claim that you wrote the original software. If you use this software in a product, an acknowledgment in the product documentation would be appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/
/*
 * This file is part of the hkl3d library.
 * inspired from logo-model.c of the GtkGLExt logo models.
 * written by Naofumi Yasufuku  <naofumi@users.sourceforge.net>
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2010      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#ifndef GL_SHAPE_DRAWER_H
#define GL_SHAPE_DRAWER_H

#include <GL/gl.h>
#include <GL/glu.h>

/* forward declaration */
class btCollisionShape;
class btShapeHull;

#include "LinearMath/btAlignedObjectArray.h"
#include "LinearMath/btVector3.h"
#include "BulletCollision/CollisionShapes/btShapeHull.h"

#if defined(BT_USE_DOUBLE_PRECISION)
#define btglLoadMatrix glLoadMatrixd
#define btglMultMatrix glMultMatrixd
#define btglColor3 glColor3d
#define btglVertex3 glVertex3d
#else
#define btglLoadMatrix glLoadMatrixf
#define btglMultMatrix glMultMatrixf
#define btglColor3 glColor3f
#define btglVertex3 glVertex3d
#endif

/// OpenGL shape drawing
class GL_ShapeDrawer
{
protected:
	struct ShapeCache
	{
		struct Edge
		{
			btVector3 n[2];
			int v[2];
		};
	ShapeCache(btConvexShape* s) : m_shapehull(s) {}
		btShapeHull m_shapehull;
		btAlignedObjectArray<Edge> m_edges;
	};
	//clean-up memory of dynamically created shape hulls
	btAlignedObjectArray<ShapeCache*> m_shapecaches;
	unsigned int m_texturehandle;
	bool m_textureenabled;
	bool m_textureinitialized;

	ShapeCache *cache(btConvexShape*);

public:
	GL_ShapeDrawer(void);
	virtual ~GL_ShapeDrawer(void);

	///drawOpenGL might allocate temporary memoty, stores pointer in shape userpointer
	virtual void drawOpenGL(btScalar *m, const btCollisionShape *shape,
				const btVector3 & color, int debugMode,
				const btVector3 & worldBoundsMin, const btVector3 & worldBoundsMax);
	virtual void drawShadow(btScalar *m, const btVector3 & extrusion, const btCollisionShape *shape,
				const btVector3 & worldBoundsMin, const btVector3 & worldBoundsMax);

	bool enableTexture(bool enable) {bool p=m_textureenabled;m_textureenabled=enable; return(p); }
	bool hasTextureEnabled(void) const {return m_textureenabled;}

	static void drawCylinder(float radius, float halfHeight, int upAxis);
	void drawSphere(btScalar r, int lats, int longs);
	static void drawCoordSystem(void);
};

void OGL_displaylist_register_shape(btCollisionShape *shape);
void OGL_displaylist_clean(void);

#endif //GL_SHAPE_DRAWER_H
