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
 * Authors: Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
 *          Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl3d-gui-model.h"
#include "btBulletCollisionCommon.h"

// Trackball utilities.

namespace Trackball {
	extern "C" {
#include "trackball.h"
	}
}
namespace GLDRAW {
	extern "C" {
#include "hkl3d-gui-gl.h"
	}
}
namespace Hkl3dGui
{
	DrawingTools::DrawingTools(Hkl3D & hkl3d)
		: _hkl3d(hkl3d)
	{
		m_shapeDrawer.enableTexture(true);
	}
	DrawingTools::~DrawingTools(void)
	{
	}

	void DrawingTools::draw_line(const btVector3 & from, const btVector3 & to,
				     const btVector3 & fromColor, const btVector3 & toColor)
	{
		glBegin(GL_LINES);
		glColor3f(fromColor.getX(), fromColor.getY(), fromColor.getZ());
		glVertex3d(from.getX(), from.getY(), from.getZ());
		glColor3f(toColor.getX(), toColor.getY(), toColor.getZ());
		glVertex3d(to.getX(), to.getY(), to.getZ());
		glEnd();
	}

	void DrawingTools::draw_line(const btVector3 & from, const btVector3 & to, const btVector3 & color)
	{
		draw_line(from, to, color, color);
	}

	void DrawingTools::draw_Aabb(const btVector3 & from, const btVector3 & to, const btVector3 & color)
        {
                btVector3 halfExtents = (to-from)* 0.5f;
                btVector3 center = (to+from) *0.5f;
                int i,j;

                btVector3 edgecoord(1.f,1.f,1.f), pa, pb;
                for (i=0;i<4;i++){
                        for (j=0;j<3;j++){
                                pa = btVector3(edgecoord[0]*halfExtents[0], edgecoord[1]*halfExtents[1],                
					       edgecoord[2]*halfExtents[2]);
                                pa += center;

                                int othercoord = j%3;
                                edgecoord[othercoord] *= -1.f;
                                pb = btVector3(edgecoord[0]*halfExtents[0], edgecoord[1]*halfExtents[1],        
					       edgecoord[2]*halfExtents[2]);
                                pb += center;

                                this->draw_line(pa, pb, color);
                        }
                        edgecoord = btVector3(-1.f,-1.f,-1.f);
                        if (i<3)
                                edgecoord[i] *= -1.f;
                }
        }

	void DrawingTools::draw_AAbbBoxes(void)
	{
		int i;
		int j;

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
		glDisable(GL_LIGHTING);

		for(i=0; i<_hkl3d.configs.size(); i++)
			for(j=0; j<_hkl3d.configs[i].objects.size(); j++){
				if(!_hkl3d.configs[i].objects[j].hide){
					Hkl3DObject & object = _hkl3d.configs[i].objects[j];
					btVector3 aabbMin, aabbMax;

					object.btShape->getAabb(object.btObject->getWorldTransform(),
								aabbMin, aabbMax);
					this->draw_Aabb(aabbMin, aabbMax, btVector3(1,0,0));
				}
			}

		glFlush();
	}

	void DrawingTools::draw_collisions(void)
	{
		int i;
		int k;
		int numManifolds;
		bool isColliding;
		btScalar m[16];
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		
		// get the world bounding box from bullet
		_hkl3d.get_bounding_boxes(worldBoundsMin, worldBoundsMax);
		///one way to draw all the contact points is iterating over contact manifolds / points:
		numManifolds = _hkl3d._btDispatcher->getNumManifolds();
		for (i=0; i<numManifolds; i++){
			btPersistentManifold *contactManifold;
			btCollisionObject *obA;
			btCollisionObject *obB;
			int numContacts;
			int j;

			contactManifold = _hkl3d._btDispatcher->getManifoldByIndexInternal(i);
			obA = static_cast<btCollisionObject*>(contactManifold->getBody0());
			obB = static_cast<btCollisionObject*>(contactManifold->getBody1());
			// now draw the manifolds / points			  
			numContacts = contactManifold->getNumContacts();
			for (j=0; j<numContacts; j++){
				btManifoldPoint & pt = contactManifold->getContactPoint(j);
				btScalar dist= pt.getDistance();
				glDisable(GL_DEPTH_TEST);
				glBegin(GL_LINES);
				glColor4f(0, 0, 0, 1);
				btVector3 ptA = pt.getPositionWorldOnA();
				btVector3 ptB = pt.getPositionWorldOnB();
				glVertex3d(ptA.x(),ptA.y(),ptA.z());
				glVertex3d(ptB.x(),ptB.y(),ptB.z());
				glEnd();
				glColor4f(1, 0, 0, 1);
				glPushMatrix(); 
				glTranslatef (ptB.x(),ptB.y(),ptB.z());
				glScaled(0.05,0.05,0.05);
				m_shapeDrawer.drawSphere(1, 10, 10);
				glPopMatrix();
				glColor4f(1, 1, 0, 1);
				glPushMatrix(); 
				glTranslatef (ptA.x(),ptA.y(),ptA.z());
				glScaled(0.05,0.05,0.05);
				m_shapeDrawer.drawSphere(1, 10, 10);
				glPopMatrix();
				glEnable(GL_DEPTH_TEST);
			}
		}
		glFlush();
	}

	void DrawingTools::draw_bullet(void)
	{
		int i;
		int j;
		btScalar m[16];
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		btVector3 aabbMin,aabbMax;

		GL_ShapeDrawer::drawCoordSystem(); 

		/* get the bounding box from bullet */
		_hkl3d.get_bounding_boxes(worldBoundsMin, worldBoundsMax);

		/* draw all visible objects */
		for(i=0; i<_hkl3d.configs.size(); i++){
			for(j=0; j<_hkl3d.configs[i].objects.size(); j++){
				Hkl3DObject *object;

				object = &_hkl3d.configs[i].objects[j];
				if(!object->hide){
					btCollisionObject *btObject;

					btObject = object->btObject;
					btObject->getWorldTransform().getOpenGLMatrix( m );
					m_shapeDrawer.drawOpenGL(m,
								 btObject->getCollisionShape(),
								 *object->color,
								 0, /* debug mode */
								 worldBoundsMin,
								 worldBoundsMax);
				}
			}
		}
		glFlush();
	}

	void DrawingTools::draw_g3dmodel(void)
	{	 
		int i;
		int j;

		/* set the alpha canal to 0.5 if there is a collision */
		for(i=0; i<_hkl3d.configs.size(); i++)
			for(j=0; j<_hkl3d.configs[i].objects.size(); j++){
				GSList *faces;
				G3DFace *face;
				G3DMaterial *material;
				double alpha;

				if(_hkl3d.configs[i].objects[j].is_colliding)
					alpha = 0.5;
				else
					alpha = 1;

				faces = _hkl3d.configs[i].objects[j].g3dObject->faces;
				while(faces){
					face = (G3DFace *)(faces->data);
					face->material->a = alpha;
					faces = g_slist_next(faces);
				}
			}

		/* draw the G3DObjects */
		GLDRAW::G3DGLRenderOptions *options =  g_new0(GLDRAW::G3DGLRenderOptions, 1);
		options->glflags = G3D_FLAG_GL_SPECULAR
			| G3D_FLAG_GL_SHININESS
			| G3D_FLAG_GL_TEXTURES
			| G3D_FLAG_GL_COLORS;
		options->updated = true;
		options->initialized = false;
		GL_ShapeDrawer::drawCoordSystem();
		GLDRAW::gl_draw(options, _hkl3d.model);
		glFlush();
	}

	ModelDraw::ModelDraw(Hkl3D & hkl3d,
			     bool enableBulletDraw, bool enableWireframe,bool enableAAbbBoxDraw)
		: _hkl3d(hkl3d),
		  model(new DrawingTools(hkl3d)),
		  bullet(enableBulletDraw),
		  wireframe(enableWireframe),
		  aabb(enableAAbbBoxDraw)
	{
		this->reset_anim();
	}

	ModelDraw::~ModelDraw(void)
	{
		delete this->model;
	}

	void ModelDraw::init_gl(DrawingTools* model)
	{
		glNewList(MODEL, GL_COMPILE);
		this->model->draw_g3dmodel();
		glEndList();
		glNewList(BULLETDRAW, GL_COMPILE);
		this->model->draw_bullet();
		glEndList();
		glNewList(COLLISION, GL_COMPILE);
		this->model->draw_collisions();
		glEndList();
		glNewList(AABBBOX, GL_COMPILE);
		this->model->draw_AAbbBoxes();
		glEndList();
	}

	void ModelDraw::draw(void)
	{
		float m[4][4];

		// first update the hkl3d internals.
		_hkl3d.update_objects_visibility();

		// Init GL context.
		init_gl(model);

		// Draw  model.
		glPushMatrix();
		glTranslatef(m_Pos[0], m_Pos[1], m_Pos[2]);

		Trackball::build_rotmatrix(m, m_Quat);
		glMultMatrixf(&m[0][0]);
		glRotatef(0.0, 0.0, 0.0, 1.0);
		
		// WireFrame
		if(this->wireframe){
			glPolygonMode(GL_FRONT, GL_LINE);
			glPolygonMode(GL_BACK, GL_LINE);
		}else{
			glPolygonMode(GL_FRONT, GL_FILL);
			glPolygonMode(GL_BACK, GL_FILL);
		}
		// AABB Box
		if(this->aabb)
			glCallList(AABBBOX);

		// Bullet and G3DModel
		if(this->bullet)
			glCallList(BULLETDRAW);
		else
			glCallList(MODEL);

		glCallList(COLLISION);
		
		glPopMatrix();
	}

	void ModelDraw::reset_anim(void)
	{
		m_Pos[0] = 0.0;
		m_Pos[1] = 0.0;
		m_Pos[2] = 0.0;

		m_Quat[0] = 0.0;
		m_Quat[1] = 0.0;
		m_Quat[2] = 0.0;
		m_Quat[3] = 1.0;
	}

} // namespace Hkl3dGui
