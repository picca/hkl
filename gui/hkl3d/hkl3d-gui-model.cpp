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

#include <iostream>

#include "hkl3d-gui-model.h"

#include "btBulletDynamicsCommon.h"
#include "GLDebugDrawer.h"


GLDebugDrawer debugDrawer;

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
	}
	DrawingTools::~DrawingTools(void)
	{
	}
	
	void DrawingTools::drawSphere(void)
	{
#ifndef M_PI 
# define M_PI 3.14159265358979323846
#endif
#define PAS (M_PI/24)
#define FIN (2*M_PI)
		for (double a = 0; a < M_PI; a+=PAS)
		{
			double b1;
			double a1 = a + PAS;
			glBegin(GL_QUADS);
			glVertex3d(0, cos(a), 0);
			glVertex3d(0, cos(a1), 0);
			glVertex3d(0, cos(a1), 0);
			glVertex3d(0, cos(a), 0);
			for (double b = 0; b < FIN; b+=PAS) {
				b1 = b + PAS;
				if (b1 > FIN)
					b1 = FIN;
				glVertex3d(sin(a)*cos(b), cos(a), sin(a)*sin(b));
				glVertex3d(sin(a1)*cos(b), cos(a1), sin(a1)*sin(b));
				glVertex3d(sin(a1)*cos(b1), cos(a1), sin(a1)*sin(b1));
				glVertex3d(sin(a)*cos(b1), cos(a), sin(a)*sin(b1));
			}
			glFlush();
			glEnd();
		}
	}

	void DrawingTools::drawAAbbBox(void)
	{	int i;
		int j;
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		btVector3 aabbMin,aabbMax;

		_hkl3d._btCollisionWorld->debugDrawWorld();
		_hkl3d._btCollisionWorld->getDispatchInfo().m_debugDraw = &debugDrawer;
		_hkl3d._btCollisionWorld->setDebugDrawer (&debugDrawer);
		_hkl3d._btCollisionWorld->getBroadphase()->getBroadphaseAabb(worldBoundsMin,
									     worldBoundsMax);
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
		glDisable(GL_LIGHTING);
		for(i=0; i<_hkl3d._hkl3dConfigs.size(); i++){
			for(j=0; j<_hkl3d._hkl3dConfigs[i].objects.size(); j++){
				if(!_hkl3d._hkl3dConfigs[i].objects[j].hide){
					btRigidBody *rigidBody;

					rigidBody = static_cast<btRigidBody*>(_hkl3d._hkl3dConfigs[i].objects[j].collisionObject);
					rigidBody->getAabb(aabbMin,aabbMax);
					_hkl3d._btCollisionWorld->getDebugDrawer()->drawAabb(aabbMin,aabbMax,btVector3(1,0,0));
				}
			}
		}
		glFlush();
	}

	void DrawingTools::model_draw_collision(void)
	{
		int i;
		int k;
		int numManifolds;
		bool isColliding;
		btScalar m[16];
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		
		// get the world bounding box from bullet
		_hkl3d._btCollisionWorld->getBroadphase()->getBroadphaseAabb(worldBoundsMin,
									     worldBoundsMax);
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
				this->drawSphere();
				glPopMatrix();
				glColor4f(1, 1, 0, 1);
				glPushMatrix(); 
				glTranslatef (ptA.x(),ptA.y(),ptA.z());
				glScaled(0.05,0.05,0.05);
				this->drawSphere();
				glPopMatrix();
				glEnable(GL_DEPTH_TEST);
			}
		}
		glFlush();
	}

	void DrawingTools::modelDrawBullet(void)
	{
		int i;
		int j;
		btScalar m[16];
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		btVector3 aabbMin,aabbMax;

		GL_ShapeDrawer::drawCoordSystem(); 
		_hkl3d._btCollisionWorld->getBroadphase()->getBroadphaseAabb(worldBoundsMin,
									     worldBoundsMax);
		for(i=0; i<_hkl3d._hkl3dConfigs.size(); i++){
			for(j=0; j<_hkl3d._hkl3dConfigs[i].objects.size(); j++){
				if(!_hkl3d._hkl3dConfigs[i].objects[j].hide){
					btCollisionObject *object;

					object = _hkl3d._hkl3dConfigs[i].objects[j].collisionObject;
					object->getWorldTransform().getOpenGLMatrix( m );
					m_shapeDrawer->drawOpenGL(m,
								  object->getCollisionShape(),
								  *_hkl3d._hkl3dConfigs[i].objects[j].color,
								  this->getDebugMode(),
								  worldBoundsMin,
								  worldBoundsMax);
					if(!_hkl3d._hkl3dConfigs[i].objects[j].AddedInWorldCollision){
						_hkl3d._btCollisionWorld->addCollisionObject(_hkl3d._hkl3dConfigs[i].objects[j].collisionObject);
						_hkl3d._hkl3dConfigs[i].objects[j].AddedInWorldCollision = true;
					}
				}else{
					/* update the G3DObject hide model value from the Hkl3DConfig */ 
					_hkl3d._hkl3dConfigs[i].objects[j].gObject->hide = true;

					/* remove this object from the Hkl3D collision world */
					_hkl3d._btCollisionWorld->removeCollisionObject(_hkl3d._hkl3dConfigs[i].objects[j].collisionObject);
					_hkl3d._hkl3dConfigs[i].objects[j].AddedInWorldCollision = false;
				}
			}
		}
		glFlush();
	}

	void DrawingTools::model_draw(void)
	{	 
		int i;
		int j;

		/* set the alpha canal to 0.5 if there is a collision */
		for(i=0; i<_hkl3d._hkl3dConfigs.size(); i++)
			for(j=0; j<_hkl3d._hkl3dConfigs[i].objects.size(); j++){
				GSList *faces;
				G3DFace *face;
				G3DMaterial *material;
				double alpha;

				if(_hkl3d._hkl3dConfigs[i].objects[j].is_colliding)
					alpha = 0.5;
				else
					alpha = 1;

				faces = _hkl3d._hkl3dConfigs[i].objects[j].gObject->faces;
				while(faces){
					face = (G3DFace *)(faces->data);
					face->material->a = alpha;
					faces = g_slist_next(faces);
				}
			}

		for(i=0; i<_hkl3d._hkl3dConfigs.size(); i++)
			for(j=0; j<_hkl3d._hkl3dConfigs[i].objects.size(); j++)
				if(!_hkl3d._hkl3dConfigs[i].objects[j].hide){
					_hkl3d._hkl3dConfigs[i].objects[j].gObject->hide = false;
					if(!_hkl3d._hkl3dConfigs[i].objects[j].AddedInWorldCollision){
						_hkl3d._btCollisionWorld->addCollisionObject(_hkl3d._hkl3dConfigs[i].objects[j].collisionObject);
						_hkl3d._hkl3dConfigs[i].objects[j].AddedInWorldCollision = true;
					}
				}else{
					_hkl3d._hkl3dConfigs[i].objects[j].gObject->hide = true;
					_hkl3d._btCollisionWorld->removeCollisionObject(_hkl3d._hkl3dConfigs[i].objects[j].collisionObject);
					_hkl3d._hkl3dConfigs[i].objects[j].AddedInWorldCollision = false;
				}

		GLDRAW::G3DGLRenderOptions *options =  g_new0(GLDRAW::G3DGLRenderOptions, 1);
		options->glflags = G3D_FLAG_GL_SPECULAR
			| G3D_FLAG_GL_SHININESS
			| G3D_FLAG_GL_TEXTURES
			| G3D_FLAG_GL_COLORS;
		options->updated=true;
		options->initialized=false;
		GL_ShapeDrawer::drawCoordSystem();
		GLDRAW::gl_draw(options, _hkl3d._model);
		glFlush();
	}

	// dummy methods
	void DrawingTools::initPhysics(void){}

	void DrawingTools::clientMoveAndDisplay(void){}

	void DrawingTools::displayCallback(void){}

	ModelDraw::ModelDraw(Hkl3D & hkl3d,
		     bool enableBulletDraw, bool enableWireframe,bool enableAAbbBoxDraw)
		: _hkl3d(hkl3d),m_EnableBulletDraw(enableBulletDraw), m_EnableWireframe(enableWireframe),
		  m_EnableAAbbBoxDraw(enableAAbbBoxDraw) ,m_Mode(0)
	{
		this->reset_anim();
	}

	ModelDraw::~ModelDraw(void)
	{
	}

	void ModelDraw::init_gl(DrawingTools* model)
	{
		this->model = new DrawingTools(_hkl3d);
		glNewList(MODEL, GL_COMPILE);
		this->model->model_draw();
		glEndList();
		glNewList(BULLETDRAW, GL_COMPILE);
		this->model->modelDrawBullet();
		glEndList();
		glNewList(COLLISION, GL_COMPILE);
		this->model->model_draw_collision();
		glEndList();
		glNewList(AABBBOX, GL_COMPILE);
		this->model->drawAAbbBox();
		glEndList();
	}

	void ModelDraw::draw(void)
	{
		// Init GL context.
		static bool initialized = false;
		if (!initialized) {
			init_gl(model);
			initialized = true;
		}else
			init_gl(model);

		// Draw  model.
		glPushMatrix();
		glTranslatef(m_Pos[0], m_Pos[1], m_Pos[2]);

		float m[4][4];
		Trackball::build_rotmatrix(m,m_Quat);
		glMultMatrixf(&m[0][0]);

		glRotatef(0.0, 0.0, 0.0, 1.0);
		
		// WireFrame
		if(m_EnableWireframe){
			glPolygonMode(GL_FRONT, GL_LINE);
			glPolygonMode(GL_BACK, GL_LINE);
		}
		else{
			glPolygonMode(GL_FRONT, GL_FILL);
			glPolygonMode(GL_BACK, GL_FILL);
		}
		// AABB Box
		if(m_EnableAAbbBoxDraw)
			glCallList(AABBBOX);

		if(!m_EnableBulletDraw){
			glCallList(COLLISION);
			glCallList(MODEL);
			
		}
		else{
			glCallList(COLLISION);
			glCallList(BULLETDRAW);
		}
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

		m_Mode = 0;
	}

} // namespace Hkl3dGui
