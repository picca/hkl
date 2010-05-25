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
namespace Logo
{
	LogoModel::LogoModel(Hkl3D & hkl3d)
		: _hkl3d(hkl3d)
	{
	}
	LogoModel::~LogoModel(void)
	{
	}
	
	void LogoModel::drawSphere(void)
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

	void LogoModel::drawAAbbBox(void)
	{	int i;
		int len;
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		btVector3 aabbMin,aabbMax;
		_hkl3d._btCollisionWorld->debugDrawWorld();
		_hkl3d._btCollisionWorld->getDispatchInfo().m_debugDraw = &debugDrawer;
		_hkl3d._btCollisionWorld->setDebugDrawer (&debugDrawer);
		_hkl3d._btCollisionWorld->getBroadphase()->getBroadphaseAabb(worldBoundsMin,
									     worldBoundsMax);
		len = _hkl3d._hkl3dObjects.size();
		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
		glDisable(GL_LIGHTING);
		for(i=0; i<len; ++i){
			btRigidBody *rigidBody;
			rigidBody=static_cast<btRigidBody*>(_hkl3d._hkl3dObjects[i].collisionObject);
			rigidBody->getAabb(aabbMin,aabbMax);
			_hkl3d._btCollisionWorld->getDebugDrawer()->drawAabb(aabbMin,aabbMax,btVector3(1,0,0));
		}
		glFlush();
	}

	void LogoModel::model_draw_collision(void)
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
				glScaled(0.2,0.2,0.2);
				this->drawSphere();
				glPopMatrix();
				glColor4f(1, 1, 0, 1);
				glPushMatrix(); 
				glTranslatef (ptA.x(),ptA.y(),ptA.z());
				glScaled(0.2,0.2,0.2);
				this->drawSphere();
				glPopMatrix();
				glEnable(GL_DEPTH_TEST);
			}
		}
		glFlush();
	}

	void LogoModel::modelDrawBullet(void)
	{
		int i;
		int len;
		btScalar m[16];
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;
		btVector3 aabbMin,aabbMax;

		GL_ShapeDrawer::drawCoordSystem(); 
		_hkl3d._btCollisionWorld->getBroadphase()->getBroadphaseAabb(worldBoundsMin,
									     worldBoundsMax);
		len = _hkl3d._hkl3dObjects.size();

		for(i=0; i<len; ++i){
			btCollisionObject *object;
			object = _hkl3d._hkl3dObjects[i].collisionObject;
			object->getWorldTransform().getOpenGLMatrix( m );
			m_shapeDrawer->drawOpenGL(m,
						  object->getCollisionShape(),
						  *_hkl3d._hkl3dObjects[i].color/*btVector3(0,1,2)*/,
						  this->getDebugMode(),
						  worldBoundsMin,
						  worldBoundsMax);
		}
		glFlush();
	}

	void LogoModel::model_draw(void)
	{	
		int k; 
		for(k=0;k<_hkl3d._hkl3dObjects.size();k++){
			if(_hkl3d._hkl3dObjects[k].is_colliding==true){
				GSList *faces;
				faces = _hkl3d._hkl3dObjects[k].gObject->faces;
				while(faces){
					G3DFace *face;
					G3DMaterial *material;
					face = (G3DFace *)(faces->data);
					face->material->a =0.5;
					faces = g_slist_next(faces);
				}	
			}else{
				GSList *faces;
				faces = _hkl3d._hkl3dObjects[k].gObject->faces;
				while(faces){
					G3DFace *face;
					G3DMaterial *material;
					face = (G3DFace *)(faces->data);
					face->material->a =1;
					faces = g_slist_next(faces);
				}
			}
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
	void LogoModel::initPhysics(void){}

	void LogoModel::clientMoveAndDisplay(void){}

	void LogoModel::displayCallback(void){}

	Model::Model(Hkl3D & hkl3d,
		     bool enableBulletDraw, bool enableWireframe,bool enableAAbbBoxDraw )
		: _hkl3d(hkl3d),m_EnableBulletDraw(enableBulletDraw), m_EnableWireframe(enableWireframe),
		  m_EnableAAbbBoxDraw(enableAAbbBoxDraw) ,m_Mode(0)
	{
		this->reset_anim();
	}

	Model::~Model(void)
	{
	}

	void Model::init_gl(LogoModel* logoM)
	{
		this->logoM = new LogoModel(_hkl3d);
		glNewList(MODEL, GL_COMPILE);
		this->logoM->model_draw();
		glEndList();
		glNewList(BULLETDRAW, GL_COMPILE);
		this->logoM->modelDrawBullet();
		glEndList();
		glNewList(COLLISION, GL_COMPILE);
		this->logoM->model_draw_collision();
		glEndList();
		glNewList(AABBBOX, GL_COMPILE);
		this->logoM->drawAAbbBox();
		glEndList();
	}

	void Model::draw(void)
	{
		// Init GL context.
		static bool initialized = false;
		if (!initialized) {
			init_gl(logoM);
			initialized = true;
		}else
			init_gl(logoM);

		// Draw logo model.
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

	void Model::reset_anim(void)
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

} // namespace Logo
