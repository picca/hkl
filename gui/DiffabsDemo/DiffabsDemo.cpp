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

/* This file is part of the hkl3d library.
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
#include "btBulletDynamicsCommon.h"
#include "GLDebugDrawer.h"

#include "LinearMath/btIDebugDraw.h"
#include "DiffabsDemo.h"

GLDebugDrawer debugDrawer;

float mu = 45.f* HKL_DEGTORAD;
float komega = 0.f * HKL_DEGTORAD;
float kappa = 0.f * HKL_DEGTORAD;
float kphi = 0.f * HKL_DEGTORAD;
float ggamma = 0.f * HKL_DEGTORAD;
float delta = 0.f * HKL_DEGTORAD;

// comment the next line for no animation
//#define ANIMATE

#define MODEL_FILE "../../data/diffabs.dae"

DiffabsDemo::DiffabsDemo(void)
{
	const HklGeometryConfig *config;

	config = hkl_geometry_factory_get_config_from_type(HKL_GEOMETRY_TYPE_KAPPA6C);
	_geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);
	hkl_geometry_set_values_v(_geometry, 6, mu, komega, kappa, kphi, ggamma, delta);
	hkl_geometry_fprintf(stdout, _geometry);

	_hkl3d = new Hkl3D(MODEL_FILE, _geometry);
}
static void drawSphere(void)
	 {
		#ifndef M_PI 
		# define M_PI 3.14159265358979323846
		#endif
		#define PAS (M_PI/24)
		#define DEBUT 0
		#define FIN (2*M_PI - DEBUT)
		for (double a = 0; a < M_PI; a+=PAS)
		{
			double b1;
			double a1 = a + PAS;
			glBegin(GL_QUADS);

			glVertex3d(sin(a)*cos(DEBUT), cos(a), sin(a)*sin(DEBUT));

			glVertex3d(sin(a1)*cos(DEBUT), cos(a1), sin(a1)*sin(DEBUT));

			glVertex3d(0, cos(a1), 0);
			glVertex3d(0, cos(a), 0);
			for (double b = DEBUT; b < FIN; b+=PAS) {
				b1 = b + PAS;
				if (b1 > FIN)
					b1 = FIN;
				glVertex3d(sin(a)*cos(b), cos(a), sin(a)*sin(b));

				glVertex3d(sin(a1)*cos(b), cos(a1), sin(a1)*sin(b));

				glVertex3d(sin(a1)*cos(b1), cos(a1), sin(a1)*sin(b1));

				glVertex3d(sin(a)*cos(b1), cos(a), sin(a)*sin(b1));
			}
			
			glEnd();
		}
	}

DiffabsDemo::~DiffabsDemo(void)
{
	if(_hkl3d)
		delete _hkl3d;

	if(_geometry)
		hkl_geometry_free(_geometry);
}

void DiffabsDemo::clientMoveAndDisplay(void)
{
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
	this->displayCallback();
}

void DiffabsDemo::displayCallback(void)
{
	int i;
	int len;
	int numManifolds;
	btScalar m[16];
	btVector3 worldBoundsMin;
	btVector3 worldBoundsMax;
	btVector3 aabbMin,aabbMax;
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); 
	glDisable(GL_LIGHTING);
	GL_ShapeDrawer::drawCoordSystem(); 
	_hkl3d->_btCollisionWorld->debugDrawWorld();
	
	_hkl3d->_btCollisionWorld->getDispatchInfo().m_debugDraw = &debugDrawer;
	_hkl3d->_btCollisionWorld->setDebugDrawer (&debugDrawer);
#ifdef ANIMATE
	// create an animation to see collisions
	mu += 5 * HKL_DEGTORAD;
	komega += 5 * HKL_DEGTORAD;
	kappa -= 5 * HKL_DEGTORAD;
	kphi += 5 * HKL_DEGTORAD;
	ggamma -= 5 * HKL_DEGTORAD;
	delta += 5 * HKL_DEGTORAD;
	hkl_geometry_set_values_v(_geometry, 6, mu, komega, kappa, kphi, ggamma, delta);
	hkl_geometry_fprintf(stdout, _geometry);
	_hkl3d->is_colliding();
#endif

	// draw the diffractometer
	// get the world bounding box from bullet
	_hkl3d->_btCollisionWorld->getBroadphase()->getBroadphaseAabb(worldBoundsMin,
								      worldBoundsMax);
	len = _hkl3d->_hkl3dObjects.size();
	for(i=0; i<len; ++i){
		btCollisionObject *object;
		object = _hkl3d->_hkl3dObjects[i].collisionObject;
		btRigidBody *rigidBody;
		rigidBody=static_cast<btRigidBody*>(_hkl3d->_hkl3dObjects[i].collisionObject);
		rigidBody->getAabb(aabbMin,aabbMax);
		object->getWorldTransform().getOpenGLMatrix( m );
		m_shapeDrawer->drawOpenGL(m,
					  object->getCollisionShape(),
					  *_hkl3d->_hkl3dObjects[i].color,
					  this->getDebugMode(),
					  worldBoundsMin,
					  worldBoundsMax);
		_hkl3d->_btCollisionWorld->getDebugDrawer()->drawAabb(aabbMin,aabbMax,btVector3(1,0,0));
	}
	///one way to draw all the contact points is iterating over contact manifolds / points:
	numManifolds = _hkl3d->_btDispatcher->getNumManifolds();
	for (i=0; i<numManifolds; i++){
		btPersistentManifold *contactManifold;
		btCollisionObject *obA;
		btCollisionObject *obB;
		int numContacts;
		int j;

		contactManifold = _hkl3d->_btDispatcher->getManifoldByIndexInternal(i);
		obA = static_cast<btCollisionObject*>(contactManifold->getBody0());
		obB = static_cast<btCollisionObject*>(contactManifold->getBody1());
		// now draw the manifolds / points
		numContacts = contactManifold->getNumContacts();
		for (j=0; j<numContacts; j++){
			btManifoldPoint & pt = contactManifold->getContactPoint(j);

			glBegin(GL_LINES);
			glColor3f(1, 0, 1);
			
			btVector3 ptA = pt.getPositionWorldOnA();
			btVector3 ptB = pt.getPositionWorldOnB();

			glVertex3d(ptA.x(),ptA.y(),ptA.z());
			glVertex3d(ptB.x(),ptB.y(),ptB.z());
			glEnd();
			glColor4f(1, 0, 0, 1);
				glPushMatrix(); 
				glTranslatef (ptB.x(),ptB.y(),ptB.z());
				glScaled(0.2,0.2,0.2);
				drawSphere();
				glPopMatrix();
				glColor4f(1, 1, 0, 1);
				glPushMatrix(); 
				glTranslatef (ptA.x(),ptA.y(),ptA.z());
				glScaled(0.2,0.2,0.2);
				drawSphere();
				glPopMatrix();
		}
	}
	glFlush();
	glutSwapBuffers();
}

void DiffabsDemo::initPhysics(void)
{
	
     
}
