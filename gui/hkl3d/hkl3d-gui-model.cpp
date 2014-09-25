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
 * Copyright (C) 2010-2014 Synchrotron SOLEIL
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
	DrawingTools::DrawingTools(Hkl3D *hkl3d)
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
		glDisable(GL_LIGHTING);
		for(i=0; i<_hkl3d->config->len; i++)
			for(j=0; j<_hkl3d->config->models[i]->len; j++){
				if(!_hkl3d->config->models[i]->objects[j]->hide){
					Hkl3DObject *object = _hkl3d->config->models[i]->objects[j];
					btVector3 aabbMin, aabbMax;

					object->btShape->getAabb(object->btObject->getWorldTransform(),
								 aabbMin, aabbMax);
					this->draw_Aabb(aabbMin, aabbMax, btVector3(1,0,0));
				}
			}

		glFlush();
	}

	void DrawingTools::draw_collisions(void)
	{
		int i;
		int numManifolds;
		bool isColliding;
		btScalar m[16];
		btVector3 worldBoundsMin;
		btVector3 worldBoundsMax;

		glDisable(GL_LIGHTING);
		// get the world bounding box from bullet
		hkl3d_get_bounding_boxes(_hkl3d, &worldBoundsMin, &worldBoundsMax);
		///one way to draw all the contact points is iterating over contact manifolds / points:
		numManifolds = hkl3d_get_nb_manifolds(_hkl3d);
		for (i=0; i<numManifolds; i++){
			int numContacts;
			int j;

			// now draw the manifolds / points
			numContacts = hkl3d_get_nb_contacts(_hkl3d, i);
			for (j=0; j<numContacts; j++){
				double xa, ya, za;
				double xb, yb, zb;

				hkl3d_get_collision_coordinates(_hkl3d, i, j,
								&xa, &ya, &za, &xb, &yb, &zb);

				glDisable(GL_DEPTH_TEST);
				glBegin(GL_LINES);
				glColor4f(0, 0, 0, 1);
				glVertex3d(xa, ya, za);
				glVertex3d(xb, yb, zb);
				glEnd();
				glColor4f(1, 0, 0, 1);
				glPushMatrix();
				glTranslatef (xb, yb, zb);
				glScaled(0.05,0.05,0.05);
				m_shapeDrawer.drawSphere(1, 10, 10);
				glPopMatrix();
				glColor4f(1, 1, 0, 1);
				glPushMatrix();
				glTranslatef (xa, ya, za);
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
		hkl3d_get_bounding_boxes(_hkl3d, &worldBoundsMin, &worldBoundsMax);

		/* draw all visible objects */
		for(i=0; i<_hkl3d->config->len; i++){
			for(j=0; j<_hkl3d->config->models[i]->len; j++){
				Hkl3DObject *object;

				object = _hkl3d->config->models[i]->objects[j];
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
		for(i=0; i<_hkl3d->config->len; i++)
			for(j=0; j<_hkl3d->config->models[i]->len; j++){
				GSList *faces;
				G3DFace *face;
				G3DMaterial *material;
				double alpha;

				if(_hkl3d->config->models[i]->objects[j]->is_colliding)
					alpha = 0.5;
				else
					alpha = 1;

				faces = _hkl3d->config->models[i]->objects[j]->g3d->faces;
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
		GLDRAW::gl_draw(options, _hkl3d->model);
		glFlush();
	}

	static void draw_g3dObject(G3DObject *object)
	{
		GSList *faces;
		G3DFace *face;
		float *vertex;

		faces = object->faces;
		vertex = object->vertex_data;

		glPushMatrix();

		/* apply the transformation of the object */
		if(object->transformation)
			glMultMatrixf(object->transformation->matrix);

		/* draw all faces with the current stencil */
		while(faces){
			G3DFace * face;

			face = (G3DFace*)faces->data;
			glBegin(GL_TRIANGLES);
			glVertex3d(vertex[3*(face->vertex_indices[0])],
				   vertex[3*(face->vertex_indices[0])+1],
				   vertex[3*(face->vertex_indices[0])+2]);
			glVertex3d(vertex[3*(face->vertex_indices[1])],
				   vertex[3*(face->vertex_indices[1])+1],
				   vertex[3*(face->vertex_indices[1])+2]);
			glVertex3d(vertex[3*(face->vertex_indices[2])],
				   vertex[3*(face->vertex_indices[2])+1],
				   vertex[3*(face->vertex_indices[2])+2]);
			glEnd();
			faces = g_slist_next(faces);
		}

		glPopMatrix();
	}

	void DrawingTools::draw_selected(void)
	{
		int i;
		int j;

		for(i=0; i<_hkl3d->config->len; i++)
			for(j=0; j<_hkl3d->config->models[i]->len; j++){
				if(_hkl3d->config->models[i]->objects[j]->selected
				   && !_hkl3d->config->models[i]->objects[j]->hide){
					// Push the GL attribute bits so that we don't wreck any settings
					glDisable(GL_LIGHTING);
					glPushAttrib( GL_ALL_ATTRIB_BITS );

					// Enable polygon offsets, and offset filled polygons forward by 2.5
					glEnable( GL_POLYGON_OFFSET_FILL );
					glPolygonOffset( -2.5, -2.5);

					// Set the render mode to be line rendering with a thick line width
					glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
					glLineWidth( 3.f );
					// Set the colour to be pink
					glColor3f( 1.f, .0f, 1.f );
					// Render the object
					draw_g3dObject(_hkl3d->config->models[i]->objects[j]->g3d);
					// Set the polygon mode to be filled triangles
					glLineWidth( 1.f );
					glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
					// Set the colour to the background
					glCullFace(GL_FRONT);
					glColor3f( 0.0f, 0.0f, 0.0f );
					// Render the object
					draw_g3dObject(_hkl3d->config->models[i]->objects[j]->g3d);

					// Pop the state changes off the attribute
					// to set things back how they were
					glPopAttrib();
				}
			}
		glFlush();
	}

	ModelDraw::ModelDraw(Hkl3D *hkl3d,
			     bool enableBulletDraw, bool enableWireframe,
			     bool enableAAbbBoxDraw, bool enableOrtho)
		: _hkl3d(hkl3d),
		  model(new DrawingTools(hkl3d)),
		  bullet(enableBulletDraw),
		  wireframe(enableWireframe),
		  aabb(enableAAbbBoxDraw),
		  ortho(enableOrtho)
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
		glNewList(BULLET, GL_COMPILE);
		this->model->draw_bullet();
		glEndList();
		glNewList(COLLISION, GL_COMPILE);
		this->model->draw_collisions();
		glEndList();
		glNewList(AABBBOX, GL_COMPILE);
		this->model->draw_AAbbBoxes();
		glEndList();
		glNewList(HIGHLIGHT, GL_COMPILE);
		this->model->draw_selected();
		glEndList();
	}

	void ModelDraw::draw(void)
	{
		float m[4][4];

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
			glCallList(BULLET);
		else
			glCallList(MODEL);
		glCallList(COLLISION);
		glCallList(HIGHLIGHT);
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
