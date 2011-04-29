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
 * Copyright (C) 2010-2011 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
 *          Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
using GL;

class Hkl3D.Gui.DrawingTools
{
	/* members */

	Hkl3D.Anticollision _hkl3d;
	//GL_ShapeDrawer m_shapeDrawer;

	/* methods */

	public DrawingTools(Hkl3D.Anticollision hkl3d)
		{
			this._hkl3d = hkl3d;
			//this.m_shapeDrawer.enableTexture(true);
		}

/*
	void draw_line(const btVector3 & from, const btVector3 & to,
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
*/
	public void draw_AAbbBoxes()
		{
			int i;
			int j;
			glDisable(GL_LIGHTING);
			for(i=0; i<this._hkl3d.configs.configs.length; i++)
				for(j=0; j<this._hkl3d.configs.configs[i].objects.length; j++){
					if(!this._hkl3d.configs.configs[i].objects[j].hide){
						Hkl3D.Object object = this._hkl3d.configs.configs[i].objects[j];
						//btVector3 aabbMin, aabbMax;

						//object->btShape->getAabb(object->btObject->getWorldTransform(),
						//						 aabbMin, aabbMax);
						//this->draw_Aabb(aabbMin, aabbMax, btVector3(1,0,0));
					}
				}

			glFlush();
		}

	public void draw_collisions()
		{
			int i;
			int numManifolds;
			//btScalar m[16];
			//btVector3 worldBoundsMin;
			//btVector3 worldBoundsMax;
		
			glDisable(GL_LIGHTING);
			// get the world bounding box from bullet
			//this._hkl3d.get_bounding_boxes(&worldBoundsMin, &worldBoundsMax);
			///one way to draw all the contact points is iterating over contact manifolds / points:
			numManifolds = this._hkl3d.get_nb_manifolds();
			for (i=0; i<numManifolds; i++){
				int numContacts;
				int j;

				// now draw the manifolds / points			  
				numContacts = this._hkl3d.get_nb_contacts(i);
				for (j=0; j<numContacts; j++){
					double xa, ya, za;
					double xb, yb, zb;

					this._hkl3d.get_collision_coordinates(i, j,
														  out xa, out ya, out za,
														  out xb, out yb, out zb);

					glDisable(GL_DEPTH_TEST);
					glBegin(GL_LINES);
					glColor4f(0, 0, 0, 1);
					glVertex3d(xa, ya, za);
					glVertex3d(xb, yb, zb);
					glEnd();
					glColor4f(1, 0, 0, 1);
					glPushMatrix(); 
					glTranslated (xb, yb, zb);
					glScaled(0.05,0.05,0.05);
					//m_shapeDrawer.drawSphere(1, 10, 10);
					glPopMatrix();
					glColor4f(1, 1, 0, 1);
					glPushMatrix();  
					glTranslated (xa, ya, za);
					glScaled(0.05,0.05,0.05);
					//m_shapeDrawer.drawSphere(1, 10, 10);
					glPopMatrix();
					glEnable(GL_DEPTH_TEST);
				}
			}
			glFlush();
		}

	public void draw_bullet()
		{
			int i;
			int j;
/*			btScalar m[16];
			btVector3 worldBoundsMin;
			btVector3 worldBoundsMax;
			btVector3 aabbMin,aabbMax;
*/
			//GL_ShapeDrawer.drawCoordSystem(); 

			/* get the bounding box from bullet */
			//this.get_bounding_boxes(&worldBoundsMin, &worldBoundsMax);

			/* draw all visible objects */
			for(i=0; i<this._hkl3d.configs.configs.length; i++){
				for(j=0; j<this._hkl3d.configs.configs[i].objects.length; j++){
					Hkl3D.Object object;

					object = this._hkl3d.configs.configs[i].objects[j];
					if(!object.hide){
/*						btCollisionObject *btObject;

						btObject = object->btObject;
						btObject->getWorldTransform().getOpenGLMatrix( m );
						m_shapeDrawer.drawOpenGL(m,
												 btObject->getCollisionShape(),
												 *object->color,
												 0, // debug mode
												 worldBoundsMin,
												 worldBoundsMax);
*/
					}
				}
			}
			glFlush();
		}

	public void draw_g3dmodel()
		{	 
			int i;
			int j;

			/* set the alpha canal to 0.5 if there is a collision */
			for(i=0; i<this._hkl3d.configs.configs.length; i++)
				for(j=0; j<this._hkl3d.configs.configs[i].objects.length; j++){
					float alpha;

					if(this._hkl3d.configs.configs[i].objects[j].is_colliding)
						alpha = 0.5f;
					else
						alpha = 1f;

					foreach(G3D.Face face in this._hkl3d.configs.configs[i].objects[j].g3dObject.faces)
						face.material.a = alpha;
				}

			/* draw the G3DObjects */
/*
			GLDRAW::G3DGLRenderOptions *options =  g_new0(GLDRAW::G3DGLRenderOptions, 1);
			options->glflags = G3D_FLAG_GL_SPECULAR
			| G3D_FLAG_GL_SHININESS
			| G3D_FLAG_GL_TEXTURES
			| G3D_FLAG_GL_COLORS;
			options->updated = true;
			options->initialized = false;
			GL_ShapeDrawer::drawCoordSystem();
			GLDRAW::gl_draw(options, _hkl3d->model);
*/
			glFlush();
		}

	static void draw_g3dObject(G3D.Object object)
		{	
			float *vertex;

			vertex = object.vertex_data;

			glPushMatrix();

			/* apply the transformation of the object */
			if(object.transformation != null)
				glMultMatrixf(object.transformation.matrix);

			/* draw all faces with the current stencil */
			foreach(G3D.Face face in object.faces){
				glBegin(GL_TRIANGLES);
				glVertex3d(vertex[3*(face.vertex_indices[0])],
						   vertex[3*(face.vertex_indices[0])+1],
						   vertex[3*(face.vertex_indices[0])+2]);
				glVertex3d(vertex[3*(face.vertex_indices[1])], 
						   vertex[3*(face.vertex_indices[1])+1], 
						   vertex[3*(face.vertex_indices[1])+2]);
				glVertex3d(vertex[3*(face.vertex_indices[2])],
						   vertex[3*(face.vertex_indices[2])+1], 
						   vertex[3*(face.vertex_indices[2])+2]);
				glEnd();
			}

			glPopMatrix();		
		}

	public void draw_selected()
		{
			foreach(var config in this._hkl3d.configs.configs){
				foreach(var object in config.objects){
					if(object.selected && !object.hide){
						// Push the GL attribute bits so that we don't wreck any settings	
						glDisable(GL_LIGHTING);
						glPushAttrib( GL_ALL_ATTRIB_BITS );

						// Enable polygon offsets, and offset filled polygons forward by 2.5	
						glEnable( GL_POLYGON_OFFSET_FILL );
						glPolygonOffset( -2.5f, -2.5f);

						// Set the render mode to be line rendering with a thick line width
						glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
						glLineWidth( 3.0f );
						// Set the colour to be pink
						glColor3f( 1.0f, 0.0f, 1.0f );
						// Render the object
						this.draw_g3dObject(object.g3dObject);
						// Set the polygon mode to be filled triangles 
						glLineWidth( 1.0f );
						glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
						// Set the colour to the background
						glCullFace(GL_FRONT);
						glColor3f( 0.0f, 0.0f, 0.0f );
						// Render the object
						this.draw_g3dObject(object.g3dObject);

						// Pop the state changes off the attribute 
						// to set things back how they were		
						glPopAttrib();	
					}
				}
				glFlush();
			}
		}
}

class Hkl3D.Gui.ModelDraw
{
	/* members */
	
	public bool wireframe;
	public bool bullet;
	public bool aabb;
	public bool ortho;

	Hkl3D.Anticollision _hkl3d;
	DrawingTools model;
	float m_Pos[3];
	float m_Quat[4];


	enum DisplayList {
		MODEL = 1,
		BULLET,			
		COLLISION,
		AABBBOX,
		HIGHLIGHT
	}

	public ModelDraw(Hkl3D.Anticollision hkl3d,
					 bool enableBulletDraw, bool enableWireframe,
					 bool enableAAbbBoxDraw, bool enableOrtho)
		{
			this._hkl3d = hkl3d;
			this.model = new DrawingTools(hkl3d);
			this.bullet = enableBulletDraw;
			this.wireframe = enableWireframe;
			this.aabb = enableAAbbBoxDraw;
			this.ortho = enableOrtho;

			this.reset_anim();
		}

	void init_gl(DrawingTools model)
		{
			glNewList(DisplayList.MODEL, GL_COMPILE);
			this.model.draw_g3dmodel();
			glEndList();
			glNewList(DisplayList.BULLET, GL_COMPILE);
			this.model.draw_bullet();
			glEndList();
			glNewList(DisplayList.COLLISION, GL_COMPILE);
			this.model.draw_collisions();
			glEndList();
			glNewList(DisplayList.AABBBOX, GL_COMPILE);
			this.model.draw_AAbbBoxes();
			glEndList();
			glNewList(DisplayList.HIGHLIGHT, GL_COMPILE);
			this.model.draw_selected();
			glEndList();
		}

	public void draw()
		{
			float m[16];

			// Init GL context.
			init_gl(model);

			// Draw  model.
			glPushMatrix();
			glTranslatef(m_Pos[0], m_Pos[1], m_Pos[2]);

			Trackball.build_rotmatrix(m, m_Quat);
			glMultMatrixf(m);
			glRotatef(0.0f, 0.0f, 0.0f, 1.0f);
		
			// WireFrame
			if(this.wireframe){
				glPolygonMode(GL_FRONT, GL_LINE);
				glPolygonMode(GL_BACK, GL_LINE);
			}else{
				glPolygonMode(GL_FRONT, GL_FILL);
				glPolygonMode(GL_BACK, GL_FILL);
			}
			// AABB Box
			if(this.aabb)
				glCallList(DisplayList.AABBBOX);

			// Bullet and G3DModel
			if(this.bullet)
				glCallList(DisplayList.BULLET);	
			else
				glCallList(DisplayList.MODEL);
			glCallList(DisplayList.COLLISION);
			glCallList(DisplayList.HIGHLIGHT);
			glPopMatrix();
		}

	public void reset_anim()
		{
			m_Pos[0] = 0.0f;
			m_Pos[1] = 0.0f;
			m_Pos[2] = 0.0f;

			m_Quat[0] = 0.0f;
			m_Quat[1] = 0.0f;
			m_Quat[2] = 0.0f;
			m_Quat[3] = 1.0f;
		}
}

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

/*
#ifndef __HKL3D_GUI_MODEL_H__
#define __HKL3D_GUI_MODEL_H__

#include <gtkmm.h>
#include <gtkglmm.h>

#include "hkl3d.h"
#include "GL_ShapeDrawer.h"

namespace Hkl3dGui
{
	// LogoModel class
	class DrawingTools
	{
		public:
		DrawingTools(Hkl3D *hkl3d);
		virtual ~DrawingTools(void);
		void draw_collisions(void);
		void draw_g3dmodel(void);
		void draw_bullet(void);
		void draw_AAbbBoxes(void);
		void draw_selected(void);

		private:
		void draw_Aabb(const btVector3 & from, const btVector3 & to, const btVector3 & color);
		void draw_line(const btVector3 & from, const btVector3 & to,
					   const btVector3 & fromColor, const btVector3 & toColor);
		void draw_line(const btVector3 & from,const btVector3 & to,const btVector3 & color);

		private:
		Hkl3D *_hkl3d;
		GL_ShapeDrawer m_shapeDrawer;
	};

	// ModelDraw class
	class ModelDraw
	{
		friend class Scene;
		friend class DrawingTools;

		public:
		enum DisplayList {
			MODEL = 1,
			BULLET,			
			COLLISION,
			AABBBOX,
			HIGHLIGHT
		};

		public:
   
		explicit ModelDraw(Hkl3D *hkl3d,
						   bool enableBulletdraw=false, bool enableWireframe=false,
						   bool enableAAbbBoxDraw=false, bool enableOrthoView=false);
		virtual ~ModelDraw(void);

		void draw(void);

		void reset_anim(void);

		void set_pos(float x, float y, float z)
			{
				m_Pos[0] = x;
				m_Pos[1] = y;
				m_Pos[2] = z;
			}

		void set_quat(float q0, float q1, float q2, float q3)
			{
				m_Quat[0] = q0;
				m_Quat[1] = q1;
				m_Quat[2] = q2;
				m_Quat[3] = q3;
			}

		public:
		bool wireframe;
		bool bullet;
		bool aabb;
		bool ortho;

		private:
		void init_gl(DrawingTools* model);


		private:
		Hkl3D *_hkl3d;
		DrawingTools *model;
		float m_Pos[3];
		float m_Quat[4];
	};

} // namespace Hkl3dGui

#endif // __HKL3D_GUI_MODEL_H__
*/