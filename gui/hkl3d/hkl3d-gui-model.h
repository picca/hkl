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

#ifndef __HKL3D_GUI_MODEL_H__
#define __HKL3D_GUI_MODEL_H__

#include <gtkmm.h>
#include <gtkglmm.h>

#include "hkl3d.h"
#include "GL_ShapeDrawer.h"

namespace Hkl3dGui
{
	/* LogoModel class */
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

	/* ModelDraw class */
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
