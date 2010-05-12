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

#ifndef __HKL3D_GUI_MODEL_H__
#define __HKL3D_GUI_MODEL_H__

#include <gtkmm.h>
#include <gtkglmm.h>

#include "hkl3d.h"
#include "GlutDemoApplication.h"
#include "BulletCollision/Gimpact/btGImpactCollisionAlgorithm.h"
///////////////////////////////////////////////////////////////////////////////
//
// Logo classes.
//
///////////////////////////////////////////////////////////////////////////////

namespace Logo
{
	//
	// LogoModel class.
	//
	class LogoModel:public GlutDemoApplication
	{
	public:
		LogoModel(Hkl3D & hkl3d);
		virtual ~LogoModel(void);
		void model_draw_collision(void);
		void model_draw (void);
		void modelDrawBullet (void);
		void drawAAbbBox(void);
		void drawSphere(void);
		//void renderContactTest(btCollisionObject*,bool);
		void initPhysics(void);
		virtual void clientMoveAndDisplay(void);
		virtual void displayCallback(void);
	protected:
		Hkl3D & _hkl3d;
	};

	//
	// Model class.
	//
	class Model
	{
		friend class Scene;
		friend class LogoModel;

	public:
		enum DisplayList {
			MODEL = 1,
			BULLETDRAW,			
			COLLISION,
			AABBBOX
		};

		static const float MAT_SPECULAR[4];
		static const float MAT_SHININESS[1];
		static const float MAT_BLACK[4];
		static const float MAT_RED[4];
		static const float MAT_GREEN[4];
		static const float MAT_BLUE[4];

		

	public:
   
		explicit Model(Hkl3D & hkl3d,bool enableBulletdraw =false, bool enableWireframe=false,bool enableAAbbBoxDraw=false);
		virtual ~Model(void);

	private:
		void init_gl(LogoModel* LogoM);

	public:
		void draw(void);

		void enableBulletDraw(void)
		{
			m_EnableBulletDraw = true;
		}
    		void enableWireframe(void)
		{
			m_EnableWireframe = true;
		}
		void enableAAbbBoxDraw(void)
		{
			m_EnableAAbbBoxDraw = true;
		}
		void disableBulletDraw(void)
		{
			m_EnableBulletDraw = false;
		}
		void disableAAbbBoxDraw(void)
		{
			m_EnableAAbbBoxDraw = false;
		}
		void disableWireframe(void)
		{
			m_EnableWireframe = false;
		}
		bool bulletDraw_is_enabled(void) const
		{
			return m_EnableBulletDraw;
		}
		bool wireframe_is_enabled(void) const
		{
			return m_EnableWireframe;
		}
		bool aabbBoxDraw_is_enabled(void) const
		{
			return m_EnableAAbbBoxDraw;
		}
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

	private:
		Hkl3D & _hkl3d;

		LogoModel* logoM;


	private:
		
		bool m_EnableWireframe;
		bool m_EnableBulletDraw;
		bool m_EnableAAbbBoxDraw;
		unsigned int m_Mode;

		float m_Pos[3];
		float m_Quat[4];
	};

} // namespace Logo

#endif // __HKL3D_GUI_MODEL_H__
