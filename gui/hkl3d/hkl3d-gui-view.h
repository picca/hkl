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

#ifndef __HKL3D_GUI_VIEW_H__
#define __HKL3D_GUI_VIEW_H__

#include <gtkmm.h>
#include <gtkglmm.h>

//#include "GlutDemoApplication.h"
//#include "hkl3d.h"

namespace  Hkl3dGui
{
	class Scene;

	class View : public sigc::trackable
	{
		friend class Scene;

	public:
		static const float NEAR_CLIP;
		static const float FAR_CLIP;

		static const float INIT_POS_X;
		static const float INIT_POS_Y;
		static const float INIT_POS_Z;

		static const float INIT_AXIS_X;
		static const float INIT_AXIS_Y;
		static const float INIT_AXIS_Z;
		static const float INIT_ANGLE;

		static const float INIT_SCALE;

		static const float SCALE_MAX;
		static const float SCALE_MIN;

	public:
		View(void);
		virtual ~View(void);

	public:
		void frustum(int w, int h);
		void ortho(int w, int h);
		void xform(void);

		void reset(void);

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

		void set_scale(float scale)
		{
			m_Scale = scale;
		}

	protected:
		// Signal handlers:
		virtual bool zoom_scroll(GdkEventScroll *event ,Scene* scene);
		virtual bool on_button_press_event(GdkEventButton* event, Scene* scene);
		virtual bool on_motion_notify_event(GdkEventMotion* event, Scene* scene);

	private:
		float m_Pos[3];
		float m_Quat[4];
		float m_Scale;

		float m_BeginX;
		float m_BeginY;
	};
} // namespace  Hkl3dGui

#endif // __HKL3D_GUI_VIEW_H__
