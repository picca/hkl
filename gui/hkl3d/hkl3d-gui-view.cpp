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

#include "hkl3d-gui-view.h"
#include "hkl3d-gui-scene.h"

//
// Trackball utilities.
//
namespace Trackball {
	extern "C" {
#include "trackball.h"
	}
}

#define DIG_2_RAD (G_PI / 180.0)
#define RAD_2_DIG (180.0 / G_PI)

namespace  Hkl3dGui
{
	//
	// View class implementation.
	//
	const float View::NEAR_CLIP   = 2.0;
	const float View::FAR_CLIP    = 60.0;

	const float View::INIT_POS_X  = 0.0;
	const float View::INIT_POS_Y  = 0.0;
	const float View::INIT_POS_Z  = -30.0;

	const float View::INIT_AXIS_X = 0.0;
	const float View::INIT_AXIS_Y = 0.0;
	const float View::INIT_AXIS_Z = 0.0;
	const float View::INIT_ANGLE  = 0.0;

	const float View::INIT_SCALE  = 7.0;

	const float View::SCALE_MAX   = 50.0;
	const float View::SCALE_MIN   = 1.0;

	View::View(void)
		: m_Scale(INIT_SCALE), m_BeginX(0.0), m_BeginY(0.0)
	{
		this->reset();
	}

	View::~View(void)
	{
	}

	void View::frustum(int w, int h)
	{
		glViewport(0, 0, w, h);

		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();

		if (w > h) {
			float aspect = static_cast<float>(w) / static_cast<float>(h);
			glFrustum(-aspect, aspect, -1.0, 1.0, NEAR_CLIP, FAR_CLIP);
		}else{
			float aspect = static_cast<float>(h) / static_cast<float>(w);
			glFrustum(-1.0, 1.0, -aspect, aspect, NEAR_CLIP, FAR_CLIP);
		}

		glMatrixMode(GL_MODELVIEW);
	}
	void View::ortho(int w, int h)
	{
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();

		glOrtho(-w/40, w/40 , -h/40 , h/40 , -1, 100);

		glMatrixMode(GL_MODELVIEW);
	}
	void View::xform(void)
	{
		glTranslatef(m_Pos[0], m_Pos[1], m_Pos[2]);

		glScalef(m_Scale, m_Scale, m_Scale);

		float m[4][4];
		Trackball::build_rotmatrix(m, m_Quat);
		glMultMatrixf(&m[0][0]);

	}

	void View::reset(void)
	{
		m_Pos[0] = INIT_POS_X;
		m_Pos[1] = INIT_POS_Y;
		m_Pos[2] = INIT_POS_Z;

		float sine = sin(0.5 * INIT_ANGLE * DIG_2_RAD);
		m_Quat[0] = INIT_AXIS_X * sine;
		m_Quat[1] = INIT_AXIS_Y * sine;
		m_Quat[2] = INIT_AXIS_Z * sine;
		m_Quat[3] = cos(0.5 * INIT_ANGLE * DIG_2_RAD);

		m_Scale = INIT_SCALE;
	}

	bool View::on_button_press_event(GdkEventButton* event,
					 Scene* scene)
	{
		m_BeginX = event->x;
		m_BeginY = event->y;

		// don't block
		return false;
	}
	bool View::zoom_scroll(GdkEventScroll *event,Scene* scene)
	{
		float x = event->x;
		float y = event->y;
		float w = scene->get_width();
		float h = scene->get_height();
		bool redraw = false;

		if (scene == 0)
			return false;


		if(event->direction == GDK_SCROLL_DOWN)
			m_Scale +=1;
		else
			m_Scale -=1;

		if (m_Scale > SCALE_MAX)
			m_Scale = SCALE_MAX;
		else if (m_Scale < SCALE_MIN)
			m_Scale = SCALE_MIN;
		redraw = true;

		if (redraw && !scene->BulletDraw_is_enabled())
			scene->invalidate();


		return false;
	}
	bool View::on_motion_notify_event(GdkEventMotion* event,
					  Scene* scene)
	{
		if (scene == 0)
			return false;

		float w = scene->get_width();
		float h = scene->get_height();
		float x = event->x;
		float y = event->y;
		float d_quat[4];
		bool redraw = false;

		// Translation && Rotation.
		if (event->state & GDK_BUTTON1_MASK) {
			if(event->state & GDK_SHIFT_MASK)
			{
				/* shift pressed, translate view */
				m_Pos[0]+=
					(gdouble)(x - m_BeginX ) /
					(gdouble)(m_Scale * 10);
				m_Pos[1]-=
					(gdouble)(y - m_BeginY) /
					(gdouble)(m_Scale * 10);
				redraw = true;
			}
			else{
				Trackball::trackball(d_quat,
						     (2.0 * m_BeginX - w) / w,
						     (h - 2.0 * m_BeginY) / h,
						     (2.0 * x - w) / w,
						     (h - 2.0 * y) / h);
				Trackball::add_quats(d_quat, m_Quat, m_Quat);
				redraw = true;
			}

		}

		// Scaling.
		if (event->state & GDK_BUTTON2_MASK) {
			m_Scale = m_Scale * (1.0 + (y - m_BeginY) / h);
			if (m_Scale > SCALE_MAX)
				m_Scale = SCALE_MAX;
			else if (m_Scale < SCALE_MIN)
				m_Scale = SCALE_MIN;
			redraw = true;
		}

		m_BeginX = x;
		m_BeginY = y;

		if (redraw && !scene->BulletDraw_is_enabled())
			scene->invalidate();

		// don't block
		return false;
	}
} //namespace  Hkl3dGui
