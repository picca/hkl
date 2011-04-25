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

public class Hkl3D.Gui.View : Gtk.Object
{
	public void set_pos(float x, float y, float z)
		{ 
			this.m_Pos[0] = x;
			this.m_Pos[1] = y;
			this.m_Pos[2] = z;
		}

	public void set_quat(float q0, float q1, float q2, float q3)
		{
			this.m_Quat[0] = q0;
			this.m_Quat[1] = q1;
			this.m_Quat[2] = q2;
			this.m_Quat[3] = q3;
		}

	public void set_scale(float scale)
		{
			this.m_Scale = scale;
		}

	/* members */
	float m_Pos[3];
	float m_Quat[4];
	float m_Scale;

	float m_BeginX;
	float m_BeginY;

	const float NEAR_CLIP   = 2.0f;
	const float FAR_CLIP    = 60.0f;

	const float INIT_POS_X  = 0.0f;
	const float INIT_POS_Y  = 0.0f;
	const float INIT_POS_Z  = -30.0f;

	const float INIT_AXIS_X = 0.0f;
	const float INIT_AXIS_Y = 0.0f;
	const float INIT_AXIS_Z = 0.0f;
	const float INIT_ANGLE  = 0.0f;

	const float INIT_SCALE  = 7.0f;

	const float SCALE_MAX   = 50.0f;
	const float SCALE_MIN   = 1.0f;

	public View()
		{
			this.m_Scale = INIT_SCALE;
			this.m_BeginX = 0.0f;
			this.m_BeginY = 0.0f;

			this.reset();
		}

	public void frustum(GLsizei w, GLsizei h)
		{
			glViewport(0, 0, w, h);

			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();

			if (w > h) {
				float aspect = (float)(w) / (float)(h);
				glFrustum(-aspect, aspect, -1.0, 1.0, NEAR_CLIP, FAR_CLIP);
			}else{
				float aspect = (float)(h) / (float)(w);
				glFrustum(-1.0, 1.0, -aspect, aspect, NEAR_CLIP, FAR_CLIP);
			}

			glMatrixMode(GL_MODELVIEW);
		}

	public void ortho(int w, int h)
		{
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
		
			glOrtho(-w/40, w/40 , -h/40 , h/40 , -1, 100);

			glMatrixMode(GL_MODELVIEW);
		}

	public void xform()
		{
			/* OpenGL begin */
			GLfloat m[16];

			glTranslatef(m_Pos[0], m_Pos[1], m_Pos[2]);
			glScalef(m_Scale, m_Scale, m_Scale);
			Trackball.build_rotmatrix(m, m_Quat);
			glMultMatrixf(m);
			/* OpenGL end */

		}

	public void reset()
		{
			this.m_Pos[0] = this.INIT_POS_X;
			this.m_Pos[1] = this.INIT_POS_Y;
			this.m_Pos[2] = this.INIT_POS_Z;

			float sine = (float)Math.sin(0.5 * INIT_ANGLE * Hkl.DEGTORAD);
			this.m_Quat[0] = this.INIT_AXIS_X * sine;
			this.m_Quat[1] = this.INIT_AXIS_Y * sine;
			this.m_Quat[2] = this.INIT_AXIS_Z * sine;
			this.m_Quat[3] = (float)Math.cos(0.5 * INIT_ANGLE * Hkl.DEGTORAD);

			this.m_Scale = this.INIT_SCALE;
		}

	public bool on_button_press_event(Gdk.EventButton event, Hkl3D.Gui.Scene scene)
		{
			this.m_BeginX = (float)event.x;
			this.m_BeginY = (float)event.y;

			// don't block
			return false;
		}

	public bool on_scroll_event(Gdk.EventScroll event, Hkl3D.Gui.Scene scene)
		{
			bool redraw = false;
	
			if(event.direction == Gdk.ScrollDirection.DOWN)
				m_Scale +=1;
			else
				m_Scale -=1;

			if (m_Scale > this.SCALE_MAX)
				m_Scale = this.SCALE_MAX;
			else if (m_Scale < this.SCALE_MIN)
				m_Scale = this.SCALE_MIN;
			redraw = true;

			if (redraw && !scene.BulletDraw_is_enabled())
				scene.invalidate();

			return false;
		}

	public bool on_motion_notify_event(Gdk.EventMotion event,
									   Hkl3D.Gui.Scene scene)
		{
			float h = scene.allocation.width;
			float w = scene.allocation.height;
			float x = (float)event.x;
			float y = (float)event.y;
			float d_quat[4];
			bool redraw = false;

			// Translation && Rotation.
			if ((event.state & Gdk.ModifierType.BUTTON1_MASK) == Gdk.ModifierType.BUTTON1_MASK) {
				if((event.state & Gdk.ModifierType.SHIFT_MASK) == Gdk.ModifierType.SHIFT_MASK){
					/* shift pressed, translate view */
					m_Pos[0] += (x - m_BeginX) / (m_Scale * 10);
					m_Pos[1] -= (y - m_BeginY) / (m_Scale * 10);
					redraw = true;
				}else{
					Trackball.trackball(d_quat,
										(2.0f * m_BeginX - w) / w,
										(h - 2.0f * m_BeginY) / h,
										(2.0f * x - w) / w,
										(h - 2.0f * y) / h);
					Trackball.add_quats(d_quat, m_Quat, m_Quat);
					redraw = true;
				}
			}
		
			// Scaling.
			if ((event.state & Gdk.ModifierType.BUTTON2_MASK) == Gdk.ModifierType.BUTTON2_MASK) {
				this.m_Scale = this.m_Scale * (1.0f + (y - this.m_BeginY) / h);
				if (this.m_Scale > this.SCALE_MAX)
					this.m_Scale = this.SCALE_MAX;
				else if (this.m_Scale < this.SCALE_MIN)
					this.m_Scale = this.SCALE_MIN;
				redraw = true;
			}

			this.m_BeginX = x;
			this.m_BeginY = y;

			if (redraw && !scene.BulletDraw_is_enabled())
				scene.invalidate();

			// don't block
			return false;
		}
} //namespace  Hkl3dGui
