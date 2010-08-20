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

#ifndef __HKL3D_GUI_SCENE_H__
#define __HKL3D_GUI_SCENE_H__

#include <gtkmm.h>
#include <gtkglmm.h>

#include "hkl3d-gui-view.h"
#include "hkl3d-gui-model.h"

namespace Hkl3dGui
{
	class Scene : public Gtk::GL::DrawingArea
	{
	public:
		static const unsigned int TIMEOUT_INTERVAL;

		// OpenGL scene related constants:
		static const float CLEAR_COLOR[4];
		static const float CLEAR_DEPTH;

		static const float LIGHT0_POSITION[4];
		static const float LIGHT0_DIFFUSE[4];
		static const float LIGHT0_SPECULAR[4];

	public:
		explicit Scene(struct Hkl3D *hkl3d, 
			       bool enableBulletDraw=false, bool enableWireframe=false,
			       bool enableAAbbBoxDraw=false, bool enableOrthoView=false);
		virtual ~Scene(void);

	protected:
		// signal handlers:
		virtual void on_realize(void);
		virtual bool on_configure_event(GdkEventConfigure* event);
		virtual bool on_expose_event(GdkEventExpose* event);
		virtual bool on_button_press_event(GdkEventButton* event);
		virtual bool on_map_event(GdkEventAny* event);
		virtual bool on_unmap_event(GdkEventAny* event);
		virtual bool on_visibility_notify_event(GdkEventVisibility* event);
		virtual bool on_timeout(void);

	public:
		// Invalidate whole window.
		void invalidate(void)
		{
			this->get_window()->invalidate_rect(get_allocation(), false);
		}

		// Update window synchronously (fast).
		void update(void)
		{
			this->get_window()->process_updates(false);
		}

	protected:
		// timeout signal connection:
		sigc::connection m_ConnectionTimeout;

		void timeout_add(void);
		void timeout_remove(void);

	public:
		// OpenGL scene related methods:
		bool BulletDraw_is_enabled(void) const
		{
			return m_Model.bullet;
		}
		bool wireframe_is_enabled(void) const
		{
			return m_Model.wireframe;
		}
		bool aabbBoxDraw_is_enabled(void) const
		{
			return m_Model.aabb;
		}
		void orthoView(void);
		void bulletDraw(void);
		void wireframe_view(void);
		void AAbbBoxDraw(void);
		void init_anim(void);

	protected:
		Gtk::Menu* create_popup_menu(void);

		// Popup menu:
		Gtk::Menu* m_Menu;

		// OpenGL scene related objects:
		View m_View;
		ModelDraw m_Model;
	};
}//namespace  Hkl3dGui

#endif //__HKL3D_GUI_SCENE_H__
