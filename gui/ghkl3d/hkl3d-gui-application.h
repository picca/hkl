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

#ifndef __HKL3D_GUI_APPLICATION_H__
#define __HKL3D_GUI_APPLICATION_H__

#include <gtkmm.h>

#include "hkl3d.h"
#include "hkl3d-gui-scene.h"

namespace Logo
{
	class Application
	{
	public:
		static const Glib::ustring APP_NAME;

	public:
		explicit Application(Hkl3D & hkl3d);
		virtual ~Application(void);

	protected:
		// signal handlers:
		virtual void on_button1_quit_clicked(void);
		virtual void on_spinbutton1_value_changed(void);
		virtual void on_spinbutton2_value_changed(void);
		virtual void on_spinbutton3_value_changed(void);
		virtual void on_spinbutton4_value_changed(void);
		virtual void on_spinbutton5_value_changed(void);
		virtual void on_spinbutton6_value_changed(void);
		virtual bool on_key_press_event(GdkEventKey* event);

	protected:
		Hkl3D &_hkl3d;

		Glib::RefPtr<Gtk::Builder> _refGlade;

		// member widgets:
		Gtk::Window *_window1;
		Gtk::VBox *_vbox1;
		Scene m_Scene;
		Gtk::SpinButton *_spinbutton1;
		Gtk::SpinButton *_spinbutton2;
		Gtk::SpinButton *_spinbutton3;
		Gtk::SpinButton *_spinbutton4;
		Gtk::SpinButton *_spinbutton5;
		Gtk::SpinButton *_spinbutton6;
		Gtk::Button *_button1;

	private:
		void get_widgets_and_objects_from_ui(void);
		void connect_all_signals(void);
	};

} // namespace Logo

#endif // __HKL3D_GUI_APPLICATION_H__
