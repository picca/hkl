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

#include "hkl3d-gui-application.h"

namespace Hkl3dGui
{
	const Glib::ustring Application::APP_NAME = "ghkl3d";

	Application::Application(Hkl3D & hkl3d)
		: _hkl3d(hkl3d), m_Scene(hkl3d,false, false,false)
	{
		this->get_widgets_and_objects_from_ui();

		//
		// Top-level window.
		//
		_window1->set_title(APP_NAME);

		// Get automatically redrawn if any of their children changed allocation.
		_window1->set_reallocate_redraws(true);

		//
		// Application scene.
		//
		m_Scene.set_size_request(1200, 800);

		_vbox1->pack_start(m_Scene);

		this->connect_all_signals();
		//
		// Show window.
		//
		_window1->show_all();
	}

	Application::~Application(void)
	{
	}

	void Application::get_widgets_and_objects_from_ui(void)
	{
		//Get Glade UI:
		_refGlade = Gtk::Builder::create();
		if(!_refGlade->add_from_file("ghkl3d.ui")){
			std::string filename = Glib::build_filename(PKGDATA, "ghkl3d.ui");
			if(!_refGlade->add_from_file(filename))
				exit(1);
		}

		// window1
		_refGlade->get_widget("window1", _window1);
		_refGlade->get_widget("vbox1", _vbox1);
		_refGlade->get_widget("button1", _button1);
		_refGlade->get_widget("spinbutton1", _spinbutton1);
		_refGlade->get_widget("spinbutton2", _spinbutton2);
		_refGlade->get_widget("spinbutton3", _spinbutton3);
		_refGlade->get_widget("spinbutton4", _spinbutton4);
		_refGlade->get_widget("spinbutton5", _spinbutton5);
		_refGlade->get_widget("spinbutton6", _spinbutton6);
	}

	void Application::connect_all_signals(void)
	{
		_spinbutton1->signal_value_changed().connect(
			sigc::mem_fun(*this, &Application::on_spinbutton1_value_changed));
		_spinbutton2->signal_value_changed().connect(
			sigc::mem_fun(*this, &Application::on_spinbutton2_value_changed));
		_spinbutton3->signal_value_changed().connect(
			sigc::mem_fun(*this, &Application::on_spinbutton3_value_changed));
		_spinbutton4->signal_value_changed().connect(
			sigc::mem_fun(*this, &Application::on_spinbutton4_value_changed));
		_spinbutton5->signal_value_changed().connect(
			sigc::mem_fun(*this, &Application::on_spinbutton5_value_changed));
		_spinbutton6->signal_value_changed().connect(
			sigc::mem_fun(*this, &Application::on_spinbutton6_value_changed));


		_button1->signal_clicked().connect(
			sigc::mem_fun(*this, &Application::on_button1_quit_clicked));
	}

	void Application::on_spinbutton1_value_changed(void)
	{
		size_t idx = 0;
		double value = _spinbutton1->get_value();
		hkl_axis_set_value_unit(&_hkl3d.geometry->axes[idx], value);
		hkl_geometry_update(_hkl3d.geometry);
		hkl_geometry_fprintf(stdout, _hkl3d.geometry);
		hkl3d_is_colliding(&_hkl3d);
		m_Scene.invalidate();	
	}

	void Application::on_spinbutton2_value_changed(void)
	{
		size_t idx = 1;
		double value = _spinbutton2->get_value();
		hkl_axis_set_value_unit(&_hkl3d.geometry->axes[idx], value);
		hkl_geometry_update(_hkl3d.geometry);
		hkl_geometry_fprintf(stdout, _hkl3d.geometry);
		hkl3d_is_colliding(&_hkl3d);
		m_Scene.invalidate();
	}

	void Application::on_spinbutton3_value_changed(void)
	{
		size_t idx = 2;
		double value = _spinbutton3->get_value();
		hkl_axis_set_value_unit(&_hkl3d.geometry->axes[idx], value);
		hkl_geometry_update(_hkl3d.geometry);
		hkl_geometry_fprintf(stdout, _hkl3d.geometry);
		hkl3d_is_colliding(&_hkl3d);
		m_Scene.invalidate();
	}

	void Application::on_spinbutton4_value_changed(void)
	{
		size_t idx = 3;
		double value = _spinbutton4->get_value();
		hkl_axis_set_value_unit(&_hkl3d.geometry->axes[idx], value);
		hkl_geometry_update(_hkl3d.geometry);
		hkl_geometry_fprintf(stdout, _hkl3d.geometry);
		hkl3d_is_colliding(&_hkl3d);
		m_Scene.invalidate();
	}

	void Application::on_spinbutton5_value_changed(void)
	{
		size_t idx = 4;
		double value = _spinbutton5->get_value();
		hkl_axis_set_value_unit(&_hkl3d.geometry->axes[idx], value);
		hkl_geometry_update(_hkl3d.geometry);
		hkl_geometry_fprintf(stdout, _hkl3d.geometry);
		hkl3d_is_colliding(&_hkl3d);
		m_Scene.invalidate();
	}

	void Application::on_spinbutton6_value_changed(void)
	{
		size_t idx = 5;
		double value = _spinbutton6->get_value();
		hkl_axis_set_value_unit(&_hkl3d.geometry->axes[idx], value);
		hkl_geometry_update(_hkl3d.geometry);
		hkl_geometry_fprintf(stdout, _hkl3d.geometry);
		hkl3d_is_colliding(&_hkl3d);
		m_Scene.invalidate();
	}

	void Application::on_button1_quit_clicked(void)
	{
		Gtk::Main::quit();
	}

	bool Application::on_key_press_event(GdkEventKey* event)
	{
		switch (event->keyval) {
		case GDK_a:
			m_Scene.bulletDraw();
			break;
		case GDK_i:
			m_Scene.init_anim();
			break;
		case GDK_w:
			m_Scene.wireframe_view();
			break;
		case GDK_Escape:
			Gtk::Main::quit();
			break;
		default:
			return true;
		}

		m_Scene.invalidate();

		return true;
	}

} // namespace Hkl3dGui
