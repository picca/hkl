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

#include <iostream>

#include "hkl3d-gui-scene.h"

///////////////////////////////////////////////////////////////////////////////
//
// OpenGL frame buffer configuration utilities.
//
///////////////////////////////////////////////////////////////////////////////

struct GLConfigUtil
{
	static void print_gl_attrib(const Glib::RefPtr<const Gdk::GL::Config>& glconfig,
				    const char* attrib_str,
				    int attrib,
				    bool is_boolean);

	static void examine_gl_attrib(const Glib::RefPtr<const Gdk::GL::Config>& glconfig);
};

//
// Print a configuration attribute.
//
void GLConfigUtil::print_gl_attrib(const Glib::RefPtr<const Gdk::GL::Config>& glconfig,
                                   const char* attrib_str,
                                   int attrib,
                                   bool is_boolean)
{
	int value;

	if (glconfig->get_attrib(attrib, value)){
		std::cout << attrib_str << " = ";
		if (is_boolean)
			std::cout << (value == true ? "true" : "false") << std::endl;
		else
			std::cout << value << std::endl;
	}else{
		std::cout << "*** Cannot get "
			  << attrib_str
			  << " attribute value\n";
	}
}

//
// Print configuration attributes.
//
void GLConfigUtil::examine_gl_attrib(const Glib::RefPtr<const Gdk::GL::Config>& glconfig)
{
	std::cout << "\nOpenGL visual configurations :\n\n";

	std::cout << "glconfig->is_rgba() = "
		  << (glconfig->is_rgba() ? "true" : "false")
		  << std::endl;
	std::cout << "glconfig->is_double_buffered() = "
		  << (glconfig->is_double_buffered() ? "true" : "false")
		  << std::endl;
	std::cout << "glconfig->is_stereo() = "
		  << (glconfig->is_stereo() ? "true" : "false")
		  << std::endl;
	std::cout << "glconfig->has_alpha() = "
		  << (glconfig->has_alpha() ? "true" : "false")
		  << std::endl;
	std::cout << "glconfig->has_depth_buffer() = "
		  << (glconfig->has_depth_buffer() ? "true" : "false")
		  << std::endl;
	std::cout << "glconfig->has_stencil_buffer() = "
		  << (glconfig->has_stencil_buffer() ? "true" : "false")
		  << std::endl;
	std::cout << "glconfig->has_accum_buffer() = "
		  << (glconfig->has_accum_buffer() ? "true" : "false")
		  << std::endl;

	std::cout << std::endl;

	print_gl_attrib(glconfig, "Gdk::GL::USE_GL",           Gdk::GL::USE_GL,           true);
	print_gl_attrib(glconfig, "Gdk::GL::BUFFER_SIZE",      Gdk::GL::BUFFER_SIZE,      false);
	print_gl_attrib(glconfig, "Gdk::GL::LEVEL",            Gdk::GL::LEVEL,            false);
	print_gl_attrib(glconfig, "Gdk::GL::RGBA",             Gdk::GL::RGBA,             true);
	print_gl_attrib(glconfig, "Gdk::GL::DOUBLEBUFFER",     Gdk::GL::DOUBLEBUFFER,     true);
	print_gl_attrib(glconfig, "Gdk::GL::STEREO",           Gdk::GL::STEREO,           true);
	print_gl_attrib(glconfig, "Gdk::GL::AUX_BUFFERS",      Gdk::GL::AUX_BUFFERS,      false);
	print_gl_attrib(glconfig, "Gdk::GL::RED_SIZE",         Gdk::GL::RED_SIZE,         false);
	print_gl_attrib(glconfig, "Gdk::GL::GREEN_SIZE",       Gdk::GL::GREEN_SIZE,       false);
	print_gl_attrib(glconfig, "Gdk::GL::BLUE_SIZE",        Gdk::GL::BLUE_SIZE,        false);
	print_gl_attrib(glconfig, "Gdk::GL::ALPHA_SIZE",       Gdk::GL::ALPHA_SIZE,       false);
	print_gl_attrib(glconfig, "Gdk::GL::DEPTH_SIZE",       Gdk::GL::DEPTH_SIZE,       false);
	print_gl_attrib(glconfig, "Gdk::GL::STENCIL_SIZE",     Gdk::GL::STENCIL_SIZE,     false);
	print_gl_attrib(glconfig, "Gdk::GL::ACCUM_RED_SIZE",   Gdk::GL::ACCUM_RED_SIZE,   false);
	print_gl_attrib(glconfig, "Gdk::GL::ACCUM_GREEN_SIZE", Gdk::GL::ACCUM_GREEN_SIZE, false);
	print_gl_attrib(glconfig, "Gdk::GL::ACCUM_BLUE_SIZE",  Gdk::GL::ACCUM_BLUE_SIZE,  false);
	print_gl_attrib(glconfig, "Gdk::GL::ACCUM_ALPHA_SIZE", Gdk::GL::ACCUM_ALPHA_SIZE, false);

	std::cout << std::endl;
}

namespace  Hkl3dGui
{
	//
	// Scene class implementation.
	//
	const unsigned int Scene::TIMEOUT_INTERVAL = 100000000;

	const float Scene::CLEAR_COLOR[4] = { 0.9, 0.8, 0.6, 1.0 };
	const float Scene::CLEAR_DEPTH    = 1.0;

	const float Scene::LIGHT0_POSITION[4] = { 0.0, 0.0, 30.0, 0.0 };
	const float Scene::LIGHT0_DIFFUSE[4]  = { 1.0, 1.0, 1.0, 1.0 };
	const float Scene::LIGHT0_SPECULAR[4] = { 1.0, 1.0, 1.0, 1.0 };

	Scene::Scene(Hkl3D & hkl3d,bool enableBulletDraw, bool enableWireframe,bool enableAAbbBoxDraw,bool enableOrthoView)
		: m_Menu(0), m_Model(hkl3d,enableBulletDraw,enableWireframe,enableAAbbBoxDraw,enableOrthoView)
	{
		//
		// Configure OpenGL-capable visual.
		//
		Glib::RefPtr<Gdk::GL::Config> glconfig;

		// Try double-buffered visual
		glconfig = Gdk::GL::Config::create(Gdk::GL::MODE_RGB    |
						   Gdk::GL::MODE_DEPTH  |
						   Gdk::GL::MODE_DOUBLE);
		if (!glconfig) {
			std::cerr << "*** Cannot find the double-buffered visual.\n"
				  << "*** Trying single-buffered visual.\n";

			// Try single-buffered visual
			glconfig = Gdk::GL::Config::create(Gdk::GL::MODE_RGB   |
							   Gdk::GL::MODE_DEPTH);
			if (!glconfig) {
				std::cerr << "*** Cannot find any OpenGL-capable visual.\n";
				std::exit(1);
			}
		}

		// print frame buffer attributes.
		GLConfigUtil::examine_gl_attrib(glconfig);

		//
		// Set OpenGL-capability to the widget.
		//
		this->set_gl_capability(glconfig);

		//
		// Add events.
		//
		this->add_events(Gdk::BUTTON1_MOTION_MASK    |
				 Gdk::BUTTON2_MOTION_MASK    |
				 Gdk::BUTTON_PRESS_MASK      |
				 Gdk::VISIBILITY_NOTIFY_MASK);

		// View transformation signals.
		signal_button_press_event().connect(
			sigc::bind(sigc::mem_fun(m_View, &View::on_button_press_event), this));
		signal_motion_notify_event().connect(
			sigc::bind(sigc::mem_fun(m_View, &View::on_motion_notify_event), this));
		signal_scroll_event().connect(
			sigc::bind(sigc::mem_fun(m_View, &View::zoom_scroll), this));

		//
		// Popup menu.
		//
		m_Menu = this->create_popup_menu();
	}

	Scene::~Scene(void)
	{
	}

	void Scene::on_realize(void)
	{
		// We need to call the base on_realize()
		Gtk::DrawingArea::on_realize();

		//
		// Get GL::Drawable.
		//
		Glib::RefPtr<Gdk::GL::Drawable> gldrawable = this->get_gl_drawable();

		//
		// GL calls.
		//
		// *** OpenGL BEGIN ***
		if (!gldrawable->gl_begin(this->get_gl_context()))
			return;

		glClearColor(CLEAR_COLOR[0], CLEAR_COLOR[1], CLEAR_COLOR[2], CLEAR_COLOR[3]);
		glClearDepth(CLEAR_DEPTH);

		glLightfv(GL_LIGHT0, GL_POSITION, LIGHT0_POSITION);
		glLightfv(GL_LIGHT0, GL_DIFFUSE,  LIGHT0_DIFFUSE);
		glLightfv(GL_LIGHT0, GL_SPECULAR, LIGHT0_SPECULAR);

		glEnable(GL_LIGHTING);
		glEnable(GL_LIGHT0);

		glEnable(GL_DEPTH_TEST);

		glShadeModel(GL_SMOOTH);

		gldrawable->gl_end();
		// *** OpenGL END ***
	}

	bool Scene::on_configure_event(GdkEventConfigure* event)
	{
		//
		// Get GL::Drawable.
		//
		Glib::RefPtr<Gdk::GL::Drawable> gldrawable = this->get_gl_drawable();

		//
		// GL calls.
		//
		// *** OpenGL BEGIN ***
		if (!gldrawable->gl_begin(this->get_gl_context()))
			return false;

		m_View.frustum(this->get_width(), this->get_height());

		gldrawable->gl_end();
		// *** OpenGL END ***

		return true;
	}

	bool Scene::on_expose_event(GdkEventExpose* event)
	{
		//
		// Get GL::Drawable.
		//
		Glib::RefPtr<Gdk::GL::Drawable> gldrawable = get_gl_drawable();

		//
		// GL calls.
		//
		// *** OpenGL BEGIN ***
		if (!gldrawable->gl_begin(get_gl_context()))
			return false;

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glLoadIdentity();

		// View transformation.
		m_View.xform();

		// model.
		m_Model.draw();

		// Swap buffers.
		if (gldrawable->is_double_buffered())
			gldrawable->swap_buffers();
		else
			glFlush();

		gldrawable->gl_end();
		// *** OpenGL END ***

		return true;
	}

	bool Scene::on_button_press_event(GdkEventButton* event)
	{
		if (event->button == 3) {
			m_Menu->popup(event->button, event->time);
			return true;
		}

		// don't block
		return false;
	}

	bool Scene::on_map_event(GdkEventAny* event)
	{
		if (m_Model.bullet)
			this->timeout_add();

		return true;
	}

	bool Scene::on_unmap_event(GdkEventAny* event)
	{
		this->timeout_remove();

		return true;
	}

	bool Scene::on_visibility_notify_event(GdkEventVisibility* event)
	{
		if (m_Model.bullet) {
			if (event->state == GDK_VISIBILITY_FULLY_OBSCURED)
				
				this->timeout_add();
		}

		return true;
	}

	bool Scene::on_timeout(void)
	{
		// Invalidate whole window.
		this->invalidate();
		// Update window synchronously (fast).
		this->update();

		return true;
	}

	void Scene::timeout_add(void)
	{
		if (!m_ConnectionTimeout.connected())
			m_ConnectionTimeout = Glib::signal_timeout().connect(
				sigc::mem_fun(*this, &Scene::on_timeout), TIMEOUT_INTERVAL);
	}

	void Scene::timeout_remove(void)
	{
		if (m_ConnectionTimeout.connected())
			m_ConnectionTimeout.disconnect();
	}

	void Scene::bulletDraw(void)
	{
		if (m_Model.bullet) {
			m_Model.bullet = false;
			this->timeout_remove();
		}else{
			m_Model.bullet = true;
			this->timeout_add();
		}
	}

	void Scene::wireframe_view(void)
	{
		m_Model.wireframe = !m_Model.wireframe;
	}
		
	void Scene::orthoView(void)
	{
		if (m_Model.ortho){
			m_View.ortho(this->get_width(), this->get_height());
			m_Model.ortho = false;
		}else{
			m_View.frustum(this->get_width(), this->get_height());	
			m_Model.ortho = true;		
		}	
	}

	void Scene::AAbbBoxDraw(void)
	{
		if (m_Model.aabb) {
			m_Model.aabb = false;
			if (m_Model.bullet)
				this->timeout_add();
			else
				this->timeout_remove();
		}else{
			m_Model.aabb = true;
			this->timeout_add();
		}
	}

	void Scene::init_anim(void)
	{
		m_View.reset();
		m_Model.reset_anim();

		this->invalidate();
	}

	Gtk::Menu* Scene::create_popup_menu(void)
	{
		Gtk::Menu* menu = Gtk::manage(new Gtk::Menu());

		Gtk::Menu::MenuList& menu_list = menu->items();

		// Enable/Disable Bullet Draw
		menu_list.push_back(
			Gtk::Menu_Helpers::MenuElem("Enable/Disable Bullet Draw",
						    sigc::mem_fun(*this, &Scene::bulletDraw)));
		// Wireframe
		menu_list.push_back(
			Gtk::Menu_Helpers::MenuElem("Wireframe",
						    sigc::mem_fun(*this, &Scene::wireframe_view)));
		// Ortho view
		menu_list.push_back(
			Gtk::Menu_Helpers::MenuElem("Enable/Disable Ortho View",
						    sigc::mem_fun(*this, &Scene::orthoView)));
		// AAbbBox
		menu_list.push_back(
			Gtk::Menu_Helpers::MenuElem("Render AABB box",
						    sigc::mem_fun(*this, &Scene::AAbbBoxDraw)));
		// Init orientation
		menu_list.push_back(
			Gtk::Menu_Helpers::MenuElem("Initialize",
						    sigc::mem_fun(*this, &Scene::init_anim)));

		// Quit
		menu_list.push_back(
			Gtk::Menu_Helpers::MenuElem("Quit",
						    sigc::ptr_fun(&Gtk::Main::quit)));    

		return menu;
	}

} // namespace  Hkl3dGui
