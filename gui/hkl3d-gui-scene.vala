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

using Gtk;
using Gdk;
using GL;

public class Hkl3D.Gui.Scene : Gtk.DrawingArea
{
	// Invalidate whole window.
	public void invalidate()
	{
		this.get_window().invalidate_rect((Gdk.Rectangle)this.allocation, false);
	}

	// Update window synchronously (fast).
	public void update()
	{
		this.get_window().process_updates(false);
	}

	// timeout signal connection:
	public signal void m_ConnectionTimeout();

	// OpenGL scene related methods:
	public bool BulletDraw_is_enabled()
	{
		return this.m_Model.bullet;
	}

	public bool wireframe_is_enabled()
	{
		return this.m_Model.wireframe;
	}

	public bool aabbBoxDraw_is_enabled()
	{
		return this.m_Model.aabb;
	}

	// Popup menu:
	private Gtk.Menu m_Menu;

	// OpenGL scene related objects:
	Hkl3D.Gui.View m_View;
	Hkl3D.Gui.ModelDraw m_Model;

	private static const uint TIMEOUT_INTERVAL = 100000000;

	private const float CLEAR_COLOR[4] = { 0.9f, 0.8f, 0.6f, 1.0f };
	private const float CLEAR_DEPTH    = 1.0f;

	private const float LIGHT0_POSITION[4] = { 0.0f, 0.0f, 30.0f, 0.0f };
	private const float LIGHT0_DIFFUSE[4]  = { 1.0f, 1.0f, 1.0f, 1.0f };
	private const float LIGHT0_SPECULAR[4] = { 1.0f, 1.0f, 1.0f, 1.0f };

	public Scene(Hkl3D.Anticollision hkl3d,
				 bool enableBulletDraw, bool enableWireframe,
				 bool enableAAbbBoxDraw, bool enableOrthoView)
	{
		this.m_View = new Hkl3D.Gui.View();

		this.m_Model = new Hkl3D.Gui.ModelDraw(hkl3d,
											   enableBulletDraw, enableWireframe,
											   enableAAbbBoxDraw, enableOrthoView);

		Gdk.GLConfig? glconfig = new Gdk.GLConfig.by_mode (Gdk.GLConfigMode.RGB
														   | Gdk.GLConfigMode.DEPTH
														   | Gdk.GLConfigMode.DOUBLE);

		if (glconfig == null) {
			stderr.printf("*** Cannot find the double-buffered visual.\n");
			stderr.printf("*** Trying single-buffered visual.\n");

			// Try single-buffered visual
			glconfig = new Gdk.GLConfig.by_mode(Gdk.GLConfigMode.RGB
												| Gdk.GLConfigMode.DEPTH);
			if (glconfig == null) {
				stderr.printf("*** Cannot find any OpenGL-capable visual.\n");
				//exit(1);
			}
		}
		//
		// Set OpenGL-capability to the widget.
		//
		Gtk.WidgetGL.set_gl_capability(this, glconfig, null, true,
									   GLRenderType.RGBA_TYPE);

		//
		// Add events.
		//
		this.add_events (Gdk.EventMask.BUTTON_PRESS_MASK
						 | Gdk.EventMask.BUTTON1_MOTION_MASK
						 | Gdk.EventMask.BUTTON2_MOTION_MASK
						 | Gdk.EventMask.VISIBILITY_NOTIFY_MASK);

		// View transformation signals.
		//this.button_press_event.connect(m_View.on_button_press_event);
		//this.motion_notify_event.connect(m_View.on_motion_notify_event);
		//this.scroll_event.connect(m_View.on_scroll_event);

		this.m_Menu = this.create_popup_menu();
	}

	public override void realize()
	{
		// We need to call the base on_realize()
		base.realize();

		//
		// Get GL::Drawable.
		//
		var gldrawable = Gtk.WidgetGL.get_gl_drawable(this);

		//
		// GL calls.
		//
		// *** OpenGL BEGIN ***
		if (!gldrawable.gl_begin(Gtk.WidgetGL.get_gl_context(this)))
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

		this.m_Model.draw();

		gldrawable.gl_end();
		// *** OpenGL END ***
	}

	public override bool configure_event(Gdk.EventConfigure event)
	{
		//
		// Get GL::Drawable.
		//
		var gldrawable = Gtk.WidgetGL.get_gl_drawable(this);

		//
		// GL calls.
		//
		// *** OpenGL BEGIN ***

		if (!gldrawable.gl_begin(Gtk.WidgetGL.get_gl_context(this)))
			return false;

		this.m_View.frustum((GL.GLsizei)this.allocation.width, (GL.GLsizei)this.allocation.height);
		gldrawable.gl_end();
		// *** OpenGL END ***

		return true;
	}

	public override bool expose_event(Gdk.EventExpose event)
	{
		//
		// Get GL::Drawable.
		//
		var gldrawable = Gtk.WidgetGL.get_gl_drawable(this);

		//
		// GL calls.
		//
		// *** OpenGL BEGIN ***
		if (!gldrawable.gl_begin(Gtk.WidgetGL.get_gl_context(this)))
			return false;

		glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

		glLoadIdentity();

		// View transformation.
		this.m_View.xform();

		// model.
		this.m_Model.draw();

		// Swap buffers.
		if (gldrawable.is_double_buffered())
			gldrawable.swap_buffers();
		else
			glFlush();

		gldrawable.gl_end();
		// *** OpenGL END ***

		return true;
	}

	public override bool button_press_event(Gdk.EventButton event)
	{
		if (event.button == 3) {
			this.m_Menu.popup(null, null, null, event.button, event.time);
			return true;
		}

		// don't block
		return false;
	}

	public override bool map_event(Gdk.Event event)
	{
		if (this.m_Model.bullet)
			this.timeout_add();

		return true;
	}

	public override bool unmap_event(Gdk.Event event)
	{
		this.timeout_remove();

		return true;
	}

	public override bool visibility_notify_event(Gdk.Event event)
	{
		if (this.m_Model.bullet) {
			//if (event.state == GDK_VISIBILITY_FULLY_OBSCURED)
				
			//	this.timeout_add();
		}
		return true;
	}

	public bool on_timeout()
	{
		// Invalidate whole window.
		this.invalidate();
		// Update window synchronously (fast).
		this.update();

		return true;
	}

	public void timeout_add()
	{
/*
  if (!m_ConnectionTimeout.connected())
  GLib.timeout().connect(this.on_timeout, this.TIMEOUT_INTERVAL);

  m_ConnectionTimeout = Glib::signal_timeout().connect(
  sigc::mem_fun(*this, &Scene::on_timeout), TIMEOUT_INTERVAL);
*/
	}

	public void timeout_remove()
	{
/*
  if (this.m_ConnectionTimeout.connected())
  m_ConnectionTimeout.disconnect();
*/
	}

	public void bulletDraw()
	{
		if (this.m_Model.bullet) {
			this.m_Model.bullet = false;
			this.timeout_remove();
		}else{
			this.m_Model.bullet = true;
			this.timeout_add();
		}
	}

	public void wireframe_view()
	{
		this.m_Model.wireframe = !this.m_Model.wireframe;
	}
		
	public void orthoView()
	{
		if (this.m_Model.ortho){
			this.m_View.frustum((GL.GLsizei)this.allocation.width, (GL.GLsizei)this.allocation.height);
			this.m_Model.ortho = false;
		}else{
			this.m_View.ortho((GL.GLsizei)this.allocation.width, (GL.GLsizei)this.allocation.height);	
			this.m_Model.ortho = true;		
		}	
	}

	public void AAbbBoxDraw()
	{
		if (this.m_Model.aabb) {
			this.m_Model.aabb = false;
			if (this.m_Model.bullet)
				this.timeout_add();
			else
				this.timeout_remove();
		}else{
			this.m_Model.aabb = true;
			this.timeout_add();
		}
	}

	public void init_anim()
	{
		this.m_View.reset();
		this.m_Model.reset_anim();

		this.invalidate();
	}

	Gtk.Menu create_popup_menu()
	{
		Gtk.Menu menu = new Gtk.Menu();

		//Gtk.Menu.MenuList menu_list = menu.items();

		// Enable/Disable Bullet Draw
		/*
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
		*/
		return menu;
	}

} // namespace  Hkl3dGui
