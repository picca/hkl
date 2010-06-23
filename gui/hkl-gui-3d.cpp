/* This file is part of the hkl library.
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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-gui-3d.h"

Hkl3DFrame::Hkl3DFrame(const char *filename, HklGeometry *geometry)
{
	Gtk::CellRenderer * renderer;

	//Get Glade UI:
	_refGlade = Gtk::Builder::create();
	try{
		_refGlade->add_from_file("3d.ui");
	}catch(...){
		std::string filename = Glib::build_filename(PKGDATA, "3d.ui");
		if(!_refGlade->add_from_file(filename))
			exit(1);
	}

	// widgets
	_refGlade->get_widget("frame1", _frame1);
	_refGlade->get_widget("vbox1", _vbox1);

	// objects
	_filenames_ListStore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore1"));
	_hkl3d_objects_ListStore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore2"));

	if(filename && geometry){
		_hkl3d = new Hkl3D(filename, geometry);
		_Scene = new Hkl3dGui::Scene(*_hkl3d, false, false, false);
		this->_vbox1->pack_start(*_Scene);
		this->_vbox1->show_all();
	}
}

Hkl3DFrame::~Hkl3DFrame(void)
{
	if(_hkl3d)
		delete _hkl3d;
	if(_Scene)
		delete _Scene;
}

void Hkl3DFrame::is_colliding(void)
{
	if(_hkl3d)
		_hkl3d->is_colliding();
}

void Hkl3DFrame::invalidate(void)
{
	if(_Scene)
		_Scene->invalidate();
}
