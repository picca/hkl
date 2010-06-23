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
#ifndef __HKL_GUI_3D_H__
#define __HKL_GUI_3D_H__

#include <gtkmm.h>

#include "hkl3d.h"
#include "hkl3d-gui-scene.h"

class FilenamesModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<Glib::ustring> name;

	FilenamesModelColumns()
	{
		this->add(name);
	}
};

class Hkl3DObjectsModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<bool> hide;

	Hkl3DObjectsModelColumns()
	{
		this->add(name);
		this->add(hide);
	}
};

class Hkl3DFrame
{
public:
	Hkl3DFrame(const char *filename, HklGeometry *geometry);
	virtual ~Hkl3DFrame(void);

	Gtk::Frame &frame(void) {return *_frame1;}
	void is_colliding(void);
	void invalidate(void);

// callback
protected:


// non callback
protected:

// members
protected:

	Hkl3D *_hkl3d;
	Hkl3dGui::Scene *_Scene;

	/* widgets */
	Gtk::Frame *_frame1;
	Gtk::VBox *_vbox1;

	/* objects */
	Glib::RefPtr<Gtk::ListStore> _filenames_ListStore;
	Glib::RefPtr<Gtk::ListStore> _hkl3d_objects_ListStore;
	FilenamesModelColumns _filenames_columns;
	Hkl3DObjectsModelColumns _hkl3d_objects_columns;
	Glib::RefPtr<Gtk::Builder> _refGlade;
};

#endif // __HKL_GUI_3D_H__
