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
	_refGlade->get_widget("treeview1", _treeview1);

	// objects
	_treestore1 = Glib::RefPtr<Gtk::TreeStore>::cast_dynamic(
		_refGlade->get_object("treestore1"));

	if(filename && geometry){
		Gtk::CellRenderer * renderer;

		_hkl3d = new Hkl3D(filename, geometry);
		_Scene = new Hkl3dGui::Scene(*_hkl3d, false, false, false);

		this->update_hkl3d_objects_TreeStore();
		this->_vbox1->pack_start(*_Scene);
		this->_vbox1->show_all();

		// connect signals

		renderer = _treeview1->get_column_cell_renderer(1); // 1 is the index of the value column
		dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_cell_treeview1_toggled));
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

void Hkl3DFrame::update_hkl3d_objects_TreeStore(void)
{
	size_t i;
	size_t j;

	if(!_hkl3d)
		return;

	_treestore1->clear();
	for(i=0; i<_hkl3d->configs.size(); ++i){
		Gtk::TreeRow row = *(_treestore1->append());
		row[_hkl3d_objects_columns.name] = _hkl3d->configs[i].filename;
		row[_hkl3d_objects_columns.config] = &_hkl3d->configs[i];
		row[_hkl3d_objects_columns.object] = NULL;
		for(j=0; j<_hkl3d->configs[i].objects.size(); ++j){
			Gtk::TreeRow crow = *(_treestore1->append(row.children()));

			crow[_hkl3d_objects_columns.name] = _hkl3d->configs[i].objects[j].name;
			crow[_hkl3d_objects_columns.hide] = _hkl3d->configs[i].objects[j].hide;
			crow[_hkl3d_objects_columns.config] = NULL;
			crow[_hkl3d_objects_columns.object] = &_hkl3d->configs[i].objects[j];
		}
	}
}

/************/
/* Callback */
/************/

void Hkl3DFrame::on_cell_treeview1_toggled(Glib::ustring const & spath)
{
	bool hide;
	Hkl3DConfig *config;
	Hkl3DObject *object;

	Gtk::TreePath path(spath);
	Gtk::TreeModel::iterator iter = _treestore1->get_iter(path);
	Gtk::TreeStore::Row row = *(iter);

	hide = !row[_hkl3d_objects_columns.hide];
	object = row[_hkl3d_objects_columns.object];
	if(object){
		object->hide = hide;
		row[_hkl3d_objects_columns.hide] = hide;
		this->is_colliding();
		this->invalidate();
	}
}
