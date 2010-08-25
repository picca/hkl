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
 *	    Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
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
	_refGlade->get_widget("toolbutton1", _toolbutton1);
	_refGlade->get_widget("toolbutton2", _toolbutton2);
	_refGlade->get_widget("filechooserdialog1", _filechooserdialog1);
	_refGlade->get_widget("button1", _button1);
	_refGlade->get_widget("button2", _button2);

	// objects
	_treestore1 = Glib::RefPtr<Gtk::TreeStore>::cast_dynamic(
		_refGlade->get_object("treestore1"));

	if(filename && geometry){
		Gtk::CellRenderer * renderer;

		_hkl3d = hkl3d_new(filename, geometry);
		_Scene = new Hkl3dGui::Scene(_hkl3d, false, false, false);

		this->update_hkl3d_objects_TreeStore();
		this->_vbox1->pack_start(*_Scene);
		this->_vbox1->show_all();

		// connect signals

		renderer = _treeview1->get_column_cell_renderer(1); // 1 is the index of the value column
		dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_cell_treeview1_toggled));

		_treeview1->signal_cursor_changed().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_treeview1_cursor_changed));

		_toolbutton1->signal_clicked().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_toolbutton1_clicked));
		_toolbutton2->signal_clicked().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_toolbutton2_clicked));
		_button1->signal_clicked().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_button1_clicked));
		_button2->signal_clicked().connect(
			sigc::mem_fun(*this, &Hkl3DFrame::on_button2_clicked));
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
		hkl3d_is_colliding(_hkl3d);
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
	for(i=0; i<_hkl3d->configs->len; ++i){
		Gtk::TreeRow row = *(_treestore1->append());
		row[_hkl3d_objects_columns.name] = _hkl3d->configs->configs[i]->filename;
		row[_hkl3d_objects_columns.config] = _hkl3d->configs->configs[i];
		row[_hkl3d_objects_columns.object] = NULL;
		for(j=0; j<_hkl3d->configs->configs[i]->len; ++j){
			Gtk::TreeRow crow = *(_treestore1->append(row.children()));

			crow[_hkl3d_objects_columns.name] = _hkl3d->configs->configs[i]->objects[j]->axis_name;
			crow[_hkl3d_objects_columns.hide] = _hkl3d->configs->configs[i]->objects[j]->hide;
			crow[_hkl3d_objects_columns.config] = _hkl3d->configs->configs[i] ;
			crow[_hkl3d_objects_columns.object] = _hkl3d->configs->configs[i]->objects[j];
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
		hkl3d_hide_object(_hkl3d, object, hide);
		row[_hkl3d_objects_columns.hide] = hide;
		this->is_colliding();
		this->invalidate();
	}else{
		config = row[_hkl3d_objects_columns.config];
		if(config){
			size_t i = 0;

			row[_hkl3d_objects_columns.hide] = hide;
			/* set all the children rows */
			iter = row.children().begin();
			Gtk::TreeModel::iterator end = row.children().end();
			while(iter != end){
				row = *(iter++);
				hkl3d_hide_object(_hkl3d, config->objects[i++], hide);
				row[_hkl3d_objects_columns.hide] = hide;
			}
			//_hkl3d->save_config(_hkl3d->filename);
			this->is_colliding();
			this->invalidate();
		}
	}
}

void Hkl3DFrame::on_treeview1_cursor_changed(void)
{
	Hkl3DObject *object;
	int i;
	int j;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_treeview1->get_cursor(path, column);
	Gtk::TreeModel::iterator iter = _treestore1->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	/* need to unselect of objects of all 3d models */
	for(i=0; i<_hkl3d->configs->len; ++i)
		for(j=0; j<_hkl3d->configs->configs[i]->len; ++j)
			_hkl3d->configs->configs[i]->objects[j]->selected = false;

	/* now select the right object */
	object = row[_hkl3d_objects_columns.object];
	if(object)
		object->selected = true;

	this->invalidate();
}

void Hkl3DFrame::on_toolbutton1_clicked(void)
{
	_filechooserdialog1->show();
}

void Hkl3DFrame::on_toolbutton2_clicked(void)
{
}

void Hkl3DFrame::on_button1_clicked(void)
{
	size_t i;
	std::string directory;

	Glib::SListHandle<Glib::ustring> const & filenames = _filechooserdialog1->get_filenames();
	directory = _filechooserdialog1->get_current_folder();
	Glib::SListHandle<Glib::ustring>::const_iterator iter = filenames.begin();
	Glib::SListHandle<Glib::ustring>::const_iterator const & end = filenames.end();
	while(iter != end){
		hkl3d_add_model_from_file(_hkl3d, (*iter).c_str(), directory.c_str());
		hkl3d_connect_all_axes(_hkl3d);
		++iter;
	}

	this->update_hkl3d_objects_TreeStore();
	//_hkl3d->save_config(_hkl3d->filename);
	_filechooserdialog1->hide();
}

void Hkl3DFrame::on_button2_clicked(void)
{
	_filechooserdialog1->hide();
}
