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
#include "pseudoaxesframe.h"

PseudoAxesFrame::PseudoAxesFrame(HklPseudoAxisEngine *engine)
{
	Gtk::CellRenderer * renderer;

	_engine = engine;

	//Get Glade UI:
	_refGlade = Gtk::Builder::create();
	if(!_refGlade->add_from_file("pseudo.ui")){
		std::string filename = Glib::build_filename(PKGDATA, "pseudo.ui");
		if(!_refGlade->add_from_file(filename))
			exit(1);
	}

	// widgets
	_refGlade->get_widget("frame1", _frame1);
	_refGlade->get_widget("label2", _label2);
	_refGlade->get_widget("combobox1", _combobox1);
	_refGlade->get_widget("expander1", _expander1);
	_refGlade->get_widget("treeview1", _treeview1);
	_refGlade->get_widget("button1", _button1);
	_refGlade->get_widget("button2", _button2);

	// objects
	_mode_ListStore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore1"));
	_pseudoAxis_ListStore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore2"));
	_mode_parameter_ListStore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore3"));

	// title
	_label2->set_label(_engine->name);

	// update all the liststore
	this->updatePseudoAxis();
	this->updateMode();
	this->updateModeParameters();

	// connect signals
	_combobox1->signal_changed().connect(
		sigc::mem_fun(*this, &PseudoAxesFrame::on_combobox1_changed) );

	renderer = _treeview1->get_column_cell_renderer(1); // 1 is the index of the value column
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &PseudoAxesFrame::on_cell_TreeView_pseudoAxis_value_edited));

	_button1->signal_clicked ().connect (
		sigc::mem_fun (*this, &PseudoAxesFrame::on_button1_clicked) );

	_button2->signal_clicked ().connect (
		sigc::mem_fun (*this, &PseudoAxesFrame::on_button2_clicked) );

}

PseudoAxesFrame::~PseudoAxesFrame(void)
{
}

void PseudoAxesFrame::update(void)
{
	Gtk::CellRenderer *renderer;
	this->updatePseudoAxis();

	renderer = _treeview1->get_column_cell_renderer(1); // 1 is the index of the value column
	renderer->property_cell_background().set_value("white");
}

/************/
/* Callback */
/************/

void PseudoAxesFrame::on_combobox1_changed(void)
{
	size_t idx = _combobox1->get_active_row_number();
	if(idx < HKL_LIST_LEN(_engine->modes)){
		hkl_pseudo_axis_engine_select_mode(_engine, idx);
		this->updateModeParameters();
	}
}

void PseudoAxesFrame::on_cell_TreeView_pseudoAxis_value_edited(Glib::ustring const & spath,
							       Glib::ustring const & newText)
{
	double value;
	HklPseudoAxis *pseudo;

	Gtk::TreePath path(spath);
	Gtk::TreeModel::iterator iter = _pseudoAxis_ListStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sscanf(newText.c_str(), "%lf", &value);

	pseudo = row[_pseudoAxis_columns.pseudo];
	if(pseudo){
		Gtk::CellRenderer *renderer;

		renderer = _treeview1->get_column_cell_renderer(1); // 1 is the index of the value column
		renderer->property_cell_background().set_value("red");
		hkl_parameter_set_value_unit((HklParameter *)pseudo, value);
		row[_pseudoAxis_columns.value] = value;
	}
}

void PseudoAxesFrame::on_button1_clicked(void)
{
	if(hkl_pseudo_axis_engine_set(_engine, NULL) == HKL_SUCCESS){
		hkl_geometry_init_geometry(_engine->engines->geometry,
					   _engine->engines->geometries->items[0]->geometry);
		this->_signal_changed();
	}
}

void PseudoAxesFrame::on_button2_clicked(void)
{
	fprintf(stdout, "coucou\n");
	if(hkl_pseudo_axis_engine_initialize(_engine, NULL) == HKL_SUCCESS){
		this->updateModeParameters(); //some initialize function modify the parameters
		hkl_pseudo_axis_engine_fprintf(stdout, _engine);
	}
}

/****************/
/* Non-Callback */
/****************/

void PseudoAxesFrame::updatePseudoAxis(void)
{
	size_t i;

	_pseudoAxis_ListStore->clear();
	for(i=0; i<HKL_LIST_LEN(_engine->pseudoAxes); ++i){
		Gtk::TreeRow row = *(_pseudoAxis_ListStore->append());
		row[_pseudoAxis_columns.name] = ((HklParameter *)_engine->pseudoAxes[i])->name;
		row[_pseudoAxis_columns.value] = hkl_parameter_get_value_unit((HklParameter *)_engine->pseudoAxes[i]);
		row[_pseudoAxis_columns.pseudo] = _engine->pseudoAxes[i];
	}
}

void PseudoAxesFrame::updateMode(void)
{
	size_t i;

	_mode_ListStore->clear();
	for(i=0; i<HKL_LIST_LEN(_engine->modes); ++i){
		Gtk::TreeRow row = *(_mode_ListStore->append());
		row[_mode_columns.name] = _engine->modes[i]->name;
	}
}

void PseudoAxesFrame::updateModeParameters(void)
{
	size_t i;

	if(_engine->mode){
		size_t len = HKL_LIST_LEN(_engine->mode->parameters);
		if(len){
			_mode_parameter_ListStore->clear();
			for(i=0; i<len; ++i){
				Gtk::TreeRow row = *(_mode_parameter_ListStore->append());
				row[_pseudoAxis_columns.name] = _engine->mode->parameters[i].name;
				row[_pseudoAxis_columns.value] = hkl_parameter_get_value_unit(&_engine->mode->parameters[i]);
			}
			_expander1->set_expanded(1);
			_expander1->show();
		}else
			_expander1->hide();
	}else
		_expander1->hide();
}

/***********/
/* signals */
/***********/

PseudoAxesFrame::type_signal_changed PseudoAxesFrame::signal_changed(void)
{
	return _signal_changed;
}
