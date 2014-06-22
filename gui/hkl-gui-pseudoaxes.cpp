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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-gui-pseudoaxes.h"

PseudoAxesFrame::PseudoAxesFrame(HklEngine *engine)
{
	Gtk::CellRenderer * renderer;

	_engine = engine;

	//Get Glade UI:
	_refGlade = Gtk::Builder::create();
	try{
		_refGlade->add_from_file("pseudo.ui");
	}catch(...){
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
	_refGlade->get_widget("treeview2", _treeview2);
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
	_label2->set_label(hkl_engine_name_get(engine));

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

	renderer = _treeview2->get_column_cell_renderer(1); // 1 is the index of the value column
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &PseudoAxesFrame::on_cell_treeview2_mode_parameter_value_edited));


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
	Gtk::TreeModel::iterator iter = _combobox1->get_active();
	Gtk::ListStore::Row row = *(iter);
	hkl_engine_select_mode(_engine, row[_mode_columns.name], NULL);
	this->updateModeParameters();
	this->_signal_changed();
}

void PseudoAxesFrame::on_cell_TreeView_pseudoAxis_value_edited(Glib::ustring const & spath,
							       Glib::ustring const & newText)
{
	double value;
	HklParameter *parameter;

	Gtk::TreePath path(spath);
	Gtk::TreeModel::iterator iter = _pseudoAxis_ListStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sscanf(newText.c_str(), "%lf", &value);

	parameter = row[_pseudoAxis_columns.parameter];
	if(parameter){
		Gtk::CellRenderer *renderer;

		renderer = _treeview1->get_column_cell_renderer(1); // 1 is the index of the value column
		renderer->property_cell_background().set_value("red");
		/* TODO check the error and change the meaning once
		   the set method will do the computation */
		hkl_parameter_value_unit_set(parameter, value, NULL);
		row[_pseudoAxis_columns.value] = value;
	}
}

void PseudoAxesFrame::on_button1_clicked(void)
{
	if(hkl_engine_set(_engine, NULL)){
		HklEngineList *engines = hkl_engine_engines_get(this->_engine);
		const HklGeometryList *solutions = hkl_engine_list_geometries_get(engines);
		const HklGeometryListItem *first = hkl_geometry_list_items_first_get(solutions);
		hkl_engine_list_select_solution(engines, first);
		this->_signal_changed();
	}
}

void PseudoAxesFrame::on_button2_clicked(void)
{
	if(hkl_engine_initialize(_engine, NULL))
		this->updateModeParameters(); //some initialize function modify the parameters
}

void PseudoAxesFrame::on_cell_treeview2_mode_parameter_value_edited(Glib::ustring const & spath,
								    Glib::ustring const & newText)
{
	double value;
	HklParameter *parameter;

	Gtk::TreePath path(spath);
	Gtk::TreeModel::iterator iter = _mode_parameter_ListStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	const char *name = row[_mode_parameter_columns.name];
	sscanf(newText.c_str(), "%lf", &value);

	parameter = hkl_parameter_new_copy(hkl_engine_parameter_get(this->_engine,
								    name, NULL));
		/* TODO check the error */
	hkl_parameter_value_unit_set(parameter, value, NULL);
	hkl_engine_parameter_set(this->_engine, name, parameter, NULL);
	hkl_parameter_free(parameter);
	row[_mode_parameter_columns.value] = value;
}

/****************/
/* Non-Callback */
/****************/

void PseudoAxesFrame::updatePseudoAxis(void)
{
	const char **pseudo_axis;
	const darray_string *pseudo_axes = hkl_engine_pseudo_axes_names_get(this->_engine);

	_pseudoAxis_ListStore->clear();
	darray_foreach(pseudo_axis, *pseudo_axes){
		Gtk::TreeRow row = *(_pseudoAxis_ListStore->append());
		row[_pseudoAxis_columns.name] = *pseudo_axis;
		row[_pseudoAxis_columns.value] = hkl_parameter_value_unit_get(hkl_engine_pseudo_axis_get(this->_engine,
													 *pseudo_axis, NULL));
	}
}

void PseudoAxesFrame::updateMode(void)
{
	const darray_string *modes = hkl_engine_modes_names_get(this->_engine);
	const char **mode;

	_mode_ListStore->clear();
	darray_foreach(mode, *modes){
		Gtk::TreeRow row = *(_mode_ListStore->append());
		row[_mode_columns.name] = *mode;
	}
}

void PseudoAxesFrame::updateModeParameters(void)
{
	const darray_string *parameters = hkl_engine_parameters_names_get(this->_engine);

	if(darray_size(*parameters)){
		const char **parameter;

		_mode_parameter_ListStore->clear();
		darray_foreach(parameter, *parameters){
			Gtk::TreeRow row = *(_mode_parameter_ListStore->append());
			row[_mode_parameter_columns.name] = *parameter;
			row[_mode_parameter_columns.value] = hkl_parameter_value_unit_get(hkl_engine_parameter_get(this->_engine, *parameter, NULL));
		}
		_expander1->set_expanded(1);
		_expander1->show();
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
