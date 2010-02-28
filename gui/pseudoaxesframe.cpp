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


	// objects
	_mode_liststore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore1"));
	_pseudoAxis_liststore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore2"));
	_mode_parameter_liststore = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
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

}

PseudoAxesFrame::~PseudoAxesFrame(void)
{
}

void PseudoAxesFrame::on_combobox1_changed(void)
{
	size_t idx = _combobox1->get_active_row_number();
	if(idx < HKL_LIST_LEN(_engine->modes)){
		hkl_pseudo_axis_engine_select_mode(_engine, idx);
		this->updateModeParameters();
	}
}

void PseudoAxesFrame::updatePseudoAxis(void)
{
	size_t i;

	for(i=0; i<HKL_LIST_LEN(_engine->pseudoAxes); ++i){
		Gtk::TreeRow row = *(_pseudoAxis_liststore->append());
		row[_pseudoAxis_columns.name] = ((HklParameter *)_engine->pseudoAxes[i])->name;
		row[_pseudoAxis_columns.value] = hkl_parameter_get_value_unit((HklParameter *)_engine->pseudoAxes[i]);
	}
}

void PseudoAxesFrame::updateMode(void)
{
	size_t i;

	for(i=0; i<HKL_LIST_LEN(_engine->modes); ++i){
		Gtk::TreeRow row = *(_mode_liststore->append());
		row[_mode_columns.name] = _engine->modes[i]->name;
	}
}

void PseudoAxesFrame::updateModeParameters(void)
{
	size_t i;

	if(_engine->mode){
		size_t len = HKL_LIST_LEN(_engine->mode->parameters);
		if(len){
			_mode_parameter_liststore->clear();
			for(i=0; i<len; ++i){
				Gtk::TreeRow row = *(_mode_parameter_liststore->append());
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

