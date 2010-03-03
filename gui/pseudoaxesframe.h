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
#ifndef __PSEUDO_AXES_FRAME_H__
#define __PSEUDO_AXES_FRAME_H__

#include <gtkmm.h>
#include "hkl.h"

class ModeModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<Glib::ustring> name;

	ModeModelColumns()
	{
		this->add(name);
	}
};

class PseudoAxisModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<double> value;
	Gtk::TreeModelColumn<HklPseudoAxis *> pseudo;

	PseudoAxisModelColumns()
	{
		this->add(name);
		this->add(value);
		this->add(pseudo);
	}
};

class PseudoAxesFrame
{
public:
	PseudoAxesFrame(HklPseudoAxisEngine *engine);
	Gtk::Frame &frame(void) {return *_frame1;}
	virtual ~PseudoAxesFrame(void);

	void update(void);

	// signals emitted
	typedef sigc::signal<void> type_signal_changed;
	type_signal_changed signal_changed(void);

// callback
protected:
	void on_combobox1_changed(void);
	virtual void on_cell_TreeView_pseudoAxis_value_edited(Glib::ustring const &,
							      Glib::ustring const &);
	void on_button1_clicked(void);
	void on_button2_clicked(void);

// non callback
protected:
	void updateMode(void);
	void updatePseudoAxis(void);
	void updateModeParameters(void);

	type_signal_changed _signal_changed;

// members
protected:
	HklPseudoAxisEngine *_engine;

	// widget
	Glib::RefPtr<Gtk::Builder> _refGlade;
	Gtk::Frame *_frame1;
	Gtk::Label *_label2;
	Gtk::ComboBox *_combobox1;
	Gtk::Expander *_expander1;
	Gtk::TreeView *_treeview1;
	Gtk::Button *_button1;
	Gtk::Button *_button2;

	// objects
	Glib::RefPtr<Gtk::ListStore> _mode_ListStore;
	Glib::RefPtr<Gtk::ListStore> _pseudoAxis_ListStore;
	Glib::RefPtr<Gtk::ListStore> _mode_parameter_ListStore;
	ModeModelColumns _mode_columns;
	PseudoAxisModelColumns _pseudoAxis_columns;
};

#endif // __PSEUDO_AXES_FRAME_H__
