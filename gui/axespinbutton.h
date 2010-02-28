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
#ifndef GTKMM_AXE_SPINBUTTON_H
#define GTKMM_AXE_SPINBUTTON_H

#include <vector>
#include <gtkmm/frame.h>
#include <gtkmm/table.h>
#include <gtkmm/label.h>
#include <gtkmm/spinbutton.h>

#include "hkl.h"

class AxeSpinButton : public Gtk::Frame
{
public:
	AxeSpinButton(HklAxis *axis);
	virtual ~AxeSpinButton(void);

	void update(void);

	//signal accessor:
	typedef sigc::signal<void> type_signal_value_changed;
	typedef sigc::signal<void> type_signal_min_changed;
	typedef sigc::signal<void> type_signal_max_changed;
	type_signal_value_changed signal_value_changed();
	type_signal_min_changed signal_min_changed();
	type_signal_max_changed signal_max_changed();


protected:
	type_signal_value_changed m_signal_value_changed;
	type_signal_min_changed m_signal_min_changed;
	type_signal_max_changed m_signal_max_changed;

	void on_spinbutton_value_value_changed(void);
	void on_spinbutton_min_value_changed(void);
	void on_spinbutton_max_value_changed(void);


	HklAxis *m_axis;
	Gtk::Table * m_table;
	Gtk::Label * m_label_value;
	Gtk::Label * m_label_min;
	Gtk::Label * m_label_max;
	Gtk::SpinButton * m_spinbutton_value;
	Gtk::SpinButton * m_spinbutton_min;
	Gtk::SpinButton * m_spinbutton_max;

	bool m_connected;
};

typedef std::vector<AxeSpinButton *> AxeSpinButtonList;

#endif // GTKMM_AXE_SPINBUTTON_H
