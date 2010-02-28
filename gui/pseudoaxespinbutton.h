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
#ifndef GTKMM_PSEUDOAXE_SPINBUTTON_H
#define GTKMM_PSEUDOAXE_SPINBUTTON_H

#include <vector>
#include <gtkmm/frame.h>
#include <gtkmm/table.h>
#include <gtkmm/label.h>
#include <gtkmm/spinbutton.h>
#include <gtkmm/togglebutton.h>

#include "hkl.h"

class PseudoAxeSpinButton : public Gtk::Frame
  {
  public:
    PseudoAxeSpinButton(HklPseudoAxis * pseudoAxis);
    virtual ~PseudoAxeSpinButton(void);

    void update(void);

    //signal accessor:
    typedef sigc::signal<void> type_signal_value_changed;
    type_signal_value_changed signal_value_changed();


  protected:
    type_signal_value_changed m_signal_value_changed;

    void on_spinbutton_value_value_changed(void);
    void on_togglebutton_toggled(void);

    HklPseudoAxis * m_pseudoAxis;
    Gtk::Table * m_table;
    Gtk::Label * m_label_value;
    Gtk::Label * m_label_min;
    Gtk::Label * m_label_max;
    Gtk::Label * m_label_min_value;
    Gtk::Label * m_label_max_value;
    Gtk::SpinButton * m_spinbutton_value;
    Gtk::ToggleButton * m_togglebutton;

    bool m_connected;
  };

typedef std::vector<PseudoAxeSpinButton *> PseudoAxeSpinButtonList;

#endif // GTKMM_PSEUDOAXE_SPINBUTTON_H
