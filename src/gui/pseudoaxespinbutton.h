#ifndef GTKMM_PSEUDOAXE_SPINBUTTON_H
#define GTKMM_PSEUDOAXE_SPINBUTTON_H

#include <vector>
#include <gtkmm/frame.h>
#include <gtkmm/table.h>
#include <gtkmm/label.h>
#include <gtkmm/spinbutton.h>
#include <gtkmm/togglebutton.h>

#include "pseudoaxe.h"

class PseudoAxeSpinButton : public Gtk::Frame
  {
  public:
    PseudoAxeSpinButton(hkl::PseudoAxe * pseudoAxe);
    virtual ~PseudoAxeSpinButton(void);

    void update(void);

    //signal accessor:
    typedef sigc::signal<void> type_signal_value_changed;
    type_signal_value_changed signal_value_changed();


  protected:
    type_signal_value_changed m_signal_value_changed;

    void on_spinbutton_value_value_changed(void);
    void on_togglebutton_toggled(void);

    hkl::PseudoAxe * m_pseudoAxe;
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
