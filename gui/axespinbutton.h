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
