#include "axespinbutton.h"

AxeSpinButton::AxeSpinButton(HklAxis *axis) :
	Gtk::Frame(),
	m_axis(axis),
	m_connected(true)
{
	// Set the name of the frame
	set_label(((HklParameter *)m_axis)->name);
	m_table = new Gtk::Table(3, 2);

	m_label_value = new Gtk::Label("xxx");
	m_label_min = new Gtk::Label("min : ");
	m_label_max = new Gtk::Label("max : ");

	// 1st spinbutton
	m_spinbutton_value = new Gtk::SpinButton();
	m_spinbutton_value->set_digits(3);
	m_spinbutton_value->set_numeric(true);
	m_spinbutton_value->set_increments(0.1, 1);
	m_spinbutton_value->set_update_policy(Gtk::UPDATE_IF_VALID);

	m_spinbutton_min = new Gtk::SpinButton();
	m_spinbutton_min->set_digits(3);
	m_spinbutton_min->set_numeric(true);
	m_spinbutton_min->set_increments(0.1, 1);
	m_spinbutton_min->set_update_policy(Gtk::UPDATE_IF_VALID);

	m_spinbutton_max = new Gtk::SpinButton();
	m_spinbutton_max->set_digits(3);
	m_spinbutton_max->set_numeric(true);
	m_spinbutton_max->set_increments(0.1, 1);
	m_spinbutton_max->set_update_policy(Gtk::UPDATE_IF_VALID);


	m_table->attach(*m_label_value, 0, 1, 0, 1, Gtk::FILL);
	m_table->attach(*m_spinbutton_value, 1, 2, 0, 1, Gtk::FILL);
	m_table->attach(*m_label_min, 0, 1, 1, 2, Gtk::FILL);
	m_table->attach(*m_label_max, 1, 2, 1, 2, Gtk::FILL);
	m_table->attach(*m_spinbutton_min, 0, 1, 2, 3, Gtk::FILL);
	m_table->attach(*m_spinbutton_max, 1, 2, 2, 3, Gtk::FILL);

	// add the table to the Frame
	add(*m_table);

	update();

	m_spinbutton_value->signal_value_changed().connect(
		mem_fun(*this, &AxeSpinButton::on_spinbutton_value_value_changed));
	m_spinbutton_min->signal_value_changed().connect(
		mem_fun(*this, &AxeSpinButton::on_spinbutton_min_value_changed));
	m_spinbutton_max->signal_value_changed().connect(
		mem_fun(*this, &AxeSpinButton::on_spinbutton_max_value_changed));
}

AxeSpinButton::~AxeSpinButton(void)
{
	delete m_table;
	delete m_label_value;
	delete m_label_min;
	delete m_label_max;
	delete m_spinbutton_value;
	delete m_spinbutton_min;
	delete m_spinbutton_max;
}

void
AxeSpinButton::update(void)
{
	double min;
	double max;
	m_connected = false;
	// update label
	double value = hkl_axis_get_value_unit(m_axis);
	char value_txt[30];
	snprintf(value_txt, 29, "%lf", value);
	m_label_value->set_text(value_txt);

	// update m_spinbutton_value range;
	hkl_axis_get_range_unit(m_axis, &min, &max);
	m_spinbutton_value->set_range(min, max);

	// update min/max range and value.
	m_spinbutton_min->set_range(-360 * HKL_DEGTORAD, value);
	m_spinbutton_min->set_value(min);
	m_spinbutton_max->set_range(value, 360 * HKL_DEGTORAD);
	m_spinbutton_max->set_value(max);
	m_spinbutton_value->set_value(value);
	m_connected = true;
}

// signal
AxeSpinButton::type_signal_value_changed
AxeSpinButton::signal_value_changed(void)
{
	return m_signal_value_changed;
}

AxeSpinButton::type_signal_min_changed
AxeSpinButton::signal_min_changed(void)
{
	return m_signal_min_changed;
}

AxeSpinButton::type_signal_max_changed
AxeSpinButton::signal_max_changed(void)
{
	return m_signal_max_changed;
}

// Callback
void
AxeSpinButton::on_spinbutton_value_value_changed(void)
{
	if (m_connected){
		double value = m_spinbutton_value->get_value();
		hkl_axis_set_value_unit(m_axis, value);

		// update label
		char value_txt[30];
		snprintf(value_txt, 29, "%lf", value);
		m_label_value->set_text(value_txt);

		// update the min max range.
		m_spinbutton_min->set_range(-360 * HKL_DEGTORAD, value);
		m_spinbutton_max->set_range(value, 360 * HKL_DEGTORAD);

		m_signal_value_changed();
	}
}

void
AxeSpinButton::on_spinbutton_min_value_changed(void)
{
	if (m_connected)
	{
		double min = m_spinbutton_min->get_value();
		double max = m_spinbutton_max->get_value();
		hkl_axis_set_range_unit(m_axis, min, max);
		m_spinbutton_value->set_range(min, max);

		m_signal_min_changed();
	}
}

void
AxeSpinButton::on_spinbutton_max_value_changed(void)
{
	if (m_connected)
	{
		double min = m_spinbutton_min->get_value();
		double max = m_spinbutton_max->get_value();
		hkl_axis_set_range(m_axis, min, max);
		m_spinbutton_value->set_range(min, max);

		m_signal_max_changed();
	}
}
