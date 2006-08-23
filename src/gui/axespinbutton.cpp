#include "axespinbutton.h"

AxeSpinButton::AxeSpinButton(hkl::Axe & axe) :
  Gtk::SpinButton(),
  m_axe(axe),
  m_connect(false)
{
    set_digits(3);
    set_range(-180,180);
    set_numeric(true);
    set_value(m_axe.get_value() * hkl::constant::math::radToDeg);
    set_increments(1,10);
    set_update_policy(Gtk::UPDATE_IF_VALID);
    set_wrap(true);
    
    connect();
}

AxeSpinButton::~AxeSpinButton(void)
{
}

void
AxeSpinButton::connect(void)
{
  m_connect = true;
}

void
AxeSpinButton::unconnect(void)
{
  m_connect = false;
}

void
AxeSpinButton::update(void)
{
  set_value(m_axe.get_value() * hkl::constant::math::radToDeg);
}

// Call back
void
AxeSpinButton::on_value_changed(void)
{
    if (m_connect)
        m_axe.set_value(get_value() * hkl::constant::math::degToRad);
}
