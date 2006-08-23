#include "pseudoaxespinbutton.h"

PseudoAxeSpinButton::PseudoAxeSpinButton(hkl::PseudoAxeInterface & pseudoAxe) :
  Gtk::SpinButton(),
  m_pseudoAxe(pseudoAxe),
  m_connect(false)
{
    SpinButton::set_digits(3);
    SpinButton::set_numeric(true);
    SpinButton::set_increments(1,10);
    SpinButton::set_update_policy(Gtk::UPDATE_IF_VALID);

    SpinButton::set_text("xxx");
    update();
    connect();
}

PseudoAxeSpinButton::~PseudoAxeSpinButton(void)
{
}

void
PseudoAxeSpinButton::connect(void)
{
    m_connect = true;
}

void
PseudoAxeSpinButton::unconnect(void)
{
    m_connect = false;
}

void
PseudoAxeSpinButton::initialize(void)
{

    try
      {
        m_pseudoAxe.initialize();
      }
    catch (HKLException const & ex)
      {
      }
    SpinButton::set_sensitive(m_pseudoAxe.get_writable());
}

void
PseudoAxeSpinButton::uninitialize(void)
{
    m_pseudoAxe.uninitialize();
    SpinButton::set_text("xxx");
    SpinButton::set_sensitive(m_pseudoAxe.get_writable());
}

void
PseudoAxeSpinButton::update(void)
{
    double value;
    try
      {
        value = m_pseudoAxe.get_value();
        SpinButton::set_value(value);
      }
    catch (HKLException const & ex)
      {
        SpinButton::set_text("xxx");
      }
    SpinButton::set_range(m_pseudoAxe.get_min(), m_pseudoAxe.get_max());
    SpinButton::set_sensitive(m_pseudoAxe.get_writable());
}

// Call back
void
PseudoAxeSpinButton::on_value_changed(void)
{
    if (m_connect)
      {
        double value = SpinButton::get_value();
        try
          {
            m_pseudoAxe.set_value(value);
          }
        catch (HKLException const & ex)
          {
            SpinButton::set_sensitive(m_pseudoAxe.get_writable());
          }
      }
}
