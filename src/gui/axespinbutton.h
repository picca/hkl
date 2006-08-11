#ifndef GTKMM_AXE_SPINBUTTON_H
#define GTKMM_AXE_SPINBUTTON_H

#include <vector>
#include <gtkmm/spinbutton.h>

#include "axe.h"

class AxeSpinButton : public Gtk::SpinButton
{
  public:
    AxeSpinButton(hkl::Axe & axe);
    virtual ~AxeSpinButton(void);

    void connect(void);
    void unconnect(void);
    void update(void);

  protected:
    virtual void on_value_changed();

    hkl::Axe & m_axe;
    bool m_connect;
};

typedef std::vector<AxeSpinButton *> AxeSpinButtonList;

#endif // GTKMM_AXE_SPINBUTTON_H
