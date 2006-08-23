#ifndef GTKMM_PSEUDOAXE_SPINBUTTON_H
#define GTKMM_PSEUDOAXE_SPINBUTTON_H

#include <vector>
#include <gtkmm/spinbutton.h>

#include "pseudoaxe.h"

class PseudoAxeSpinButton : public Gtk::SpinButton
{
  public:
    PseudoAxeSpinButton(hkl::PseudoAxeInterface & pseudoAxe);
    virtual ~PseudoAxeSpinButton(void);

    void connect(void);
    void unconnect(void);
    void update(void);
    void initialize(void);
    void uninitialize(void);

  protected:
    virtual void on_value_changed();

    hkl::PseudoAxeInterface & m_pseudoAxe;
    bool m_connect;
};

typedef std::vector<PseudoAxeSpinButton *> PseudoAxeSpinButtonList;

#endif // GTKMM_PSEUDOAXE_SPINBUTTON_H
