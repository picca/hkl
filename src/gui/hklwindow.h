#ifndef GTKMM_HKL_WINDOW_H
#define GTKMM_HKL_WINDOW_H

#include <map>

#include <libglademm/variablesmap.h>
#include <gtkmm.h>

#include "diffractometer.h"
#include "axespinbutton.h"
#include "pseudoaxespinbutton.h"
#include "modelcolumns.h"

class HKLWindow : public Gtk::Window
{
  public:
    HKLWindow(hkl::DiffractometerInterface *);
    virtual ~HKLWindow(void);

    hkl::Axe & get_axe(Glib::ustring const & name);

  protected:
    //Signal handlers:
    virtual void on_button_goto_hkl_clicked(void);
    virtual void on_comboboxentrytext_modes_changed(void);
    virtual void on_comboboxentrytext_affinement_changed(void);
    virtual void on_spinbutton_a_value_changed(void);
    virtual void on_spinbutton_b_value_changed(void);
    virtual void on_spinbutton_c_value_changed(void);
    virtual void on_spinbutton_alpha_value_changed(void);
    virtual void on_spinbutton_beta_value_changed(void);
    virtual void on_spinbutton_gamma_value_changed(void);
    virtual void on_spinbutton_a_min_value_changed(void);
    virtual void on_spinbutton_b_min_value_changed(void);
    virtual void on_spinbutton_c_min_value_changed(void);
    virtual void on_spinbutton_alpha_min_value_changed(void);
    virtual void on_spinbutton_beta_min_value_changed(void);
    virtual void on_spinbutton_gamma_min_value_changed(void);
    virtual void on_spinbutton_a_max_value_changed(void);
    virtual void on_spinbutton_b_max_value_changed(void);
    virtual void on_spinbutton_c_max_value_changed(void);
    virtual void on_spinbutton_alpha_max_value_changed(void);
    virtual void on_spinbutton_beta_max_value_changed(void);
    virtual void on_spinbutton_gamma_max_value_changed(void);
    virtual void on_spinbutton_lambda_value_changed(void);
    virtual void on_spinbutton_max_iteration_value_changed(void);
    virtual void on_checkbutton_a_toggled(void);
    virtual void on_checkbutton_b_toggled(void);
    virtual void on_checkbutton_c_toggled(void);
    virtual void on_checkbutton_alpha_toggled(void);
    virtual void on_checkbutton_beta_toggled(void);
    virtual void on_checkbutton_gamma_toggled(void);
    virtual void on_checkbutton_U_toggled(void);
    virtual void on_axeSpinButton_changed(void);
    virtual void on_pseudoAxeSpinButton_value_changed(void);
    virtual void on_cell_name_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_a_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_b_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_c_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_alpha_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_beta_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_gamma_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_h_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_k_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_l_edited(Glib::ustring const &, Glib::ustring const &);
    virtual void on_cell_flag_toggled(Glib::ustring const &);
    virtual void on_toolbutton_add_reflection_clicked(void);
    virtual void on_toolbutton_goto_reflection_clicked(void);
    virtual void on_toolbutton_del_reflection_clicked(void);
    virtual void on_toolbutton_computeUB_clicked(void);
    virtual void on_toolbutton_add_crystal_clicked(void);
    virtual void on_toolbutton_copy_crystal_clicked(void);
    virtual void on_toolbutton_del_crystal_clicked(void);
    virtual void on_toolbutton_affiner_clicked(void);
    virtual bool on_treeViewReflections_key_press_event(GdkEventKey *);
    virtual void on_treeViewCrystals_cursor_changed(void);
    virtual bool on_treeViewCrystals_key_press_event(GdkEventKey *);

  protected:
    //Non-Signal handlers
    void updateSource(void);
    void updateAxes(void);
    void updatePseudoAxes(void);
    void updateLattice(void);
    void updateLatticeParameters(void);
    void updateReciprocalLattice(void);
    void updateFitness(void);
    void updateHKL(void);
    void updateTreeViewCrystals(void);
    void updateUB(void);
    void updateReflections(Glib::ustring const &, Glib::RefPtr<Gtk::ListStore> &);
    void updateStatusBar(HKLException const & ex);
    void updateAffinement(void);
    void updateCrystalModel(Glib::ustring const &);

  private:
    //variables
    Glib::RefPtr<Gnome::Glade::Xml> m_refGlade;
    // pointers on usefull widgets.
    Gtk::Label * m_label_UB11;
    Gtk::Label * m_label_UB12;
    Gtk::Label * m_label_UB13;
    Gtk::Label * m_label_UB21;
    Gtk::Label * m_label_UB22;
    Gtk::Label * m_label_UB23;
    Gtk::Label * m_label_UB31;
    Gtk::Label * m_label_UB32;
    Gtk::Label * m_label_UB33;
    Gtk::Label * m_label_fitness;
    Gtk::Label * m_label_nb_iterations;
    Gtk::SpinButton * m_spinbutton_a;
    Gtk::SpinButton * m_spinbutton_b;
    Gtk::SpinButton * m_spinbutton_c;
    Gtk::SpinButton * m_spinbutton_alpha;
    Gtk::SpinButton * m_spinbutton_beta;
    Gtk::SpinButton * m_spinbutton_gamma;
    Gtk::SpinButton * m_spinbutton_a_min;
    Gtk::SpinButton * m_spinbutton_b_min;
    Gtk::SpinButton * m_spinbutton_c_min;
    Gtk::SpinButton * m_spinbutton_alpha_min;
    Gtk::SpinButton * m_spinbutton_beta_min;
    Gtk::SpinButton * m_spinbutton_gamma_min;
    Gtk::SpinButton * m_spinbutton_a_max;
    Gtk::SpinButton * m_spinbutton_b_max;
    Gtk::SpinButton * m_spinbutton_c_max;
    Gtk::SpinButton * m_spinbutton_alpha_max;
    Gtk::SpinButton * m_spinbutton_beta_max;
    Gtk::SpinButton * m_spinbutton_gamma_max;
    Gtk::SpinButton * m_spinbutton_h;
    Gtk::SpinButton * m_spinbutton_k;
    Gtk::SpinButton * m_spinbutton_l;
    Gtk::SpinButton * m_spinbutton_lambda;
    Gtk::SpinButton * m_spinbutton_a_star;
    Gtk::SpinButton * m_spinbutton_b_star;
    Gtk::SpinButton * m_spinbutton_c_star;
    Gtk::SpinButton * m_spinbutton_alpha_star;
    Gtk::SpinButton * m_spinbutton_beta_star;
    Gtk::SpinButton * m_spinbutton_gamma_star;
    Gtk::SpinButton * m_spinbutton_max_iteration;
    Gtk::CheckButton * m_checkbutton_a;
    Gtk::CheckButton * m_checkbutton_b;
    Gtk::CheckButton * m_checkbutton_c;
    Gtk::CheckButton * m_checkbutton_alpha;
    Gtk::CheckButton * m_checkbutton_beta;
    Gtk::CheckButton * m_checkbutton_gamma;
    Gtk::CheckButton * m_checkbutton_U;
    Gtk::Button * m_button_goto_hkl;
    Gtk::TreeView * m_treeViewReflections;
    Gtk::TreeView * m_treeViewCrystals;
    Gtk::ToolButton * m_toolbutton_add_reflection;
    Gtk::ToolButton * m_toolbutton_goto_reflection;
    Gtk::ToolButton * m_toolbutton_del_reflection;
    Gtk::ToolButton * m_toolbutton_computeUB;
    Gtk::ToolButton * m_toolbutton_add_crystal;
    Gtk::ToolButton * m_toolbutton_copy_crystal;
    Gtk::ToolButton * m_toolbutton_del_crystal;
    Gtk::ToolButton * m_toolbutton_affiner;
    Gtk::Statusbar * m_statusBar;

    hkl::DiffractometerInterface * m_diffractometer;
    
    AxeSpinButtonList m_axeSpinButtonList;
    unsigned int m_nb_axes;
    unsigned int m_nb_sampleAxes;
    unsigned int m_nb_detectorAxes;
    vector<string> m_sampleAxesNames;
    vector<string> m_detectorAxesNames;

    PseudoAxeSpinButtonList m_pseudoAxeSpinButtonList;
    unsigned int m_nb_pseudoAxes;
    vector<string> m_pseudoAxesNames;

    Gtk::ComboBoxText m_comboboxentrytext_modes;
    Gtk::ComboBoxText m_comboboxentrytext_affinement;

    ReflectionModelColumns m_reflectionModelColumns;
    std::map<Glib::ustring, Glib::RefPtr<Gtk::ListStore> > m_mapReflectionModel;
    
    CrystalModelColumns m_crystalModelColumns;
    Glib::RefPtr<Gtk::ListStore> m_crystalModel;
    
    Gtk::MessageDialog * m_message;
};

#endif // GTKMM_HKL_WINDOW_H
