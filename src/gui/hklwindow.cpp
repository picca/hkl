//#include <iostream>
/*
#include <sigc++/bind.h>
#include <gtkmm/comboboxentry.h>
*/

#include "hklwindow.h"
#include "axespinbutton.h"

#include "constants.h"
#include "HKLException.h"

#include <gtkmm/notebook.h>



HKLWindow::HKLWindow(hkl::DiffractometerInterface * diffractometer)
: m_diffractometer(diffractometer)
{
    // Sets the border width of the window.
    set_border_width(10);

    //Get Glade UI:
    m_refGlade = Gnome::Glade::Xml::create("diffractometer2.glade");
    // Get all pointers on usefull widgets
    m_refGlade->get_widget("label_UB11", m_label_UB11);
    m_refGlade->get_widget("label_UB12", m_label_UB12);
    m_refGlade->get_widget("label_UB13", m_label_UB13);
    m_refGlade->get_widget("label_UB21", m_label_UB21);
    m_refGlade->get_widget("label_UB22", m_label_UB22);
    m_refGlade->get_widget("label_UB23", m_label_UB23);
    m_refGlade->get_widget("label_UB31", m_label_UB31);
    m_refGlade->get_widget("label_UB32", m_label_UB32);
    m_refGlade->get_widget("label_UB33", m_label_UB33);
    m_refGlade->get_widget("label_fitness", m_label_fitness);
    m_refGlade->get_widget("label_nb_iterations", m_label_nb_iterations);
    m_refGlade->get_widget("button_goto_hkl", m_button_goto_hkl);
    m_refGlade->get_widget("spinbutton_a_star", m_spinbutton_a_star);
    m_refGlade->get_widget("spinbutton_b_star", m_spinbutton_b_star);
    m_refGlade->get_widget("spinbutton_c_star", m_spinbutton_c_star);
    m_refGlade->get_widget("spinbutton_alpha_star", m_spinbutton_alpha_star);
    m_refGlade->get_widget("spinbutton_beta_star", m_spinbutton_beta_star);
    m_refGlade->get_widget("spinbutton_gamma_star", m_spinbutton_gamma_star);
    m_refGlade->get_widget("spinbutton_a", m_spinbutton_a);
    m_refGlade->get_widget("spinbutton_b", m_spinbutton_b);
    m_refGlade->get_widget("spinbutton_c", m_spinbutton_c);
    m_refGlade->get_widget("spinbutton_alpha", m_spinbutton_alpha);
    m_refGlade->get_widget("spinbutton_beta", m_spinbutton_beta);
    m_refGlade->get_widget("spinbutton_gamma", m_spinbutton_gamma);
    m_refGlade->get_widget("spinbutton_a_min", m_spinbutton_a_min);
    m_refGlade->get_widget("spinbutton_b_min", m_spinbutton_b_min);
    m_refGlade->get_widget("spinbutton_c_min", m_spinbutton_c_min);
    m_refGlade->get_widget("spinbutton_alpha_min", m_spinbutton_alpha_min);
    m_refGlade->get_widget("spinbutton_beta_min", m_spinbutton_beta_min);
    m_refGlade->get_widget("spinbutton_gamma_min", m_spinbutton_gamma_min);
    m_refGlade->get_widget("spinbutton_a_max", m_spinbutton_a_max);
    m_refGlade->get_widget("spinbutton_b_max", m_spinbutton_b_max);
    m_refGlade->get_widget("spinbutton_c_max", m_spinbutton_c_max);
    m_refGlade->get_widget("spinbutton_alpha_max", m_spinbutton_alpha_max);
    m_refGlade->get_widget("spinbutton_beta_max", m_spinbutton_beta_max);
    m_refGlade->get_widget("spinbutton_gamma_max", m_spinbutton_gamma_max);
    m_refGlade->get_widget("spinbutton_h", m_spinbutton_h);
    m_refGlade->get_widget("spinbutton_k", m_spinbutton_k);
    m_refGlade->get_widget("spinbutton_l", m_spinbutton_l);
    m_refGlade->get_widget("spinbutton_lambda", m_spinbutton_lambda);
    m_refGlade->get_widget("spinbutton_max_iteration", m_spinbutton_max_iteration);
    m_refGlade->get_widget("checkbutton_a", m_checkbutton_a);
    m_refGlade->get_widget("checkbutton_b", m_checkbutton_b);
    m_refGlade->get_widget("checkbutton_c", m_checkbutton_c);
    m_refGlade->get_widget("checkbutton_alpha", m_checkbutton_alpha);
    m_refGlade->get_widget("checkbutton_beta", m_checkbutton_beta);
    m_refGlade->get_widget("checkbutton_gamma", m_checkbutton_gamma);
    m_refGlade->get_widget("checkbutton_U", m_checkbutton_U);
    m_refGlade->get_widget("treeview_reflections", m_treeViewReflections);
    m_refGlade->get_widget("treeview_crystals", m_treeViewCrystals);
    m_refGlade->get_widget("toolbutton_add_reflection", m_toolbutton_add_reflection);
    m_refGlade->get_widget("toolbutton_goto_reflection", m_toolbutton_goto_reflection);
    m_refGlade->get_widget("toolbutton_del_reflection", m_toolbutton_del_reflection);
    m_refGlade->get_widget("toolbutton_computeUB", m_toolbutton_computeUB);
    m_refGlade->get_widget("toolbutton_add_crystal", m_toolbutton_add_crystal);
    m_refGlade->get_widget("toolbutton_copy_crystal", m_toolbutton_copy_crystal);
    m_refGlade->get_widget("toolbutton_del_crystal", m_toolbutton_del_crystal);
    m_refGlade->get_widget("toolbutton_affiner", m_toolbutton_affiner);
    m_refGlade->get_widget("statusbar", m_statusBar);

    // fill the comboboxentrytext with the modes.
    vector<string> modeNames = m_diffractometer->getModeNames();
    vector<string>::iterator iter = modeNames.begin();
    vector<string>::const_iterator end = modeNames.end();
    while(iter != end)
      {
        m_comboboxentrytext_modes.append_text(*iter);
        ++iter;
      }
    // set active the correct mode.
    try
      {
        string name = m_diffractometer->getCurrentModeName();
        m_comboboxentrytext_modes.set_active_text(name);
      }
    catch (HKLException const & ex)
      { }
    m_comboboxentrytext_modes.signal_changed().connect(mem_fun(*this, &HKLWindow::on_comboboxentrytext_modes_changed));
    Gtk::HBox * phbox = NULL;
    m_refGlade->get_widget("hbox_modes", phbox);
    phbox->pack_start(m_comboboxentrytext_modes, Gtk::PACK_SHRINK);
    phbox->reorder_child(m_comboboxentrytext_modes, 1);
    phbox->show_all();

    // fill the comboboxentrytext with the affinement.
    vector<string> affinementNames = m_diffractometer->getAffinementNames();
    iter = affinementNames.begin();
    end = affinementNames.end();
    while(iter != end)
      {
        m_comboboxentrytext_affinement.append_text(*iter);
        ++iter;
      }
    m_comboboxentrytext_affinement.signal_changed().connect(mem_fun(*this, &HKLWindow::on_comboboxentrytext_affinement_changed));
    Gtk::Table * ptable = NULL;
    m_refGlade->get_widget("table_affinement", ptable);
    ptable->attach(m_comboboxentrytext_affinement, 1, 2, 0, 1, Gtk::FILL, Gtk::FILL);
    ptable->show_all();

    // Add the axes widgets.
    m_axesNames = m_diffractometer->getAxesNames();
    m_nb_axes = m_axesNames.size();
    m_refGlade->get_widget("table_axes", ptable);
    ptable->resize(m_nb_axes, 2);

    if (ptable)
      {
        unsigned int i;
        for(i=0;i<m_nb_axes;i++)
          {
            std::string name(m_axesNames[i]);
            hkl::Axe & axe = m_diffractometer->getAxe(name);
            Gtk::Label * axeLabel = manage( new Gtk::Label(name + ":", Gtk::ALIGN_LEFT));
            AxeSpinButton * axeSpinButton = manage( new AxeSpinButton(axe));
            m_axeSpinButtonList.push_back(axeSpinButton);
            ptable->attach(*axeLabel, 0, 1, i, i+1, Gtk::FILL);
            ptable->attach(*axeSpinButton, 1, 2, i, i+1, Gtk::FILL);
            axeSpinButton->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_value_changed));
          }
      }
    ptable->show_all();


    //Set up the treeViewReflections
    m_treeViewReflections->append_column("index", m_reflectionModelColumns.index);

    int index = m_treeViewReflections->append_column_numeric_editable("h", m_reflectionModelColumns.h, "%lf");
    Gtk::CellRenderer * renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_h_edited));

    index = m_treeViewReflections->append_column_numeric_editable("k", m_reflectionModelColumns.k, "%lf");
    renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_k_edited));

    index = m_treeViewReflections->append_column_numeric_editable("l", m_reflectionModelColumns.l, "%lf");
    renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_l_edited));

    index = m_treeViewReflections->append_column_editable("flag", m_reflectionModelColumns.flag);
    renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_flag_toggled));

    m_treeViewReflections->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

    //Set up the treeViewCrystals
    index = m_treeViewCrystals->append_column_editable("name", m_crystalModelColumns.name);
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_name_edited));

    index = m_treeViewCrystals->append_column_numeric_editable("a", m_crystalModelColumns.a, "%lf");
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_a_edited));

    index = m_treeViewCrystals->append_column_numeric_editable("b", m_crystalModelColumns.b, "%lf");
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_b_edited));

    index = m_treeViewCrystals->append_column_numeric_editable("c", m_crystalModelColumns.c, "%lf");
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_c_edited));

    index = m_treeViewCrystals->append_column_numeric_editable("alpha", m_crystalModelColumns.alpha, "%lf");
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_alpha_edited));

    index = m_treeViewCrystals->append_column_numeric_editable("beta", m_crystalModelColumns.beta, "%lf");
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_beta_edited));

    index = m_treeViewCrystals->append_column_numeric_editable("gamma", m_crystalModelColumns.gamma, "%lf");
    renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
    dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_gamma_edited));

    m_treeViewCrystals->append_column("fitness", m_crystalModelColumns.fitness);

    m_treeViewCrystals->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

    //update the widgets
    updateTreeViewCrystals();
    updateSource();
    updateLattice();
    updateLatticeParameters();
    updateReciprocalLattice();
    updateFitness();
    updateUB();
    updateHKL();
    updateAffinement();

    //signal connection
    m_spinbutton_a->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_a_value_changed));
    m_spinbutton_b->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_b_value_changed));
    m_spinbutton_c->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_c_value_changed));
    m_spinbutton_alpha->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_alpha_value_changed));
    m_spinbutton_beta->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_beta_value_changed));
    m_spinbutton_gamma->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_gamma_value_changed));
    m_spinbutton_a_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_a_min_value_changed));
    m_spinbutton_b_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_b_min_value_changed));
    m_spinbutton_c_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_c_min_value_changed));
    m_spinbutton_alpha_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_alpha_min_value_changed));
    m_spinbutton_beta_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_beta_min_value_changed));
    m_spinbutton_gamma_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_gamma_min_value_changed));
    m_spinbutton_a_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_a_max_value_changed));
    m_spinbutton_b_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_b_max_value_changed));
    m_spinbutton_c_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_c_max_value_changed));
    m_spinbutton_alpha_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_alpha_max_value_changed));
    m_spinbutton_beta_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_beta_max_value_changed));
    m_spinbutton_gamma_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_gamma_max_value_changed));
    m_spinbutton_lambda->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_lambda_value_changed));
    m_spinbutton_max_iteration->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_max_iteration_value_changed));

    m_checkbutton_a->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_a_toggled));
    m_checkbutton_b->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_b_toggled));
    m_checkbutton_c->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_c_toggled));
    m_checkbutton_alpha->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_alpha_toggled));
    m_checkbutton_beta->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_beta_toggled));
    m_checkbutton_gamma->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_gamma_toggled));
    m_checkbutton_U->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_U_toggled));

    m_button_goto_hkl->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_button_goto_hkl_clicked));

    m_treeViewReflections->signal_key_press_event().connect(mem_fun(*this, &HKLWindow::on_treeViewReflections_key_press_event));
    m_treeViewCrystals->signal_cursor_changed().connect(mem_fun(*this, &HKLWindow::on_treeViewCrystals_cursor_changed));
    m_treeViewCrystals->signal_key_press_event().connect(mem_fun(*this, &HKLWindow::on_treeViewCrystals_key_press_event));

    m_toolbutton_add_reflection->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_add_reflection_clicked));
    m_toolbutton_goto_reflection->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_goto_reflection_clicked));
    m_toolbutton_del_reflection->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_del_reflection_clicked));
    m_toolbutton_computeUB->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_computeUB_clicked));
    m_toolbutton_add_crystal->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_add_crystal_clicked));
    m_toolbutton_copy_crystal->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_copy_crystal_clicked));
    m_toolbutton_del_crystal->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_del_crystal_clicked));
    m_toolbutton_affiner->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_affiner_clicked));

    show_all_children();

}

HKLWindow::~HKLWindow()
{
}

// Callback
void
HKLWindow::on_comboboxentrytext_modes_changed(void)
{
    m_diffractometer->setCurrentMode(m_comboboxentrytext_modes.get_active_text());
}

void
HKLWindow::on_treeViewCrystals_cursor_changed(void)
{
    Gtk::TreeModel::Path path;
    Gtk::TreeViewColumn * column;
    m_treeViewCrystals->get_cursor(path, column);
    Gtk::TreeModel::iterator iter = m_crystalModel->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    
    Glib::ustring name = row[m_crystalModelColumns.name];
    m_diffractometer->setCurrentCrystal(name);
    m_treeViewReflections->set_model(m_mapReflectionModel[name]);
    updateLattice();
    updateLatticeParameters();
    updateReciprocalLattice();
    updateFitness();
    updateHKL();
}

void
HKLWindow::on_comboboxentrytext_affinement_changed(void)
{
    Glib::ustring name = m_comboboxentrytext_affinement.get_active_text();
    try
      {
        updateAffinement();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_a_value_changed(void)
{
    double a, b, c, alpha, beta, gamma;
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        double na = m_spinbutton_a->get_value();
        m_diffractometer->setCrystalLattice(name, na, b, c, alpha, beta, gamma);
        updateCrystalModel(name);
        updateReciprocalLattice();
        updateFitness();
        updateUB();
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_b_value_changed(void)
{
    double a, b, c, alpha, beta, gamma;
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        double nb = m_spinbutton_b->get_value();
        m_diffractometer->setCrystalLattice(name, a, nb, c, alpha, beta, gamma);
        updateCrystalModel(name);
        updateReciprocalLattice();
        updateFitness();
        updateUB();
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_c_value_changed(void)
{
    double a, b, c, alpha, beta, gamma;
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        double nc = m_spinbutton_c->get_value();
        m_diffractometer->setCrystalLattice(name, a, b, nc, alpha, beta, gamma);
        updateCrystalModel(name);
        updateReciprocalLattice();
        updateFitness();
        updateUB();
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_alpha_value_changed(void)
{
    double a, b, c, alpha, beta, gamma;
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        double nalpha = m_spinbutton_alpha->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalLattice(name, a, b, c, nalpha, beta, gamma);
        updateCrystalModel(name);
        updateReciprocalLattice();
        updateFitness();
        updateUB();
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_beta_value_changed(void)
{
    double a, b, c, alpha, beta, gamma;
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        double nbeta = m_spinbutton_beta->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, nbeta, gamma);
        updateCrystalModel(name);
        updateReciprocalLattice();
        updateFitness();
        updateUB();
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_gamma_value_changed(void)
{
    double a, b, c, alpha, beta, gamma;
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        double ngamma = m_spinbutton_gamma->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, beta, ngamma);
        updateCrystalModel(name);
        updateReciprocalLattice();
        updateFitness();
        updateUB();
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_a_min_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "a", &value, &min, &max, &to_fit);
        min = m_spinbutton_a_min->get_value();
        m_diffractometer->setCrystalParameterValues(name, "a", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_b_min_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "b", &value, &min, &max, &to_fit);
        min = m_spinbutton_b_min->get_value();
        m_diffractometer->setCrystalParameterValues(name, "b", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_c_min_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "c", &value, &min, &max, &to_fit);
        min = m_spinbutton_c_min->get_value();
        m_diffractometer->setCrystalParameterValues(name, "c", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_alpha_min_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "alpha", &value, &min, &max, &to_fit);
        min = m_spinbutton_alpha_min->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalParameterValues(name, "alpha", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_beta_min_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "beta", &value, &min, &max, &to_fit);
        min = m_spinbutton_beta_min->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalParameterValues(name, "beta", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_gamma_min_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "gamma", &value, &min, &max, &to_fit);
        min = m_spinbutton_gamma_min->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalParameterValues(name, "gamma", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_a_max_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "a", &value, &min, &max, &to_fit);
        max = m_spinbutton_a_max->get_value();
        m_diffractometer->setCrystalParameterValues(name, "a", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_b_max_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "b", &value, &min, &max, &to_fit);
        max = m_spinbutton_b_max->get_value();
        m_diffractometer->setCrystalParameterValues(name, "b", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_c_max_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "c", &value, &min, &max, &to_fit);
        max = m_spinbutton_c_max->get_value();
        m_diffractometer->setCrystalParameterValues(name, "c", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_alpha_max_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "alpha", &value, &min, &max, &to_fit);
        max = m_spinbutton_alpha_max->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalParameterValues(name, "alpha", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_beta_max_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "beta", &value, &min, &max, &to_fit);
        max = m_spinbutton_beta_max->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalParameterValues(name, "beta", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_gamma_max_value_changed(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "gamma", &value, &min, &max, &to_fit);
        max = m_spinbutton_gamma_max->get_value() * hkl::constant::math::degToRad;
        m_diffractometer->setCrystalParameterValues(name, "gamma", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_lambda_value_changed(void)
{
    try
      {
        double lambda = m_spinbutton_lambda->get_value();
        m_diffractometer->setWaveLength(lambda);
        updateHKL();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_spinbutton_max_iteration_value_changed(void)
{
    string name;
    try
      {
        name = m_comboboxentrytext_affinement.get_active_text();
        unsigned int max = (unsigned int) m_spinbutton_max_iteration->get_value();
        m_diffractometer->setAffinementMaxIteration(name, max);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }

}

void
HKLWindow::on_checkbutton_a_toggled(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "a", &value, &min, &max, &to_fit);
        to_fit = m_checkbutton_a->get_active();
        m_diffractometer->setCrystalParameterValues(name, "a", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_checkbutton_b_toggled(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "b", &value, &min, &max, &to_fit);
        to_fit = m_checkbutton_b->get_active();
        m_diffractometer->setCrystalParameterValues(name, "b", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_checkbutton_c_toggled(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "c", &value, &min, &max, &to_fit);
        to_fit = m_checkbutton_c->get_active();
        m_diffractometer->setCrystalParameterValues(name, "c", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_checkbutton_alpha_toggled(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "alpha", &value, &min, &max, &to_fit);
        to_fit = m_checkbutton_alpha->get_active();
        m_diffractometer->setCrystalParameterValues(name, "alpha", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_checkbutton_beta_toggled(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "beta", &value, &min, &max, &to_fit);
        to_fit = m_checkbutton_beta->get_active();
        m_diffractometer->setCrystalParameterValues(name, "beta", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_checkbutton_gamma_toggled(void)
{
    string name;
    double value, min, max;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "gamma", &value, &min, &max, &to_fit);
        to_fit = m_checkbutton_gamma->get_active();
        m_diffractometer->setCrystalParameterValues(name, "gamma", value, min, max, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_checkbutton_U_toggled(void)
{
    string name;
    double value_x, min_x, max_x;
    double value_y, min_y, max_y;
    double value_z, min_z, max_z;
    bool to_fit;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalParameterValues(name, "euler_x", &value_x, &min_x, &max_x, &to_fit);
        m_diffractometer->getCrystalParameterValues(name, "euler_y", &value_y, &min_y, &max_y, &to_fit);
        m_diffractometer->getCrystalParameterValues(name, "euler_z", &value_z, &min_z, &max_z, &to_fit);
        to_fit = m_checkbutton_U->get_active();
        m_diffractometer->setCrystalParameterValues(name, "euler_x", value_x, min_x, max_x, to_fit);
        m_diffractometer->setCrystalParameterValues(name, "euler_y", value_y, min_y, max_y, to_fit);
        m_diffractometer->setCrystalParameterValues(name, "euler_z", value_z, min_z, max_z, to_fit);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_button_goto_hkl_clicked(void)
{
    try
      {
        double h, k, l;
        h = m_spinbutton_h->get_value();
        k = m_spinbutton_k->get_value();
        l = m_spinbutton_l->get_value();
        m_diffractometer->computeAngles(h, k, l);
        updateAxes();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_axeSpinButton_value_changed(void)
{
    updateHKL();
}

void
HKLWindow::on_cell_name_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        m_diffractometer->renameCrystal(name, newText);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
      updateTreeViewCrystals();
}

void
HKLWindow::on_cell_a_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        sscanf(newText.c_str(), "%lf", &a);
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, beta, gamma);
        row[m_crystalModelColumns.a] = a;
        m_spinbutton_a->set_value(a);
        row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_b_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        sscanf(newText.c_str(), "%lf", &b);
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, beta, gamma);
        row[m_crystalModelColumns.b] = b;
        m_spinbutton_b->set_value(b);
        row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_c_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        sscanf(newText.c_str(), "%lf", &c);
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, beta, gamma);
        row[m_crystalModelColumns.c] = c;
        m_spinbutton_c->set_value(c);
        row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_alpha_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        sscanf(newText.c_str(), "%lf", &alpha);
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha * hkl::constant::math::degToRad, beta, gamma);
        row[m_crystalModelColumns.alpha] = alpha;
        m_spinbutton_alpha->set_value(alpha);
        row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_beta_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        sscanf(newText.c_str(), "%lf", &beta);
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, beta * hkl::constant::math::degToRad, gamma);
        row[m_crystalModelColumns.beta] = beta;
        m_spinbutton_beta->set_value(beta);
        row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_gamma_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        Glib::ustring name = row[m_crystalModelColumns.name];
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        sscanf(newText.c_str(), "%lf", &gamma);
        m_diffractometer->setCrystalLattice(name, a, b, c, alpha, beta, gamma * hkl::constant::math::degToRad);
        row[m_crystalModelColumns.gamma] = gamma;
        m_spinbutton_gamma->set_value(gamma);
        row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_h_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        int index = row[m_reflectionModelColumns.index];
        double h, k, l;
        int relevance;
        bool flag;
        m_diffractometer->getCrystalReflectionParameters(name, index, &h, &k, &l, &relevance, &flag);
        cout << " " << newText << " " << h;
        sscanf(newText.c_str(), "%lf", &h);
        cout << " " << h << endl;
        m_diffractometer->setCrystalReflectionParameters(name, index, h, k, l, relevance, flag);
        row[m_reflectionModelColumns.h] = h;
        row[m_reflectionModelColumns.flag] = flag;
        updateCrystalModel(name);
        updateFitness();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_k_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        int index = row[m_reflectionModelColumns.index];
        double h, k, l;
        int relevance;
        bool flag;
        m_diffractometer->getCrystalReflectionParameters(name, index, &h, &k, &l, &relevance, &flag);
        sscanf(newText.c_str(), "%lf", &k);
        m_diffractometer->setCrystalReflectionParameters(name, index, h, k, l, relevance, flag);
        row[m_reflectionModelColumns.k] = k;
        row[m_reflectionModelColumns.flag] = flag;
        updateCrystalModel(name);
        updateFitness();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_l_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        int index = row[m_reflectionModelColumns.index];
        double h, k, l;
        int relevance;
        bool flag;
        m_diffractometer->getCrystalReflectionParameters(name, index, &h, &k, &l, &relevance, &flag);
        sscanf(newText.c_str(), "%lf", &l);
        m_diffractometer->setCrystalReflectionParameters(name, index, h, k, l, relevance, flag);
        row[m_reflectionModelColumns.l] = l;
        row[m_reflectionModelColumns.flag] = flag;
        updateCrystalModel(name);
        updateFitness();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_cell_flag_toggled(Glib::ustring const & spath)
{
    Gtk::TreePath path(spath);
    Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model(); 
    Gtk::TreeModel::iterator iter = listStore->get_iter(path);
    Gtk::ListStore::Row row = *(iter);
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        int index = row[m_reflectionModelColumns.index];
        double h, k, l;
        int relevance;
        bool flag;
        m_diffractometer->getCrystalReflectionParameters(name, index, &h, &k, &l, &relevance, &flag);
        flag = !flag;
        m_diffractometer->setCrystalReflectionParameters(name, index, h, k, l, relevance, flag);
        row[m_reflectionModelColumns.flag] = flag;
        updateFitness();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_toolbutton_add_reflection_clicked(void)
{
    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        bool flag;
        unsigned int index;
        int relevance;
        double h, k, l;
        h = m_spinbutton_h->get_value();
        k = m_spinbutton_k->get_value();
        l = m_spinbutton_l->get_value();
        index = m_diffractometer->addCrystalReflection(name, h, k, l, 0, true);
        updateReflections(name, m_mapReflectionModel[name]);
        updateFitness();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::on_toolbutton_goto_reflection_clicked(void)
{
    string name;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
        return;
      }

    Glib::RefPtr<Gtk::TreeSelection> selection = m_treeViewReflections->get_selection();
    unsigned int nb_rows = selection->count_selected_rows();
    if (nb_rows == 1)
      {
        Gtk::TreeSelection::ListHandle_Path list_path = selection->get_selected_rows();
        Gtk::TreePath path = *(list_path.begin());
        Glib::RefPtr<Gtk::ListStore> liststore = m_mapReflectionModel[name];
        Gtk::ListStore::Row row = *(liststore->get_iter(path));
        unsigned int index = row[m_reflectionModelColumns.index];
        try
          {
            m_diffractometer->setAxesFromCrystalReflection(name, index);
          }
        catch (HKLException const & ex)
          {
            updateStatusBar(ex);
            return;
          }
        updateSource();
        updateAxes();
        updateHKL();
      }
    else
      {
        if (nb_rows)
            m_statusBar->push("Please select only one reflection.");
        else
            m_statusBar->push("Please select one reflection.");
      }
}

void
HKLWindow::on_toolbutton_del_reflection_clicked(void)
{
    string name;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
        return;
      }

    Glib::RefPtr<Gtk::TreeSelection> selection = m_treeViewReflections->get_selection();
    unsigned int nb_rows = selection->count_selected_rows();
    if (nb_rows)
      {
        Gtk::TreeSelection::ListHandle_Path list = selection->get_selected_rows();
        Gtk::TreeSelection::ListHandle_Path::iterator iter = list.begin();
        Gtk::TreeSelection::ListHandle_Path::iterator last = list.end();
        Glib::RefPtr<Gtk::ListStore> liststore = m_mapReflectionModel[name];
        // fill indexes with the reflections index
        vector<unsigned int> indexes;
        while(iter != last)
          {
            Gtk::ListStore::Row row = *(liststore->get_iter(*iter));
            indexes.push_back(row[m_reflectionModelColumns.index]);
            ++iter;
          }
        ostringstream os;
        os << "Are you sure you want to delete reflections :";
        for(unsigned int i=0; i< indexes.size();i++)
          {
            os << " " << indexes[i];
          }
        m_message = new Gtk::MessageDialog("", false, Gtk::MESSAGE_WARNING, Gtk::BUTTONS_YES_NO);
        m_message->set_message(os.str());
        m_message->show();
        int respons = m_message->run();
        switch (respons)
          {
          case Gtk::RESPONSE_YES:
            for(unsigned int i=0;i<indexes.size();i++)
              {
                // compute the correct index of the reflection
                unsigned int index = indexes[i] - i;
                try
                  {
                    m_diffractometer->delCrystalReflection(name, index);
                  }
                catch (HKLException const & ex)
                  {
                    updateStatusBar(ex);
                    return;
                  }
              }
            updateReflections(name, liststore);
            break;
          }
        delete m_message;
      }
    else
        m_statusBar->push("Please select at least one reflection.");
    updateFitness();
}

void
HKLWindow::on_toolbutton_computeUB_clicked(void)
{
    try
      {
        m_diffractometer->computeU();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
        return;
      }
    updateUB();
    updateHKL();
}

void
HKLWindow::on_toolbutton_add_crystal_clicked(void)
{
    try
      {
        m_diffractometer->addNewCrystal("new_crystal");
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
        return;
      }

    m_diffractometer->setCurrentCrystal("new_crystal");
    updateTreeViewCrystals();
    // activate for edition the name of the new crystal
    Gtk::TreeModel::Path path;
    Gtk::TreeView::Column * column;
    m_treeViewCrystals->get_cursor(path, column);
    column = m_treeViewCrystals->get_column(0);
    m_treeViewCrystals->set_cursor(path, *column, true);
}

void
HKLWindow::on_toolbutton_copy_crystal_clicked(void)
{
    Glib::ustring name;
    Glib::ustring newname;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        newname = name + " copy";
        m_diffractometer->copyCrystalAsNew(name, newname);
      }
    catch (HKLException const & ex)
      {
        m_statusBar->push("Please select a crystal to copy.");
        return;
      }

    m_diffractometer->setCurrentCrystal(newname);
    updateTreeViewCrystals();
    // activate for edition the name of the new crystal
    Gtk::TreeModel::Path path;
    Gtk::TreeView::Column * column;
    m_treeViewCrystals->get_cursor(path, column);
    column = m_treeViewCrystals->get_column(0);
    m_treeViewCrystals->set_cursor(path, *column, false);
}

void
HKLWindow::on_toolbutton_del_crystal_clicked(void)
{
    Glib::ustring name;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->delCrystal(name);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
        return;
      }

    updateTreeViewCrystals();
}

void
HKLWindow::on_toolbutton_affiner_clicked(void)
{
    Glib::ustring name;
    Glib::ustring method;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        method = m_comboboxentrytext_affinement.get_active_text();
        m_diffractometer->affineCrystal(name, method);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
        return;
      }
      updateCrystalModel(name);
      updateFitness();
      updateLattice();
      updateReciprocalLattice();
      updateHKL();
}

bool
HKLWindow::on_treeViewReflections_key_press_event(GdkEventKey * event)
{
    switch (event->keyval)
      {
      case GDK_Insert:
      case GDK_KP_Insert:
        on_toolbutton_add_reflection_clicked();
        break;
      case GDK_Delete:
      case GDK_KP_Delete:
        on_toolbutton_del_reflection_clicked();
        break;
      }
}

bool
HKLWindow::on_treeViewCrystals_key_press_event(GdkEventKey * event)
{
    switch (event->keyval)
      {
      case GDK_Insert:
      case GDK_KP_Insert:
        on_toolbutton_add_crystal_clicked();
        break;
      case GDK_Delete:
      case GDK_KP_Delete:
        on_toolbutton_del_crystal_clicked();
        break;
      }
}

// Non-Callback
hkl::Axe &
HKLWindow::get_axe(Glib::ustring const & name)
{
    return m_diffractometer->getAxe(name);
}

void
HKLWindow::updateSource(void)
{
    double a, b, c, alpha, beta, gamma;

    try
      {
        double lambda = m_diffractometer->getWaveLength();
        m_spinbutton_lambda->set_value(lambda);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::updateAxes(void)
{
    AxeSpinButtonList::iterator iter = m_axeSpinButtonList.begin();
    AxeSpinButtonList::iterator last = m_axeSpinButtonList.end();
    while(iter != last)
      {
        // decoonect when updating to avoid computation of hkl 
        (*iter)->unconnect();
        (*iter)->update();
        (*iter)->connect();
        ++iter;
      }
}

void
HKLWindow::updateLattice(void)
{
    double a, b, c, alpha, beta, gamma;

    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        m_spinbutton_a->set_value(a);
        m_spinbutton_b->set_value(b);
        m_spinbutton_c->set_value(c);
        m_spinbutton_alpha->set_value(alpha * hkl::constant::math::radToDeg);
        m_spinbutton_beta->set_value(beta * hkl::constant::math::radToDeg);
        m_spinbutton_gamma->set_value(gamma * hkl::constant::math::radToDeg);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::updateLatticeParameters(void)
{
    double value, min, max;
    bool to_fit;
    string name;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
    m_diffractometer->getCrystalParameterValues(name, "a", &value, &min, &max, &to_fit);
    m_spinbutton_a_min->set_value(min);
    m_spinbutton_a_max->set_value(max);
    m_checkbutton_a->set_active(to_fit);
    m_diffractometer->getCrystalParameterValues(name, "b", &value, &min, &max, &to_fit);
    m_spinbutton_b_min->set_value(min);
    m_spinbutton_b_max->set_value(max);
    m_checkbutton_b->set_active(to_fit);
    m_diffractometer->getCrystalParameterValues(name, "c", &value, &min, &max, &to_fit);
    m_spinbutton_c_min->set_value(min);
    m_spinbutton_c_max->set_value(max);
    m_checkbutton_c->set_active(to_fit);
    m_diffractometer->getCrystalParameterValues(name, "alpha", &value, &min, &max, &to_fit);
    m_spinbutton_alpha_min->set_value(min * hkl::constant::math::radToDeg);
    m_spinbutton_alpha_max->set_value(max * hkl::constant::math::radToDeg);
    m_checkbutton_alpha->set_active(to_fit);
    m_diffractometer->getCrystalParameterValues(name, "beta", &value, &min, &max, &to_fit);
    m_spinbutton_beta_min->set_value(min * hkl::constant::math::radToDeg);
    m_spinbutton_beta_max->set_value(max * hkl::constant::math::radToDeg);
    m_checkbutton_beta->set_active(to_fit);
    m_diffractometer->getCrystalParameterValues(name, "gamma", &value, &min, &max, &to_fit);
    m_spinbutton_gamma_min->set_value(min * hkl::constant::math::radToDeg);
    m_spinbutton_gamma_max->set_value(max * hkl::constant::math::radToDeg);
    m_checkbutton_gamma->set_active(to_fit);

    //compute the state of the checkbutton_U
    double value2, min2, max2;
    double value3, min3, max3;
    bool to_fit2, to_fit3;
    m_diffractometer->getCrystalParameterValues(name, "euler_x", &value, &min, &max, &to_fit);
    m_diffractometer->getCrystalParameterValues(name, "euler_y", &value2, &min2, &max2, &to_fit2);
    m_diffractometer->getCrystalParameterValues(name, "euler_z", &value2, &min3, &max3, &to_fit3);
    if (to_fit && to_fit2 && to_fit3)
        m_checkbutton_U->set_active(true);
    else
      {
        m_diffractometer->setCrystalParameterValues(name, "euler_x", value, min, max, false);
        m_diffractometer->setCrystalParameterValues(name, "euler_y", value2, min2, max2, false);
        m_diffractometer->setCrystalParameterValues(name, "euler_z", value2, min3, max3, false);
        m_checkbutton_U->set_active(false);
      }
}

void
HKLWindow::updateReciprocalLattice(void)
{
    double a, b, c, alpha, beta, gamma;

    try
      {
        string name = m_diffractometer->getCurrentCrystalName();
        m_diffractometer->getCrystalReciprocalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
        m_spinbutton_a_star->set_value(a);
        m_spinbutton_b_star->set_value(b);
        m_spinbutton_c_star->set_value(c);
        m_spinbutton_alpha_star->set_value(alpha * hkl::constant::math::radToDeg);
        m_spinbutton_beta_star->set_value(beta * hkl::constant::math::radToDeg);
        m_spinbutton_gamma_star->set_value(gamma * hkl::constant::math::radToDeg);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::updateUB(void)
{
    try{
        string name = m_diffractometer->getCurrentCrystalName();
        hkl::smatrix UB = m_diffractometer->getCrystal_UB(name);
        m_label_UB11->set_text(Glib::Ascii::dtostr(UB.get(0,0)));
        m_label_UB12->set_text(Glib::Ascii::dtostr(UB.get(0,1)));
        m_label_UB13->set_text(Glib::Ascii::dtostr(UB.get(0,2)));
        m_label_UB21->set_text(Glib::Ascii::dtostr(UB.get(1,0)));
        m_label_UB22->set_text(Glib::Ascii::dtostr(UB.get(1,1)));
        m_label_UB23->set_text(Glib::Ascii::dtostr(UB.get(1,2)));
        m_label_UB31->set_text(Glib::Ascii::dtostr(UB.get(2,0)));
        m_label_UB32->set_text(Glib::Ascii::dtostr(UB.get(2,1)));
        m_label_UB33->set_text(Glib::Ascii::dtostr(UB.get(2,2)));
    }
    catch(HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::updateHKL(void)
{
    double h, k, l;
    try
      {
        m_diffractometer->computeHKL(h, k, l);
        m_spinbutton_h->set_value(h);
        m_spinbutton_k->set_value(k);
        m_spinbutton_l->set_value(l);
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }

}

void
HKLWindow::updateTreeViewCrystals(void)
{
    Gtk::ListStore::Row row;
    Gtk::TreeModel::Children::iterator iter_row;
    Gtk::TreeModel::Children::iterator iter_current;
    Glib::ustring current_crystal_name;

    bool is_current_crystal_set = false;

    //clear all data in the models
    m_crystalModel = Gtk::ListStore::create(m_crystalModelColumns);

    // erase all reflections.
    m_mapReflectionModel.clear();

    // get the current Crystal name
    try
      {
        current_crystal_name = m_diffractometer->getCurrentCrystalName();
        is_current_crystal_set = true;
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }

    //Fill the models from the crystalList
    vector<string> crystalNames = m_diffractometer->getCrystalNames();
    vector<string>::iterator iter = crystalNames.begin();
    vector<string>::const_iterator const & end = crystalNames.end();
    while(iter != end)
      {
        double a, b, c, alpha, beta, gamma;
        m_diffractometer->getCrystalLattice(*iter, &a, &b, &c, &alpha, &beta, &gamma);
        iter_row = *(m_crystalModel->append());
        if (is_current_crystal_set && current_crystal_name == *iter)
            iter_current = iter_row;
        row = *(iter_row);
        row[m_crystalModelColumns.name] = *iter;
        row[m_crystalModelColumns.a] = a;
        row[m_crystalModelColumns.b] = b;
        row[m_crystalModelColumns.c] = c;
        row[m_crystalModelColumns.alpha] = alpha * hkl::constant::math::radToDeg;
        row[m_crystalModelColumns.beta] = beta * hkl::constant::math::radToDeg;
        row[m_crystalModelColumns.gamma] = gamma * hkl::constant::math::radToDeg;
        try
          {
            row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(*iter);
          }
        catch (HKLException const & ex)
          {
            row[m_crystalModelColumns.fitness] = NAN;
            updateStatusBar(ex);
          }

        Glib::RefPtr<Gtk::ListStore> listStore = Gtk::ListStore::create(m_reflectionModelColumns);
        m_mapReflectionModel[*iter] = listStore;
        updateReflections(*iter, listStore);
        ++iter;
      }

    //Set the model for the treeview
    m_treeViewCrystals->set_model(m_crystalModel);
    if (is_current_crystal_set)
      {
        Gtk::TreeModel::Path path = m_crystalModel->get_path(iter_current);
        m_treeViewCrystals->set_cursor(path);
        m_treeViewReflections->set_model(m_mapReflectionModel[current_crystal_name]);
      }
}

void
HKLWindow::updateReflections(Glib::ustring const & name, Glib::RefPtr<Gtk::ListStore> & listStore)
{
    listStore->clear();
    unsigned int nb_reflections = m_diffractometer->getCrystalNumberOfReflection(name);
    double h, k, l;
    Gtk::ListStore::Row row;
    int relevance;
    bool flag;
    for(unsigned int i=0;i<nb_reflections;i++)
      {
        m_diffractometer->getCrystalReflectionParameters(name, i, &h, &k, &l, &relevance, &flag);
        row = *(listStore->append());
        row[m_reflectionModelColumns.index] = i;
        row[m_reflectionModelColumns.h] = h;
        row[m_reflectionModelColumns.k] = k;
        row[m_reflectionModelColumns.l] = l;
        row[m_reflectionModelColumns.flag] = flag;
      }
}

void
HKLWindow::updateStatusBar(HKLException const & ex)
{
    m_statusBar->push(ex.errors[0].reason + " " + ex.errors[0].desc + " " + ex.errors[0].origin);
}

void
HKLWindow::updateFitness(void)
{
    Glib::ustring name;
    try
      {
        name = m_diffractometer->getCurrentCrystalName();
        double fitness = m_diffractometer->getCrystalFitness(name);
        ostringstream os;
        os << fitness;
        m_label_fitness->set_text(os.str());
      }
    catch (HKLException const & ex)
      {
        m_label_fitness->set_text("xxx");
        updateStatusBar(ex);
      }
}

void
HKLWindow::updateAffinement(void)
{
    Glib::ustring name;
    name = m_comboboxentrytext_affinement.get_active_text();
    try
      {
        // update the max iteration
        unsigned int max = m_diffractometer->getAffinementMaxIteration(name);
        m_spinbutton_max_iteration->set_value(max);
        // update the nb iteration
        unsigned int nb_iterations = m_diffractometer->getAffinementIterations(name);
        ostringstream os;
        os << nb_iterations;
        m_label_nb_iterations->set_text(os.str());
      }
    catch (HKLException const & ex)
      {
        updateStatusBar(ex);
      }
}

void
HKLWindow::updateCrystalModel(Glib::ustring const & name)
{
    Gtk::TreeModel::Children children = m_crystalModel->children(); 
    Gtk::TreeModel::Children::iterator iter = children.begin();
    Gtk::TreeModel::Children::iterator end = children.end();
    while (iter != end)
      {
        Gtk::TreeModel::Row const & row = *iter;
        if (row[m_crystalModelColumns.name] == name)
          {
            double a, b, c, alpha, beta, gamma;
            m_diffractometer->getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
            row[m_crystalModelColumns.a] = a;
            row[m_crystalModelColumns.b] = b;
            row[m_crystalModelColumns.c] = c;
            row[m_crystalModelColumns.alpha] = alpha * hkl::constant::math::radToDeg;
            row[m_crystalModelColumns.beta] = beta * hkl::constant::math::radToDeg;
            row[m_crystalModelColumns.gamma] = gamma * hkl::constant::math::radToDeg;
            try
              {
                row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
              }
            catch (HKLException const & ex)
              {
                updateStatusBar(ex);
              }
            iter = end;
          }
        else
            ++iter;
      }
}
