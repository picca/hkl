//#include <iostream>
/*
#include <sigc++/bind.h>
#include <gtkmm/comboboxentry.h>
*/

#include <gtkmm/notebook.h>

#include "constant.h"
#include "HKLException.h"
#include "sample_monocrystal.h"
#include "mode.h"
#include "affinement_simplex.h"

#include "hklwindow.h"
#include "axespinbutton.h"

HKLWindow::HKLWindow(hkl::Diffractometer * diffractometer)
    : m_diffractometer(diffractometer)
{
  // Sets the border width of the window.
  set_border_width(10);

  //Get Glade UI:
  m_refGlade = Gnome::Glade::Xml::create("diffractometer2.glade", "window1");
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
  hkl::ModeList & modes = m_diffractometer->modes();
  hkl::ModeList::iterator modelist_iter = modes.begin();
  hkl::ModeList::iterator modelist_end = modes.end();
  while(modelist_iter != modelist_end)
    {
      m_comboboxentrytext_modes.append_text((*modelist_iter)->get_name());
      ++modelist_iter;
    }
  // set active the correct mode.
  hkl::Mode * mode = m_diffractometer->modes().current();
  if (mode)
    m_comboboxentrytext_modes.set_active_text(mode->get_name());
  m_comboboxentrytext_modes.signal_changed().connect(mem_fun(*this, &HKLWindow::on_comboboxentrytext_modes_changed));
  Gtk::HBox * phbox = NULL;
  m_refGlade->get_widget("hbox_modes", phbox);
  phbox->pack_start(m_comboboxentrytext_modes, Gtk::PACK_SHRINK);
  phbox->reorder_child(m_comboboxentrytext_modes, 1);
  phbox->show_all();

  // fill the comboboxentrytext with the affinement.
  hkl::AffinementList & affinementList = m_diffractometer->affinements();
  hkl::AffinementList::iterator affinementlist_iter = affinementList.begin();
  hkl::AffinementList::iterator affinementlist_end = affinementList.end();
  while(affinementlist_iter != affinementlist_end)
    {
      m_comboboxentrytext_affinement.append_text((*affinementlist_iter)->get_name());
      ++affinementlist_iter;
    }
  m_comboboxentrytext_affinement.signal_changed().connect(mem_fun(*this, &HKLWindow::on_comboboxentrytext_affinement_changed));
  Gtk::Table * ptable = NULL;
  m_refGlade->get_widget("table_affinement", ptable);
  ptable->attach(m_comboboxentrytext_affinement, 1, 2, 0, 1, Gtk::FILL, Gtk::FILL);
  ptable->show_all();

  // Add the axes widgets.
  m_nb_sampleAxes = m_diffractometer->geometry()->get_samples().size();
  m_nb_detectorAxes = m_diffractometer->geometry()->get_detectors().size();
  m_refGlade->get_widget("table_axes", ptable);
  ptable->resize(m_nb_detectorAxes < m_nb_sampleAxes ? m_nb_sampleAxes : m_nb_detectorAxes, 2);

  if (ptable)
    {
      unsigned int i = 0;
      hkl::AxeList & axeList = m_diffractometer->geometry()->get_samples();
      hkl::AxeList::iterator iter = axeList.begin();
      hkl::AxeList::iterator end = axeList.end();
      while(iter != end)
        {
          hkl::Axe & axe = **iter;
          AxeSpinButton * axeSpinButton = manage( new AxeSpinButton(axe));
          m_axeSpinButtonList.push_back(axeSpinButton);
          ptable->attach(*axeSpinButton, 0, 1, i, i+1, Gtk::FILL);
          axeSpinButton->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_changed));
          axeSpinButton->signal_min_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_changed));
          axeSpinButton->signal_max_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_changed));
          ++iter;
          ++i;
        }
      axeList = m_diffractometer->geometry()->get_detectors();
      iter = axeList.begin();
      end = axeList.end();
      i = 0;
      while(iter != end)
        {
          hkl::Axe & axe = **iter;
          AxeSpinButton * axeSpinButton = manage( new AxeSpinButton(axe));
          m_axeSpinButtonList.push_back(axeSpinButton);
          ptable->attach(*axeSpinButton, 1, 2, i, i+1, Gtk::FILL);
          axeSpinButton->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_changed));
          axeSpinButton->signal_min_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_changed));
          axeSpinButton->signal_max_changed().connect(mem_fun(*this, &HKLWindow::on_axeSpinButton_changed));
          ++iter;
          ++i;
        }
    }
  ptable->show_all_children();

  // Add the pseudoAxes.
  m_nb_pseudoAxes = m_diffractometer->pseudoAxes().size();
  m_refGlade->get_widget("table_pseudoaxes", ptable);
  ptable->resize(m_nb_pseudoAxes, 1);

  if (ptable)
    {
      unsigned int i = 0;
      hkl::PseudoAxeList & pseudoAxes = m_diffractometer->pseudoAxes();
      hkl::PseudoAxeList::iterator iter = pseudoAxes.begin();
      hkl::PseudoAxeList::iterator end = pseudoAxes.end();
      while(iter != end)
        {
          hkl::PseudoAxe * pseudoAxe = *iter;
          PseudoAxeSpinButton * pseudoAxeSpinButton = manage( new PseudoAxeSpinButton(pseudoAxe));
          m_pseudoAxeSpinButtonList.push_back(pseudoAxeSpinButton);
          ptable->attach(*pseudoAxeSpinButton, 0, 1, i, i+1, Gtk::FILL);
          pseudoAxeSpinButton->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_pseudoAxeSpinButton_value_changed));
          ++iter;
          ++i;
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
{}

// Callback
void
HKLWindow::on_comboboxentrytext_modes_changed(void)
{
  Glib::ustring const & name = m_comboboxentrytext_modes.get_active_text();
  m_diffractometer->modes().set_current(name);
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
  m_diffractometer->samples().set_current(name);
  m_treeViewReflections->set_model(m_mapReflectionModel[name]);
  updateLattice();
  updateLatticeParameters();
  updateReciprocalLattice();
  updateUB();
  updateFitness();
  updatePseudoAxes();
  updateHKL();
}

void
HKLWindow::on_comboboxentrytext_affinement_changed(void)
{
  //Glib::ustring const & name = m_comboboxentrytext_affinement.get_active_text();
  try
    {
      updateAffinement();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_a_value_changed(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      hkl::FitParameter & a = sample->lattice().a();
      double na = m_spinbutton_a->get_value();
      a.set_current(na);

      updateCrystalModel(sample);
      updateReciprocalLattice();
      updateFitness();
      updateUB();
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_b_value_changed(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      hkl::FitParameter & b = sample->lattice().b();
      double nb = m_spinbutton_b->get_value();
      b.set_current(nb);

      updateCrystalModel(sample);
      updateReciprocalLattice();
      updateFitness();
      updateUB();
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_c_value_changed(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      hkl::FitParameter & c = sample->lattice().c();
      double nc = m_spinbutton_c->get_value();
      c.set_current(nc);

      updateCrystalModel(sample);
      updateReciprocalLattice();
      updateFitness();
      updateUB();
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_alpha_value_changed(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      hkl::FitParameter & alpha = sample->lattice().alpha();
      double nalpha = m_spinbutton_alpha->get_value() * hkl::constant::math::degToRad;
      alpha.set_current(nalpha);

      updateCrystalModel(sample);
      updateReciprocalLattice();
      updateFitness();
      updateUB();
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_beta_value_changed(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      hkl::FitParameter & beta = sample->lattice().beta();
      double nbeta = m_spinbutton_beta->get_value() * hkl::constant::math::degToRad;
      beta.set_current(nbeta);

      updateCrystalModel(sample);
      updateReciprocalLattice();
      updateFitness();
      updateUB();
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_gamma_value_changed(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      hkl::FitParameter & gamma = sample->lattice().gamma();
      double ngamma = m_spinbutton_gamma->get_value() * hkl::constant::math::degToRad;
      gamma.set_current(ngamma);

      updateCrystalModel(sample);
      updateReciprocalLattice();
      updateFitness();
      updateUB();
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_a_min_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().a();
  try
    {
      min = m_spinbutton_a_min->get_value();
      max = fitParameter.get_max().get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_b_min_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().b();
  try
    {
      min = m_spinbutton_b_min->get_value();
      max = fitParameter.get_max().get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_c_min_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().c();
  try
    {
      min = m_spinbutton_c_min->get_value();
      max = fitParameter.get_max().get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_alpha_min_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().alpha();
  try
    {
      min = m_spinbutton_alpha_min->get_value() * hkl::constant::math::degToRad;
      max = fitParameter.get_max().get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_beta_min_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().beta();
  try
    {
      min = m_spinbutton_beta_min->get_value() * hkl::constant::math::degToRad;
      max = fitParameter.get_max().get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_gamma_min_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().gamma();
  try
    {
      min = m_spinbutton_gamma_min->get_value() * hkl::constant::math::degToRad;
      max = fitParameter.get_max().get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_a_max_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().a();
  try
    {
      min = fitParameter.get_min().get_value();
      max = m_spinbutton_a_max->get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_b_max_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().b();
  try
    {
      min = fitParameter.get_min().get_value();
      max = m_spinbutton_b_max->get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_c_max_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().c();
  try
    {
      min = fitParameter.get_min().get_value();
      max = m_spinbutton_c_max->get_value();
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_alpha_max_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().alpha();
  try
    {
      min = fitParameter.get_min().get_value();
      max = m_spinbutton_alpha_max->get_value() * hkl::constant::math::degToRad;
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_beta_max_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().beta();
  try
    {
      min = fitParameter.get_min().get_value();
      max = m_spinbutton_beta_max->get_value() * hkl::constant::math::degToRad;
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_spinbutton_gamma_max_value_changed(void)
{
  double min, max;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().gamma();
  try
    {
      min = fitParameter.get_min().get_value();
      max = m_spinbutton_gamma_max->get_value() * hkl::constant::math::degToRad;
      fitParameter.set_range(min, max);
    }
  catch (hkl::HKLException const & ex)
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
      m_diffractometer->geometry()->get_source().setWaveLength(lambda);
      updatePseudoAxes();
      updateHKL();
    }
  catch (hkl::HKLException const & ex)
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
      //name = m_comboboxentrytext_affinement.get_active_text();
      unsigned int max = (unsigned int) m_spinbutton_max_iteration->get_value();
      m_diffractometer->affinements().current()->set_nb_max_iterations(max);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }

}

void
HKLWindow::on_checkbutton_a_toggled(void)
{
  bool to_fit;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().a();
  try
    {
      to_fit = m_checkbutton_a->get_active();
      fitParameter.set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_checkbutton_b_toggled(void)
{
  bool to_fit;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().b();
  try
    {
      to_fit = m_checkbutton_b->get_active();
      fitParameter.set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_checkbutton_c_toggled(void)
{
  bool to_fit;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().c();
  try
    {
      to_fit = m_checkbutton_c->get_active();
      fitParameter.set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_checkbutton_alpha_toggled(void)
{
  bool to_fit;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().alpha();
  try
    {
      to_fit = m_checkbutton_alpha->get_active();
      fitParameter.set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_checkbutton_beta_toggled(void)
{
  bool to_fit;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().beta();
  try
    {
      to_fit = m_checkbutton_beta->get_active();
      fitParameter.set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_checkbutton_gamma_toggled(void)
{
  bool to_fit;
  hkl::FitParameter & fitParameter = m_diffractometer->samples().current()->lattice().gamma();
  try
    {
      to_fit = m_checkbutton_gamma->get_active();
      fitParameter.set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_checkbutton_U_toggled(void)
{
  bool to_fit;
  try
    {
      to_fit = m_checkbutton_U->get_active();
      m_diffractometer->samples().current()->operator[]("euler_x")->set_flagFit(to_fit);
      m_diffractometer->samples().current()->operator[]("euler_y")->set_flagFit(to_fit);
      m_diffractometer->samples().current()->operator[]("euler_z")->set_flagFit(to_fit);
    }
  catch (hkl::HKLException const & ex)
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
      hkl::smatrix const & UB = m_diffractometer->samples().current()->get_UB();
      m_diffractometer->modes().current()->computeAngles(h, k, l, UB);
      updateAxes();
      updatePseudoAxes();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_axeSpinButton_changed(void)
{
  updatePseudoAxes();
  updateHKL();
}

void
HKLWindow::on_pseudoAxeSpinButton_value_changed(void)
{
  updateAxes();
  updatePseudoAxes();
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
      m_diffractometer->samples()[name]->set_name(newText);
    }
  catch (hkl::HKLException const & ex)
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
      double a;
      sscanf(newText.c_str(), "%lf", &a);
      hkl::FitParameter & fitParameter = m_diffractometer->samples()[name]->lattice().a();
      fitParameter.set_current(a);
      row[m_crystalModelColumns.a] = a;
      m_spinbutton_a->set_value(a);
      //row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
    }
  catch (hkl::HKLException const & ex)
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
      double b;
      sscanf(newText.c_str(), "%lf", &b);
      hkl::FitParameter & fitParameter = m_diffractometer->samples()[name]->lattice().b();
      fitParameter.set_current(b);
      row[m_crystalModelColumns.b] = b;
      m_spinbutton_b->set_value(b);
      //row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
    }
  catch (hkl::HKLException const & ex)
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
      double c;
      sscanf(newText.c_str(), "%lf", &c);
      hkl::FitParameter & fitParameter = m_diffractometer->samples()[name]->lattice().c();
      fitParameter.set_current(c);
      row[m_crystalModelColumns.c] = c;
      m_spinbutton_c->set_value(c);
      //row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
    }
  catch (hkl::HKLException const & ex)
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
      double alpha;
      sscanf(newText.c_str(), "%lf", &alpha);
      hkl::FitParameter & fitParameter = m_diffractometer->samples()[name]->lattice().alpha();
      fitParameter.set_current(alpha * hkl::constant::math::degToRad);
      row[m_crystalModelColumns.alpha] = alpha;
      m_spinbutton_alpha->set_value(alpha);
      //row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
    }
  catch (hkl::HKLException const & ex)
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
      double beta;
      sscanf(newText.c_str(), "%lf", &beta);
      hkl::FitParameter & fitParameter = m_diffractometer->samples()[name]->lattice().beta();
      fitParameter.set_current(beta * hkl::constant::math::degToRad);
      row[m_crystalModelColumns.beta] = beta;
      m_spinbutton_beta->set_value(beta);
      //row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
    }
  catch (hkl::HKLException const & ex)
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
      double gamma;
      sscanf(newText.c_str(), "%lf", &gamma);
      hkl::FitParameter & fitParameter = m_diffractometer->samples()[name]->lattice().gamma();
      fitParameter.set_current(gamma * hkl::constant::math::degToRad);
      row[m_crystalModelColumns.gamma] = gamma;
      m_spinbutton_gamma->set_value(gamma);
      //row[m_crystalModelColumns.fitness] = m_diffractometer->getCrystalFitness(name);
    }
  catch (hkl::HKLException const & ex)
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
      hkl::Sample * sample = m_diffractometer->samples().current();
      int index = row[m_reflectionModelColumns.index];
      double h;
      hkl::Reflection * reflection = sample->reflections()[index];
      hkl::svector hkl = reflection->get_hkl();
      sscanf(newText.c_str(), "%lf", &h);
      hkl.x() = h;
      reflection->set_hkl(hkl);
      row[m_reflectionModelColumns.h] = h;
      row[m_reflectionModelColumns.flag] = reflection->flag();
      updateCrystalModel(sample);
      updateFitness();
    }
  catch (hkl::HKLException const & ex)
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
      hkl::Sample * sample = m_diffractometer->samples().current();
      int index = row[m_reflectionModelColumns.index];
      double k;
      hkl::Reflection * reflection = sample->reflections()[index];
      hkl::svector hkl = reflection->get_hkl();
      sscanf(newText.c_str(), "%lf", &k);
      hkl.y() = k;
      reflection->set_hkl(hkl);
      row[m_reflectionModelColumns.k] = k;
      row[m_reflectionModelColumns.flag] = reflection->flag();
      updateCrystalModel(sample);
      updateFitness();
    }
  catch (hkl::HKLException const & ex)
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
      hkl::Sample * sample = m_diffractometer->samples().current();
      int index = row[m_reflectionModelColumns.index];
      double l;
      hkl::Reflection * reflection = sample->reflections()[index];
      hkl::svector hkl = reflection->get_hkl();
      sscanf(newText.c_str(), "%lf", &l);
      hkl.z() = l;
      reflection->set_hkl(hkl);
      row[m_reflectionModelColumns.l] = l;
      row[m_reflectionModelColumns.flag] = reflection->flag();
      updateCrystalModel(sample);
      updateFitness();
    }
  catch (hkl::HKLException const & ex)
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
      hkl::Sample * sample = m_diffractometer->samples().current();
      int index = row[m_reflectionModelColumns.index];
      hkl::Reflection * reflection = sample->reflections()[index];
      bool & flag = reflection->flag();
      flag = !flag;
      row[m_reflectionModelColumns.flag] = flag;
      updateFitness();
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::on_toolbutton_add_reflection_clicked(void)
{
  try
    {
      hkl::Sample * sample = m_diffractometer->samples().current();
      string const & name = sample->get_name();
      double h, k, l;
      h = m_spinbutton_h->get_value();
      k = m_spinbutton_k->get_value();
      l = m_spinbutton_l->get_value();
      hkl::svector hkl(h, k, l);
      sample->reflections().add(hkl);
      updateReflections(sample, m_mapReflectionModel[name]);
      updateFitness();
    }
  catch (hkl::HKLException const & ex)
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
      name = m_diffractometer->samples().current()->get_name();
    }
  catch (hkl::HKLException const & ex)
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
          //need a geometry type.
          hkl::Geometry const & geometry = m_diffractometer->samples().current()->reflections()[index]->get_geometry();
          m_diffractometer->geometry()->setFromGeometry(geometry, true);
        }
      catch (hkl::HKLException const & ex)
        {
          updateStatusBar(ex);
          return;
        }
      updateSource();
      updateAxes();
      updatePseudoAxes();
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
  hkl::Sample * sample;
  try
    {
      sample = m_diffractometer->samples().current();
    }
  catch (hkl::HKLException const & ex)
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
      Glib::RefPtr<Gtk::ListStore> liststore = m_mapReflectionModel[sample->get_name()];
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
                  m_diffractometer->samples().current()->reflections().del(index);
                }
              catch (hkl::HKLException const & ex)
                {
                  updateStatusBar(ex);
                  return;
                }
            }
          updateReflections(sample, liststore);
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
      hkl::sample::MonoCrystal * sample = dynamic_cast<hkl::sample::MonoCrystal *>(m_diffractometer->samples().current());
      //fill index1, index2;
      unsigned int index1 =0;
      unsigned int index2 = 1;
      sample->computeU(index1, index2);
    }
  catch (hkl::HKLException const & ex)
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
      m_diffractometer->samples().add("new_sample", hkl::SAMPLE_MONOCRYSTAL);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
      return;
    }

  m_diffractometer->samples().set_current("new_sample");
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
      name = m_diffractometer->samples().current()->get_name();
    }
  catch (hkl::HKLException const & ex)
    {
      m_statusBar->push("Please select a crystal to copy.");
      return;
    }

  m_diffractometer->samples().set_current(newname.c_str());
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
  try
  {}
  catch (hkl::HKLException const & ex)
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
      m_diffractometer->affinements().current()->fit(*(m_diffractometer->samples().current()));
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
      return;
    }
  updateCrystalModel(m_diffractometer->samples().current());
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
  return true;
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
  return true;
}

// Non-Callback
hkl::Axe &
HKLWindow::get_axe(Glib::ustring const & name)
{
  return m_diffractometer->geometry()->get_axe(name.c_str());
}

void
HKLWindow::updateSource(void)
{
  try
    {
      double lambda = m_diffractometer->geometry()->get_source().get_waveLength().get_value();
      m_spinbutton_lambda->set_value(lambda);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::updateAxes(void)
{
  cout << "updateAxes" << endl;
  AxeSpinButtonList::iterator iter = m_axeSpinButtonList.begin();
  AxeSpinButtonList::iterator last = m_axeSpinButtonList.end();
  while(iter != last)
    {
      (*iter)->update();
      ++iter;
    }
}

void
HKLWindow::updatePseudoAxes(void)
{
  cout << "updatePseudoAxes" << endl;
  PseudoAxeSpinButtonList::iterator iter = m_pseudoAxeSpinButtonList.begin();
  PseudoAxeSpinButtonList::iterator last = m_pseudoAxeSpinButtonList.end();
  while(iter != last)
    {
      (*iter)->update();
      ++iter;
    }
}

void
HKLWindow::updateLattice(void)
{
  double a, b, c, alpha, beta, gamma;

  try
    {
      hkl::Lattice & lattice = m_diffractometer->samples().current()->lattice();
      a = lattice.a().get_current().get_value();
      b = lattice.b().get_current().get_value();
      c = lattice.c().get_current().get_value();
      alpha = lattice.alpha().get_current().get_value() * hkl::constant::math::radToDeg;
      beta = lattice.beta().get_current().get_value() * hkl::constant::math::radToDeg;
      gamma = lattice.gamma().get_current().get_value() * hkl::constant::math::radToDeg;
      m_spinbutton_a->set_value(a);
      m_spinbutton_b->set_value(b);
      m_spinbutton_c->set_value(c);
      m_spinbutton_alpha->set_value(alpha);
      m_spinbutton_beta->set_value(beta);
      m_spinbutton_gamma->set_value(gamma);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::updateLatticeParameters(void)
{
  double min;
  double max;
  bool to_fit;

  hkl::Lattice & lattice = m_diffractometer->samples().current()->lattice();
  hkl::FitParameter & fitParameter = lattice.a();
  min = fitParameter.get_min().get_value();
  max = fitParameter.get_max().get_value();
  to_fit = fitParameter.get_flagFit();
  m_spinbutton_a_min->set_value(min);
  m_spinbutton_a_max->set_value(max);
  m_checkbutton_a->set_active(to_fit);

  fitParameter = lattice.b();
  min = fitParameter.get_min().get_value();
  max = fitParameter.get_max().get_value();
  to_fit = fitParameter.get_flagFit();
  m_spinbutton_b_min->set_value(min);
  m_spinbutton_b_max->set_value(max);
  m_checkbutton_b->set_active(to_fit);

  fitParameter = lattice.c();
  min = fitParameter.get_min().get_value();
  max = fitParameter.get_max().get_value();
  to_fit = fitParameter.get_flagFit();
  m_spinbutton_c_min->set_value(min);
  m_spinbutton_c_max->set_value(max);
  m_checkbutton_c->set_active(to_fit);

  fitParameter = lattice.alpha();
  min = fitParameter.get_min().get_value() * hkl::constant::math::radToDeg;
  max = fitParameter.get_max().get_value() * hkl::constant::math::radToDeg;
  to_fit = fitParameter.get_flagFit();
  m_spinbutton_alpha_min->set_value(min * hkl::constant::math::radToDeg);
  m_spinbutton_alpha_max->set_value(max * hkl::constant::math::radToDeg);
  m_checkbutton_alpha->set_active(to_fit);

  fitParameter = lattice.beta();
  min = fitParameter.get_min().get_value() * hkl::constant::math::radToDeg;
  max = fitParameter.get_max().get_value() * hkl::constant::math::radToDeg;
  to_fit = fitParameter.get_flagFit();
  m_spinbutton_beta_min->set_value(min * hkl::constant::math::radToDeg);
  m_spinbutton_beta_max->set_value(max * hkl::constant::math::radToDeg);
  m_checkbutton_beta->set_active(to_fit);

  fitParameter = lattice.gamma();
  min = fitParameter.get_min().get_value() * hkl::constant::math::radToDeg;
  max = fitParameter.get_max().get_value() * hkl::constant::math::radToDeg;
  to_fit = fitParameter.get_flagFit();
  m_spinbutton_gamma_min->set_value(min * hkl::constant::math::radToDeg);
  m_spinbutton_gamma_max->set_value(max * hkl::constant::math::radToDeg);
  m_checkbutton_gamma->set_active(to_fit);

  //compute the state of the checkbutton_U
  hkl::FitParameter * euler_x = m_diffractometer->samples().current()->operator[]("euler_x");
  hkl::FitParameter * euler_y = m_diffractometer->samples().current()->operator[]("euler_y");
  hkl::FitParameter * euler_z = m_diffractometer->samples().current()->operator[]("euler_z");
  if (euler_x->get_flagFit() && euler_y->get_flagFit() && euler_z->get_flagFit())
    m_checkbutton_U->set_active(true);
  else
    {
      euler_x->set_flagFit(false);
      euler_y->set_flagFit(false);
      euler_z->set_flagFit(false);
      m_checkbutton_U->set_active(false);
    }
}

void
HKLWindow::updateReciprocalLattice(void)
{
  try
    {
      hkl::Lattice & lattice = m_diffractometer->samples().current()->lattice();
      hkl::Lattice reciprocal = lattice.reciprocal();
      m_spinbutton_a_star->set_value(reciprocal.a().get_current().get_value());
      m_spinbutton_b_star->set_value(reciprocal.b().get_current().get_value());
      m_spinbutton_c_star->set_value(reciprocal.c().get_current().get_value());
      m_spinbutton_alpha_star->set_value(reciprocal.alpha().get_current().get_value() * hkl::constant::math::radToDeg);
      m_spinbutton_beta_star->set_value(reciprocal.beta().get_current().get_value() * hkl::constant::math::radToDeg);
      m_spinbutton_gamma_star->set_value(reciprocal.gamma().get_current().get_value() * hkl::constant::math::radToDeg);
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::updateUB(void)
{
  try
    {
      hkl::smatrix UB = m_diffractometer->samples().current()->get_UB();
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
  catch(hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }
}

void
HKLWindow::updateHKL(void)
{
  cout << "updateHKL" << endl;
  double h, k, l;
  try
    {
      hkl::smatrix UB = m_diffractometer->samples().current()->get_UB();
      m_diffractometer->geometry()->computeHKL(h, k, l, UB);
      m_spinbutton_h->set_value(h);
      m_spinbutton_k->set_value(k);
      m_spinbutton_l->set_value(l);
    }
  catch (hkl::HKLException const & ex)
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
      current_crystal_name = m_diffractometer->samples().current()->get_name();
      is_current_crystal_set = true;
    }
  catch (hkl::HKLException const & ex)
    {
      updateStatusBar(ex);
    }

  //Fill the models from the crystalList
  hkl::SampleList::iterator iter = m_diffractometer->samples().begin();
  hkl::SampleList::iterator end = m_diffractometer->samples().end();
  while(iter != end)
    {
      hkl::Lattice & lattice = (*iter)->lattice();
      iter_row = *(m_crystalModel->append());
      if (is_current_crystal_set && current_crystal_name == (*iter)->get_name())
        iter_current = iter_row;
      row = *(iter_row);
      row[m_crystalModelColumns.name] = (*iter)->get_name();
      row[m_crystalModelColumns.a] = lattice.a().get_current().get_value();
      row[m_crystalModelColumns.b] = lattice.b().get_current().get_value();
      row[m_crystalModelColumns.c] = lattice.c().get_current().get_value();
      row[m_crystalModelColumns.alpha] = lattice.alpha().get_current().get_value() * hkl::constant::math::radToDeg;
      row[m_crystalModelColumns.beta] = lattice.beta().get_current().get_value() * hkl::constant::math::radToDeg;
      row[m_crystalModelColumns.gamma] = lattice.gamma().get_current().get_value() * hkl::constant::math::radToDeg;
      try
        {
          //row[m_crystalModelColumns.fitness] = (*iter)->fitness();
        }
      catch (hkl::HKLException const & ex)
        {
          row[m_crystalModelColumns.fitness] = NAN;
          updateStatusBar(ex);
        }

      Glib::RefPtr<Gtk::ListStore> listStore = Gtk::ListStore::create(m_reflectionModelColumns);
      m_mapReflectionModel[(*iter)->get_name()] = listStore;
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
HKLWindow::updateReflections(hkl::Sample * sample, Glib::RefPtr<Gtk::ListStore> & listStore)
{
  listStore->clear();
  Gtk::ListStore::Row row;
  hkl::ReflectionList::iterator iter = sample->reflections().begin();
  hkl::ReflectionList::iterator end = sample->reflections().end();
  unsigned int i = 0;
  while(iter != end)
    {
      row = *(listStore->append());
      hkl::svector const & hkl = (*iter)->get_hkl();
      row[m_reflectionModelColumns.index] = i;
      row[m_reflectionModelColumns.h] = hkl.x();
      row[m_reflectionModelColumns.k] = hkl.y();
      row[m_reflectionModelColumns.l] = hkl.z();
      row[m_reflectionModelColumns.flag] = (*iter)->flag();
      ++iter;
      ++i;
    }
}

void
HKLWindow::updateStatusBar(hkl::HKLException const & ex)
{
  m_statusBar->push(ex.errors[0].reason + " " + ex.errors[0].desc + " " + ex.errors[0].origin);
}

void
HKLWindow::updateFitness(void)
{
  Glib::ustring name;
  try
    {
      //double fitness = m_diffractometer->samples().current()->fitness();
      ostringstream os;
      //os << fitness;
      //m_label_fitness->set_text(os.str());
    }
  catch (hkl::HKLException const & ex)
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
  hkl::Affinement * affinement = m_diffractometer->affinements().current();
  if (affinement)
    {
      unsigned int max = affinement->get_nb_max_iterations();
      m_spinbutton_max_iteration->set_value(max);
      // update the nb iteration
      unsigned int nb_iterations = affinement->get_nb_iterations();
      ostringstream os;
      os << nb_iterations;
      m_label_nb_iterations->set_text(os.str());
    }
}

void
HKLWindow::updateCrystalModel(hkl::Sample * sample)
{
  Gtk::TreeModel::Children children = m_crystalModel->children();
  Gtk::TreeModel::Children::iterator iter = children.begin();
  Gtk::TreeModel::Children::iterator end = children.end();
  while (iter != end)
    {
      Gtk::TreeModel::Row const & row = *iter;
      if (row[m_crystalModelColumns.name] == sample->get_name())
        {
          hkl::Lattice & lattice = sample->lattice();
          row[m_crystalModelColumns.a] = lattice.a().get_current().get_value();
          row[m_crystalModelColumns.b] = lattice.b().get_current().get_value();
          row[m_crystalModelColumns.c] = lattice.c().get_current().get_value();
          row[m_crystalModelColumns.alpha] = lattice.alpha().get_current().get_value() * hkl::constant::math::radToDeg;
          row[m_crystalModelColumns.beta] = lattice.beta().get_current().get_value() * hkl::constant::math::radToDeg;
          row[m_crystalModelColumns.gamma] = lattice.gamma().get_current().get_value() * hkl::constant::math::radToDeg;
          try
            {
              //row[m_crystalModelColumns.fitness] = sample->fitness();
            }
          catch (hkl::HKLException const & ex)
            {
              updateStatusBar(ex);
            }
          iter = end;
        }
      else
        ++iter;
    }
}
