#include <gtkmm/notebook.h>

#include "hklwindow.h"
#include "axespinbutton.h"

HKLWindow::HKLWindow(HklGeometryType type)
{
	size_t i;
	HklSample *sample;

	_geometry = hkl_geometry_factory_new(type, 50 * HKL_DEGTORAD);
	
	_detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
        _detector->idx = 1;
	
	_samples = hkl_sample_list_new();
	// add a default crystal
	sample = hkl_sample_new("test", HKL_SAMPLE_MONOCRYSTAL);
	hkl_sample_list_append(_samples, sample);
	hkl_sample_list_select_current(_samples, "test");

	_engines = hkl_pseudo_axis_engine_list_factory(type);
	_hkl = hkl_pseudo_axis_engine_list_get_by_name(_engines, "hkl");
	hkl_pseudo_axis_engine_list_init(_engines, _geometry, _detector, _samples->current);

	// Sets the border width of the window.
	this->set_border_width(10);

	//Get Glade UI:
	m_refGlade = Gtk::Builder::create();
	m_refGlade->add_from_file("hkl.ui");

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
	m_refGlade->get_widget("treeview_axes", m_TreeView_axes);
	m_refGlade->get_widget("treeview_pseudoAxes", m_TreeView_pseudoAxes);
	m_refGlade->get_widget("treeview_pseudoAxes_parameters", m_TreeView_pseudoAxes_parameters);
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
	for(i=0; i<HKL_LIST_LEN(_hkl->modes); ++i)
		m_comboboxentrytext_modes.append_text(_hkl->modes[i]->name);

	// set active the correct mode.
	if(_hkl->mode)
		m_comboboxentrytext_modes.set_active_text(_hkl->mode->name);
	m_comboboxentrytext_modes.signal_changed().connect(mem_fun(*this, &HKLWindow::on_comboboxentrytext_modes_changed));
	Gtk::HBox * phbox = NULL;
	m_refGlade->get_widget("hbox_modes", phbox);
	phbox->pack_start(m_comboboxentrytext_modes, Gtk::PACK_SHRINK);
	phbox->reorder_child(m_comboboxentrytext_modes, 1);
	phbox->show_all();

	// fill the comboboxentrytext with the affinement.
	m_comboboxentrytext_affinement.append_text("simplex");
	m_comboboxentrytext_affinement.signal_changed().connect(mem_fun(*this, &HKLWindow::on_comboboxentrytext_affinement_changed));
	Gtk::Table * ptable = NULL;
	m_refGlade->get_widget("table_affinement", ptable);
	ptable->attach(m_comboboxentrytext_affinement, 1, 2, 0, 1, Gtk::FILL, Gtk::FILL);
	ptable->show_all();

	this->set_up_TreeView_axes();
	this->set_up_TreeView_pseudoAxes_parameters();
	this->set_up_TreeView_pseudoAxes();

  
	int index;
	Gtk::CellRenderer * renderer;

	//Set up the treeViewReflections
	m_treeViewReflections->append_column("index", m_reflectionModelColumns.index);

	index = m_treeViewReflections->append_column_numeric_editable("h", m_reflectionModelColumns.h, "%lf");
	renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_h_edited));

	index = m_treeViewReflections->append_column_numeric_editable("k", m_reflectionModelColumns.k, "%lf");
	renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_k_edited));

	index = m_treeViewReflections->append_column_numeric_editable("l", m_reflectionModelColumns.l, "%lf");
	renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_l_edited));

	index = m_treeViewReflections->append_column_editable("flag", m_reflectionModelColumns.flag);
	renderer = m_treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_flag_toggled));

	m_treeViewReflections->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

	//Set up the treeViewCrystals
	index = m_treeViewCrystals->append_column_editable("name", m_crystalModelColumns.name);
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_name_edited));

	index = m_treeViewCrystals->append_column_numeric_editable("a", m_crystalModelColumns.a, "%lf");
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_a_edited));

	index = m_treeViewCrystals->append_column_numeric_editable("b", m_crystalModelColumns.b, "%lf");
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_b_edited));

	index = m_treeViewCrystals->append_column_numeric_editable("c", m_crystalModelColumns.c, "%lf");
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_c_edited));

	index = m_treeViewCrystals->append_column_numeric_editable("alpha", m_crystalModelColumns.alpha, "%lf");
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_alpha_edited));

	index = m_treeViewCrystals->append_column_numeric_editable("beta", m_crystalModelColumns.beta, "%lf");
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_beta_edited));

	index = m_treeViewCrystals->append_column_numeric_editable("gamma", m_crystalModelColumns.gamma, "%lf");
	renderer = m_treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_gamma_edited));

	m_treeViewCrystals->append_column("fitness", m_crystalModelColumns.fitness);

	m_treeViewCrystals->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

	//update the widgets
	this->updateTreeViewCrystals();
	this->updateSource();
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updateHKL();
	this->updateAffinement();

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
	m_TreeView_pseudoAxes->signal_cursor_changed().connect(mem_fun(*this, &HKLWindow::on_treeView_pseudoAxes_cursor_changed));
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

	this->show_all_children();
}

HKLWindow::~HKLWindow()
{
	hkl_geometry_free(_geometry);
	hkl_detector_free(_detector);
	hkl_pseudo_axis_engine_list_free(_engines);
	hkl_sample_list_free(_samples);
}

// Callback
void
HKLWindow::on_comboboxentrytext_modes_changed(void)
{
	size_t idx;
	Glib::ustring const & name = m_comboboxentrytext_modes.get_active_text();
	for(idx=0; idx<HKL_LIST_LEN(_hkl->modes); ++idx)
		if(name == _hkl->modes[idx]->name)
			hkl_pseudo_axis_engine_select_mode(_hkl, idx);
}

void
HKLWindow::on_treeView_pseudoAxes_cursor_changed(void)
{
	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	m_TreeView_pseudoAxes->get_cursor(path, column);
	Gtk::ListStore::Row row = *(m_pseudoAxeModel->get_iter(path));
	HklPseudoAxis *pseudoAxis = row[m_pseudoAxeModelColumns.pseudoAxis];
	m_TreeView_pseudoAxes_parameters->set_model(m_mapPseudoAxeParameterModel[pseudoAxis]);
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
	hkl_sample_list_select_current(_samples, name.c_str());
	hkl_pseudo_axis_engine_list_init(_engines, _geometry, _detector, _samples->current);
	m_treeViewReflections->set_model(m_mapReflectionModel[name]);
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUB();
	this->updateFitness();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_comboboxentrytext_affinement_changed(void)
{
	this->updateAffinement();
}

void
HKLWindow::on_spinbutton_a_value_changed(void)
{
	double b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double na = m_spinbutton_a->get_value();
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, na, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_spinbutton_b_value_changed(void)
{
	double a, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = m_spinbutton_b->get_value();
	a = sample->lattice->a->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, value, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_spinbutton_c_value_changed(void)
{
	double a, b, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = m_spinbutton_c->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, value, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_spinbutton_alpha_value_changed(void)
{
	double a, b, c, beta, gamma;
	HklSample *sample = _samples->current;

	double value = m_spinbutton_alpha->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, value, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_spinbutton_beta_value_changed(void)
{
	double a, b, c, alpha, gamma;
	HklSample *sample = _samples->current;

	double value = m_spinbutton_b->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, value, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_spinbutton_gamma_value_changed(void)
{
	double a, b, c, alpha, beta;
	HklSample *sample = _samples->current;

	double value = m_spinbutton_b->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, value);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateFitness();
	this->updateUB();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_spinbutton_a_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->a;
		double min = m_spinbutton_a_min->get_value();
		double max = parameter->range.max;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_b_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->b;
		double min = m_spinbutton_b_min->get_value();
		double max = parameter->range.max;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_c_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->c;
		double min = m_spinbutton_c_min->get_value();
		double max = parameter->range.max;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_alpha_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->alpha;
		double min = m_spinbutton_alpha_min->get_value();
		double max = parameter->range.max;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_beta_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->beta;
		double min = m_spinbutton_beta_min->get_value();
		double max = parameter->range.max;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_gamma_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->gamma;
		double min = m_spinbutton_gamma_min->get_value();
		double max = parameter->range.max;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_a_max_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->a;
		double max = m_spinbutton_a_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_b_max_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->b;
		double max = m_spinbutton_b_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_c_max_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->c;
		double max = m_spinbutton_c_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_alpha_max_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->alpha;
		double max = m_spinbutton_alpha_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_beta_max_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->beta;
		double max = m_spinbutton_beta_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_gamma_max_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->gamma;
		double max = m_spinbutton_gamma_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_lambda_value_changed(void)
{
	_geometry->source.wave_length = m_spinbutton_lambda->get_value();
	this->updatePseudoAxes();
	this->updateHKL();
}

// TODO delete
void
HKLWindow::on_spinbutton_max_iteration_value_changed(void)
{
}

void
HKLWindow::on_checkbutton_a_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->a->fit = m_checkbutton_a->get_active();
}

void
HKLWindow::on_checkbutton_b_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->b->fit = m_checkbutton_b->get_active();
}

void
HKLWindow::on_checkbutton_c_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->c->fit = m_checkbutton_c->get_active();
}

void
HKLWindow::on_checkbutton_alpha_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->alpha->fit = m_checkbutton_alpha->get_active();
}

void
HKLWindow::on_checkbutton_beta_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->beta->fit = m_checkbutton_beta->get_active();
}

void
HKLWindow::on_checkbutton_gamma_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->gamma->fit = m_checkbutton_gamma->get_active();
}

// TODO affinement
void
HKLWindow::on_checkbutton_U_toggled(void)
{
}

void
HKLWindow::on_button_goto_hkl_clicked(void)
{
	if(_hkl->mode){
		HklError *error = NULL;

		hkl_parameter_set_value_unit((HklParameter *)_hkl->pseudoAxes[0],
					     m_spinbutton_h->get_value());
		hkl_parameter_set_value_unit((HklParameter *)_hkl->pseudoAxes[1],
					     m_spinbutton_k->get_value());
		hkl_parameter_set_value_unit((HklParameter *)_hkl->pseudoAxes[2],
					     m_spinbutton_l->get_value());

		hkl_sample_fprintf(stdout, _samples->current);
		hkl_geometry_fprintf(stdout, _geometry);
		hkl_pseudo_axis_engine_set(_hkl, &error);
		if(error){
			m_message = new Gtk::MessageDialog("", false,
							   Gtk::MESSAGE_WARNING,
							   Gtk::BUTTONS_YES_NO);
			m_message->set_message ("error");
			m_message->show();
			int respons = m_message->run ();
			switch (respons){
			case Gtk::RESPONSE_YES:
				break;
			}
			delete m_message;
			hkl_error_clear(&error);
			return;
		}

		hkl_pseudo_axis_engine_list_fprintf(stdout, _engines);
		//hkl_geometry_init_geometry(_geometry,
		//			   _engines->geometries->geometries[0]);
		this->updateAxes();
		this->updatePseudoAxes();
	}
}

void
HKLWindow::on_axeSpinButton_changed(void)
{
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_pseudoAxeSpinButton_value_changed(void)
{
	this->updateAxes();
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_cell_TreeView_axes_read_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[m_axeModelColumns.name];
	double value;
	sscanf(newText.c_str(), "%lf", &value);
	HklAxis *axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_axis_set_value_unit(axis, value);
	hkl_geometry_update(_geometry);

	row[m_axeModelColumns.read] = value;
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_cell_TreeView_axes_write_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[m_axeModelColumns.name];
	double value;
	sscanf(newText.c_str(), "%lf", &value);
	HklAxis *axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_axis_set_value_unit(axis, value);
	hkl_geometry_update(_geometry);

	row[m_axeModelColumns.write] = value;
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_cell_TreeView_axes_min_edited(Glib::ustring const & spath,
					    Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	double shit;
	double max;
	double value;
	HklAxis *axis;

	Glib::ustring name = row[m_axeModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_parameter_get_range_unit((HklParameter *)axis, &shit, &max);
	hkl_parameter_set_range_unit((HklParameter *)axis, value, max);

	row[m_axeModelColumns.min] = value;
	this->updatePseudoAxes();
	this->updateHKL();
}

void
HKLWindow::on_cell_TreeView_axes_max_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	double min, shit;
	double value;
	HklAxis *axis;

	Glib::ustring name = row[m_axeModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_parameter_get_range_unit((HklParameter *)axis, &min, &shit);
	hkl_parameter_set_range_unit((HklParameter *)axis, min, value);

	row[m_axeModelColumns.max] = value;
	this->updatePseudoAxes();
	this->updateHKL();
}

// PseudoAxes
void
HKLWindow::on_cell_TreeView_pseudoAxes_write_edited(Glib::ustring const & spath,
						    Glib::ustring const & newText)
{
	double value;
	HklPseudoAxis *pseudoAxis;
	HklError *error;
	int res;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_TreeView_pseudoAxes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	pseudoAxis = row[m_pseudoAxeModelColumns.pseudoAxis];
	Glib::ustring name = row[m_pseudoAxeModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	hkl_parameter_set_value_unit((HklParameter *)pseudoAxis, value);
	error = NULL;
	if(hkl_pseudo_axis_engine_set(pseudoAxis->engine, &error) == HKL_SUCCESS){
		hkl_geometry_init_geometry(_geometry, _engines->geometries->geometries[0]);
		hkl_pseudo_axis_engine_list_get(_engines);
		row[m_pseudoAxeModelColumns.write] = value;
		this->updateAxes();
		this->updatePseudoAxes();
		this->updateHKL();
	}
}

void
HKLWindow::on_cell_TreeView_pseudoAxes_is_initialized_toggled(Glib::ustring const & spath)
{
	Gtk::TreePath path(spath);
	Gtk::TreeModel::iterator iter = m_pseudoAxeModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	HklPseudoAxis *pseudoAxis = row[m_pseudoAxeModelColumns.pseudoAxis];
	bool old_flag = row[m_pseudoAxeModelColumns.is_initialized];
	if (!old_flag){
		int res;

		res = hkl_pseudo_axis_engine_initialize(pseudoAxis->engine, NULL);
		if(res == HKL_SUCCESS)
			this->updatePseudoAxes();
	}
}

//PseuodAxes Parameters
void
HKLWindow::on_cell_TreeView_pseudoAxes_parameters_value_edited(Glib::ustring const & spath,
							       Glib::ustring const & newText)
{
	double value;
	HklParameter *parameter;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_TreeView_pseudoAxes_parameters->get_model();
	Gtk::ListStore::Row row = *(listStore->get_iter(path));
	sscanf(newText.c_str(), "%lf", &value);

	parameter = row[m_parameterModelColumns.parameter];
	hkl_parameter_set_value_unit(parameter, value);

	row[m_parameterModelColumns.value] = value;
	this->updatePseudoAxes();
	this->update_pseudoAxes_parameters();
}
void
HKLWindow::on_cell_TreeView_crystals_name_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[m_crystalModelColumns.name];
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_sample_set_name(sample, newText.c_str());

		this-> updateTreeViewCrystals();
	}
}

void
HKLWindow::on_cell_TreeView_crystals_a_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[m_crystalModelColumns.name];
	double a;
	sscanf(newText.c_str(), "%lf", &a);

	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value(sample->lattice->a, a);

		row[m_crystalModelColumns.a] = a;
		m_spinbutton_a->set_value(a);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_b_edited(Glib::ustring const & spath,
					      Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[m_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value(sample->lattice->b, value);

		row[m_crystalModelColumns.b] = value;
		m_spinbutton_b->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_c_edited(Glib::ustring const & spath,
					      Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[m_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value(sample->lattice->c, value);

		row[m_crystalModelColumns.c] = value;
		m_spinbutton_b->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_alpha_edited(Glib::ustring const & spath,
						  Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[m_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value(sample->lattice->alpha, value);

		row[m_crystalModelColumns.alpha] = value;
		m_spinbutton_alpha->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_beta_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[m_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);
	
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value(sample->lattice->beta, value);

		row[m_crystalModelColumns.beta] = value;
		m_spinbutton_beta->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_gamma_edited(Glib::ustring const & spath,
						  Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[m_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value(sample->lattice->gamma, value);

		row[m_crystalModelColumns.gamma] = value;
		m_spinbutton_gamma->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_reflections_h_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		index = row[m_reflectionModelColumns.index];
		reflection = sample->reflections[index];

		sscanf(newText.c_str(), "%lf", &h);
		k = reflection->hkl.data[1];
		l = reflection->hkl.data[2];

		hkl_sample_reflection_set_hkl(reflection, h, k, l);

		row[m_reflectionModelColumns.h] = h;
		row[m_reflectionModelColumns.flag] = reflection->flag;
		this->updateCrystalModel(sample);
		this->updateFitness();
	}
}

void
HKLWindow::on_cell_TreeView_reflections_k_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;


		index = row[m_reflectionModelColumns.index];
		reflection = sample->reflections[index];

		h = reflection->hkl.data[0];
		sscanf(newText.c_str(), "%lf", &k);
		l = reflection->hkl.data[2];

		hkl_sample_reflection_set_hkl(reflection, h, k, l);
		row[m_reflectionModelColumns.k] = k;
		row[m_reflectionModelColumns.flag] = reflection->flag;
		this->updateCrystalModel(sample);
		this->updateFitness();
	}
}

void
HKLWindow::on_cell_TreeView_reflections_l_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		index = row[m_reflectionModelColumns.index];
		reflection = sample->reflections[index];

		h = reflection->hkl.data[0];
		k = reflection->hkl.data[1];
		sscanf(newText.c_str(), "%lf", &l);
		hkl_sample_reflection_set_hkl(reflection, h, k, l);
		row[m_reflectionModelColumns.l] = l;
		row[m_reflectionModelColumns.flag] = reflection->flag;
		this->updateCrystalModel(sample);
		this->updateFitness();
	}
}

void
HKLWindow::on_cell_TreeView_reflections_flag_toggled(Glib::ustring const & spath)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = m_treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		int flag;
		HklSampleReflection *reflection;

		index = row[m_reflectionModelColumns.index];
		reflection = sample->reflections[index];
		flag = !reflection->flag;
		hkl_sample_reflection_set_flag(reflection, flag);
		row[m_reflectionModelColumns.flag] = flag;
		this->updateFitness();
	}
}

void
HKLWindow::on_toolbutton_add_reflection_clicked(void)
{
	HklSample *sample;

	sample=_samples->current;
	if(sample){
		double h = m_spinbutton_h->get_value();
		double k = m_spinbutton_k->get_value();
		double l = m_spinbutton_l->get_value();

		hkl_sample_add_reflection(sample, _geometry, _detector, h, k, l);

		this->updateReflections(sample, m_mapReflectionModel[sample->name]);
		this->updateFitness();
	}
}

void
HKLWindow::on_toolbutton_goto_reflection_clicked(void)
{
	HklSample *sample;

	sample = _samples->current;
	if(sample){
		Glib::RefPtr<Gtk::TreeSelection> selection = m_treeViewReflections->get_selection();
		unsigned int nb_rows = selection->count_selected_rows();
		if (nb_rows == 1){
			Gtk::TreeSelection::ListHandle_Path list_path = selection->get_selected_rows();
			Gtk::TreePath path = *(list_path.begin());
			Glib::RefPtr<Gtk::ListStore> liststore = m_mapReflectionModel[sample->name];
			Gtk::ListStore::Row row = *(liststore->get_iter(path));
			unsigned int index = row[m_reflectionModelColumns.index];

			hkl_geometry_init_geometry(_geometry,
						   sample->reflections[index]->geometry);

			this->updateSource();
			this->updateAxes();
			this->updatePseudoAxes();
			this->updateHKL();
		}else{
			if (nb_rows)
				m_statusBar->push("Please select only one reflection.");
			else
				m_statusBar->push("Please select one reflection.");
		}
	}
}

void
HKLWindow::on_toolbutton_del_reflection_clicked(void)
{
	HklSample * sample;

	sample = _samples->current;
	if(sample){
		Glib::RefPtr<Gtk::TreeSelection> selection = m_treeViewReflections->get_selection();
		unsigned int nb_rows = selection->count_selected_rows();
		if (nb_rows){
			Gtk::TreeSelection::ListHandle_Path list = selection->get_selected_rows();
			Gtk::TreeSelection::ListHandle_Path::iterator iter = list.begin();
			Gtk::TreeSelection::ListHandle_Path::iterator last = list.end();
			Glib::RefPtr<Gtk::ListStore> liststore = m_mapReflectionModel[sample->name];
			// fill indexes with the reflections index
			std::vector<unsigned int> indexes;
			while(iter != last){
				Gtk::ListStore::Row row = *(liststore->get_iter(*iter));
				indexes.push_back(row[m_reflectionModelColumns.index]);
				++iter;
			}
			std::ostringstream os;
			os << "Are you sure you want to delete reflections :";
			for(unsigned int i=0; i< indexes.size();i++)
				os << " " << indexes[i];

			m_message = new Gtk::MessageDialog("", false,
							   Gtk::MESSAGE_WARNING,
							   Gtk::BUTTONS_YES_NO);
			m_message->set_message(os.str());
			m_message->show();
			int respons = m_message->run();
			switch (respons){
			case Gtk::RESPONSE_YES:
				for(unsigned int i=0;i<indexes.size();i++){
					// compute the correct index of the reflection
					unsigned int index = indexes[i] - i;
					hkl_sample_del_reflection(sample, index);
				}
				this->updateReflections(sample, liststore);
				break;
			}
			delete m_message;
		}else
			m_statusBar->push("Please select at least one reflection.");
		this->updateFitness();
	}
}

void
HKLWindow::on_toolbutton_computeUB_clicked(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		hkl_sample_compute_UB_busing_levy(sample, 0, 1);
		this->updateUB();
		this->updateHKL();
	}
}

void
HKLWindow::on_toolbutton_add_crystal_clicked(void)
{
	HklSample *sample = hkl_sample_new("new_sample", HKL_SAMPLE_MONOCRYSTAL);
	if(sample){
		hkl_sample_list_append(_samples, sample);
		hkl_sample_list_select_current(_samples, "new_sample");
		this->updateTreeViewCrystals();

		// activate for edition the name of the new crystal
		Gtk::TreeModel::Path path;
		Gtk::TreeView::Column * column;
		m_treeViewCrystals->get_cursor(path, column);
		column = m_treeViewCrystals->get_column(0);
		m_treeViewCrystals->set_cursor(path, *column, true);
	}
}

void
HKLWindow::on_toolbutton_copy_crystal_clicked(void)
{
	Glib::ustring name;
	Glib::ustring newname;
	HklSample *old_sample = _samples->current;
	if(!old_sample){
		m_statusBar->push("Please select a crystal to copy.");
		return;
	}

	hkl_sample_list_select_current(_samples, newname.c_str());
	if(_samples->current){	
		this->updateTreeViewCrystals();
		// activate for edition the name of the new crystal
		Gtk::TreeModel::Path path;
		Gtk::TreeView::Column * column;
		m_treeViewCrystals->get_cursor(path, column);
		column = m_treeViewCrystals->get_column(0);
		m_treeViewCrystals->set_cursor(path, *column, false);
	}
}

void
HKLWindow::on_toolbutton_del_crystal_clicked(void)
{
	if(_samples->current){
		hkl_sample_list_del(_samples, _samples->current);
		this->updateTreeViewCrystals();
	}
}

void
HKLWindow::on_toolbutton_affiner_clicked(void)
{
	Glib::ustring name;
	Glib::ustring method;
	HklSample *sample = _samples->current;
	if(sample)
		hkl_sample_affine(sample);

	this->updateCrystalModel(_samples->current);
	this->updateFitness();
	this->updateLattice();
	this->updateReciprocalLattice();
	this->updateHKL();
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
HklAxis *
HKLWindow::get_axe(Glib::ustring const & name)
{
	return hkl_geometry_get_axis_by_name(_geometry, name.c_str());
}

void
HKLWindow::set_up_TreeView_axes(void)
{
	size_t i;
	int index;
	HklHolder *holder;
	HklAxis *axes;
	Gtk::CellRenderer * renderer;

	//Create the Model
	m_axeModel = Gtk::ListStore::create(m_axeModelColumns);

	// add the columns
	index = m_TreeView_axes->append_column("name", m_axeModelColumns.name);

	index = m_TreeView_axes->append_column_numeric_editable("read",
								m_axeModelColumns.read, "%lf");
	renderer = m_TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_read_edited));
  
	index = m_TreeView_axes->append_column_numeric_editable("write",
								m_axeModelColumns.write, "%lf");
	renderer = m_TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_write_edited));
  
	index = m_TreeView_axes->append_column_numeric_editable("min",
								m_axeModelColumns.min, "%lf");
	renderer = m_TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_min_edited));
  
	index = m_TreeView_axes->append_column_numeric_editable("max",
								m_axeModelColumns.max, "%lf");
	renderer = m_TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_max_edited));

	//Fill the models from the diffractometerAxes
	// samples
	holder = &_geometry->holders[0];
	axes = _geometry->axes;
	for(i=0; i<HKL_LIST_LEN(holder->idx); ++i){
		HklAxis *axis = &axes[holder->idx[i]];

		Gtk::TreeModel::Children::iterator iter_row = *(m_axeModel->append());
		Gtk::ListStore::Row row = *(iter_row);
		row[m_axeModelColumns.axis] = axis;
		row[m_axeModelColumns.name] = ((HklParameter *)axis)->name;
	}
	// detector
	holder = &_geometry->holders[1];
	for(i=0; i<HKL_LIST_LEN(holder->idx); ++i){
		HklAxis *axis = &axes[holder->idx[i]];

		Gtk::TreeModel::Children::iterator iter_row = *(m_axeModel->append());
		Gtk::ListStore::Row row = *(iter_row);
		row[m_axeModelColumns.axis] = axis;
		row[m_axeModelColumns.name] = ((HklParameter *)axis)->name;
	}

	//Set the model for the TreeView
	m_TreeView_axes->set_model(m_axeModel);
	this->updateAxes();
}

void
HKLWindow::set_up_TreeView_pseudoAxes(void)
{
	size_t i;
	size_t j;
	size_t k;
	int index;
	Gtk::CellRenderer * renderer;

	/* add the columns */
	m_TreeView_pseudoAxes->append_column("name", m_pseudoAxeModelColumns.name);

	m_TreeView_pseudoAxes->append_column_numeric("read", m_pseudoAxeModelColumns.read, "%lf");
  
	index = m_TreeView_pseudoAxes->append_column_numeric_editable("write", m_pseudoAxeModelColumns.write, "%lf");
	renderer = m_TreeView_pseudoAxes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this,
			      &HKLWindow::on_cell_TreeView_pseudoAxes_write_edited));
  
	m_TreeView_pseudoAxes->append_column_numeric("min", m_pseudoAxeModelColumns.min, "%lf");
  
	m_TreeView_pseudoAxes->append_column_numeric("max", m_pseudoAxeModelColumns.max, "%lf");

	index = m_TreeView_pseudoAxes->append_column_editable(
		"initialized",
		m_pseudoAxeModelColumns.is_initialized);
	renderer = m_TreeView_pseudoAxes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(
		sigc::mem_fun(*this,
			      &HKLWindow::on_cell_TreeView_pseudoAxes_is_initialized_toggled));
  
	index = m_TreeView_pseudoAxes->append_column_editable(
		"readable",
		m_pseudoAxeModelColumns.is_readable);
  
	index = m_TreeView_pseudoAxes->append_column_editable(
		"writable",
		m_pseudoAxeModelColumns.is_writable);

	//Create the Model
	m_pseudoAxeModel = Gtk::ListStore::create(m_pseudoAxeModelColumns);

	//Fill the models from the diffractometer pseudoAxes
	for(i=0; i<HKL_LIST_LEN(_engines->engines); ++i){
		HklPseudoAxisEngine *engine = _engines->engines[i];

		for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j){
			HklPseudoAxis *pseudoAxis = engine->pseudoAxes[j];
			Gtk::ListStore::Row row = *(m_pseudoAxeModel->append());
			row[m_pseudoAxeModelColumns.pseudoAxis] = pseudoAxis;
			row[m_pseudoAxeModelColumns.name] = ((HklParameter *)pseudoAxis)->name;

			if(HKL_LIST_LEN(engine->mode->parameters)){
				Glib::RefPtr<Gtk::ListStore> model = Gtk::ListStore::create(m_parameterModelColumns);
				for(k=0; k<HKL_LIST_LEN(engine->mode->parameters); ++k){
					HklParameter *parameter = &engine->mode->parameters[k];

					Glib::RefPtr<Gtk::ListStore> model = Gtk::ListStore::create(m_parameterModelColumns);
					row = *(model->append());
					row[m_parameterModelColumns.parameter] = parameter;
					row[m_parameterModelColumns.name] = parameter->name;
					row[m_parameterModelColumns.value] = hkl_parameter_get_value_unit(parameter);
				}
				m_mapPseudoAxeParameterModel.insert(std::pair<HklPseudoAxis *,  Glib::RefPtr<Gtk::ListStore> >(pseudoAxis, model));
			}
		}
	}
	//Set the model for the TreeView
	m_TreeView_pseudoAxes->set_model(m_pseudoAxeModel);
	this->updatePseudoAxes();
}

void
HKLWindow::set_up_TreeView_pseudoAxes_parameters(void)
{
	int index;
	Gtk::CellRenderer * renderer;

	// add the columns
	m_TreeView_pseudoAxes_parameters->append_column(
		"name", m_parameterModelColumns.name);

	index = m_TreeView_pseudoAxes_parameters->append_column_numeric_editable(
		"value", m_parameterModelColumns.value, "%lf");
	renderer = m_TreeView_pseudoAxes_parameters->get_column_cell_renderer(index-1);

	// connect the signal_edited
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this,
			      &HKLWindow::on_cell_TreeView_pseudoAxes_parameters_value_edited));
}

void
HKLWindow::updateSource(void)
{
	double lambda = hkl_source_get_wavelength(&_geometry->source);
	m_spinbutton_lambda->set_value(lambda);
}

void
HKLWindow::updateAxes(void)
{
	// update the model
	Gtk::TreeModel::Children rows = m_axeModel->children();
	Gtk::TreeModel::Children::iterator iter = rows.begin();
	Gtk::TreeModel::Children::iterator end = rows.end();
	while(iter != end){
		double min;
		double max;
		HklAxis * axis;

		Gtk::TreeRow row = *iter;
		axis = row[m_axeModelColumns.axis];
		row[m_axeModelColumns.read] = hkl_axis_get_value_unit(axis);
		row[m_axeModelColumns.write] = hkl_axis_get_value_unit(axis);
		hkl_parameter_get_range_unit((HklParameter *)axis, &min, &max);
		row[m_axeModelColumns.min] = min;
		row[m_axeModelColumns.max] = max;
		++iter;
	}
}

void
HKLWindow::updatePseudoAxes(void)
{
	// first compute all the pseudoAxes values
	hkl_pseudo_axis_engine_list_get(_engines);

	// update the model
	Gtk::TreeModel::Children rows = m_pseudoAxeModel->children();
	Gtk::TreeModel::Children::iterator iter = rows.begin();
	Gtk::TreeModel::Children::iterator end = rows.end();
	while(iter != end){
		double min;
		double max;
		HklParameter *parameter;
		HklPseudoAxis *pseudoAxis;

		Gtk::TreeRow row = *iter;
		pseudoAxis = row[m_pseudoAxeModelColumns.pseudoAxis];
		parameter = (HklParameter *)pseudoAxis;
		row[m_pseudoAxeModelColumns.read] = hkl_parameter_get_value_unit(parameter);
		row[m_pseudoAxeModelColumns.write] = hkl_parameter_get_value_unit(parameter);
		hkl_parameter_get_range_unit(parameter, &min, &max);
		row[m_pseudoAxeModelColumns.min] = min;
		row[m_pseudoAxeModelColumns.max] = max;

		row[m_pseudoAxeModelColumns.is_initialized] = true;
		row[m_pseudoAxeModelColumns.is_readable] = true;
		row[m_pseudoAxeModelColumns.is_writable] = true;
		++iter;
	}
}

void
HKLWindow::update_pseudoAxes_parameters(void)
{
	std::map<HklPseudoAxis *, Glib::RefPtr<Gtk::ListStore> >::iterator iter = m_mapPseudoAxeParameterModel.begin();
	std::map<HklPseudoAxis *, Glib::RefPtr<Gtk::ListStore> >::iterator end = m_mapPseudoAxeParameterModel.end();
	while(iter != end){
		Gtk::TreeModel::Children rows = iter->second->children();
		Gtk::TreeModel::Children::iterator iter_row = rows.begin();
		Gtk::TreeModel::Children::iterator end_row = rows.end();
		while(iter_row != end_row){
			Gtk::TreeRow row = *iter_row;
			HklParameter *parameter = row[m_parameterModelColumns.parameter];
			row[m_parameterModelColumns.name] = parameter->name;
			row[m_parameterModelColumns.value] = hkl_parameter_get_value_unit(parameter);
			++iter_row;
		}
		++iter;
	}
}

void
HKLWindow::updateLattice(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		double a = hkl_parameter_get_value_unit(sample->lattice->a);
		double b = hkl_parameter_get_value_unit(sample->lattice->b);
		double c = hkl_parameter_get_value_unit(sample->lattice->c);
		double alpha = hkl_parameter_get_value_unit(sample->lattice->alpha);
		double beta = hkl_parameter_get_value_unit(sample->lattice->beta);
		double gamma = hkl_parameter_get_value_unit(sample->lattice->gamma);

		m_spinbutton_a->set_value(a);
		m_spinbutton_b->set_value(b);
		m_spinbutton_c->set_value(c);
		m_spinbutton_alpha->set_value(alpha);
		m_spinbutton_beta->set_value(beta);
		m_spinbutton_gamma->set_value(gamma);
	}
}

void
HKLWindow::updateLatticeParameters(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		double min;
		double max;
		bool to_fit;
		HklParameter *parameter;


		parameter = sample->lattice->a;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		m_spinbutton_a_min->set_value(min);
		m_spinbutton_a_max->set_value(max);
		m_checkbutton_a->set_active(parameter->fit);

		parameter = sample->lattice->b;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		m_spinbutton_b_min->set_value(min);
		m_spinbutton_b_max->set_value(max);
		m_checkbutton_b->set_active(parameter->fit);

		parameter = sample->lattice->c;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		m_spinbutton_c_min->set_value(min);
		m_spinbutton_c_max->set_value(max);
		m_checkbutton_c->set_active(parameter->fit);

		parameter = sample->lattice->alpha;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		m_spinbutton_alpha_min->set_value(min);
		m_spinbutton_alpha_max->set_value(max);
		m_checkbutton_alpha->set_active(parameter->fit);

		parameter = sample->lattice->beta;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		m_spinbutton_beta_min->set_value(min);
		m_spinbutton_beta_max->set_value(max);
		m_checkbutton_beta->set_active(parameter->fit);

		parameter = sample->lattice->gamma;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		m_spinbutton_gamma_min->set_value(min);
		m_spinbutton_gamma_max->set_value(max);
		m_checkbutton_gamma->set_active(parameter->fit);

		m_checkbutton_U->set_active(true);
	}
}

void
HKLWindow::updateReciprocalLattice(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklLattice *reciprocal = hkl_lattice_new_default();
		hkl_lattice_reciprocal(sample->lattice, reciprocal);

		m_spinbutton_a_star->set_value(hkl_parameter_get_value_unit(reciprocal->a));
		m_spinbutton_b_star->set_value(hkl_parameter_get_value_unit(reciprocal->b));
		m_spinbutton_c_star->set_value(hkl_parameter_get_value_unit(reciprocal->c));
		m_spinbutton_alpha_star->set_value(hkl_parameter_get_value_unit(reciprocal->alpha));
		m_spinbutton_beta_star->set_value(hkl_parameter_get_value_unit(reciprocal->beta));
		m_spinbutton_gamma_star->set_value(hkl_parameter_get_value_unit(reciprocal->gamma));
	}
}

void
HKLWindow::updateUB(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklMatrix UB;

		hkl_sample_get_UB(sample, &UB);
		m_label_UB11->set_text(Glib::Ascii::dtostr(UB.data[0][0]));
		m_label_UB12->set_text(Glib::Ascii::dtostr(UB.data[0][1]));
		m_label_UB13->set_text(Glib::Ascii::dtostr(UB.data[0][2]));
		m_label_UB21->set_text(Glib::Ascii::dtostr(UB.data[1][0]));
		m_label_UB22->set_text(Glib::Ascii::dtostr(UB.data[1][1]));
		m_label_UB23->set_text(Glib::Ascii::dtostr(UB.data[1][2]));
		m_label_UB31->set_text(Glib::Ascii::dtostr(UB.data[2][0]));
		m_label_UB32->set_text(Glib::Ascii::dtostr(UB.data[2][1]));
		m_label_UB33->set_text(Glib::Ascii::dtostr(UB.data[2][2]));
	}
}

void
HKLWindow::updateHKL(void)
{
	double h, k, l;

	h = hkl_parameter_get_value_unit((HklParameter *)(_hkl->pseudoAxes[0]));
	k = hkl_parameter_get_value_unit((HklParameter *)(_hkl->pseudoAxes[1]));
	l = hkl_parameter_get_value_unit((HklParameter *)(_hkl->pseudoAxes[2]));

	m_spinbutton_h->set_value(h);
	m_spinbutton_k->set_value(k);
	m_spinbutton_l->set_value(l);
}

void
HKLWindow::updateTreeViewCrystals(void)
{
	size_t i;
	HklSample *sample;

	Gtk::ListStore::Row row;
	Gtk::TreeModel::Children::iterator iter_row;
	Gtk::TreeModel::Children::iterator iter_current;
	Glib::ustring current_crystal_name;

	bool is_current_crystal_set = false;

	//clear all data in the models
	m_crystalModel = Gtk::ListStore::create(m_crystalModelColumns);

	// erase all reflections.
	m_mapReflectionModel.clear();

	if(_samples->current){
		is_current_crystal_set = true;
		current_crystal_name = _samples->current->name;
	}

	//Fill the models from the crystalList
	for(i=0; i<HKL_LIST_LEN(_samples->samples); ++i){
		HklLattice *lattice;

		sample = _samples->samples[i];
		lattice = sample->lattice;
		iter_row = *(m_crystalModel->append());
		if (is_current_crystal_set && current_crystal_name == sample->name)
			iter_current = iter_row;
		row = *(iter_row);
		row[m_crystalModelColumns.name] = sample->name;
		row[m_crystalModelColumns.a] = hkl_parameter_get_value_unit(lattice->a);
		row[m_crystalModelColumns.b] = hkl_parameter_get_value_unit(lattice->b);
		row[m_crystalModelColumns.c] = hkl_parameter_get_value_unit(lattice->c);
		row[m_crystalModelColumns.alpha] = hkl_parameter_get_value_unit(lattice->alpha);
		row[m_crystalModelColumns.beta] = hkl_parameter_get_value_unit(lattice->beta);
		row[m_crystalModelColumns.gamma] = hkl_parameter_get_value_unit(lattice->gamma);

		Glib::RefPtr<Gtk::ListStore> listStore = Gtk::ListStore::create(m_reflectionModelColumns);
		m_mapReflectionModel[sample->name] = listStore;
		this->updateReflections(sample, listStore);
	}

	//Set the model for the TreeView
	m_treeViewCrystals->set_model(m_crystalModel);
	if (is_current_crystal_set)
	{
		Gtk::TreeModel::Path path = m_crystalModel->get_path(iter_current);
		m_treeViewCrystals->set_cursor(path);
		m_treeViewReflections->set_model(m_mapReflectionModel[current_crystal_name]);
	}
}

void
HKLWindow::updateReflections(const HklSample *sample, Glib::RefPtr<Gtk::ListStore> & listStore)
{
	size_t i;

	listStore->clear();
	Gtk::ListStore::Row row;
	for(i=0; i<HKL_LIST_LEN(sample->reflections); ++i){
		HklSampleReflection *reflection = sample->reflections[i];

		row = *(listStore->append());
		row[m_reflectionModelColumns.index] = i;
		row[m_reflectionModelColumns.h] = reflection->hkl.data[0];
		row[m_reflectionModelColumns.k] = reflection->hkl.data[1];
		row[m_reflectionModelColumns.l] = reflection->hkl.data[2];
		row[m_reflectionModelColumns.flag] = reflection->flag;
	}
}

void
HKLWindow::updateStatusBar(const HklError *error)
{
	m_statusBar->push(error->message);
}

void
HKLWindow::updateFitness(void)
{
}

void
HKLWindow::updateAffinement(void)
{
}

void
HKLWindow::updateCrystalModel(HklSample * sample)
{
	Gtk::TreeModel::Children children = m_crystalModel->children();
	Gtk::TreeModel::Children::iterator iter = children.begin();
	Gtk::TreeModel::Children::iterator end = children.end();
	while (iter != end){
		Gtk::TreeModel::Row const & row = *iter;
		if (row[m_crystalModelColumns.name] == sample->name){
			HklLattice *lattice = sample->lattice;
			row[m_crystalModelColumns.a] = hkl_parameter_get_value_unit(lattice->a);
			row[m_crystalModelColumns.b] = hkl_parameter_get_value_unit(lattice->b);
			row[m_crystalModelColumns.c] = hkl_parameter_get_value_unit(lattice->c);
			row[m_crystalModelColumns.alpha] = hkl_parameter_get_value_unit(lattice->alpha);
			row[m_crystalModelColumns.beta] = hkl_parameter_get_value_unit(lattice->beta);
			row[m_crystalModelColumns.gamma] = hkl_parameter_get_value_unit(lattice->gamma);
			iter = end;
		}
		else
			++iter;
	}
}
