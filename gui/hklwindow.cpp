/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "hklwindow.h"

HKLWindow::HKLWindow(HklGeometryType type)
{
	size_t i;
	HklSample *sample;

	_geometry = hkl_geometry_factory_new(type, 50 * HKL_DEGTORAD);
	
	_detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
        _detector->idx = 1;
	
	_samples = hkl_sample_list_new();
	// add a default crystal
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_list_append(_samples, sample);
	hkl_sample_list_select_current(_samples, "test");

	_engines = hkl_pseudo_axis_engine_list_factory(type);
	_hkl = hkl_pseudo_axis_engine_list_get_by_name(_engines, "hkl");
	hkl_pseudo_axis_engine_list_init(_engines, _geometry, _detector, _samples->current);

	// Sets the border width of the window.
	this->set_border_width(10);

	//Get Glade UI:
	_refGlade = Gtk::Builder::create();
	if(!_refGlade->add_from_file("hkl.ui")){
		std::string filename = Glib::build_filename(PKGDATA, "hkl.ui");
		if(!_refGlade->add_from_file(filename))
			exit(1);
	}

	for(i=0; i<HKL_LIST_LEN(_engines->engines); ++i)
		_pseudoAxesFrames.push_back(new PseudoAxesFrame(_engines->engines[i]));

	// Get all pointers on usefull widgets
	_refGlade->get_widget("label_UB11", _label_UB11);
	_refGlade->get_widget("label_UB12", _label_UB12);
	_refGlade->get_widget("label_UB13", _label_UB13);
	_refGlade->get_widget("label_UB21", _label_UB21);
	_refGlade->get_widget("label_UB22", _label_UB22);
	_refGlade->get_widget("label_UB23", _label_UB23);
	_refGlade->get_widget("label_UB31", _label_UB31);
	_refGlade->get_widget("label_UB32", _label_UB32);
	_refGlade->get_widget("label_UB33", _label_UB33);
	_refGlade->get_widget("spinbutton_a_star", _spinbutton_a_star);
	_refGlade->get_widget("spinbutton_b_star", _spinbutton_b_star);
	_refGlade->get_widget("spinbutton_c_star", _spinbutton_c_star);
	_refGlade->get_widget("spinbutton_alpha_star", _spinbutton_alpha_star);
	_refGlade->get_widget("spinbutton_beta_star", _spinbutton_beta_star);
	_refGlade->get_widget("spinbutton_gamma_star", _spinbutton_gamma_star);
	_refGlade->get_widget("spinbutton_a", _spinbutton_a);
	_refGlade->get_widget("spinbutton_b", _spinbutton_b);
	_refGlade->get_widget("spinbutton_c", _spinbutton_c);
	_refGlade->get_widget("spinbutton_alpha", _spinbutton_alpha);
	_refGlade->get_widget("spinbutton_beta", _spinbutton_beta);
	_refGlade->get_widget("spinbutton_gamma", _spinbutton_gamma);
	_refGlade->get_widget("spinbutton_a_min", _spinbutton_a_min);
	_refGlade->get_widget("spinbutton_b_min", _spinbutton_b_min);
	_refGlade->get_widget("spinbutton_c_min", _spinbutton_c_min);
	_refGlade->get_widget("spinbutton_alpha_min", _spinbutton_alpha_min);
	_refGlade->get_widget("spinbutton_beta_min", _spinbutton_beta_min);
	_refGlade->get_widget("spinbutton_gamma_min", _spinbutton_gamma_min);
	_refGlade->get_widget("spinbutton_a_max", _spinbutton_a_max);
	_refGlade->get_widget("spinbutton_b_max", _spinbutton_b_max);
	_refGlade->get_widget("spinbutton_c_max", _spinbutton_c_max);
	_refGlade->get_widget("spinbutton_alpha_max", _spinbutton_alpha_max);
	_refGlade->get_widget("spinbutton_beta_max", _spinbutton_beta_max);
	_refGlade->get_widget("spinbutton_gamma_max", _spinbutton_gamma_max);
	_refGlade->get_widget("spinbutton_lambda", _spinbutton_lambda);
	_refGlade->get_widget("spinbutton_ux", _spinbutton_ux);
	_refGlade->get_widget("spinbutton_uy", _spinbutton_uy);
	_refGlade->get_widget("spinbutton_uz", _spinbutton_uz);
	_refGlade->get_widget("checkbutton_a", _checkbutton_a);
	_refGlade->get_widget("checkbutton_b", _checkbutton_b);
	_refGlade->get_widget("checkbutton_c", _checkbutton_c);
	_refGlade->get_widget("checkbutton_alpha", _checkbutton_alpha);
	_refGlade->get_widget("checkbutton_beta", _checkbutton_beta);
	_refGlade->get_widget("checkbutton_gamma", _checkbutton_gamma);
	_refGlade->get_widget("checkbutton_U", _checkbutton_U);
	_refGlade->get_widget("treeview_reflections", _treeViewReflections);
	_refGlade->get_widget("treeview_crystals", _treeViewCrystals);
	_refGlade->get_widget("treeview_axes", _TreeView_axes);
	_refGlade->get_widget("treeview_pseudoAxes", _TreeView_pseudoAxes);
	_refGlade->get_widget("treeview_pseudoAxes_parameters", _TreeView_pseudoAxes_parameters);
	_refGlade->get_widget("treeview1", _treeview1);
	_refGlade->get_widget("toolbutton_add_reflection", _toolbutton_add_reflection);
	_refGlade->get_widget("toolbutton_goto_reflection", _toolbutton_goto_reflection);
	_refGlade->get_widget("toolbutton_del_reflection", _toolbutton_del_reflection);
	_refGlade->get_widget("toolbutton_computeUB", _toolbutton_computeUB);
	_refGlade->get_widget("toolbutton_add_crystal", _toolbutton_add_crystal);
	_refGlade->get_widget("toolbutton_copy_crystal", _toolbutton_copy_crystal);
	_refGlade->get_widget("toolbutton_del_crystal", _toolbutton_del_crystal);
	_refGlade->get_widget("toolbutton_affiner", _toolbutton_affiner);
	_refGlade->get_widget("statusbar", _statusBar);
	_refGlade->get_widget("menuitem5", _menuitem5);

	// dialog1
	_refGlade->get_widget("dialog1", _dialog1);
	_refGlade->get_widget("button1", _button1);
	_refGlade->get_widget("combobox1", _combobox1);

	_diffractometerModel = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore1"));
	// TODO add the diffractometers types and names.

	// add all the pseudo axes frames
	Gtk::VBox *vbox2 = NULL;
	_refGlade->get_widget("vbox2", vbox2);
	for(i=0; i<_pseudoAxesFrames.size(); ++i){
		vbox2->add(_pseudoAxesFrames[i]->frame());
		_pseudoAxesFrames[i]->signal_changed ().connect (
			sigc::mem_fun (*this, &HKLWindow::on_pseudoAxesFrame_changed) );
	}
	vbox2->show_all();

	this->set_up_TreeView_axes();
	this->set_up_TreeView_pseudoAxes_parameters();
	this->set_up_TreeView_pseudoAxes();

	_solutionModelColumns = 0;
	this->set_up_TreeView_treeview1();

  
	int index;
	Gtk::CellRenderer * renderer;

	//Set up the treeViewReflections
	_treeViewReflections->append_column("index", _reflectionModelColumns.index);

	index = _treeViewReflections->append_column_numeric_editable("h", _reflectionModelColumns.h, "%lf");
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_h_edited));

	index = _treeViewReflections->append_column_numeric_editable("k", _reflectionModelColumns.k, "%lf");
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_k_edited));

	index = _treeViewReflections->append_column_numeric_editable("l", _reflectionModelColumns.l, "%lf");
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_l_edited));

	index = _treeViewReflections->append_column_editable("flag", _reflectionModelColumns.flag);
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_flag_toggled));

	_treeViewReflections->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

	//Set up the treeViewCrystals
	index = _treeViewCrystals->append_column_editable("name", _crystalModelColumns.name);
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_name_edited));

	index = _treeViewCrystals->append_column_numeric_editable("a", _crystalModelColumns.a, "%lf");
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_a_edited));

	index = _treeViewCrystals->append_column_numeric_editable("b", _crystalModelColumns.b, "%lf");
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_b_edited));

	index = _treeViewCrystals->append_column_numeric_editable("c", _crystalModelColumns.c, "%lf");
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_c_edited));

	index = _treeViewCrystals->append_column_numeric_editable("alpha", _crystalModelColumns.alpha, "%lf");
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_alpha_edited));

	index = _treeViewCrystals->append_column_numeric_editable("beta", _crystalModelColumns.beta, "%lf");
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_beta_edited));

	index = _treeViewCrystals->append_column_numeric_editable("gamma", _crystalModelColumns.gamma, "%lf");
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_gamma_edited));

	_treeViewCrystals->append_column("fitness", _crystalModelColumns.fitness);

	_treeViewCrystals->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);

	//update the widgets
	this->updateTreeViewCrystals();
	this->updateSource();
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUB();

	//signal connection
	_spinbutton_a->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_a_value_changed));
	_spinbutton_b->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_b_value_changed));
	_spinbutton_c->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_c_value_changed));
	_spinbutton_alpha->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_alpha_value_changed));
	_spinbutton_beta->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_beta_value_changed));
	_spinbutton_gamma->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_gamma_value_changed));
	_spinbutton_a_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_a_min_value_changed));
	_spinbutton_b_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_b_min_value_changed));
	_spinbutton_c_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_c_min_value_changed));
	_spinbutton_alpha_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_alpha_min_value_changed));
	_spinbutton_beta_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_beta_min_value_changed));
	_spinbutton_gamma_min->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_gamma_min_value_changed));
	_spinbutton_a_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_a_max_value_changed));
	_spinbutton_b_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_b_max_value_changed));
	_spinbutton_c_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_c_max_value_changed));
	_spinbutton_alpha_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_alpha_max_value_changed));
	_spinbutton_beta_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_beta_max_value_changed));
	_spinbutton_gamma_max->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_gamma_max_value_changed));
	_spinbutton_lambda->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_lambda_value_changed));
	_spinbutton_ux->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_uxuyuz_value_changed));
	_spinbutton_uy->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_uxuyuz_value_changed));
	_spinbutton_uz->signal_value_changed().connect(mem_fun(*this, &HKLWindow::on_spinbutton_uxuyuz_value_changed));

	_checkbutton_a->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_a_toggled));
	_checkbutton_b->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_b_toggled));
	_checkbutton_c->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_c_toggled));
	_checkbutton_alpha->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_alpha_toggled));
	_checkbutton_beta->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_beta_toggled));
	_checkbutton_gamma->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_gamma_toggled));
	_checkbutton_U->signal_toggled().connect(mem_fun(*this, &HKLWindow::on_checkbutton_U_toggled));

	_treeViewReflections->signal_key_press_event().connect(mem_fun(*this, &HKLWindow::on_treeViewReflections_key_press_event));
	_TreeView_pseudoAxes->signal_cursor_changed().connect(mem_fun(*this, &HKLWindow::on_treeView_pseudoAxes_cursor_changed));
	_treeViewCrystals->signal_cursor_changed().connect(mem_fun(*this, &HKLWindow::on_treeViewCrystals_cursor_changed));
	_treeViewCrystals->signal_key_press_event().connect(mem_fun(*this, &HKLWindow::on_treeViewCrystals_key_press_event));

	_toolbutton_add_reflection->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_add_reflection_clicked));
	_toolbutton_goto_reflection->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_goto_reflection_clicked));
	_toolbutton_del_reflection->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_del_reflection_clicked));
	_toolbutton_computeUB->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_computeUB_clicked));
	_toolbutton_add_crystal->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_add_crystal_clicked));
	_toolbutton_copy_crystal->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_copy_crystal_clicked));
	_toolbutton_del_crystal->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_del_crystal_clicked));
	_toolbutton_affiner->signal_clicked().connect(mem_fun(*this, &HKLWindow::on_toolbutton_affiner_clicked));

	_menuitem5->signal_activate().connect(
		mem_fun(*this, &HKLWindow::on_menuitem5_activate));

	// dialog1
	_button1->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_button1_clicked));
	_combobox1->signal_changed().connect(
		mem_fun(*this, &HKLWindow::on_combobox1_changed));

	this->show_all_children();
}

HKLWindow::~HKLWindow()
{
	hkl_geometry_free(_geometry);
	hkl_detector_free(_detector);
	hkl_pseudo_axis_engine_list_free(_engines);
	hkl_sample_list_free(_samples);
}

/************/
/* Callback */
/************/

void
HKLWindow::on_treeView_pseudoAxes_cursor_changed(void)
{
	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_TreeView_pseudoAxes->get_cursor(path, column);
	Gtk::ListStore::Row row = *(_pseudoAxeModel->get_iter(path));
	HklPseudoAxis *pseudoAxis = row[_pseudoAxeModelColumns.pseudoAxis];
	_TreeView_pseudoAxes_parameters->set_model(_mapPseudoAxeParameterModel[pseudoAxis]);
}

void
HKLWindow::on_treeViewCrystals_cursor_changed(void)
{
	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_treeViewCrystals->get_cursor(path, column);
	Gtk::TreeModel::iterator iter = _crystalModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_crystalModelColumns.name];
	hkl_sample_list_select_current(_samples, name.c_str());
	hkl_pseudo_axis_engine_list_init(_engines, _geometry, _detector, _samples->current);
	_treeViewReflections->set_model(_mapReflectionModel[name]);
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_a_value_changed(void)
{
	double a, b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = _spinbutton_a->get_value();
	hkl_parameter_set_value_unit(sample->lattice->a, value);
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_b_value_changed(void)
{
	double a, b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = _spinbutton_b->get_value();
	hkl_parameter_set_value_unit(sample->lattice->b, value);
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_c_value_changed(void)
{
	double a, b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = _spinbutton_c->get_value();
	hkl_parameter_set_value_unit(sample->lattice->c, value);
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_alpha_value_changed(void)
{
	double a, b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = _spinbutton_alpha->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	hkl_parameter_set_value_unit(sample->lattice->alpha, value);
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_beta_value_changed(void)
{
	double a, b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = _spinbutton_b->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	hkl_parameter_set_value_unit(sample->lattice->beta, value);
	beta = sample->lattice->beta->value;
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_gamma_value_changed(void)
{
	double a, b, c, alpha, beta, gamma;
	HklSample *sample = _samples->current;

	double value = _spinbutton_b->get_value();
	a = sample->lattice->a->value;
	b = sample->lattice->b->value;
	c = sample->lattice->c->value;
	alpha = sample->lattice->alpha->value;
	beta = sample->lattice->beta->value;
	hkl_parameter_set_value_unit(sample->lattice->gamma, value);
	gamma = sample->lattice->gamma->value;
	hkl_sample_set_lattice(sample, a, b, c, alpha, beta, gamma);

	this->updateCrystalModel(sample);
	this->updateReciprocalLattice();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_spinbutton_a_min_value_changed(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklParameter *parameter = sample->lattice->a;
		double min = _spinbutton_a_min->get_value();
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
		double min = _spinbutton_b_min->get_value();
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
		double min = _spinbutton_c_min->get_value();
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
		double min = _spinbutton_alpha_min->get_value();
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
		double min = _spinbutton_beta_min->get_value();
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
		double min = _spinbutton_gamma_min->get_value();
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
		double max = _spinbutton_a_max->get_value();
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
		double max = _spinbutton_b_max->get_value();
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
		double max = _spinbutton_c_max->get_value();
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
		double max = _spinbutton_alpha_max->get_value();
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
		double max = _spinbutton_beta_max->get_value();
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
		double max = _spinbutton_gamma_max->get_value();
		double min = parameter->range.min;
		hkl_parameter_set_range_unit(parameter, min, max);
	}
}

void
HKLWindow::on_spinbutton_lambda_value_changed(void)
{
	_geometry->source.wave_length = _spinbutton_lambda->get_value();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void HKLWindow::on_spinbutton_uxuyuz_value_changed(void)
{
	if(_samples->current){
		hkl_sample_set_U_from_euler(_samples->current,
					    _spinbutton_ux->get_value() * HKL_DEGTORAD,
					    _spinbutton_uy->get_value() * HKL_DEGTORAD,
					    _spinbutton_uz->get_value() * HKL_DEGTORAD);

		this->updateUB();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void
HKLWindow::on_checkbutton_a_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->a->fit = _checkbutton_a->get_active();
}

void
HKLWindow::on_checkbutton_b_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->b->fit = _checkbutton_b->get_active();
}

void
HKLWindow::on_checkbutton_c_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->c->fit = _checkbutton_c->get_active();
}

void
HKLWindow::on_checkbutton_alpha_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->alpha->fit = _checkbutton_alpha->get_active();
}

void
HKLWindow::on_checkbutton_beta_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->beta->fit = _checkbutton_beta->get_active();
}

void
HKLWindow::on_checkbutton_gamma_toggled(void)
{
	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->gamma->fit = _checkbutton_gamma->get_active();
}

// TODO affinement
void
HKLWindow::on_checkbutton_U_toggled(void)
{
}

void
HKLWindow::on_axeSpinButton_changed(void)
{
	this->updatePseudoAxes();
}

void
HKLWindow::on_pseudoAxeSpinButton_value_changed(void)
{
	this->updateAxes();
	this->updatePseudoAxes();
}

void
HKLWindow::on_cell_TreeView_axes_read_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_axeModelColumns.name];
	double value;
	sscanf(newText.c_str(), "%lf", &value);
	HklAxis *axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_axis_set_value_unit(axis, value);
	hkl_geometry_update(_geometry);

	row[_axeModelColumns.read] = value;
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_cell_TreeView_axes_write_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_axeModelColumns.name];
	double value;
	sscanf(newText.c_str(), "%lf", &value);
	HklAxis *axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_axis_set_value_unit(axis, value);
	hkl_geometry_update(_geometry);

	row[_axeModelColumns.write] = value;
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void
HKLWindow::on_cell_TreeView_axes_min_edited(Glib::ustring const & spath,
					    Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	double shit;
	double max;
	double value;
	HklAxis *axis;

	Glib::ustring name = row[_axeModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_parameter_get_range_unit((HklParameter *)axis, &shit, &max);
	hkl_parameter_set_range_unit((HklParameter *)axis, value, max);

	row[_axeModelColumns.min] = value;
	this->updatePseudoAxes();
}

void
HKLWindow::on_cell_TreeView_axes_max_edited(Glib::ustring const & spath, Glib::ustring const & newText)
{
	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	double min, shit;
	double value;
	HklAxis *axis;

	Glib::ustring name = row[_axeModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	axis = hkl_geometry_get_axis_by_name(_geometry, name.c_str());
	hkl_parameter_get_range_unit((HklParameter *)axis, &min, &shit);
	hkl_parameter_set_range_unit((HklParameter *)axis, min, value);

	row[_axeModelColumns.max] = value;
	this->updatePseudoAxes();
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
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_pseudoAxes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	pseudoAxis = row[_pseudoAxeModelColumns.pseudoAxis];
	Glib::ustring name = row[_pseudoAxeModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	hkl_parameter_set_value_unit((HklParameter *)pseudoAxis, value);
	error = NULL;
	if(hkl_pseudo_axis_engine_set(pseudoAxis->engine, &error) == HKL_SUCCESS){
		hkl_geometry_init_geometry(_geometry, _engines->geometries->geometries[0]);
		hkl_pseudo_axis_engine_list_get(_engines);
		row[_pseudoAxeModelColumns.write] = value;
		this->updateAxes();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
		this->updateSolutions();
	}
}

void
HKLWindow::on_cell_TreeView_pseudoAxes_is_initialized_toggled(Glib::ustring const & spath)
{
	Gtk::TreePath path(spath);
	Gtk::TreeModel::iterator iter = _pseudoAxeModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	HklPseudoAxis *pseudoAxis = row[_pseudoAxeModelColumns.pseudoAxis];
	bool old_flag = row[_pseudoAxeModelColumns.is_initialized];
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
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_pseudoAxes_parameters->get_model();
	Gtk::ListStore::Row row = *(listStore->get_iter(path));
	sscanf(newText.c_str(), "%lf", &value);

	parameter = row[_parameterModelColumns.parameter];
	hkl_parameter_set_value_unit(parameter, value);

	row[_parameterModelColumns.value] = value;
	this->updatePseudoAxes();
	this->update_pseudoAxes_parameters();
}
void
HKLWindow::on_cell_TreeView_crystals_name_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[_crystalModelColumns.name];
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
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[_crystalModelColumns.name];
	double a;
	sscanf(newText.c_str(), "%lf", &a);

	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value_unit(sample->lattice->a, a);

		row[_crystalModelColumns.a] = a;
		_spinbutton_a->set_value(a);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_b_edited(Glib::ustring const & spath,
					      Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value_unit(sample->lattice->b, value);

		row[_crystalModelColumns.b] = value;
		_spinbutton_b->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_c_edited(Glib::ustring const & spath,
					      Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value_unit(sample->lattice->c, value);

		row[_crystalModelColumns.c] = value;
		_spinbutton_c->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_alpha_edited(Glib::ustring const & spath,
						  Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value_unit(sample->lattice->alpha, value);

		row[_crystalModelColumns.alpha] = value;
		_spinbutton_alpha->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_beta_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);
	
	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value_unit(sample->lattice->beta, value);

		row[_crystalModelColumns.beta] = value;
		_spinbutton_beta->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_crystals_gamma_edited(Glib::ustring const & spath,
						  Glib::ustring const & newText)
{
	HklSample *sample;
	double value;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_crystalModelColumns.name];
	sscanf(newText.c_str(), "%lf", &value);

	sample = hkl_sample_list_get_by_name(_samples, name.c_str());
	if(sample){
		hkl_parameter_set_value_unit(sample->lattice->gamma, value);

		row[_crystalModelColumns.gamma] = value;
		_spinbutton_gamma->set_value(value);
	}
}

void
HKLWindow::on_cell_TreeView_reflections_h_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		index = row[_reflectionModelColumns.index];
		reflection = sample->reflections[index];

		sscanf(newText.c_str(), "%lf", &h);
		k = reflection->hkl.data[1];
		l = reflection->hkl.data[2];

		hkl_sample_reflection_set_hkl(reflection, h, k, l);

		row[_reflectionModelColumns.h] = h;
		row[_reflectionModelColumns.flag] = reflection->flag;
		this->updateCrystalModel(sample);
	}
}

void
HKLWindow::on_cell_TreeView_reflections_k_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;


		index = row[_reflectionModelColumns.index];
		reflection = sample->reflections[index];

		h = reflection->hkl.data[0];
		sscanf(newText.c_str(), "%lf", &k);
		l = reflection->hkl.data[2];

		hkl_sample_reflection_set_hkl(reflection, h, k, l);
		row[_reflectionModelColumns.k] = k;
		row[_reflectionModelColumns.flag] = reflection->flag;
		this->updateCrystalModel(sample);
	}
}

void
HKLWindow::on_cell_TreeView_reflections_l_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		index = row[_reflectionModelColumns.index];
		reflection = sample->reflections[index];

		h = reflection->hkl.data[0];
		k = reflection->hkl.data[1];
		sscanf(newText.c_str(), "%lf", &l);
		hkl_sample_reflection_set_hkl(reflection, h, k, l);
		row[_reflectionModelColumns.l] = l;
		row[_reflectionModelColumns.flag] = reflection->flag;
		this->updateCrystalModel(sample);
	}
}

void
HKLWindow::on_cell_TreeView_reflections_flag_toggled(Glib::ustring const & spath)
{
	HklSample *sample;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	sample = _samples->current;
	if(sample){
		int index;
		int flag;
		HklSampleReflection *reflection;

		index = row[_reflectionModelColumns.index];
		reflection = sample->reflections[index];
		flag = !reflection->flag;
		hkl_sample_reflection_set_flag(reflection, flag);
		row[_reflectionModelColumns.flag] = flag;
	}
}

void
HKLWindow::on_toolbutton_add_reflection_clicked(void)
{
	HklSample *sample;

	sample=_samples->current;
	if(sample){
		double h;
		double k;
		double l;

		hkl_sample_add_reflection(sample, _geometry, _detector, h, k, l);

		this->updateReflections(sample, _mapReflectionModel[sample->name]);
	}
}

void
HKLWindow::on_toolbutton_goto_reflection_clicked(void)
{
	HklSample *sample;

	sample = _samples->current;
	if(sample){
		Glib::RefPtr<Gtk::TreeSelection> selection = _treeViewReflections->get_selection();
		unsigned int nb_rows = selection->count_selected_rows();
		if (nb_rows == 1){
			Gtk::TreeSelection::ListHandle_Path list_path = selection->get_selected_rows();
			Gtk::TreePath path = *(list_path.begin());
			Glib::RefPtr<Gtk::ListStore> liststore = _mapReflectionModel[sample->name];
			Gtk::ListStore::Row row = *(liststore->get_iter(path));
			unsigned int index = row[_reflectionModelColumns.index];

			hkl_geometry_init_geometry(_geometry,
						   sample->reflections[index]->geometry);

			this->updateSource();
			this->updateAxes();
			this->updatePseudoAxes();
		}else{
			if (nb_rows)
				_statusBar->push("Please select only one reflection.");
			else
				_statusBar->push("Please select one reflection.");
		}
	}
}

void
HKLWindow::on_toolbutton_del_reflection_clicked(void)
{
	HklSample * sample;

	sample = _samples->current;
	if(sample){
		Glib::RefPtr<Gtk::TreeSelection> selection = _treeViewReflections->get_selection();
		unsigned int nb_rows = selection->count_selected_rows();
		if (nb_rows){
			Gtk::TreeSelection::ListHandle_Path list = selection->get_selected_rows();
			Gtk::TreeSelection::ListHandle_Path::iterator iter = list.begin();
			Gtk::TreeSelection::ListHandle_Path::iterator last = list.end();
			Glib::RefPtr<Gtk::ListStore> liststore = _mapReflectionModel[sample->name];
			// fill indexes with the reflections index
			std::vector<unsigned int> indexes;
			while(iter != last){
				Gtk::ListStore::Row row = *(liststore->get_iter(*iter));
				indexes.push_back(row[_reflectionModelColumns.index]);
				++iter;
			}
			std::ostringstream os;
			os << "Are you sure you want to delete reflections :";
			for(unsigned int i=0; i< indexes.size();i++)
				os << " " << indexes[i];

			_message = new Gtk::MessageDialog("", false,
							   Gtk::MESSAGE_WARNING,
							   Gtk::BUTTONS_YES_NO);
			_message->set_message(os.str());
			_message->show();
			int respons = _message->run();
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
			delete _message;
		}else
			_statusBar->push("Please select at least one reflection.");
	}
}

void
HKLWindow::on_toolbutton_computeUB_clicked(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		hkl_sample_compute_UB_busing_levy(sample, 0, 1);
		this->updateUB();
		this->updateUxUyUz();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void
HKLWindow::on_toolbutton_add_crystal_clicked(void)
{
	HklSample *sample = hkl_sample_new("new_sample", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	if(sample){
		hkl_sample_list_append(_samples, sample);
		hkl_sample_list_select_current(_samples, "new_sample");
		this->updateTreeViewCrystals();

		// activate for edition the name of the new crystal
		Gtk::TreeModel::Path path;
		Gtk::TreeView::Column * column;
		_treeViewCrystals->get_cursor(path, column);
		column = _treeViewCrystals->get_column(0);
		_treeViewCrystals->set_cursor(path, *column, true);
	}
}

void
HKLWindow::on_toolbutton_copy_crystal_clicked(void)
{
	Glib::ustring name;
	Glib::ustring newname;
	HklSample *old_sample = _samples->current;
	HklSample *sample;
	if(!old_sample){
		_statusBar->push("Please select a crystal to copy.");
		return;
	}

	sample = hkl_sample_new_copy(_samples->current);
	hkl_sample_set_name(sample, "copy");
	hkl_sample_list_append(_samples, sample);
	hkl_sample_list_select_current(_samples, "copy");
	this->updateTreeViewCrystals();

	// activate for edition the name of the new crystal
	Gtk::TreeModel::Path path;
	Gtk::TreeView::Column * column;
	_treeViewCrystals->get_cursor(path, column);
	column = _treeViewCrystals->get_column(0);
	_treeViewCrystals->set_cursor(path, *column, true);
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
	this->updateLattice();
	this->updateReciprocalLattice();
	this->updateUB();
	this->updateUxUyUz();
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

void
HKLWindow::on_treeview1_cursor_changed(void)
{
	size_t index;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_treeview1->get_cursor(path, column);
	Gtk::TreeModel::iterator iter = _solutionModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	index = row[_solutionModelColumns->index];

	hkl_geometry_init_geometry(_geometry, _engines->geometries->geometries[index]);
	hkl_pseudo_axis_engine_list_get(_engines);

	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUB();
	this->updateAxes();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void HKLWindow::on_pseudoAxesFrame_changed(void)
{
	this->updateAxes();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
	this->updateSolutions();
}

void HKLWindow::on_menuitem5_activate(void)
{
	_dialog1->show();
}

void HKLWindow::on_button1_clicked(void)
{
	_dialog1->hide();
}

void HKLWindow::on_combobox1_changed(void)
{
	size_t idx = _combobox1->get_active_row_number();
}

/****************/
/* Non-Callback */
/****************/

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
	_axeModel = Gtk::ListStore::create(_axeModelColumns);

	// add the columns
	index = _TreeView_axes->append_column("name", _axeModelColumns.name);

	index = _TreeView_axes->append_column_numeric_editable("read",
								_axeModelColumns.read, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_read_edited));
  
	index = _TreeView_axes->append_column_numeric_editable("write",
								_axeModelColumns.write, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_write_edited));
  
	index = _TreeView_axes->append_column_numeric_editable("min",
								_axeModelColumns.min, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_min_edited));
  
	index = _TreeView_axes->append_column_numeric_editable("max",
								_axeModelColumns.max, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_max_edited));

	//Fill the models from the diffractometerAxes
	// samples
	holder = &_geometry->holders[0];
	axes = _geometry->axes;
	for(i=0; i<HKL_LIST_LEN(holder->idx); ++i){
		HklAxis *axis = &axes[holder->idx[i]];

		Gtk::TreeModel::Children::iterator iter_row = *(_axeModel->append());
		Gtk::ListStore::Row row = *(iter_row);
		row[_axeModelColumns.axis] = axis;
		row[_axeModelColumns.name] = ((HklParameter *)axis)->name;
	}
	// detector
	holder = &_geometry->holders[1];
	for(i=0; i<HKL_LIST_LEN(holder->idx); ++i){
		HklAxis *axis = &axes[holder->idx[i]];

		Gtk::TreeModel::Children::iterator iter_row = *(_axeModel->append());
		Gtk::ListStore::Row row = *(iter_row);
		row[_axeModelColumns.axis] = axis;
		row[_axeModelColumns.name] = ((HklParameter *)axis)->name;
	}

	//Set the model for the TreeView
	_TreeView_axes->set_model(_axeModel);
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
	_TreeView_pseudoAxes->append_column("name", _pseudoAxeModelColumns.name);

	_TreeView_pseudoAxes->append_column_numeric("read", _pseudoAxeModelColumns.read, "%lf");
  
	index = _TreeView_pseudoAxes->append_column_numeric_editable("write", _pseudoAxeModelColumns.write, "%lf");
	renderer = _TreeView_pseudoAxes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this,
			      &HKLWindow::on_cell_TreeView_pseudoAxes_write_edited));
  
	_TreeView_pseudoAxes->append_column_numeric("min", _pseudoAxeModelColumns.min, "%lf");
  
	_TreeView_pseudoAxes->append_column_numeric("max", _pseudoAxeModelColumns.max, "%lf");

	index = _TreeView_pseudoAxes->append_column_editable(
		"initialized",
		_pseudoAxeModelColumns.is_initialized);
	renderer = _TreeView_pseudoAxes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(
		sigc::mem_fun(*this,
			      &HKLWindow::on_cell_TreeView_pseudoAxes_is_initialized_toggled));
  
	index = _TreeView_pseudoAxes->append_column_editable(
		"readable",
		_pseudoAxeModelColumns.is_readable);
  
	index = _TreeView_pseudoAxes->append_column_editable(
		"writable",
		_pseudoAxeModelColumns.is_writable);

	//Create the Model
	_pseudoAxeModel = Gtk::ListStore::create(_pseudoAxeModelColumns);

	//Fill the models from the diffractometer pseudoAxes
	for(i=0; i<HKL_LIST_LEN(_engines->engines); ++i){
		HklPseudoAxisEngine *engine = _engines->engines[i];

		for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j){
			HklPseudoAxis *pseudoAxis = engine->pseudoAxes[j];
			Gtk::ListStore::Row row = *(_pseudoAxeModel->append());
			row[_pseudoAxeModelColumns.pseudoAxis] = pseudoAxis;
			row[_pseudoAxeModelColumns.name] = ((HklParameter *)pseudoAxis)->name;

			if(HKL_LIST_LEN(engine->mode->parameters)){
				Glib::RefPtr<Gtk::ListStore> model = Gtk::ListStore::create(_parameterModelColumns);
				for(k=0; k<HKL_LIST_LEN(engine->mode->parameters); ++k){
					HklParameter *parameter = &engine->mode->parameters[k];

					Glib::RefPtr<Gtk::ListStore> model = Gtk::ListStore::create(_parameterModelColumns);
					row = *(model->append());
					row[_parameterModelColumns.parameter] = parameter;
					row[_parameterModelColumns.name] = parameter->name;
					row[_parameterModelColumns.value] = hkl_parameter_get_value_unit(parameter);
				}
				_mapPseudoAxeParameterModel.insert(std::pair<HklPseudoAxis *,  Glib::RefPtr<Gtk::ListStore> >(pseudoAxis, model));
			}
		}
	}
	//Set the model for the TreeView
	_TreeView_pseudoAxes->set_model(_pseudoAxeModel);
	this->updatePseudoAxes();
}

void
HKLWindow::set_up_TreeView_pseudoAxes_parameters(void)
{
	int index;
	Gtk::CellRenderer * renderer;

	// add the columns
	_TreeView_pseudoAxes_parameters->append_column(
		"name", _parameterModelColumns.name);

	index = _TreeView_pseudoAxes_parameters->append_column_numeric_editable(
		"value", _parameterModelColumns.value, "%lf");
	renderer = _TreeView_pseudoAxes_parameters->get_column_cell_renderer(index-1);

	// connect the signal_edited
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this,
			      &HKLWindow::on_cell_TreeView_pseudoAxes_parameters_value_edited));
}

void HKLWindow::set_up_TreeView_treeview1(void)
{
	size_t i;
	size_t j;
	size_t k;
	int index;
	Gtk::CellRenderer * renderer;

	//Create the Columns
	if(_solutionModelColumns)
		delete _solutionModelColumns;
	_solutionModelColumns = new SolutionModelColumns(_geometry);

	/* add the columns */
	_treeview1->append_column("index", _solutionModelColumns->index);
	for(i=0; i<HKL_LIST_LEN(_geometry->axes); ++i)
		_treeview1->append_column_numeric(((HklParameter *)&_geometry->axes[i])->name,
						  _solutionModelColumns->axes[i],
						  "%lf");

	//Create the model from the columns
	_solutionModel = Gtk::ListStore::create(*_solutionModelColumns);

	_treeview1->set_model(_solutionModel);

	_treeview1->signal_cursor_changed().connect(mem_fun(*this, &HKLWindow::on_treeview1_cursor_changed));

	this->updateSolutions();
}

void
HKLWindow::updateSource(void)
{
	double lambda = hkl_source_get_wavelength(&_geometry->source);
	_spinbutton_lambda->set_value(lambda);
}

void
HKLWindow::updateAxes(void)
{
	fprintf(stdout, "updateAxes\n");

	// update the model
	Gtk::TreeModel::Children rows = _axeModel->children();
	Gtk::TreeModel::Children::iterator iter = rows.begin();
	Gtk::TreeModel::Children::iterator end = rows.end();
	while(iter != end){
		double min;
		double max;
		HklAxis * axis;

		Gtk::TreeRow row = *iter;
		axis = row[_axeModelColumns.axis];
		row[_axeModelColumns.read] = hkl_axis_get_value_unit(axis);
		row[_axeModelColumns.write] = hkl_axis_get_value_unit(axis);
		hkl_parameter_get_range_unit((HklParameter *)axis, &min, &max);
		row[_axeModelColumns.min] = min;
		row[_axeModelColumns.max] = max;
		++iter;
	}
}

void
HKLWindow::updatePseudoAxes(void)
{
	// first compute all the pseudoAxes values
	hkl_pseudo_axis_engine_list_get(_engines);

	// update the model
	Gtk::TreeModel::Children rows = _pseudoAxeModel->children();
	Gtk::TreeModel::Children::iterator iter = rows.begin();
	Gtk::TreeModel::Children::iterator end = rows.end();
	while(iter != end){
		double min;
		double max;
		HklParameter *parameter;
		HklPseudoAxis *pseudoAxis;

		Gtk::TreeRow row = *iter;
		pseudoAxis = row[_pseudoAxeModelColumns.pseudoAxis];
		parameter = (HklParameter *)pseudoAxis;
		row[_pseudoAxeModelColumns.read] = hkl_parameter_get_value_unit(parameter);
		row[_pseudoAxeModelColumns.write] = hkl_parameter_get_value_unit(parameter);
		hkl_parameter_get_range_unit(parameter, &min, &max);
		row[_pseudoAxeModelColumns.min] = min;
		row[_pseudoAxeModelColumns.max] = max;

		row[_pseudoAxeModelColumns.is_initialized] = true;
		row[_pseudoAxeModelColumns.is_readable] = true;
		row[_pseudoAxeModelColumns.is_writable] = true;
		++iter;
	}
}

void
HKLWindow::update_pseudoAxes_parameters(void)
{
	std::map<HklPseudoAxis *, Glib::RefPtr<Gtk::ListStore> >::iterator iter = _mapPseudoAxeParameterModel.begin();
	std::map<HklPseudoAxis *, Glib::RefPtr<Gtk::ListStore> >::iterator end = _mapPseudoAxeParameterModel.end();
	while(iter != end){
		Gtk::TreeModel::Children rows = iter->second->children();
		Gtk::TreeModel::Children::iterator iter_row = rows.begin();
		Gtk::TreeModel::Children::iterator end_row = rows.end();
		while(iter_row != end_row){
			Gtk::TreeRow row = *iter_row;
			HklParameter *parameter = row[_parameterModelColumns.parameter];
			row[_parameterModelColumns.name] = parameter->name;
			row[_parameterModelColumns.value] = hkl_parameter_get_value_unit(parameter);
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

		_spinbutton_a->set_value(a);
		_spinbutton_b->set_value(b);
		_spinbutton_c->set_value(c);
		_spinbutton_alpha->set_value(alpha);
		_spinbutton_beta->set_value(beta);
		_spinbutton_gamma->set_value(gamma);
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
		_spinbutton_a_min->set_value(min);
		_spinbutton_a_max->set_value(max);
		_checkbutton_a->set_active(parameter->fit);

		parameter = sample->lattice->b;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		_spinbutton_b_min->set_value(min);
		_spinbutton_b_max->set_value(max);
		_checkbutton_b->set_active(parameter->fit);

		parameter = sample->lattice->c;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		_spinbutton_c_min->set_value(min);
		_spinbutton_c_max->set_value(max);
		_checkbutton_c->set_active(parameter->fit);

		parameter = sample->lattice->alpha;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		_spinbutton_alpha_min->set_value(min);
		_spinbutton_alpha_max->set_value(max);
		_checkbutton_alpha->set_active(parameter->fit);

		parameter = sample->lattice->beta;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		_spinbutton_beta_min->set_value(min);
		_spinbutton_beta_max->set_value(max);
		_checkbutton_beta->set_active(parameter->fit);

		parameter = sample->lattice->gamma;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		_spinbutton_gamma_min->set_value(min);
		_spinbutton_gamma_max->set_value(max);
		_checkbutton_gamma->set_active(parameter->fit);

		_checkbutton_U->set_active(true);
	}
}

void
HKLWindow::updateReciprocalLattice(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklLattice *reciprocal = hkl_lattice_new_default();
		hkl_lattice_reciprocal(sample->lattice, reciprocal);

		_spinbutton_a_star->set_value(hkl_parameter_get_value_unit(reciprocal->a));
		_spinbutton_b_star->set_value(hkl_parameter_get_value_unit(reciprocal->b));
		_spinbutton_c_star->set_value(hkl_parameter_get_value_unit(reciprocal->c));
		_spinbutton_alpha_star->set_value(hkl_parameter_get_value_unit(reciprocal->alpha));
		_spinbutton_beta_star->set_value(hkl_parameter_get_value_unit(reciprocal->beta));
		_spinbutton_gamma_star->set_value(hkl_parameter_get_value_unit(reciprocal->gamma));
	}
}

void
HKLWindow::updateUB(void)
{
	HklSample *sample = _samples->current;
	if(sample){
		HklMatrix UB;

		hkl_sample_get_UB(sample, &UB);
		_label_UB11->set_text(Glib::Ascii::dtostr(UB.data[0][0]));
		_label_UB12->set_text(Glib::Ascii::dtostr(UB.data[0][1]));
		_label_UB13->set_text(Glib::Ascii::dtostr(UB.data[0][2]));
		_label_UB21->set_text(Glib::Ascii::dtostr(UB.data[1][0]));
		_label_UB22->set_text(Glib::Ascii::dtostr(UB.data[1][1]));
		_label_UB23->set_text(Glib::Ascii::dtostr(UB.data[1][2]));
		_label_UB31->set_text(Glib::Ascii::dtostr(UB.data[2][0]));
		_label_UB32->set_text(Glib::Ascii::dtostr(UB.data[2][1]));
		_label_UB33->set_text(Glib::Ascii::dtostr(UB.data[2][2]));
	}
}

void HKLWindow::updateUxUyUz(void)
{
	if(_samples->current){
		double ux;
		double uy;
		double uz;

		hkl_matrix_to_euler(&_samples->current->U, &ux, &uy, &uz);
		_spinbutton_ux->set_value(ux * HKL_RADTODEG);
		_spinbutton_uy->set_value(uy * HKL_RADTODEG);
		_spinbutton_uz->set_value(uz * HKL_RADTODEG);
	}
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
	_crystalModel = Gtk::ListStore::create(_crystalModelColumns);

	// erase all reflections.
	_mapReflectionModel.clear();

	if(_samples->current){
		is_current_crystal_set = true;
		current_crystal_name = _samples->current->name;
	}

	//Fill the models from the crystalList
	for(i=0; i<HKL_LIST_LEN(_samples->samples); ++i){
		HklLattice *lattice;

		sample = _samples->samples[i];
		lattice = sample->lattice;
		iter_row = *(_crystalModel->append());
		if (is_current_crystal_set && current_crystal_name == sample->name)
			iter_current = iter_row;
		row = *(iter_row);
		row[_crystalModelColumns.name] = sample->name;
		row[_crystalModelColumns.a] = hkl_parameter_get_value_unit(lattice->a);
		row[_crystalModelColumns.b] = hkl_parameter_get_value_unit(lattice->b);
		row[_crystalModelColumns.c] = hkl_parameter_get_value_unit(lattice->c);
		row[_crystalModelColumns.alpha] = hkl_parameter_get_value_unit(lattice->alpha);
		row[_crystalModelColumns.beta] = hkl_parameter_get_value_unit(lattice->beta);
		row[_crystalModelColumns.gamma] = hkl_parameter_get_value_unit(lattice->gamma);

		Glib::RefPtr<Gtk::ListStore> listStore = Gtk::ListStore::create(_reflectionModelColumns);
		_mapReflectionModel[sample->name] = listStore;
		this->updateReflections(sample, listStore);
	}

	//Set the model for the TreeView
	_treeViewCrystals->set_model(_crystalModel);
	if (is_current_crystal_set)
	{
		Gtk::TreeModel::Path path = _crystalModel->get_path(iter_current);
		_treeViewCrystals->set_cursor(path);
		_treeViewReflections->set_model(_mapReflectionModel[current_crystal_name]);
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
		row[_reflectionModelColumns.index] = i;
		row[_reflectionModelColumns.h] = reflection->hkl.data[0];
		row[_reflectionModelColumns.k] = reflection->hkl.data[1];
		row[_reflectionModelColumns.l] = reflection->hkl.data[2];
		row[_reflectionModelColumns.flag] = reflection->flag;
	}
}

void
HKLWindow::updateStatusBar(const HklError *error)
{
	_statusBar->push(error->message);
}

void
HKLWindow::updateCrystalModel(HklSample * sample)
{
	Gtk::TreeModel::Children children = _crystalModel->children();
	Gtk::TreeModel::Children::iterator iter = children.begin();
	Gtk::TreeModel::Children::iterator end = children.end();
	while (iter != end){
		Gtk::TreeModel::Row const & row = *iter;
		if (row[_crystalModelColumns.name] == sample->name){
			HklLattice *lattice = sample->lattice;
			row[_crystalModelColumns.a] = hkl_parameter_get_value_unit(lattice->a);
			row[_crystalModelColumns.b] = hkl_parameter_get_value_unit(lattice->b);
			row[_crystalModelColumns.c] = hkl_parameter_get_value_unit(lattice->c);
			row[_crystalModelColumns.alpha] = hkl_parameter_get_value_unit(lattice->alpha);
			row[_crystalModelColumns.beta] = hkl_parameter_get_value_unit(lattice->beta);
			row[_crystalModelColumns.gamma] = hkl_parameter_get_value_unit(lattice->gamma);
			iter = end;
		}
		else
			++iter;
	}
}

void HKLWindow::updatePseudoAxesFrames(void)
{
	size_t i;

	for(i=0; i<_pseudoAxesFrames.size(); ++i)
		_pseudoAxesFrames[i]->update();
}

void HKLWindow::updateSolutions(void)
{
	size_t i;

	_solutionModel->clear();
	Gtk::ListStore::Row row;
	for(i=0; i<HKL_LIST_LEN(_engines->geometries->geometries); ++i){
		size_t j;
		HklGeometry *geometry;

		geometry = _engines->geometries->geometries[i];

		row = *(_solutionModel->append());
		row[_solutionModelColumns->index] = i;
		for(j=0; j<HKL_LIST_LEN(geometry->axes); ++j)
			row[_solutionModelColumns->axes[j]] = 
				hkl_parameter_get_value_unit((HklParameter *)&geometry->axes[j]);
	}
}
