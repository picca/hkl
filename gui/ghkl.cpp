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

#include "ghkl.h"

HKLWindow::HKLWindow(void)
{
	LOG;

	size_t i;
	HklSample *sample;

	_geometry = NULL;
	_engines = NULL;

	_detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
        _detector->idx = 1;
	
	_samples = hkl_sample_list_new();
	// add a default crystal
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_list_append(_samples, sample);
	hkl_sample_list_select_current(_samples, "test");

	// create the reciprocal lattice
	_reciprocal = hkl_lattice_new_default();

	// Sets the border width of the window.
	this->set_border_width(10);

	this->get_widgets_and_objects_from_ui();

	_diffractometerModelColumns = NULL;
	this->set_up_diffractometer_model();

	this->set_up_TreeView_reflections();
	this->set_up_TreeView_crystals();

	//update the widgets
	this->updateTreeViewCrystals();
	this->updateSource();
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUxUyUz();
	this->updateUB();

	this->connect_all_signals();

	this->show_all_children();
}

HKLWindow::~HKLWindow()
{
	LOG;

	hkl_geometry_free(_geometry);
	hkl_detector_free(_detector);
	hkl_pseudo_axis_engine_list_free(_engines);
	hkl_sample_list_free(_samples);
	hkl_lattice_free(_reciprocal);

	if(_diffractometerModelColumns)
		delete _diffractometerModelColumns;

	if(_solutionModelColumns)
		delete _solutionModelColumns;
}

void HKLWindow::get_widgets_and_objects_from_ui(void)
{
	LOG;

	//Get Glade UI:
	_refGlade = Gtk::Builder::create();
	if(!_refGlade->add_from_file("ghkl.ui")){
		std::string filename = Glib::build_filename(PKGDATA, "ghkl.ui");
		if(!_refGlade->add_from_file(filename))
			exit(1);
	}

	// objects
	_diffractometerModel = Glib::RefPtr<Gtk::ListStore>::cast_dynamic(
		_refGlade->get_object("liststore1"));

	// window1
	_refGlade->get_widget("label_UB11", _label_UB11);
	_refGlade->get_widget("label_UB12", _label_UB12);
	_refGlade->get_widget("label_UB13", _label_UB13);
	_refGlade->get_widget("label_UB21", _label_UB21);
	_refGlade->get_widget("label_UB22", _label_UB22);
	_refGlade->get_widget("label_UB23", _label_UB23);
	_refGlade->get_widget("label_UB31", _label_UB31);
	_refGlade->get_widget("label_UB32", _label_UB32);
	_refGlade->get_widget("label_UB33", _label_UB33);
	_refGlade->get_widget("button2", _button2);
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
	_refGlade->get_widget("spinbutton_U11", _spinbutton_U11);
	_refGlade->get_widget("spinbutton_U12", _spinbutton_U12);
	_refGlade->get_widget("spinbutton_U13", _spinbutton_U13);
	_refGlade->get_widget("spinbutton_U21", _spinbutton_U21);
	_refGlade->get_widget("spinbutton_U22", _spinbutton_U22);
	_refGlade->get_widget("spinbutton_U23", _spinbutton_U23);
	_refGlade->get_widget("spinbutton_U31", _spinbutton_U31);
	_refGlade->get_widget("spinbutton_U32", _spinbutton_U32);
	_refGlade->get_widget("spinbutton_U33", _spinbutton_U33);
	_refGlade->get_widget("checkbutton_a", _checkbutton_a);
	_refGlade->get_widget("checkbutton_b", _checkbutton_b);
	_refGlade->get_widget("checkbutton_c", _checkbutton_c);
	_refGlade->get_widget("checkbutton_alpha", _checkbutton_alpha);
	_refGlade->get_widget("checkbutton_beta", _checkbutton_beta);
	_refGlade->get_widget("checkbutton_gamma", _checkbutton_gamma);
	_refGlade->get_widget("checkbutton_Ux", _checkbutton_Ux);
	_refGlade->get_widget("checkbutton_Uy", _checkbutton_Uy);
	_refGlade->get_widget("checkbutton_Uz", _checkbutton_Uz);
	_refGlade->get_widget("treeview_reflections", _treeViewReflections);
	_refGlade->get_widget("treeview_crystals", _treeViewCrystals);
	_refGlade->get_widget("treeview_axes", _TreeView_axes);
	_refGlade->get_widget("treeview_pseudoAxes", _TreeView_pseudoAxes);
	_refGlade->get_widget("treeview_pseudoAxes_parameters", _TreeView_pseudoAxes_parameters);
	_refGlade->get_widget("treeview1", _treeview1);
	_refGlade->get_widget("toolbutton_add_reflection", _toolbutton_add_reflection);
	_refGlade->get_widget("toolbutton_goto_reflection", _toolbutton_goto_reflection);
	_refGlade->get_widget("toolbutton_del_reflection", _toolbutton_del_reflection);
	_refGlade->get_widget("toolbutton_setUB", _toolbutton_setUB);
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
}

void HKLWindow::connect_all_signals(void)
{
	LOG;

	_spinbutton_a->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_a_value_changed));
	_spinbutton_b->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_b_value_changed));
	_spinbutton_c->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_c_value_changed));
	_spinbutton_alpha->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_alpha_value_changed));
	_spinbutton_beta->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_beta_value_changed));
	_spinbutton_gamma->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_gamma_value_changed));
	_spinbutton_a_min->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_a_min_value_changed));
	_spinbutton_b_min->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_b_min_value_changed));
	_spinbutton_c_min->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_c_min_value_changed));
	_spinbutton_alpha_min->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_alpha_min_value_changed));
	_spinbutton_beta_min->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_beta_min_value_changed));
	_spinbutton_gamma_min->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_gamma_min_value_changed));
	_spinbutton_a_max->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_a_max_value_changed));
	_spinbutton_b_max->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_b_max_value_changed));
	_spinbutton_c_max->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_c_max_value_changed));
	_spinbutton_alpha_max->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_alpha_max_value_changed));
	_spinbutton_beta_max->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_beta_max_value_changed));
	_spinbutton_gamma_max->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_gamma_max_value_changed));
	_spinbutton_lambda->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_lambda_value_changed));
	_spinbutton_ux->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_uxuyuz_value_changed));
	_spinbutton_uy->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_uxuyuz_value_changed));
	_spinbutton_uz->signal_value_changed().connect(
		mem_fun(*this, &HKLWindow::on_spinbutton_uxuyuz_value_changed));

	_button2->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_button2_clicked));

	_checkbutton_a->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_a_toggled));
	_checkbutton_b->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_b_toggled));
	_checkbutton_c->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_c_toggled));
	_checkbutton_alpha->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_alpha_toggled));
	_checkbutton_beta->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_beta_toggled));
	_checkbutton_gamma->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_gamma_toggled));
	_checkbutton_Ux->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_Ux_toggled));
	_checkbutton_Uy->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_Uy_toggled));
	_checkbutton_Uz->signal_toggled().connect(
		mem_fun(*this, &HKLWindow::on_checkbutton_Uz_toggled));

	_treeViewReflections->signal_key_press_event().connect(
		mem_fun(*this, &HKLWindow::on_treeViewReflections_key_press_event));
	_TreeView_pseudoAxes->signal_cursor_changed().connect(
		mem_fun(*this, &HKLWindow::on_treeView_pseudoAxes_cursor_changed));
	_treeViewCrystals->signal_cursor_changed().connect(
		mem_fun(*this, &HKLWindow::on_treeViewCrystals_cursor_changed));
	_treeViewCrystals->signal_key_press_event().connect(
		mem_fun(*this, &HKLWindow::on_treeViewCrystals_key_press_event));

	_toolbutton_add_reflection->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_add_reflection_clicked));
	_toolbutton_goto_reflection->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_goto_reflection_clicked));
	_toolbutton_del_reflection->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_del_reflection_clicked));
	_toolbutton_setUB->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_setUB_clicked));
	_toolbutton_computeUB->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_computeUB_clicked));
	_toolbutton_add_crystal->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_add_crystal_clicked));
	_toolbutton_copy_crystal->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_copy_crystal_clicked));
	_toolbutton_del_crystal->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_del_crystal_clicked));
	_toolbutton_affiner->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_toolbutton_affiner_clicked));

	_menuitem5->signal_activate().connect(
		mem_fun(*this, &HKLWindow::on_menuitem5_activate));

	// dialog1
	_button1->signal_clicked().connect(
		mem_fun(*this, &HKLWindow::on_button1_clicked));
	_combobox1->signal_changed().connect(
		mem_fun(*this, &HKLWindow::on_combobox1_changed));
}

void HKLWindow::set_up_pseudo_axes_frames(void)
{
	LOG;

	size_t i;
	Gtk::VBox *vbox2 = NULL;

	_refGlade->get_widget("vbox2", vbox2);

	// first clear the previous frames
	for(i=0; i<_pseudoAxesFrames.size(); ++i){
		vbox2->remove(_pseudoAxesFrames[i]->frame());
		delete _pseudoAxesFrames[i];
	}
	_pseudoAxesFrames.clear();

	for(i=0; i<HKL_LIST_LEN(_engines->engines); ++i){
		PseudoAxesFrame *pseudo;

		pseudo = new PseudoAxesFrame (_engines->engines[i]);
		_pseudoAxesFrames.push_back (pseudo);
		vbox2->add (pseudo->frame());
		pseudo->signal_changed ().connect (
			sigc::mem_fun (*this, &HKLWindow::on_pseudoAxesFrame_changed) );
	}
	vbox2->show_all();
}

void HKLWindow::set_up_diffractometer_model(void)
{
	size_t i;

	if(_diffractometerModelColumns)
		delete _diffractometerModelColumns;
	_diffractometerModelColumns = new DiffractometerModelColumns();

	i = 0;
	while(hkl_geometry_factory_configs[i].name){
		Gtk::ListStore::Row row;

		row = *(_diffractometerModel->append());
		row[_diffractometerModelColumns->name] = hkl_geometry_factory_configs[i++].name;
	}
}

void HKLWindow::set_up_TreeView_axes(void)
{
	LOG;

	size_t i;
	int index;
	Gtk::CellRenderer * renderer;

	//Create the Model
	_axeModel = Gtk::ListStore::create(_axeModelColumns);

	// add the columns
	_TreeView_axes->remove_all_columns();

	index = _TreeView_axes->append_column("name", _axeModelColumns.name);

	index = _TreeView_axes->append_column_numeric_editable("read",
								_axeModelColumns.read, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_read_edited));
  
	index = _TreeView_axes->append_column_numeric_editable("write",
								_axeModelColumns.write, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_write_edited));
  
	index = _TreeView_axes->append_column_numeric_editable("min",
								_axeModelColumns.min, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_min_edited));
  
	index = _TreeView_axes->append_column_numeric_editable("max",
								_axeModelColumns.max, "%lf");
	renderer = _TreeView_axes->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_axes_max_edited));

	//Fill the models from the diffractometerAxes
	for(i=0; i<HKL_LIST_LEN(_geometry->axes); ++i){
		HklAxis *axis = &_geometry->axes[i];

		Gtk::ListStore::Row row = *(_axeModel->append());
		row[_axeModelColumns.axis] = axis;
		row[_axeModelColumns.name] = ((HklParameter *)axis)->name;
	}

	//Set the model for the TreeView
	_TreeView_axes->set_model(_axeModel);
	this->updateAxes();
}

void HKLWindow::set_up_TreeView_pseudoAxes(void)
{
	LOG;

	size_t i;
	size_t j;
	size_t k;
	int index;
	Gtk::CellRenderer * renderer;

	/* add the columns */
	_TreeView_pseudoAxes->remove_all_columns();

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

void HKLWindow::set_up_TreeView_pseudoAxes_parameters(void)
{
	LOG;

	int index;
	Gtk::CellRenderer * renderer;

	// add the columns
	_TreeView_pseudoAxes_parameters->remove_all_columns();

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
	LOG;

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
	_treeview1->remove_all_columns();
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

void HKLWindow::set_up_TreeView_reflections(void)
{
	LOG;

	int index;
	Gtk::CellRenderer *renderer;

	//Set up the treeViewReflections
	_treeViewReflections->remove_all_columns();

	_treeViewReflections->append_column("index", _reflectionModelColumns.index);

	index = _treeViewReflections->append_column_numeric_editable("h", _reflectionModelColumns.h, "%lf");
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_h_edited));

	index = _treeViewReflections->append_column_numeric_editable("k", _reflectionModelColumns.k, "%lf");
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_k_edited));

	index = _treeViewReflections->append_column_numeric_editable("l", _reflectionModelColumns.l, "%lf");
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_l_edited));

	index = _treeViewReflections->append_column_editable("flag", _reflectionModelColumns.flag);
	renderer = _treeViewReflections->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererToggle *>(renderer)->signal_toggled().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_reflections_flag_toggled));

	_treeViewReflections->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
}

void HKLWindow::set_up_TreeView_crystals(void)
{
	LOG;

	int index;
	Gtk::CellRenderer *renderer;

	//Set up the treeViewCrystals
	_treeViewCrystals->remove_all_columns();

	index = _treeViewCrystals->append_column_editable("name", _crystalModelColumns.name);
	renderer = _treeViewCrystals->get_column_cell_renderer(index-1);
	dynamic_cast<Gtk::CellRendererText *>(renderer)->signal_edited().connect(
		sigc::mem_fun(*this, &HKLWindow::on_cell_TreeView_crystals_name_edited));

	_treeViewCrystals->append_column_numeric("a", _crystalModelColumns.a, "%lf");
	_treeViewCrystals->append_column_numeric("b", _crystalModelColumns.b, "%lf");
	_treeViewCrystals->append_column_numeric("c", _crystalModelColumns.c, "%lf");
	_treeViewCrystals->append_column_numeric("alpha", _crystalModelColumns.alpha, "%lf");
	_treeViewCrystals->append_column_numeric("beta", _crystalModelColumns.beta, "%lf");
	_treeViewCrystals->append_column_numeric("gamma", _crystalModelColumns.gamma, "%lf");

	_treeViewCrystals->get_selection()->set_mode(Gtk::SELECTION_MULTIPLE);
}

void HKLWindow::updateSource(void)
{
	LOG;

	if(_geometry){
		double lambda = hkl_source_get_wavelength(&_geometry->source);
		_spinbutton_lambda->set_value(lambda);
	}
}

void HKLWindow::updateAxes(void)
{
	LOG;

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

void HKLWindow::updatePseudoAxes(void)
{
	LOG;

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
		++iter;
	}
}

void HKLWindow::update_pseudoAxes_parameters(void)
{
	LOG;

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

void HKLWindow::updateLattice(void)
{
	LOG;

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

void HKLWindow::updateLatticeParameters(void)
{
	LOG;

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
	}
}

void HKLWindow::updateReciprocalLattice(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample){
		hkl_lattice_reciprocal(sample->lattice, _reciprocal);

		_spinbutton_a_star->set_value(hkl_parameter_get_value_unit(_reciprocal->a));
		_spinbutton_b_star->set_value(hkl_parameter_get_value_unit(_reciprocal->b));
		_spinbutton_c_star->set_value(hkl_parameter_get_value_unit(_reciprocal->c));
		_spinbutton_alpha_star->set_value(hkl_parameter_get_value_unit(_reciprocal->alpha));
		_spinbutton_beta_star->set_value(hkl_parameter_get_value_unit(_reciprocal->beta));
		_spinbutton_gamma_star->set_value(hkl_parameter_get_value_unit(_reciprocal->gamma));
	}
}

void HKLWindow::updateUB(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample){
		static const char *format = "%f";
		char tmp[100];
		HklMatrix UB;

		hkl_sample_get_UB(sample, &UB);
		sprintf(tmp, format, UB.data[0][0]);
		_label_UB11->set_text(tmp);
		sprintf(tmp, format, UB.data[0][1]);
		_label_UB12->set_text(tmp);
		sprintf(tmp, format, UB.data[0][2]);
		_label_UB13->set_text(tmp);
		sprintf(tmp, format, UB.data[1][0]);
		_label_UB21->set_text(tmp);
		sprintf(tmp, format, UB.data[1][1]);
		_label_UB22->set_text(tmp);
		sprintf(tmp, format, UB.data[1][2]);
		_label_UB23->set_text(tmp);
		sprintf(tmp, format, UB.data[2][0]);
		_label_UB31->set_text(tmp);
		sprintf(tmp, format, UB.data[2][1]);
		_label_UB32->set_text(tmp);
		sprintf(tmp, format, UB.data[2][2]);
		_label_UB33->set_text(tmp);
	}
}

void HKLWindow::updateUxUyUz(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample){
		_spinbutton_ux->set_value(hkl_parameter_get_value_unit(sample->ux));
		_spinbutton_uy->set_value(hkl_parameter_get_value_unit(sample->uy));
		_spinbutton_uz->set_value(hkl_parameter_get_value_unit(sample->uz));
		_checkbutton_Ux->set_active(sample->ux->fit);
		_checkbutton_Uy->set_active(sample->uy->fit);
		_checkbutton_Uz->set_active(sample->uz->fit);
	}
}

void HKLWindow::updateTreeViewCrystals(void)
{
	LOG;

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

void HKLWindow::updateReflections(const HklSample *sample,
				  Glib::RefPtr<Gtk::ListStore> & listStore)
{
	LOG;

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

void HKLWindow::updateStatusBar(const HklError *error)
{
	LOG;

	_statusBar->push(error->message);
}

void HKLWindow::updateCrystalModel(HklSample * sample)
{
	LOG;

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
	LOG;

	size_t i;

	for(i=0; i<_pseudoAxesFrames.size(); ++i)
		_pseudoAxesFrames[i]->update();
}

void HKLWindow::updateSolutions(void)
{
	LOG;

	size_t i;

	_solutionModel->clear();
	Gtk::ListStore::Row row;
	for(i=0; i<hkl_geometry_list_len(_engines->geometries); ++i){
		size_t j;
		HklGeometry *geometry;

		geometry = _engines->geometries->items[i]->geometry;

		row = *(_solutionModel->append());
		row[_solutionModelColumns->index] = i;
		for(j=0; j<HKL_LIST_LEN(geometry->axes); ++j)
			row[_solutionModelColumns->axes[j]] = 
				hkl_parameter_get_value_unit((HklParameter *)&geometry->axes[j]);
	}
}
