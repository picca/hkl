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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "hkl-gui.h"
#include "hkl.h"

void HKLWindow::on_treeView_pseudoAxes_cursor_changed(void)
{
	LOG;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_TreeView_pseudoAxes->get_cursor(path, column);
	Gtk::ListStore::Row row = *(_pseudoAxeModel->get_iter(path));
	HklParameter *parameter = row[_pseudoAxeModelColumns.parameter];
	_TreeView_pseudoAxes_parameters->set_model(_mapPseudoAxeParameterModel[parameter]);
}

void HKLWindow::on_treeViewCrystals_cursor_changed(void)
{
	LOG;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_treeViewCrystals->get_cursor(path, column);
	Gtk::TreeModel::iterator iter = _crystalModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	Glib::ustring name = row[_crystalModelColumns.name];
	_sample = _samples[name];
	hkl_engine_list_init(_engines, _geometry, _detector, _sample);
	_treeViewReflections->set_model(_mapReflectionModel[name]);
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUxUyUz();
	this->updateUB();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
}

void HKLWindow::on_spinbutton_a_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_b_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_c_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_alpha_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_beta_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_gamma_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_a_min_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_b_min_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_c_min_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_alpha_min_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_beta_min_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_gamma_min_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_a_max_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_b_max_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_c_max_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_alpha_max_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_beta_max_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_gamma_max_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_spinbutton_lambda_value_changed(void)
{
	LOG;

	if(_geometry){
		hkl_geometry_wavelength_set(this->_geometry,
					    _spinbutton_lambda->get_value());
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void HKLWindow::on_spinbutton_uxuyuz_value_changed(void)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void HKLWindow::on_button2_clicked(void)
{
	LOG;

	if(_sample){
		HklMatrix *U;
		HklParameter *parameter;
		HklLattice *lattice;

		lattice = hkl_lattice_new(_spinbutton_a->get_value(),
					  _spinbutton_b->get_value(),
					  _spinbutton_c->get_value(),
					  _spinbutton_alpha->get_value() * HKL_DEGTORAD,
					  _spinbutton_beta->get_value() * HKL_DEGTORAD,
					  _spinbutton_gamma->get_value() * HKL_DEGTORAD);

		/* set min/max a */
		parameter = hkl_parameter_new_copy(hkl_lattice_a_get(lattice));
		hkl_parameter_min_max_unit_set(parameter,
					       _spinbutton_a_min->get_value(),
					       _spinbutton_a_max->get_value());
		hkl_lattice_a_set(lattice, parameter);
		hkl_parameter_free(parameter);

		/* set min/max b */
		parameter = hkl_parameter_new_copy(hkl_lattice_b_get(lattice));
		hkl_parameter_min_max_unit_set(parameter,
					       _spinbutton_b_min->get_value(),
					       _spinbutton_b_max->get_value());
		hkl_lattice_b_set(lattice, parameter);
		hkl_parameter_free(parameter);

		parameter = hkl_parameter_new_copy(hkl_lattice_c_get(lattice));
		hkl_parameter_min_max_unit_set(parameter,
					       _spinbutton_c_min->get_value(),
					       _spinbutton_c_max->get_value());
		hkl_lattice_c_set(lattice, parameter);
		hkl_parameter_free(parameter);

		parameter = hkl_parameter_new_copy(hkl_lattice_alpha_get(lattice));
		hkl_parameter_min_max_unit_set(parameter,
					       _spinbutton_alpha_min->get_value(),
					       _spinbutton_alpha_max->get_value());
		hkl_lattice_alpha_set(lattice, parameter);
		hkl_parameter_free(parameter);

		parameter = hkl_parameter_new_copy(hkl_lattice_beta_get(lattice));
		hkl_parameter_min_max_unit_set(parameter,
					       _spinbutton_beta_min->get_value(),
					       _spinbutton_beta_max->get_value());
		hkl_lattice_beta_set(lattice, parameter);
		hkl_parameter_free(parameter);

		parameter = hkl_parameter_new_copy(hkl_lattice_gamma_get(lattice));
		hkl_parameter_min_max_unit_set(parameter,
					       _spinbutton_gamma_min->get_value(),
					       _spinbutton_gamma_max->get_value());
		hkl_lattice_gamma_set(lattice, parameter);
		hkl_parameter_free(parameter);


		hkl_sample_lattice_set(_sample, lattice);
		U = hkl_matrix_new_euler(_spinbutton_ux->get_value() * HKL_DEGTORAD,
					 _spinbutton_uy->get_value() * HKL_DEGTORAD,
					 _spinbutton_uz->get_value() * HKL_DEGTORAD);
		hkl_sample_U_set(_sample, U);
		hkl_matrix_free(U);
		hkl_lattice_free(lattice);

		this->updateCrystalModel(_sample);
		this->updateReciprocalLattice();
		this->updateUB();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

#define ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(p, sample)		\
	void HKLWindow::on_checkbutton_##p##_toggled(void)		\
	{								\
		LOG;							\
		if(_sample){						\
			HklLattice *lattice = hkl_lattice_new_copy(hkl_sample_lattice_get((sample))); \
			HklParameter *parameter = hkl_parameter_new_copy(hkl_lattice_##p##_get(lattice)); \
			hkl_parameter_fit_set(parameter, _checkbutton_##p->get_active()); \
			hkl_lattice_##p##_set(lattice, parameter);	\
			hkl_sample_lattice_set((sample), lattice);	\
			hkl_parameter_free(parameter);			\
			hkl_lattice_free(lattice);			\
		}							\
	}

ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(a, _sample);
ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(b, _sample);
ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(c, _sample);
ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(alpha, _sample);
ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(beta, _sample);
ON_CHECKBUTTON_LATTICE_PARAMETER_TOGGLE(gamma, _sample);

#define ON_CHECKBUTTON_UXUYUZ_TOGGLED(p, sample)			\
	void HKLWindow::on_checkbutton_##p##_toggled(void)			\
	{								\
		LOG;							\
		if(_sample){						\
			HklParameter *parameter = hkl_parameter_new_copy(hkl_sample_##p##_get((sample))); \
			hkl_parameter_fit_set(parameter, _checkbutton_##p->get_active()); \
			hkl_sample_##p##_set(sample, parameter);	\
			hkl_parameter_free(parameter);			\
		}							\
	}

ON_CHECKBUTTON_UXUYUZ_TOGGLED(ux, _sample)
ON_CHECKBUTTON_UXUYUZ_TOGGLED(uy, _sample)
ON_CHECKBUTTON_UXUYUZ_TOGGLED(uz, _sample)

void HKLWindow::on_cell_TreeView_axes_read_edited(Glib::ustring const & spath,
						  Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	HklParameter *axis = row[_axeModelColumns.axis];
	double value;

	sscanf(newText.c_str(), "%lf", &value);
	hkl_parameter_value_unit_set(axis, value, NULL);
	hkl_geometry_axis_set(this->_geometry, axis);

	row[_axeModelColumns.read] = value;
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();

#ifdef HKL3D
	if(_Scene){
		_Scene->is_colliding();
		_Scene->invalidate();
	}
#endif
}

void HKLWindow::on_cell_TreeView_axes_write_edited(Glib::ustring const & spath,
						   Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	HklParameter *axis = row[_axeModelColumns.axis];
	double value;
	sscanf(newText.c_str(), "%lf", &value);
	hkl_parameter_value_unit_set(axis, value, NULL);
	hkl_geometry_axis_set(this->_geometry, axis);

	row[_axeModelColumns.write] = value;
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();

#ifdef HKL3D
	if(_Scene){
		_Scene->is_colliding();
		_Scene->invalidate();
	}
#endif
}

void HKLWindow::on_cell_TreeView_axes_min_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	HklParameter *axis = row[_axeModelColumns.axis];

	double shit;
	double max;
	double value;

	sscanf(newText.c_str(), "%lf", &value);

	hkl_parameter_min_max_unit_get(axis, &shit, &max);
	hkl_parameter_min_max_unit_set(axis, value, max);
	hkl_geometry_axis_set(this->_geometry, axis);

	row[_axeModelColumns.min] = value;
	this->updatePseudoAxes();
}

void HKLWindow::on_cell_TreeView_axes_max_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_axes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	HklParameter *axis = row[_axeModelColumns.axis];

	double min, shit;
	double value;

	sscanf(newText.c_str(), "%lf", &value);

	hkl_parameter_min_max_unit_get(axis, &min, &shit);
	hkl_parameter_min_max_unit_set(axis, min, value);
	hkl_geometry_axis_set(this->_geometry, axis);

	row[_axeModelColumns.max] = value;
	this->updatePseudoAxes();
}

// PseudoAxes
void HKLWindow::on_cell_TreeView_pseudoAxes_write_edited(Glib::ustring const & spath,
							 Glib::ustring const & newText)
{
	LOG;

	double value;
	HklParameter *parameter;
	HklEngine *engine;
	int res;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_pseudoAxes->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	parameter = row[_pseudoAxeModelColumns.parameter];
	engine = row[_pseudoAxeModelColumns.engine];
	sscanf(newText.c_str(), "%lf", &value);

	if(hkl_parameter_value_unit_set(parameter, value, NULL))
		if(hkl_engine_set(engine, NULL)){
			hkl_engine_list_select_solution(this->_engines, 0);

			row[_pseudoAxeModelColumns.write] = value;
			this->updateAxes();
			this->updatePseudoAxes();
			this->updatePseudoAxesFrames();
			this->updateSolutions();

#ifdef HKL3D
			if(_Scene){
				_Scene->is_colliding();
				_Scene->invalidate();
			}
#endif
		}
}

//PseudoAxes Parameters
void HKLWindow::on_cell_TreeView_pseudoAxes_parameters_value_edited(Glib::ustring const & spath,
								    Glib::ustring const & newText)
{
	LOG;

	double value;
	HklEngine *engine;
	HklParameter *parameter;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _TreeView_pseudoAxes_parameters->get_model();
	Gtk::ListStore::Row row = *(listStore->get_iter(path));
	sscanf(newText.c_str(), "%lf", &value);

	
	engine = row[_parameterModelColumns.engine];
	parameter = hkl_parameter_new_copy(hkl_engine_parameter_get(engine,
								    row[_parameterModelColumns.name]));
	/* TODO error check */
	hkl_parameter_value_unit_set(parameter, value, NULL);
	hkl_engine_parameter_set(engine, parameter);
	hkl_parameter_free(parameter);

	row[_parameterModelColumns.value] = value;
	this->updatePseudoAxes();
	this->update_pseudoAxes_parameters();
}

void HKLWindow::on_cell_TreeView_crystals_name_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewCrystals->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	Glib::ustring name = row[_crystalModelColumns.name];
	std::map<std::string, HklSample *>::iterator it = _samples.find(name);
	if (it != _samples.end()){
		HklSample *sample = it->second;

		hkl_sample_name_set(sample, newText.c_str());
		_samples.erase(it);
		_samples.insert(std::pair<std::string, HklSample *>(hkl_sample_name_get(sample),
								    sample));

		this-> updateTreeViewCrystals();
	}
}

void HKLWindow::on_cell_TreeView_reflections_h_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	if(_sample){
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		reflection = row[_reflectionModelColumns.reflection];

		hkl_sample_reflection_hkl_get(reflection, &h, &k, &l);
		sscanf(newText.c_str(), "%lf", &h);
		hkl_sample_reflection_hkl_set(reflection, h, k, l);
		row[_reflectionModelColumns.h] = h;

		row[_reflectionModelColumns.flag] = hkl_sample_reflection_flag_get(reflection);

		this->updateCrystalModel(_sample);
	}
}

void HKLWindow::on_cell_TreeView_reflections_k_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	if(_sample){
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;


		reflection = row[_reflectionModelColumns.reflection];

		hkl_sample_reflection_hkl_get(reflection, &h, &k, &l);
		sscanf(newText.c_str(), "%lf", &k);
		hkl_sample_reflection_hkl_set(reflection, h, k, l);
		row[_reflectionModelColumns.k] = k;

		row[_reflectionModelColumns.flag] = hkl_sample_reflection_flag_get(reflection);
		this->updateCrystalModel(_sample);
	}
}

void HKLWindow::on_cell_TreeView_reflections_l_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	if(_sample){
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		reflection = row[_reflectionModelColumns.reflection];

		hkl_sample_reflection_hkl_get(reflection, &h, &k, &l);
		sscanf(newText.c_str(), "%lf", &l);
		hkl_sample_reflection_hkl_set(reflection, h, k, l);
		row[_reflectionModelColumns.l] = l;

		row[_reflectionModelColumns.flag] = hkl_sample_reflection_flag_get(reflection);

		this->updateCrystalModel(_sample);
	}
}

void HKLWindow::on_cell_TreeView_reflections_flag_toggled(Glib::ustring const & spath)
{
	LOG;

	Gtk::TreePath path(spath);
	Glib::RefPtr<Gtk::TreeModel> listStore = _treeViewReflections->get_model();
	Gtk::TreeModel::iterator iter = listStore->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	if(_sample){
		int flag;
		HklSampleReflection *reflection;

		reflection = row[_reflectionModelColumns.reflection];
		flag = !hkl_sample_reflection_flag_get(reflection);
		hkl_sample_reflection_flag_set(reflection, flag);
		row[_reflectionModelColumns.flag] = flag;
	}
}

void HKLWindow::on_toolbutton_add_reflection_clicked(void)
{
	LOG;

	if(_sample){
		HklSampleReflection *reflection;
		double h = 0;
		double k = 0;
		double l = 0;

		reflection = hkl_sample_reflection_new(_geometry, _detector, h, k, l);
		hkl_sample_add_reflection(_sample, reflection);

		this->updateReflections(_sample,
					_mapReflectionModel[hkl_sample_name_get(_sample)]);
	}
}

void HKLWindow::on_toolbutton_goto_reflection_clicked(void)
{
	LOG;

	if(_sample){
		Glib::RefPtr<Gtk::TreeSelection> selection = _treeViewReflections->get_selection();
		unsigned int nb_rows = selection->count_selected_rows();
		if (nb_rows == 1){
			Gtk::TreeSelection::ListHandle_Path list_path = selection->get_selected_rows();
			Gtk::TreePath path = *(list_path.begin());
			Glib::RefPtr<Gtk::ListStore> liststore = _mapReflectionModel[hkl_sample_name_get(_sample)];
			Gtk::ListStore::Row row = *(liststore->get_iter(path));
			HklSampleReflection *reflection = row[_reflectionModelColumns.reflection];

			hkl_geometry_set(this->_geometry,
					 hkl_sample_reflection_geometry_get(reflection));

			this->updateSource();
			this->updateAxes();
			this->updatePseudoAxes();
#ifdef HKL3D
			if(_Scene){
				_Scene->is_colliding();
				_Scene->invalidate();
			}
#endif
		}else{
			if (nb_rows)
				_statusBar->push("Please select only one reflection.");
			else
				_statusBar->push("Please select one reflection.");
		}
	}
}

void HKLWindow::on_toolbutton_del_reflection_clicked(void)
{
	LOG;

	if(_sample){
		Glib::RefPtr<Gtk::TreeSelection> selection = _treeViewReflections->get_selection();
		unsigned int nb_rows = selection->count_selected_rows();
		if (nb_rows){
			Gtk::TreeSelection::ListHandle_Path list = selection->get_selected_rows();
			Gtk::TreeSelection::ListHandle_Path::iterator iter = list.begin();
			Gtk::TreeSelection::ListHandle_Path::iterator last = list.end();
			Glib::RefPtr<Gtk::ListStore> liststore = _mapReflectionModel[hkl_sample_name_get(_sample)];
			// fill indexes with the reflections index
			std::vector<HklSampleReflection *> to_delete;
			while(iter != last){
				Gtk::ListStore::Row row = *(liststore->get_iter(*iter));
				to_delete.push_back(row[_reflectionModelColumns.reflection]);
				++iter;
			}
			std::ostringstream os;
			os << "Are you sure you want to delete reflections :";
			for(unsigned int i=0; i< to_delete.size();i++){
				double h, k, l;

				hkl_sample_reflection_hkl_get(to_delete[i], &h, &k, &l);
				os << " "
				   << " h: " << h
				   << " k: " << k
				   << " l: " << l;
			}

			_message = new Gtk::MessageDialog("", false,
							  Gtk::MESSAGE_WARNING,
							  Gtk::BUTTONS_YES_NO);
			_message->set_message(os.str());
			_message->show();
			int respons = _message->run();
			switch (respons){
			case Gtk::RESPONSE_YES:
				for(unsigned int i=0;i<to_delete.size();i++){
					hkl_sample_del_reflection(_sample,
								  to_delete[i]);
				}
				this->updateReflections(_sample, liststore);
				break;
			}
			delete _message;
		}else
			_statusBar->push("Please select at least one reflection.");
	}
}
void HKLWindow::on_toolbutton_setUB_clicked(void)
{
	LOG;

	if(_sample){
		HklMatrix *UB = hkl_matrix_new_full(
			_spinbutton_U11->get_value(),
			_spinbutton_U12->get_value(),
			_spinbutton_U13->get_value(),
			_spinbutton_U21->get_value(),
			_spinbutton_U22->get_value(),
			_spinbutton_U23->get_value(),
			_spinbutton_U31->get_value(),
			_spinbutton_U32->get_value(),
			_spinbutton_U33->get_value());

		hkl_sample_UB_set(_sample, UB);
		hkl_matrix_free(UB);

		this->updateLattice();
		this->updateLatticeParameters();
		this->updateReciprocalLattice();
		this->updateCrystalModel(_sample);
		this->updateUB();
		this->updateUxUyUz();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void HKLWindow::on_toolbutton_computeUB_clicked(void)
{
	LOG;

	if(_sample){
		const HklSampleReflection *r1 = hkl_sample_first_reflection_get(_sample);
		const HklSampleReflection *r2 = hkl_sample_next_reflection_get(_sample,
									       const_cast<HklSampleReflection*>(r1));

		hkl_sample_compute_UB_busing_levy(_sample, r1, r2);
		this->updateUB();
		this->updateUxUyUz();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void HKLWindow::on_toolbutton_add_crystal_clicked(void)
{
	LOG;

	_sample = hkl_sample_new("new_sample");
	if(_sample){
		_samples.insert(std::pair<std::string, HklSample *>(hkl_sample_name_get(_sample),
								    _sample));
		this->updateTreeViewCrystals();

		// activate for edition the name of the new crystal
		Gtk::TreeModel::Path path;
		Gtk::TreeView::Column * column;
		_treeViewCrystals->get_cursor(path, column);
		column = _treeViewCrystals->get_column(0);
		_treeViewCrystals->set_cursor(path, *column, true);
	}
}

void HKLWindow::on_toolbutton_copy_crystal_clicked(void)
{
	LOG;

	Glib::ustring name;
	Glib::ustring newname;
	HklSample *old_sample = _sample;
	HklSample *sample;
	if(!old_sample){
		_statusBar->push("Please select a crystal to copy.");
		return;
	}

	sample = hkl_sample_new_copy(_sample);
	hkl_sample_name_set(sample, "copy");
	_samples.insert(std::pair<std::string, HklSample *>(hkl_sample_name_get(sample),
							    sample));
	_sample = sample;
	this->updateTreeViewCrystals();

	// activate for edition the name of the new crystal
	Gtk::TreeModel::Path path;
	Gtk::TreeView::Column * column;
	_treeViewCrystals->get_cursor(path, column);
	column = _treeViewCrystals->get_column(0);
	_treeViewCrystals->set_cursor(path, *column, true);
}

void HKLWindow::on_toolbutton_del_crystal_clicked(void)
{
	LOG;

	_samples.erase(_samples.find(hkl_sample_name_get(_sample)));
	this->updateTreeViewCrystals();
}

void HKLWindow::on_toolbutton_affiner_clicked(void)
{
	LOG;

	Glib::ustring name;
	Glib::ustring method;
	if(_sample)
		hkl_sample_affine(_sample);

	this->updateCrystalModel(_sample);
	this->updateLattice();
	this->updateReciprocalLattice();
	this->updateUB();
	this->updateUxUyUz();
}

bool HKLWindow::on_treeViewReflections_key_press_event(GdkEventKey * event)
{
	LOG;

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

bool HKLWindow::on_treeViewCrystals_key_press_event(GdkEventKey * event)
{
	LOG;

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

void HKLWindow::on_treeview1_cursor_changed(void)
{
	LOG;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_treeview1->get_cursor(path, column);
	Gtk::TreeModel::iterator iter = _solutionModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);
	const HklGeometryListItem *item = row[_solutionModelColumns->item];

	const HklGeometry *geometry = hkl_geometry_list_item_geometry_get(item);
	hkl_engine_list_geometry_set(this->_engines, geometry);

	this->updateAxes();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
#ifdef HKL3D
	if(_Scene){
		_Scene->is_colliding();
		_Scene->invalidate();
	}
#endif
}

void HKLWindow::on_pseudoAxesFrame_changed(void)
{
	LOG;

	this->updateAxes();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
	this->updateSolutions();
}

void HKLWindow::on_menuitem5_activate(void)
{
	LOG;

	_dialog1->show();
}

void HKLWindow::on_button1_clicked(void)
{
	LOG;

	_dialog1->hide();
}

void HKLWindow::on_combobox1_changed(void)
{
	LOG;

	size_t idx = _combobox1->get_active_row_number();
	unsigned int n;
	HklFactory **factories;

	factories = hkl_factory_get_all(&n);
	this->_factory = factories[idx];

	if(_geometry)
		hkl_geometry_free(_geometry);
	_geometry = hkl_factory_create_new_geometry(this->_factory);

	if(_engines)
		hkl_engine_list_free(_engines);
	_engines = hkl_factory_create_new_engine_list(this->_factory);
	hkl_engine_list_init(_engines, _geometry, _detector, _sample);

	this->set_up_pseudo_axes_frames();
	this->set_up_TreeView_axes();
	this->set_up_TreeView_pseudoAxes_parameters();
	this->set_up_TreeView_pseudoAxes();

	_solutionModelColumns = 0;
	this->set_up_TreeView_treeview1();
#ifdef HKL3D
	this->set_up_3D();
#endif
}
