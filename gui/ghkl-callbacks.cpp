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

void HKLWindow::on_treeView_pseudoAxes_cursor_changed(void)
{
	LOG;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_TreeView_pseudoAxes->get_cursor(path, column);
	Gtk::ListStore::Row row = *(_pseudoAxeModel->get_iter(path));
	HklPseudoAxis *pseudoAxis = row[_pseudoAxeModelColumns.pseudoAxis];
	_TreeView_pseudoAxes_parameters->set_model(_mapPseudoAxeParameterModel[pseudoAxis]);
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
	hkl_sample_list_select_current(_samples, name.c_str());
	hkl_pseudo_axis_engine_list_init(_engines, _geometry, _detector, _samples->current);
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
		_geometry->source.wave_length = _spinbutton_lambda->get_value();
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

	HklSample *sample;

	sample = _samples->current;
	if(sample){
		hkl_sample_set_lattice(sample,
				       _spinbutton_a->get_value(),
				       _spinbutton_b->get_value(),
				       _spinbutton_c->get_value(),
				       _spinbutton_alpha->get_value() * HKL_DEGTORAD,
				       _spinbutton_beta->get_value() * HKL_DEGTORAD,
				       _spinbutton_gamma->get_value() * HKL_DEGTORAD);

		hkl_sample_set_U_from_euler(sample,
					    _spinbutton_ux->get_value() * HKL_DEGTORAD,
					    _spinbutton_uy->get_value() * HKL_DEGTORAD,
					    _spinbutton_uz->get_value() * HKL_DEGTORAD);

		// set min/max
		hkl_parameter_set_range_unit(sample->lattice->a,
					     _spinbutton_a_min->get_value(),
					     _spinbutton_a_max->get_value());
		hkl_parameter_set_range_unit(sample->lattice->b,
					     _spinbutton_b_min->get_value(),
					     _spinbutton_b_max->get_value());
		hkl_parameter_set_range_unit(sample->lattice->c,
					     _spinbutton_c_min->get_value(),
					     _spinbutton_c_max->get_value());
		hkl_parameter_set_range_unit(sample->lattice->alpha,
					     _spinbutton_alpha_min->get_value(),
					     _spinbutton_alpha_max->get_value());
		hkl_parameter_set_range_unit(sample->lattice->beta,
					     _spinbutton_beta_min->get_value(),
					     _spinbutton_beta_max->get_value());
		hkl_parameter_set_range_unit(sample->lattice->gamma,
					     _spinbutton_gamma_min->get_value(),
					     _spinbutton_gamma_max->get_value());

		this->updateCrystalModel(sample);
		this->updateReciprocalLattice();
		this->updateUB();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void HKLWindow::on_checkbutton_a_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->a->fit = _checkbutton_a->get_active();
}

void HKLWindow::on_checkbutton_b_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->b->fit = _checkbutton_b->get_active();
}

void HKLWindow::on_checkbutton_c_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->c->fit = _checkbutton_c->get_active();
}

void HKLWindow::on_checkbutton_alpha_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->alpha->fit = _checkbutton_alpha->get_active();
}

void HKLWindow::on_checkbutton_beta_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->beta->fit = _checkbutton_beta->get_active();
}

void HKLWindow::on_checkbutton_gamma_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->lattice->gamma->fit = _checkbutton_gamma->get_active();
}

void HKLWindow::on_checkbutton_Ux_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->ux->fit = _checkbutton_Ux->get_active();
}

void HKLWindow::on_checkbutton_Uy_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->uy->fit = _checkbutton_Uy->get_active();
}

void HKLWindow::on_checkbutton_Uz_toggled(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample)
		sample->uz->fit = _checkbutton_Uz->get_active();
}

void HKLWindow::on_cell_TreeView_axes_read_edited(Glib::ustring const & spath,
						  Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_axes_write_edited(Glib::ustring const & spath,
						   Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_axes_min_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_axes_max_edited(Glib::ustring const & spath,
						 Glib::ustring const & newText)
{
	LOG;

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
void HKLWindow::on_cell_TreeView_pseudoAxes_write_edited(Glib::ustring const & spath,
							 Glib::ustring const & newText)
{
	LOG;

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
		hkl_geometry_init_geometry(_geometry,
					   _engines->geometries->items[0]->geometry);
		hkl_pseudo_axis_engine_list_get(_engines);
		row[_pseudoAxeModelColumns.write] = value;
		this->updateAxes();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
		this->updateSolutions();
	}
}

void HKLWindow::on_cell_TreeView_pseudoAxes_is_initialized_toggled(Glib::ustring const & spath)
{
	LOG;

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
void HKLWindow::on_cell_TreeView_pseudoAxes_parameters_value_edited(Glib::ustring const & spath,
								    Glib::ustring const & newText)
{
	LOG;

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
void HKLWindow::on_cell_TreeView_crystals_name_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_reflections_h_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_reflections_k_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_reflections_l_edited(Glib::ustring const & spath,
						      Glib::ustring const & newText)
{
	LOG;

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

void HKLWindow::on_cell_TreeView_reflections_flag_toggled(Glib::ustring const & spath)
{
	LOG;

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

void HKLWindow::on_toolbutton_add_reflection_clicked(void)
{
	LOG;

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

void HKLWindow::on_toolbutton_goto_reflection_clicked(void)
{
	LOG;

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

void HKLWindow::on_toolbutton_del_reflection_clicked(void)
{
	LOG;

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
void HKLWindow::on_toolbutton_setUB_clicked(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample){
		HklMatrix UB;

		UB.data[0][0] = _spinbutton_U11->get_value();
		UB.data[0][1] = _spinbutton_U12->get_value();
		UB.data[0][2] = _spinbutton_U13->get_value();
		UB.data[1][0] = _spinbutton_U21->get_value();
		UB.data[1][1] = _spinbutton_U22->get_value();
		UB.data[1][2] = _spinbutton_U23->get_value();
		UB.data[2][0] = _spinbutton_U31->get_value();
		UB.data[2][1] = _spinbutton_U32->get_value();
		UB.data[2][2] = _spinbutton_U33->get_value();

		hkl_sample_set_UB(sample, &UB);
		hkl_sample_fprintf(stdout, sample);

		this->updateLattice();
		this->updateLatticeParameters();
		this->updateReciprocalLattice();
		this->updateCrystalModel(_samples->current);
		this->updateUB();
		this->updateUxUyUz();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void HKLWindow::on_toolbutton_computeUB_clicked(void)
{
	LOG;

	HklSample *sample = _samples->current;
	if(sample){
		hkl_sample_compute_UB_busing_levy(sample, 0, 1);
		this->updateUB();
		this->updateUxUyUz();
		this->updatePseudoAxes();
		this->updatePseudoAxesFrames();
	}
}

void HKLWindow::on_toolbutton_add_crystal_clicked(void)
{
	LOG;

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

void HKLWindow::on_toolbutton_copy_crystal_clicked(void)
{
	LOG;

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

void HKLWindow::on_toolbutton_del_crystal_clicked(void)
{
	LOG;

	if(_samples->current){
		hkl_sample_list_del(_samples, _samples->current);
		this->updateTreeViewCrystals();
	}
}

void HKLWindow::on_toolbutton_affiner_clicked(void)
{
	LOG;

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

	size_t index;

	Gtk::TreeModel::Path path;
	Gtk::TreeViewColumn * column;
	_treeview1->get_cursor(path, column);
	Gtk::TreeModel::iterator iter = _solutionModel->get_iter(path);
	Gtk::ListStore::Row row = *(iter);

	index = row[_solutionModelColumns->index];

	hkl_geometry_init_geometry(_geometry,
				   _engines->geometries->items[index]->geometry);
	hkl_pseudo_axis_engine_list_get(_engines);

	/*
	this->updateLattice();
	this->updateLatticeParameters();
	this->updateReciprocalLattice();
	this->updateUB();
	*/
	this->updateAxes();
	this->updatePseudoAxes();
	this->updatePseudoAxesFrames();
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

	const HklGeometryConfig *config;
	size_t idx = _combobox1->get_active_row_number();

	config = &hkl_geometry_factory_configs[idx];
	if(_geometry)
		hkl_geometry_free(_geometry);
	_geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

	if(_engines)
		hkl_pseudo_axis_engine_list_free(_engines);

	_engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(_engines, _geometry, _detector, _samples->current);

	this->set_up_pseudo_axes_frames();
	this->set_up_TreeView_axes();
	this->set_up_TreeView_pseudoAxes_parameters();
	this->set_up_TreeView_pseudoAxes();

	_solutionModelColumns = 0;
	this->set_up_TreeView_treeview1();
}
