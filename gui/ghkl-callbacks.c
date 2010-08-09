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
#include <gdk/gdkkeysyms.h>

void on_tree_view_pseudo_axes_cursor_changed(GtkTreeView *tree_view,
					     gpointer user_data)
{
	LOG;
	GtkTreePath *path;
	GtkTreeViewColumn *focus_column;
	GtkTreeModel *model;
	GtkTreeIter iter;
	HklPseudoAxis *pseudoAxis;

	g_return_if_fail(user_data);

	gtk_tree_view_get_cursor(tree_view, &path, &focus_column);
	model = gtk_tree_view_get_model(tree_view);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_model_get(model, &iter, PSEUDOAXIS_COL_PSEUDOAXIS, &pseudoAxis, -1);
	gtk_tree_view_set_model(((HklGuiWindow *)user_data)->_treeview_pseudo_axes_parameters, model);
}

void on_tree_view_crystals_cursor_changed(GtkTreeView *tree_view,
					  gpointer user_data)
{
	LOG;
	HklGuiWindow *hkl;
	GtkTreePath *path;
	GtkTreeViewColumn *focus_column;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *name;

	g_return_if_fail(user_data);

	hkl = user_data;
	gtk_tree_view_get_cursor(tree_view, &path, &focus_column);
	model = gtk_tree_view_get_model(tree_view);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_model_get(model, &iter, SAMPLE_COL_NAME, &name, -1);

	hkl_sample_list_select_current(hkl->samples, name);
	hkl_pseudo_axis_engine_list_init(hkl->engines, hkl->geometry,
					 hkl->detector, hkl->samples->current);
	gtk_tree_view_set_model(hkl->_treeview_reflections, model);
	hkl_gui_update_lattice(hkl);
	hkl_gui_update_lattice_parameters(hkl);
	hkl_gui_update_reciprocal_lattice(hkl);
	hkl_gui_update_UxUyUz(hkl);
	hkl_gui_update_UB(hkl);
	hkl_gui_update_pseudo_axes(hkl);
	hkl_gui_update_pseudo_axes_frames(hkl);
}

void on_spinbutton_a_value_changed(GtkSpinButton *spinbutton,
				   GtkScrollType  arg1,
				   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_b_value_changed(GtkSpinButton *spinbutton,
				   GtkScrollType  arg1,
				   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_c_value_changed(GtkSpinButton *spinbutton,
				   GtkScrollType  arg1,
				   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_alpha_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_beta_value_changed(GtkSpinButton *spinbutton,
				      GtkScrollType  arg1,
				      gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_gamma_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_a_min_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_b_min_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_c_min_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_alpha_min_value_changed(GtkSpinButton *spinbutton,
					   GtkScrollType  arg1,
					   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_beta_min_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType  arg1,
					  gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_gamma_min_value_changed(GtkSpinButton *spinbutton,
					   GtkScrollType  arg1,
					   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_a_max_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_b_max_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_c_max_value_changed(GtkSpinButton *spinbutton,
				       GtkScrollType  arg1,
				       gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_alpha_max_value_changed(GtkSpinButton *spinbutton,
					   GtkScrollType  arg1,
					   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_beta_max_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType  arg1,
					  gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_gamma_max_value_changed(GtkSpinButton *spinbutton,
					   GtkScrollType  arg1,
					   gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_spinbutton_lambda_value_changed(GtkSpinButton *spinbutton,
					GtkScrollType  arg1,
					gpointer       user_data)
{
	HklGuiWindow *hkl;

	g_return_if_fail(user_data);

	hkl = user_data;
	if(hkl->geometry){
		hkl->geometry->source.wave_length = gtk_spin_button_get_value(spinbutton);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}
}

void on_spinbutton_uxuyuz_value_changed(GtkSpinButton *spinbutton,
					GtkScrollType  arg1,
					gpointer       user_data)
{
	LOG;
	// TODO change the cell background color if not synchro
}

void on_button2_clicked(GtkButton *button,
			gpointer   user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample){
		hkl_sample_set_lattice(sample,
				       gtk_spin_button_get_value(hkl->_spinbutton_a),
				       gtk_spin_button_get_value(hkl->_spinbutton_b),
				       gtk_spin_button_get_value(hkl->_spinbutton_c),
				       gtk_spin_button_get_value(hkl->_spinbutton_alpha) * HKL_DEGTORAD,
				       gtk_spin_button_get_value(hkl->_spinbutton_beta) * HKL_DEGTORAD,
				       gtk_spin_button_get_value(hkl->_spinbutton_gamma) * HKL_DEGTORAD);

		hkl_sample_set_U_from_euler(sample,
					    gtk_spin_button_get_value(hkl->_spinbutton_ux) * HKL_DEGTORAD,
					    gtk_spin_button_get_value(hkl->_spinbutton_uy) * HKL_DEGTORAD,
					    gtk_spin_button_get_value(hkl->_spinbutton_uz) * HKL_DEGTORAD);

		// set min/max
		hkl_parameter_set_range_unit(sample->lattice->a,
					     gtk_spin_button_get_value(hkl->_spinbutton_a_min),
					     gtk_spin_button_get_value(hkl->_spinbutton_a_max));
		hkl_parameter_set_range_unit(sample->lattice->b,
					     gtk_spin_button_get_value(hkl->_spinbutton_b_min),
					     gtk_spin_button_get_value(hkl->_spinbutton_b_max));
		hkl_parameter_set_range_unit(sample->lattice->c,
					     gtk_spin_button_get_value(hkl->_spinbutton_c_min),
					     gtk_spin_button_get_value(hkl->_spinbutton_c_max));
		hkl_parameter_set_range_unit(sample->lattice->alpha,
					     gtk_spin_button_get_value(hkl->_spinbutton_alpha_min),
					     gtk_spin_button_get_value(hkl->_spinbutton_alpha_max));
		hkl_parameter_set_range_unit(sample->lattice->beta,
					     gtk_spin_button_get_value(hkl->_spinbutton_beta_min),
					     gtk_spin_button_get_value(hkl->_spinbutton_beta_max));
		hkl_parameter_set_range_unit(sample->lattice->gamma,
					     gtk_spin_button_get_value(hkl->_spinbutton_gamma_min),
					     gtk_spin_button_get_value(hkl->_spinbutton_gamma_max));

		hkl_gui_update_crystal_model(hkl, sample);
		hkl_gui_update_reciprocal_lattice(hkl);
		hkl_gui_update_UB(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}
}

void on_checkbutton_a_toggled(GtkToggleButton *togglebutton,
			      gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->lattice->a->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_b_toggled(GtkToggleButton *togglebutton,
			      gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->lattice->b->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_c_toggled(GtkToggleButton *togglebutton,
			      gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->lattice->c->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_alpha_toggled(GtkToggleButton *togglebutton,
				  gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->lattice->alpha->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_beta_toggled(GtkToggleButton *togglebutton,
				 gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->lattice->beta->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_gamma_toggled(GtkToggleButton *togglebutton,
				  gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->lattice->gamma->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_Ux_toggled(GtkToggleButton *togglebutton,
			       gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->ux->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_Uy_toggled(GtkToggleButton *togglebutton,
			       gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->uy->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_checkbutton_Uz_toggled(GtkToggleButton *togglebutton,
			       gpointer         user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		sample->uz->fit = gtk_toggle_button_get_active(togglebutton);
}

void on_cell_tree_view_axes_read_edited(GtkCellRendererText *renderer,
					gchar *path,
					gchar *new_text,
					gpointer user_data)
{
	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *name;
	double value;
	HklAxis *axis;

	g_return_if_fail(user_data);

	hkl = user_data;

	model = gtk_tree_view_get_model(hkl->_treeview_axes);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

	sscanf(new_text, "%lf", &value);
	axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
	hkl_axis_set_value_unit(axis, value);
	hkl_geometry_update(hkl->geometry);

	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   AXIS_COL_READ, value,
			   -1);
	hkl_gui_update_pseudo_axes(hkl);
	hkl_gui_update_pseudo_axes_frames(hkl);
}

void on_cell_tree_view_axes_write_edited(GtkCellRendererText *renderer,
					 gchar *path,
					 gchar *new_text,
					 gpointer user_data)
{
	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *name;
	double value;
	HklAxis *axis;

	g_return_if_fail(user_data);

	hkl = user_data;

	model = gtk_tree_view_get_model(hkl->_treeview_axes);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

	sscanf(new_text, "%lf", &value);
	axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
	hkl_axis_set_value_unit(axis, value);
	hkl_geometry_update(hkl->geometry);

	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   AXIS_COL_WRITE, value,
			   -1);
	hkl_gui_update_pseudo_axes(hkl);
	hkl_gui_update_pseudo_axes_frames(hkl);
}

void on_cell_tree_view_axes_min_edited(GtkCellRendererText *renderer,
				       gchar *path,
				       gchar *new_text,
				       gpointer user_data)
{
	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *name;
	double value;
	HklAxis *axis;
	double shit;
	double max;

	g_return_if_fail(user_data);

	hkl = user_data;
	LOG;

	model = gtk_tree_view_get_model(hkl->_treeview_axes);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

	sscanf(new_text, "%lf", &value);
	axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
	hkl_parameter_get_range_unit((HklParameter *)axis, &shit, &max);
	hkl_parameter_set_range_unit((HklParameter *)axis, value, max);

	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   AXIS_COL_MIN, value,
			   -1);
	hkl_gui_update_pseudo_axes(hkl);
}

void on_cell_tree_view_axes_max_edited(GtkCellRendererText *renderer,
				       gchar *path,
				       gchar *new_text,
				       gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *name;
	double value;
	HklAxis *axis;
	double shit;
	double min;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_axes);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

	sscanf(new_text, "%lf", &value);

	axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
	hkl_parameter_get_range_unit((HklParameter *)axis, &min, &shit);
	hkl_parameter_set_range_unit((HklParameter *)axis, min, value);

	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   AXIS_COL_MAX, value,
			   -1);
	hkl_gui_update_pseudo_axes(hkl);
}

// PseudoAxes
void on_cell_tree_view_pseudo_axes_write_edited(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	gchar *name;
	double value;
	HklPseudoAxis *pseudoAxis;
	HklError *error;
	int res;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_pseudo_axes);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, PSEUDOAXIS_COL_PSEUDOAXIS, &pseudoAxis, -1);
	gtk_tree_model_get(model, &iter, PSEUDOAXIS_COL_NAME, &name, -1);

	sscanf(new_text, "%lf", &value);

	hkl_parameter_set_value_unit((HklParameter *)pseudoAxis, value);
	error = NULL;
	if(hkl_pseudo_axis_engine_set(pseudoAxis->engine, &error) == HKL_SUCCESS){
		hkl_geometry_init_geometry(hkl->geometry,
					   hkl->engines->geometries->items[0]->geometry);
		hkl_pseudo_axis_engine_list_get(hkl->engines);
		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
				   PSEUDOAXIS_COL_WRITE, value,
				   -1);
		hkl_gui_update_axes(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
		hkl_gui_update_solutions(hkl);
	}
}

void on_cell_tree_view_pseudo_axes_is_initialized_toggled(GtkCellRendererToggle *cell_renderer,
							  gchar *path,
							  gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	HklPseudoAxis *pseudoAxis;
	int old_flag;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_pseudo_axes);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter,
			   PSEUDOAXIS_COL_PSEUDOAXIS, &pseudoAxis,
			   PSEUDOAXIS_COL_INITIALIZED, &old_flag,
			   -1);

	if (!old_flag){
		int res;

		res = hkl_pseudo_axis_engine_initialize(pseudoAxis->engine, NULL);
		if(res == HKL_SUCCESS)
			hkl_gui_update_pseudo_axes(hkl);
	}
}

/* PseudoAxes Parameters */
void on_cell_tree_view_pseudo_axes_parameters_value_edited(GtkCellRendererText *renderer,
							   gchar *path,
							   gchar *new_text,
							   gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	double value;
	HklParameter *parameter;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_pseudo_axes_parameters);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, PARAMETER_COL_PARAMETER, &parameter, -1);

	sscanf(new_text, "%lf", &value);

	hkl_parameter_set_value_unit(parameter, value);

	gtk_list_store_set(GTK_LIST_STORE(model), &iter,
			   PARAMETER_COL_VALUE, value,
			   -1);
	hkl_gui_update_pseudo_axes(hkl);
	hkl_gui_update_pseudo_axes_parameters(hkl);
}

void on_cell_tree_view_crystals_name_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	HklSample *sample;
	gchar *name;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_crystals);
	gtk_tree_model_get_iter_from_string(model, &iter, path);
	gtk_tree_model_get(model, &iter, SAMPLE_COL_NAME, &name, -1);

	sample = hkl_sample_list_get_by_name(hkl->samples, name);
	if(sample){
		hkl_sample_set_name(sample, new_text);
		hkl_gui_update_tree_view_crystals(hkl);
	}
}

void on_cell_tree_view_reflections_h_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_reflections);
	gtk_tree_model_get_iter_from_string(model, &iter, path);

	sample = hkl->samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
		reflection = sample->reflections[index];

		sscanf(new_text, "%lf", &h);
		k = reflection->hkl.data[1];
		l = reflection->hkl.data[2];

		hkl_sample_reflection_set_hkl(reflection, h, k, l);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
				   REFLECTION_COL_H, h,
				   REFLECTION_COL_FLAG, reflection->flag,
				   -1);
		hkl_gui_update_crystal_model(hkl, sample);
	}
}

void on_cell_tree_view_reflections_k_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_reflections);
	gtk_tree_model_get_iter_from_string(model, &iter, path);

	sample = hkl->samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
		reflection = sample->reflections[index];

		h = reflection->hkl.data[0];
		sscanf(new_text, "%lf", &k);
		l = reflection->hkl.data[2];

		hkl_sample_reflection_set_hkl(reflection, h, k, l);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
				   REFLECTION_COL_K, k,
				   REFLECTION_COL_FLAG, reflection->flag,
				   -1);
		hkl_gui_update_crystal_model(hkl, sample);
	}
}

void on_cell_tree_view_reflections_l_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	GtkTreeModel *model;
	GtkTreeIter iter;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	model = gtk_tree_view_get_model(hkl->_treeview_reflections);
	gtk_tree_model_get_iter_from_string(model, &iter, path);

	sample = hkl->samples->current;
	if(sample){
		int index;
		double h;
		double k;
		double l;
		HklSampleReflection *reflection;

		gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
		reflection = sample->reflections[index];

		h = reflection->hkl.data[0];
		k = reflection->hkl.data[1];
		sscanf(new_text, "%lf", &l);

		hkl_sample_reflection_set_hkl(reflection, h, k, l);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
				   REFLECTION_COL_L, l,
				   REFLECTION_COL_FLAG, reflection->flag,
				   -1);
		hkl_gui_update_crystal_model(hkl, sample);
	}
}

void on_cell_tree_view_reflections_flag_toggled(GtkCellRendererToggle *cell_renderer,
						gchar *path,
						gpointer user_data)
{
	LOG;

	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample){
		int index;
		int flag;
		HklSampleReflection *reflection;
		GtkTreeModel *model;
		GtkTreeIter iter;

		model = gtk_tree_view_get_model(hkl->_treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
		reflection = sample->reflections[index];
		flag = !reflection->flag;
		hkl_sample_reflection_set_flag(reflection, flag);
		gtk_list_store_set(GTK_LIST_STORE(model), &iter, REFLECTION_COL_FLAG, flag, -1);
	}
}

void on_toolbutton_add_reflection_clicked(GtkToolButton *toolbutton,
					  gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample=hkl->samples->current;
	if(sample){
		double h;
		double k;
		double l;

		hkl_sample_add_reflection(sample, hkl->geometry, hkl->detector, h, k, l);
		hkl_gui_update_reflections(hkl, sample);
	}
}

void on_toolbutton_goto_reflection_clicked(GtkToolButton *toolbutton,
					   gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample){
		GtkTreeSelection *selection;
		unsigned int nb_rows;

		selection = gtk_tree_view_get_selection(hkl->_treeview_reflections);
		nb_rows = gtk_tree_selection_count_selected_rows(selection);
		if(nb_rows == 1){
			GList *list;
			unsigned int index;
			GtkTreeIter iter;
			GtkTreeModel *model;

			/* first need to find the right reflections_store and put it in model */
			list = gtk_tree_selection_get_selected_rows(selection, &model);
			gtk_tree_model_get_iter(model, &iter, list->data);
			gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);

			hkl_geometry_init_geometry(hkl->geometry,
						   sample->reflections[index]->geometry);

			hkl_gui_update_source(hkl);
			hkl_gui_update_axes(hkl);
			hkl_gui_update_pseudo_axes(hkl);

			g_list_foreach (list, (GFunc) gtk_tree_path_free, NULL);
			g_list_free (list);
		}else{
			if (nb_rows)
				gtk_statusbar_push(hkl->_statusBar, 0, "Please select only one reflection.");
			else
				gtk_statusbar_push(hkl->_statusBar, 0, "Please select at least one reflection.");
		}
	}
}

void on_toolbutton_del_reflection_clicked(GtkToolButton *toolbutton,
					  gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample){
		GtkTreeSelection *selection;
		unsigned int nb_rows;

		selection = gtk_tree_view_get_selection(hkl->_treeview_reflections);
		nb_rows = gtk_tree_selection_count_selected_rows(selection);
		if (nb_rows){
			GList *list;
			GtkTreeModel *model;
			GtkTreeIter iter;
			uint index;
			uint *indexes;
			int i;
			GtkWidget *dialog;
			int respons;

			list = gtk_tree_selection_get_selected_rows(selection, &model);
			// fill indexes with the reflections index
			indexes = malloc(nb_rows * sizeof(*indexes));
			i = 0;
			while(list){
				gtk_tree_model_get_iter(model, &iter, list->data);
				gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &indexes[i++], -1);
				g_list_next(list);
			}

			/* display a dialog message to confirm the deletion */
			dialog = gtk_message_dialog_new(NULL,
							GTK_DIALOG_DESTROY_WITH_PARENT,
							GTK_MESSAGE_WARNING,
							GTK_BUTTONS_YES_NO,
							"Are you sure that you want to delete reflections");
			//"Are you sure that you want to delete reflections");
			respons = gtk_dialog_run (GTK_DIALOG (dialog));
			switch (respons){
			case GTK_RESPONSE_YES:
				for(i=0;i<nb_rows;++i){
					// compute the correct index of the reflection
					unsigned int index = indexes[i] - i;
					hkl_sample_del_reflection(sample, index);
				}
				hkl_gui_update_reflections(hkl, sample);
				break;
			}
			gtk_widget_destroy(dialog);

			free(indexes);
		}else
			gtk_statusbar_push(hkl->_statusBar, 0, "Please select at least one reflection.");
	}
}
void on_toolbutton_setUB_clicked(GtkToolButton *toolbutton,
				 gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample){
		HklMatrix UB;

		UB.data[0][0] = gtk_spin_button_get_value(hkl->_spinbutton_U11);
		UB.data[0][1] = gtk_spin_button_get_value(hkl->_spinbutton_U12);
		UB.data[0][2] = gtk_spin_button_get_value(hkl->_spinbutton_U13);
		UB.data[1][0] = gtk_spin_button_get_value(hkl->_spinbutton_U21);
		UB.data[1][1] = gtk_spin_button_get_value(hkl->_spinbutton_U22);
		UB.data[1][2] = gtk_spin_button_get_value(hkl->_spinbutton_U23);
		UB.data[2][0] = gtk_spin_button_get_value(hkl->_spinbutton_U31);
		UB.data[2][1] = gtk_spin_button_get_value(hkl->_spinbutton_U32);
		UB.data[2][2] = gtk_spin_button_get_value(hkl->_spinbutton_U33);

		hkl_sample_set_UB(sample, &UB);
		hkl_sample_fprintf(stdout, sample);

		hkl_gui_update_lattice(hkl);
		hkl_gui_update_lattice_parameters(hkl);
		hkl_gui_update_reciprocal_lattice(hkl);
		hkl_gui_update_crystal_model(hkl, sample);
		hkl_gui_update_UB(hkl);
		hkl_gui_update_UxUyUz(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}
}

void on_toolbutton_computeUB_clicked(GtkToolButton *toolbutton,
				     gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample){
		hkl_sample_compute_UB_busing_levy(sample, 0, 1);
		hkl_gui_update_UB(hkl);
		hkl_gui_update_UxUyUz(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}
}

void on_toolbutton_add_crystal_clicked(GtkToolButton *toolbutton,
				       gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl_sample_new("new_sample", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	if(sample){
		GtkTreePath *path;
		GtkTreeViewColumn *column;

		hkl_sample_list_append(hkl->samples, sample);
		hkl_sample_list_select_current(hkl->samples, "new_sample");
		hkl_gui_update_tree_view_crystals(hkl);

		// activate for edition the name of the new crystal
		gtk_tree_view_get_cursor(hkl->_treeview_crystals, &path, &column);
		column = gtk_tree_view_get_column(hkl->_treeview_crystals, 0);
		gtk_tree_view_set_cursor(hkl->_treeview_crystals, path, column, TRUE);
	}
}

void on_toolbutton_copy_crystal_clicked(GtkToolButton *toolbutton,
					gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;
	HklSample *old_sample;
	GtkTreePath *path;
	GtkTreeViewColumn *column;

	g_return_if_fail(user_data);

	hkl = user_data;
	old_sample = hkl->samples->current;
	if(!old_sample){
		gtk_statusbar_push(hkl->_statusBar, 0, "Please select a crystal to copy.");
		return;
	}

	sample = hkl_sample_new_copy(hkl->samples->current);
	hkl_sample_set_name(sample, "copy");
	hkl_sample_list_append(hkl->samples, sample);
	hkl_sample_list_select_current(hkl->samples, "copy");
	hkl_gui_update_tree_view_crystals(hkl);

	// activate for edition the name of the new crystal
	gtk_tree_view_get_cursor(hkl->_treeview_crystals, &path, &column);
	column = gtk_tree_view_get_column(hkl->_treeview_crystals, 0);
	gtk_tree_view_set_cursor(hkl->_treeview_crystals, path, column, TRUE);
}

void on_toolbutton_del_crystal_clicked(GtkToolButton *toolbutton,
				       gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	if(hkl->samples->current){
		hkl_sample_list_del(hkl->samples, hkl->samples->current);
		hkl_gui_update_tree_view_crystals(hkl);
	}
}

void on_toolbutton_affiner_clicked(GtkToolButton *toolbutton,
				   gpointer user_data)
{
	HklGuiWindow *hkl;
	HklSample *sample;

	g_return_if_fail(user_data);

	hkl = user_data;
	sample = hkl->samples->current;
	if(sample)
		hkl_sample_affine(sample);

	hkl_gui_update_crystal_model(hkl, sample);
	hkl_gui_update_lattice(hkl);
	hkl_gui_update_reciprocal_lattice(hkl);
	hkl_gui_update_UB(hkl);
	hkl_gui_update_UxUyUz(hkl);
}

gboolean on_tree_view_reflections_key_press_event(GtkWidget *widget,
						  GdkEventKey *event,
						  gpointer user_data)
{
	LOG;
	HklGuiWindow *hkl;

	g_return_if_fail(user_data);

	hkl = user_data;
	switch (event->keyval)
	{
	case GDK_Insert:
	case GDK_KP_Insert:
		on_toolbutton_add_reflection_clicked(hkl->_toolbutton_add_crystal, user_data);
		break;
	case GDK_Delete:
	case GDK_KP_Delete:
		on_toolbutton_del_reflection_clicked(hkl->_toolbutton_del_reflection, user_data);
		break;
	}
	return TRUE;
}

gboolean on_tree_view_crystals_key_press_event(GtkWidget *widget,
					       GdkEventKey *event,
					       gpointer user_data)
{
	LOG;
	HklGuiWindow *hkl;

	g_return_if_fail(user_data);

	hkl = user_data;
	switch (event->keyval)
	{
	case GDK_Insert:
	case GDK_KP_Insert:
		on_toolbutton_add_crystal_clicked(hkl->_toolbutton_add_crystal, user_data);
		break;
	case GDK_Delete:
	case GDK_KP_Delete:
		on_toolbutton_del_crystal_clicked(hkl->_toolbutton_del_reflection, user_data);
		break;
	}
	return TRUE;
}

void on_tree_view1_cursor_changed(GtkTreeView *tree_view,
				 gpointer user_data)
{
	LOG;
	HklGuiWindow *hkl;
	GtkTreePath *path;
	GtkTreeViewColumn *focus_column;
	GtkTreeModel *model;
	GtkTreeIter iter;
	size_t index;

	g_return_if_fail(user_data);

	hkl = user_data;
	gtk_tree_view_get_cursor(tree_view, &path, &focus_column);
	model = gtk_tree_view_get_model(tree_view);
	gtk_tree_model_get_iter(model, &iter, path);
	gtk_tree_model_get(model, &iter, SOLUTION_COL_INDEX, &index, -1);


	hkl_geometry_init_geometry(hkl->geometry,
				   hkl->engines->geometries->items[index]->geometry);
	hkl_pseudo_axis_engine_list_get(hkl->engines);

	hkl_gui_update_axes(hkl);
	hkl_gui_update_pseudo_axes(hkl);
	hkl_gui_update_pseudo_axes_frames(hkl);
}

void on_pseudo_axes_frame_changed(gpointer user_data)
{
	LOG;
	HklGuiWindow *hkl;

	g_return_if_fail(user_data);

	hkl = user_data;
	hkl_gui_update_axes(hkl);
	hkl_gui_update_pseudo_axes(hkl);
	hkl_gui_update_pseudo_axes_frames(hkl);
	hkl_gui_update_solutions(hkl);
}

void on_menuitem5_activate(GtkMenuItem *menuitem,
			   gpointer user_data)
{
	LOG;
	g_return_if_fail(user_data);

	gtk_widget_show(GTK_WIDGET(((HklGuiWindow *)user_data)->_dialog1));
}

void on_button1_clicked(GtkButton *button,
			gpointer user_data)
{
	LOG;
	g_return_if_fail(user_data);

	gtk_widget_hide(GTK_WIDGET(((HklGuiWindow *)user_data)->_dialog1));
}

void on_combobox1_changed(GtkComboBox *widget,
			  gpointer user_data)
{
	LOG;
	HklGuiWindow *hkl;
	size_t idx;
	const HklGeometryConfig *config;

	g_return_if_fail(user_data);

	hkl = user_data;
	idx = gtk_combo_box_get_active(widget);

	config = &hkl_geometry_factory_configs[idx];
	if(hkl->geometry)
		hkl_geometry_free(hkl->geometry);
	hkl->geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

	if(hkl->engines)
		hkl_pseudo_axis_engine_list_free(hkl->engines);

	hkl->engines = hkl_pseudo_axis_engine_list_factory(config);
	hkl_pseudo_axis_engine_list_init(hkl->engines, hkl->geometry,
					 hkl->detector, hkl->samples->current);

	hkl_gui_set_up_pseudo_axes_frames(hkl);
	hkl_gui_set_up_tree_view_axes(hkl);
	hkl_gui_set_up_tree_view_pseudo_axes_parameters(hkl);
	hkl_gui_set_up_tree_view_pseudo_axes(hkl);

	/* FIXME create the right solution Model Column */
	/* hkl->_solutionModelColumns = 0; */
	hkl_gui_set_up_tree_view_treeview1(hkl);
}
