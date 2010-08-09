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
#include "ghkl-callbacks.h"

/**********/
/* STATIC */
/**********/

static void hkl_gui_get_widgets_and_objects_from_ui(HklGuiWindow *self)
{
	LOG;

	GtkBuilder *builder = NULL;

	builder = gtk_builder_new();
	if(builder){
		GError *error = NULL;

		self->builder = builder;
		gtk_builder_add_from_file(builder, "ghkl.ui", &error);
		if(error == NULL){
			GtkCellRenderer *renderer;

			// objects
			self->store_diffractometer = (GtkListStore *)gtk_builder_get_object(builder, "liststore1");

			// window1
			self->_label_UB11 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB11");
			self->_label_UB12 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB12");
			self->_label_UB13 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB13");
			self->_label_UB21 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB21");
			self->_label_UB22 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB22");
			self->_label_UB23 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB23");
			self->_label_UB31 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB31");
			self->_label_UB32 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB32");
			self->_label_UB33 = (GtkLabel *)gtk_builder_get_object(builder, "label_UB33");
			self->_button2 = (GtkButton *)gtk_builder_get_object(builder, "button2");
			self->_spinbutton_a_star = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_a_star");
			self->_spinbutton_b_star = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_b_star");
			self->_spinbutton_c_star = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_c_star");
			self->_spinbutton_alpha_star = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_alpha_star");
			self->_spinbutton_beta_star = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_beta_star");
			self->_spinbutton_gamma_star = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_gamma_star");
			self->_spinbutton_a = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_a");
			self->_spinbutton_b = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_b");
			self->_spinbutton_c = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_c");
			self->_spinbutton_alpha = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_alpha");
			self->_spinbutton_beta = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_beta");
			self->_spinbutton_gamma = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_gamma");
			self->_spinbutton_a_min = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_a_min");
			self->_spinbutton_b_min = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_b_min");
			self->_spinbutton_c_min = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_c_min");
			self->_spinbutton_alpha_min = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_alpha_min");
			self->_spinbutton_beta_min = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_beta_min");
			self->_spinbutton_gamma_min = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_gamma_min");
			self->_spinbutton_a_max = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_a_max");
			self->_spinbutton_b_max = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_b_max");
			self->_spinbutton_c_max = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_c_max");
			self->_spinbutton_alpha_max = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_alpha_max");
			self->_spinbutton_beta_max = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_beta_max");
			self->_spinbutton_gamma_max = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_gamma_max");
			self->_spinbutton_lambda = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_lambda");
			self->_spinbutton_ux = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_ux");
			self->_spinbutton_uy = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_uy");
			self->_spinbutton_uz = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_uz");
			self->_spinbutton_U11 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U11");
			self->_spinbutton_U12 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U12");
			self->_spinbutton_U13 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U13");
			self->_spinbutton_U21 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U21");
			self->_spinbutton_U22 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U22");
			self->_spinbutton_U23 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U23");
			self->_spinbutton_U31 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U31");
			self->_spinbutton_U32 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U32");
			self->_spinbutton_U33 = (GtkSpinButton *)gtk_builder_get_object(builder, "spinbutton_U33");
			self->_checkbutton_a = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_a");
			self->_checkbutton_b = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_b");
			self->_checkbutton_c = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_c");
			self->_checkbutton_alpha = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_alpha");
			self->_checkbutton_beta = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_beta");
			self->_checkbutton_gamma = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_gamma");
			self->_checkbutton_Ux = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_Ux");
			self->_checkbutton_Uy = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_Uy");
			self->_checkbutton_Uz = (GtkCheckButton *)gtk_builder_get_object(builder, "checkbutton_Uz");
			self->_treeview_reflections = (GtkTreeView *)gtk_builder_get_object(builder, "treeview_reflections");
			self->_treeview_crystals = (GtkTreeView *)gtk_builder_get_object(builder, "treeview_crystals");
			self->_treeview_axes = (GtkTreeView *)gtk_builder_get_object(builder, "treeview_axes");
			self->_treeview_pseudo_axes = (GtkTreeView *)gtk_builder_get_object(builder, "treeview_pseudoAxes");
			self->_treeview_pseudo_axes_parameters = (GtkTreeView *)gtk_builder_get_object(builder, "treeview_pseudoAxes_parameters");
			self->_treeview1 = (GtkTreeView *)gtk_builder_get_object(builder, "treeview1");
			self->_toolbutton_add_reflection = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_add_reflection");
			self->_toolbutton_goto_reflection = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_goto_reflection");
			self->_toolbutton_del_reflection = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_del_reflection");
			self->_toolbutton_setUB = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_setUB");
			self->_toolbutton_computeUB = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_computeUB");
			self->_toolbutton_add_crystal = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_add_crystal");
			self->_toolbutton_copy_crystal = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_copy_crystal");
			self->_toolbutton_del_crystal = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_del_crystal");
			self->_toolbutton_affiner = (GtkToolButton *)gtk_builder_get_object(builder, "toolbutton_affiner");
			self->_statusBar = (GtkStatusbar *)gtk_builder_get_object(builder, "statusbar");
			self->_menuitem5 = (GtkImageMenuItem *)gtk_builder_get_object(builder, "menuitem5");

			// dialog1
			self->_dialog1 = (GtkDialog *)gtk_builder_get_object(builder, "dialog1");
			self->_button1 = (GtkButton *)gtk_builder_get_object(builder, "button1");
			self->_combobox1 = (GtkComboBox *)gtk_builder_get_object(builder, "combobox1");
		}else{
			/* Affichage du message d'erreur de GTK+ */
			g_error ("%s", error->message);
			g_error_free (error);
		}
	}
}

static void hkl_gui_set_up_diffractometer_model(HklGuiWindow *self)
{
	LOG;

	size_t i;

	i= 0;
	while(hkl_geometry_factory_configs[i].name){
		GtkTreeIter iter;
		gtk_list_store_append(self->store_diffractometer, &iter);
		gtk_list_store_set(self->store_diffractometer, &iter,
				   DIFFRACTOMETER_COL_NAME, hkl_geometry_factory_configs[i++].name,
				   -1);
	}
}

static void hkl_gui_connect_all_signals(HklGuiWindow *self)
{
	LOG;

	g_signal_connect(G_OBJECT(self->_spinbutton_a), "value-changed", 
			 G_CALLBACK(on_spinbutton_a_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_b), "value-changed", 
			 G_CALLBACK(on_spinbutton_b_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_c), "value-changed", 
			 G_CALLBACK(on_spinbutton_c_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_alpha), "value-changed", 
			 G_CALLBACK(on_spinbutton_alpha_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_beta), "value-changed", 
			 G_CALLBACK(on_spinbutton_beta_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_gamma), "value-changed", 
			 G_CALLBACK(on_spinbutton_gamma_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_a_min), "value-changed", 
			 G_CALLBACK(on_spinbutton_a_min_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_b_min), "value-changed", 
			 G_CALLBACK(on_spinbutton_b_min_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_c_min), "value-changed", 
			 G_CALLBACK(on_spinbutton_c_min_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_alpha_min), "value-changed", 
			 G_CALLBACK(on_spinbutton_alpha_min_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_beta_min), "value-changed", 
			 G_CALLBACK(on_spinbutton_beta_min_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_gamma_min), "value-changed", 
			 G_CALLBACK(on_spinbutton_gamma_min_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_a_max), "value-changed", 
			 G_CALLBACK(on_spinbutton_a_max_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_b_max), "value-changed", 
			 G_CALLBACK(on_spinbutton_b_max_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_c_max), "value-changed", 
			 G_CALLBACK(on_spinbutton_c_max_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_alpha_max), "value-changed", 
			 G_CALLBACK(on_spinbutton_alpha_max_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_beta_max), "value-changed", 
			 G_CALLBACK(on_spinbutton_beta_max_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_gamma_max), "value-changed", 
			 G_CALLBACK(on_spinbutton_gamma_max_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_lambda), "value-changed", 
			 G_CALLBACK(on_spinbutton_lambda_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_ux), "value-changed", 
			 G_CALLBACK(on_spinbutton_uxuyuz_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_uy), "value-changed", 
			 G_CALLBACK(on_spinbutton_uxuyuz_value_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_spinbutton_uz), "value-changed", 
			 G_CALLBACK(on_spinbutton_uxuyuz_value_changed), 
			 self);

	g_signal_connect(G_OBJECT(self->_button2), "clicked", 
			 G_CALLBACK(on_button2_clicked), 
			 self);

	g_signal_connect(G_OBJECT(self->_checkbutton_a), "toggled", 
			 G_CALLBACK(on_checkbutton_a_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_b), "toggled", 
			 G_CALLBACK(on_checkbutton_b_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_c), "toggled", 
			 G_CALLBACK(on_checkbutton_c_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_alpha), "toggled", 
			 G_CALLBACK(on_checkbutton_alpha_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_beta), "toggled", 
			 G_CALLBACK(on_checkbutton_beta_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_gamma), "toggled", 
			 G_CALLBACK(on_checkbutton_gamma_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_Ux), "toggled", 
			 G_CALLBACK(on_checkbutton_Ux_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_Uy), "toggled", 
			 G_CALLBACK(on_checkbutton_Uy_toggled), 
			 self);
	g_signal_connect(G_OBJECT(self->_checkbutton_Uz), "toggled", 
			 G_CALLBACK(on_checkbutton_Uz_toggled), 
			 self);

	g_signal_connect(G_OBJECT(self->_treeview_reflections), "key-press-event", 
			 G_CALLBACK(on_tree_view_reflections_key_press_event), 
			 self);
	g_signal_connect(G_OBJECT(self->_treeview_pseudo_axes), "cursor-changed", 
			 G_CALLBACK(on_tree_view_pseudo_axes_cursor_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_treeview_crystals), "cursor-changed", 
			 G_CALLBACK(on_tree_view_crystals_cursor_changed), 
			 self);
	g_signal_connect(G_OBJECT(self->_treeview_crystals), "key-press-event", 
			 G_CALLBACK(on_tree_view_crystals_key_press_event), 
			 self);

	g_signal_connect(G_OBJECT(self->_toolbutton_add_reflection), "clicked", 
			 G_CALLBACK(on_toolbutton_add_reflection_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_goto_reflection), "clicked", 
			 G_CALLBACK(on_toolbutton_goto_reflection_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_del_reflection), "clicked", 
			 G_CALLBACK(on_toolbutton_del_reflection_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_setUB), "clicked", 
			 G_CALLBACK(on_toolbutton_setUB_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_computeUB), "clicked", 
			 G_CALLBACK(on_toolbutton_computeUB_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_add_crystal), "clicked", 
			 G_CALLBACK(on_toolbutton_add_crystal_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_copy_crystal), "clicked", 
			 G_CALLBACK(on_toolbutton_copy_crystal_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_del_crystal), "clicked", 
			 G_CALLBACK(on_toolbutton_del_crystal_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_toolbutton_affiner), "clicked", 
			 G_CALLBACK(on_toolbutton_affiner_clicked), 
			 self);

	g_signal_connect(G_OBJECT(self->_menuitem5), "activate", 
			 G_CALLBACK(on_menuitem5_activate), 
			 self);

	// dialog1
	g_signal_connect(G_OBJECT(self->_button1), "clicked", 
			 G_CALLBACK(on_button1_clicked), 
			 self);
	g_signal_connect(G_OBJECT(self->_combobox1), "changed", 
			 G_CALLBACK(on_combobox1_changed), 
			 self);
}

/**********/
/* PUBLIC */
/**********/

HklGuiWindow *hkl_gui_window_new()
{
	LOG;

	HklGuiWindow *self;
	size_t i;
	HklSample *sample;

	self = HKL_MALLOC(HklGuiWindow);

	self->geometry = NULL;
	self->engines = NULL;

	self->detector = hkl_detector_factory_new(HKL_DETECTOR_TYPE_0D);
        self->detector->idx = 1;
	
	self->samples = hkl_sample_list_new();
	/* add a default crystal */
	sample = hkl_sample_new("test", HKL_SAMPLE_TYPE_MONOCRYSTAL);
	hkl_sample_list_append(self->samples, sample);
	hkl_sample_list_select_current(self->samples, "test");

	self->store_samples = NULL;

	/* create the reciprocal lattice */
	self->reciprocal = hkl_lattice_new_default();

	self->pseudoAxesFrames = NULL;
	self->pseudoAxesFrames_len = 0;

	hkl_gui_get_widgets_and_objects_from_ui(self);

	hkl_gui_set_up_diffractometer_model(self);
	hkl_gui_set_up_tree_view_reflections(self);
	hkl_gui_set_up_tree_view_crystals(self);

	/* update the widgets */
	hkl_gui_update_tree_view_crystals(self);
	hkl_gui_update_source(self);
	hkl_gui_update_lattice(self);
	hkl_gui_update_lattice_parameters(self);
	hkl_gui_update_reciprocal_lattice(self);
	hkl_gui_update_UxUyUz(self);
	hkl_gui_update_UB(self);

	hkl_gui_connect_all_signals(self);

	//gtk_widget_show_all(NULL);
//GTK_WIDGET(self->window));
}

void hkl_gui_window_free(HklGuiWindow *self)
{
	LOG;

	/* first the hkl part */
	hkl_geometry_free(self->geometry);
	hkl_detector_free(self->detector);
	hkl_pseudo_axis_engine_list_free(self->engines);
	hkl_sample_list_free(self->samples);
	hkl_lattice_free(self->reciprocal);
}

void hkl_gui_set_up_pseudo_axes_frames(HklGuiWindow *self)
{
	LOG;

	size_t i;
	GtkVBox *vbox2 = NULL;

	vbox2 = (GtkVBox *)gtk_builder_get_object(self->builder, "vbox2");

	// first clear the previous frames
	for(i=0; i<self->pseudoAxesFrames_len; ++i){
		gtk_container_remove(GTK_CONTAINER(vbox2),
				     GTK_WIDGET(self->pseudoAxesFrames[i]->frame1));
		hkl_gui_pseudo_axes_frame_free(self->pseudoAxesFrames[i]);
	}
	free(self->pseudoAxesFrames);
	self->pseudoAxesFrames_len = 0;

	for(i=0; i<HKL_LIST_LEN(self->engines->engines); ++i){
		HklGuiPseudoAxesFrame *pseudo;

		pseudo = hkl_gui_pseudo_axes_frame_new(self->engines->engines[i]);
		self->pseudoAxesFrames = realloc(self->pseudoAxesFrames,
						 (self->pseudoAxesFrames_len + 1) * sizeof(*self->pseudoAxesFrames));
		self->pseudoAxesFrames[self->pseudoAxesFrames_len++] = pseudo;

		gtk_container_add(GTK_CONTAINER(vbox2),
				  GTK_WIDGET(pseudo->frame1));

		/* FIXME need to create a signal for the pseudo axes frames
		g_signal_connect(G_OBJECT(pseudo), "changed", 
				 G_CALLBACK(on_pseudo_axes_frame_changed), 
				 self);
		*/
	}
	gtk_widget_show_all(GTK_WIDGET(vbox2));
}

void hkl_gui_set_up_tree_view_axes(HklGuiWindow *self)
{
	LOG;

	size_t i;
	int index;
	GtkCellRenderer * renderer;
	GList *columns, *col;
	GtkTreeViewColumn *column;

	g_return_if_fail(self);

	/* Create the Model and fill it */
	/* FIXME remove in the destructor and when changing the diffractometer */
	self->store_axis = gtk_list_store_new(AXIS_N_COLUMNS,
					      G_TYPE_POINTER,
					      G_TYPE_STRING, G_TYPE_DOUBLE,
					      G_TYPE_DOUBLE, G_TYPE_DOUBLE,
					      G_TYPE_DOUBLE);
	for(i=0; i<HKL_LIST_LEN(self->geometry->axes); ++i){
		HklAxis *axis;
		GtkTreeIter iter;

		axis = &self->geometry->axes[i];
		gtk_list_store_append(self->store_axis, &iter);
		gtk_list_store_set(self->store_axis, &iter,
				   AXIS_COL_AXIS, axis,
				   AXIS_COL_NAME, ((HklParameter *)axis)->name,
				   -1);
	}

	/* first remove all the columns */
	columns = col = gtk_tree_view_get_columns(self->_treeview_axes);
	while(col){
		gtk_tree_view_remove_column(self->_treeview_axes, col->data);
		g_list_next(col);
	}
	g_list_free(columns);

	/* add the columns */
	/* name */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("name",
							   renderer,
							   "text", AXIS_COL_NAME,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_axes, column);

	/* read */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_axes_read_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("read",
							   renderer,
							   "text", AXIS_COL_READ,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_axes, column);

	/* write */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_axes_write_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("write",
							   renderer,
							   "text", AXIS_COL_WRITE,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_axes, column);

	/* min */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_axes_min_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("min",
							   renderer,
							   "text", AXIS_COL_MIN,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_axes, column);

	/* max */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_axes_max_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("max",
							   renderer,
							   "text", AXIS_COL_MAX,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_axes, column);

	//Set the model for the TreeView
	gtk_tree_view_set_model(self->_treeview_axes,
				GTK_TREE_MODEL( self->store_axis));
	hkl_gui_update_axes(self);
}

void hkl_gui_set_up_tree_view_pseudo_axes(HklGuiWindow *self)
{
	LOG;

	size_t i;
	size_t j;
	size_t k;
	int index;
	GtkCellRenderer * renderer;
	GList *columns, *col;
	GtkTreeViewColumn *column;

	g_return_if_fail(self);

	/* first remove all columns of the tree view */
	columns = col = gtk_tree_view_get_columns(self->_treeview_pseudo_axes);
	while(col){
		gtk_tree_view_remove_column(self->_treeview_pseudo_axes, col->data);
		g_list_next(col);
	}
	g_list_free(columns);

	/* add the columns */
	/* name */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("name",
							   renderer,
							   "text", PSEUDOAXIS_COL_NAME,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes, column);

	/* read */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("read",
							   renderer,
							   "text", PSEUDOAXIS_COL_READ,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes, column);

	/* write */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_pseudo_axes_write_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("write",
							   renderer,
							   "text", PSEUDOAXIS_COL_WRITE,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes, column);

	/* min */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("min",
							   renderer,
							   "text", PSEUDOAXIS_COL_MIN,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes, column);

	/* max */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("max",
							   renderer,
							   "text", PSEUDOAXIS_COL_MAX,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes, column);


	/* initialized */
	renderer = gtk_cell_renderer_toggle_new ();
	g_signal_connect(G_OBJECT(renderer), "toggled", 
			 G_CALLBACK(on_cell_tree_view_pseudo_axes_is_initialized_toggled), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("write",
							   renderer,
							   "active", PSEUDOAXIS_COL_INITIALIZED,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes, column);

	/* Create the Model and fill it */
	/* FIXME remove in the destructor and when changing the diffractometer */
	self->store_pseudo_axis = gtk_list_store_new(PSEUDOAXIS_N_COLUMNS,
						     G_TYPE_POINTER, /* pseudoaxis */
						     G_TYPE_STRING, /* name */
						     G_TYPE_DOUBLE, /* read */
						     G_TYPE_DOUBLE, /* write */
						     G_TYPE_DOUBLE, /* min */
						     G_TYPE_DOUBLE, /* max */
						     G_TYPE_BOOLEAN); /* initialized */
	/* FIXME remove in the destructor */
	self->hash_store_pseudo_axis_parameter = g_hash_table_new(NULL, NULL);
	for(i=0; i<HKL_LIST_LEN(self->engines->engines); ++i){
		HklPseudoAxisEngine *engine = self->engines->engines[i];

		for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j){
			HklPseudoAxis *pseudo_axis;
			GtkTreeIter iter;

			pseudo_axis = engine->pseudoAxes[j];
			gtk_list_store_append(self->store_pseudo_axis, &iter);
			gtk_list_store_set(self->store_pseudo_axis, &iter,
					   PSEUDOAXIS_COL_PSEUDOAXIS, pseudo_axis,
					   PSEUDOAXIS_COL_NAME, ((HklParameter *)pseudo_axis)->name,
					   -1);

			if(HKL_LIST_LEN(engine->mode->parameters)){
				GtkListStore *model;

				model = gtk_list_store_new(PARAMETER_N_COLUMNS,
							   G_TYPE_POINTER, /* pseudoaxis */
							   G_TYPE_STRING, /* name */
							   G_TYPE_DOUBLE); /* value */
				for(k=0; k<HKL_LIST_LEN(engine->mode->parameters); ++k){
					HklParameter *parameter;

					parameter = &engine->mode->parameters[k];
					gtk_list_store_append(model, &iter);
					gtk_list_store_set(model, &iter,
							   PARAMETER_COL_PARAMETER, parameter,
							   PARAMETER_COL_NAME, parameter->name,
							   PARAMETER_COL_VALUE, hkl_parameter_get_value_unit(parameter),
							   -1);
				}
				g_hash_table_insert(self->hash_store_pseudo_axis_parameter,
						    pseudo_axis, model);
			}
		}
	}
	/* Set the model for the TreeView */
	gtk_tree_view_set_model(self->_treeview_pseudo_axes,
				GTK_TREE_MODEL(self->store_pseudo_axis));
	hkl_gui_update_pseudo_axes(self);
}

void hkl_gui_set_up_tree_view_pseudo_axes_parameters(HklGuiWindow *self)
{
	LOG;

	GtkCellRenderer * renderer;
	GList *columns, *col;
	GtkTreeViewColumn *column;

	g_return_if_fail(self);

	/* first remove all columns of the tree view */
	columns = col = gtk_tree_view_get_columns(self->_treeview_pseudo_axes_parameters);
	while(col){
		gtk_tree_view_remove_column(self->_treeview_pseudo_axes_parameters, col->data);
		g_list_next(col);
	}
	g_list_free(columns);

	/* add the columns */
	/* name */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("name",
							   renderer,
							   "text", PARAMETER_COL_NAME,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes_parameters, column);

	/* value */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_pseudo_axes_parameters_value_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("read",
							   renderer,
							   "text", PARAMETER_COL_VALUE,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_pseudo_axes_parameters, column);
}

void hkl_gui_set_up_tree_view_treeview1(HklGuiWindow *self)
{
	LOG;

	size_t i;
	GList *columns, *col;
	GtkTreeViewColumn *column;
	GtkCellRenderer * renderer;
	GType *types;

	g_return_if_fail(self);

	/* first remove all columns of the tree view */
	columns = col = gtk_tree_view_get_columns(self->_treeview1);
	while(col){
		gtk_tree_view_remove_column(self->_treeview1, col->data);
		g_list_next(col);
	}
	g_list_free(columns);

	/* add the columns index + axes */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("index",
							   renderer,
							   "text", SOLUTION_COL_INDEX,
							   NULL);
	gtk_tree_view_append_column(self->_treeview1, column);
	for(i=0; i<HKL_LIST_LEN(self->geometry->axes); ++i){
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes (((HklParameter *)&self->geometry->axes[i])->name,
								   renderer,
								   "text", SOLUTION_N_COLUMNS + i,
								   NULL);
		gtk_tree_view_append_column(self->_treeview1, column);
	}

	/* Create the Model and fill it */
	/* FIXME remove in the destructor and when changing the diffractometer */
	types = g_new(GType, SOLUTION_N_COLUMNS + HKL_LIST_LEN(self->geometry->axes));
	types[0] = G_TYPE_INT;
	for(i=0; i<HKL_LIST_LEN(self->geometry->axes); ++i)
		types[SOLUTION_N_COLUMNS + i] = G_TYPE_DOUBLE;
	self->store_solutions = gtk_list_store_newv(SOLUTION_N_COLUMNS, types);
	g_free(types);

	gtk_tree_view_set_model(self->_treeview1, GTK_TREE_MODEL(self->store_solutions));

	g_signal_connect(G_OBJECT(self->_treeview1), "cursor-changed", 
			 G_CALLBACK(on_tree_view1_cursor_changed), 
			 self);

	hkl_gui_update_solutions(self);
}

void hkl_gui_set_up_tree_view_reflections(HklGuiWindow *self)
{
	LOG;

	GList *columns, *col;
	GtkTreeViewColumn *column;
	GtkCellRenderer * renderer;

	g_return_if_fail(self);

	/* first remove all columns of the tree view */
	columns = col = gtk_tree_view_get_columns(self->_treeview_reflections);
	while(col){
		gtk_tree_view_remove_column(self->_treeview_reflections, col->data);
		g_list_next(col);
	}
	g_list_free(columns);

	/* add the columns */
	/* index */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("index",
							   renderer,
							   "text", REFLECTION_COL_INDEX,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_reflections, column);

	/* h */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_reflections_h_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("h",
							   renderer,
							   "text", REFLECTION_COL_H,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_reflections, column);

	/* k */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_reflections_k_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("k",
							   renderer,
							   "text", REFLECTION_COL_K,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_reflections, column);

	/* l */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_reflections_h_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("l",
							   renderer,
							   "text", REFLECTION_COL_L,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_reflections, column);

	/* flag */
	renderer = gtk_cell_renderer_toggle_new ();
	g_signal_connect(G_OBJECT(renderer), "toggled", 
			 G_CALLBACK(on_cell_tree_view_reflections_flag_toggled), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("flag",
							   renderer,
							   "text", REFLECTION_COL_FLAG,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_reflections, column);


	gtk_tree_selection_set_mode(gtk_tree_view_get_selection(self->_treeview_reflections),
				    GTK_SELECTION_MULTIPLE);
}

void hkl_gui_set_up_tree_view_crystals(HklGuiWindow *self)
{
	LOG;

	GList *columns, *col;
	GtkTreeViewColumn *column;
	GtkCellRenderer * renderer;

	g_return_if_fail(self);

	/* first remove all columns of the tree view */
	columns = col = gtk_tree_view_get_columns(self->_treeview_crystals);
	while(col){
		gtk_tree_view_remove_column(self->_treeview_crystals, col->data);
		g_list_next(col);
	}
	g_list_free(columns);

	/* add the columns */
	/* name */
	renderer = gtk_cell_renderer_text_new ();
	g_signal_connect(G_OBJECT(renderer), "edited", 
			 G_CALLBACK(on_cell_tree_view_crystals_name_edited), 
			 self);
	column = gtk_tree_view_column_new_with_attributes ("name",
							   renderer,
							   "text", SAMPLE_COL_NAME,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	/* a */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("a",
							   renderer,
							   "text", SAMPLE_COL_A,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	/* b */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("b",
							   renderer,
							   "text", SAMPLE_COL_B,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	/* c */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("c",
							   renderer,
							   "text", SAMPLE_COL_C,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	/* alpha */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("alpha",
							   renderer,
							   "text", SAMPLE_COL_ALPHA,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	/* beta */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("beta",
							   renderer,
							   "text", SAMPLE_COL_BETA,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	/* gamma */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("gamma",
							   renderer,
							   "text", SAMPLE_COL_GAMMA,
							   NULL);
	gtk_tree_view_append_column(self->_treeview_crystals, column);

	gtk_tree_selection_set_mode(gtk_tree_view_get_selection(self->_treeview_crystals),
				    GTK_SELECTION_MULTIPLE);
}

void hkl_gui_update_source(HklGuiWindow *self)
{
	LOG;

	g_return_if_fail(self);

	if(self->geometry){
		double lambda = hkl_source_get_wavelength(&self->geometry->source);
		gtk_spin_button_set_value(self->_spinbutton_lambda, lambda);
	}
}

void hkl_gui_update_axes(HklGuiWindow *self)
{
	HklAxis *axis;
	double min;
	double max;
	gboolean valid;
	GtkTreeIter iter;

	LOG;

	// update the model
	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(self->store_axis), &iter);
	while(valid){
		gtk_tree_model_get(GTK_TREE_MODEL(self->store_axis), &iter,
				   AXIS_COL_AXIS, &axis,
				   -1);
		hkl_parameter_get_range_unit((HklParameter *)axis, &min, &max);
		gtk_list_store_set(self->store_axis, &iter,
				   AXIS_COL_READ, hkl_axis_get_value_unit(axis),
				   AXIS_COL_WRITE, hkl_axis_get_value_unit(axis),
				   AXIS_COL_MIN, min,
				   AXIS_COL_MAX, max,
				   -1);
		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(self->store_axis), &iter);
	}
}

void hkl_gui_update_pseudo_axes(HklGuiWindow *self)
{
	double min;
	double max;
	HklParameter *parameter;
	gboolean valid;
	GtkTreeIter iter;

	LOG;

	// first compute all the pseudoAxes values
	hkl_pseudo_axis_engine_list_get(self->engines);

	// update the model
	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(self->store_pseudo_axis), &iter);
	while(valid){
		gtk_tree_model_get(GTK_TREE_MODEL(self->store_pseudo_axis), &iter,
				   PSEUDOAXIS_COL_PSEUDOAXIS, &parameter,
				   -1);
		hkl_parameter_get_range_unit((HklParameter *)parameter, &min, &max);
		gtk_list_store_set(self->store_pseudo_axis, &iter,
				   PSEUDOAXIS_COL_READ, hkl_parameter_get_value_unit(parameter),
				   PSEUDOAXIS_COL_WRITE, hkl_parameter_get_value_unit(parameter),
				   PSEUDOAXIS_COL_MIN, min,
				   PSEUDOAXIS_COL_MAX, max,
				   PSEUDOAXIS_COL_INITIALIZED, TRUE,
				   -1);
		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(self->store_pseudo_axis), &iter);
	}
}

void hkl_gui_update_pseudo_axes_parameters(HklGuiWindow *self)
{
	GHashTableIter iter;
	gpointer key, value;

	LOG;

	g_hash_table_iter_init (&iter, self->hash_store_pseudo_axis_parameter);
	while (g_hash_table_iter_next (&iter, &key, &value)) /* key = pseudo_axis, value = model */
	{
		GtkListStore *model;
		GtkTreeIter iter2;
		gboolean valid;

		model = value;
		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(model), &iter2);
		while(valid){
			HklParameter *parameter;

			gtk_tree_model_get(GTK_TREE_MODEL(model), &iter2,
					   PARAMETER_COL_PARAMETER, &parameter,
					   -1);
			gtk_list_store_set(model, &iter2,
					   PARAMETER_COL_NAME, parameter->name,
					   PARAMETER_COL_VALUE, hkl_parameter_get_value_unit(parameter),
					   -1);
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(model), &iter2);
		}
	}
}

void hkl_gui_update_lattice(HklGuiWindow *self)
{
	LOG;

	HklSample *sample = self->samples->current;
	if(sample){
		gtk_spin_button_set_value(self->_spinbutton_a, 
					  hkl_parameter_get_value_unit(sample->lattice->a));
		gtk_spin_button_set_value(self->_spinbutton_b,
					  hkl_parameter_get_value_unit(sample->lattice->b));
		gtk_spin_button_set_value(self->_spinbutton_c,
					  hkl_parameter_get_value_unit(sample->lattice->c));
		gtk_spin_button_set_value(self->_spinbutton_alpha,
					  hkl_parameter_get_value_unit(sample->lattice->alpha));
		gtk_spin_button_set_value(self->_spinbutton_beta,
					  hkl_parameter_get_value_unit(sample->lattice->beta));
		gtk_spin_button_set_value(self->_spinbutton_gamma,
					  hkl_parameter_get_value_unit(sample->lattice->gamma));
	}
}

void hkl_gui_update_lattice_parameters(HklGuiWindow *self)
{
	LOG;

	HklSample *sample = self->samples->current;
	if(sample){
		double min;
		double max;
		gboolean to_fit;
		HklParameter *parameter;


		parameter = sample->lattice->a;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		gtk_spin_button_set_value(self->_spinbutton_a_min, min);
		gtk_spin_button_set_value(self->_spinbutton_a_max, max);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_a),
					     parameter->fit);

		parameter = sample->lattice->b;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		gtk_spin_button_set_value(self->_spinbutton_b_min, min);
		gtk_spin_button_set_value(self->_spinbutton_b_max, max);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_b),
					     parameter->fit);

		parameter = sample->lattice->c;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		gtk_spin_button_set_value(self->_spinbutton_c_min, min);
		gtk_spin_button_set_value(self->_spinbutton_c_max, max);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_c),
					     parameter->fit);

		parameter = sample->lattice->alpha;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		gtk_spin_button_set_value(self->_spinbutton_alpha_min, min);
		gtk_spin_button_set_value(self->_spinbutton_alpha_max, max);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_alpha),
					     parameter->fit);

		parameter = sample->lattice->beta;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		gtk_spin_button_set_value(self->_spinbutton_beta_min, min);
		gtk_spin_button_set_value(self->_spinbutton_beta_max, max);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_beta),
					     parameter->fit);

		parameter = sample->lattice->gamma;
		hkl_parameter_get_range_unit(parameter, &min, &max);
		gtk_spin_button_set_value(self->_spinbutton_gamma_min, min);
		gtk_spin_button_set_value(self->_spinbutton_gamma_max, max);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_gamma),
					     parameter->fit);
	}
}

void hkl_gui_update_reciprocal_lattice(HklGuiWindow *self)
{
	LOG;

	HklSample *sample = self->samples->current;
	if(sample){
		hkl_lattice_reciprocal(sample->lattice, self->reciprocal);

		gtk_spin_button_set_value(self->_spinbutton_a_star,
					  hkl_parameter_get_value_unit(self->reciprocal->a));
		gtk_spin_button_set_value(self->_spinbutton_b_star,
					  hkl_parameter_get_value_unit(self->reciprocal->b));
		gtk_spin_button_set_value(self->_spinbutton_c_star,
					  hkl_parameter_get_value_unit(self->reciprocal->c));
		gtk_spin_button_set_value(self->_spinbutton_alpha_star,
					  hkl_parameter_get_value_unit(self->reciprocal->alpha));
		gtk_spin_button_set_value(self->_spinbutton_beta_star,
					  hkl_parameter_get_value_unit(self->reciprocal->beta));
		gtk_spin_button_set_value(self->_spinbutton_gamma_star,
					  hkl_parameter_get_value_unit(self->reciprocal->gamma));
	}
}

void hkl_gui_update_UB(HklGuiWindow *self)
{
	LOG;

	HklSample *sample = self->samples->current;
	if(sample){
		static const char *format = "%f";
		char tmp[100];
		HklMatrix UB;

		hkl_sample_get_UB(sample, &UB);
		sprintf(tmp, format, UB.data[0][0]);
		gtk_label_set_text(self->_label_UB11, tmp);
		sprintf(tmp, format, UB.data[0][1]);
		gtk_label_set_text(self->_label_UB12, tmp);
		sprintf(tmp, format, UB.data[0][2]);
		gtk_label_set_text(self->_label_UB13, tmp);
		sprintf(tmp, format, UB.data[1][0]);
		gtk_label_set_text(self->_label_UB21, tmp);
		sprintf(tmp, format, UB.data[1][1]);
		gtk_label_set_text(self->_label_UB22, tmp);
		sprintf(tmp, format, UB.data[1][2]);
		gtk_label_set_text(self->_label_UB23, tmp);
		sprintf(tmp, format, UB.data[2][0]);
		gtk_label_set_text(self->_label_UB31, tmp);
		sprintf(tmp, format, UB.data[2][1]);
		gtk_label_set_text(self->_label_UB32, tmp);
		sprintf(tmp, format, UB.data[2][2]);
		gtk_label_set_text(self->_label_UB33, tmp);
	}
}

void hkl_gui_update_UxUyUz(HklGuiWindow *self)
{
	LOG;

	HklSample *sample = self->samples->current;
	if(sample){
		gtk_spin_button_set_value(self->_spinbutton_ux, hkl_parameter_get_value_unit(sample->ux));
		gtk_spin_button_set_value(self->_spinbutton_uy, hkl_parameter_get_value_unit(sample->uy));
		gtk_spin_button_set_value(self->_spinbutton_uz, hkl_parameter_get_value_unit(sample->uz));
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_Ux), sample->ux->fit);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_Uy), sample->uy->fit);
		gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(self->_checkbutton_Uz), sample->uz->fit);
	}
}

static void _hkl_gui_update_reflections(const HklSample *sample, GtkListStore *model)
{
	LOG;

	size_t i;

	gtk_list_store_clear(model);
	for(i=0; i<HKL_LIST_LEN(sample->reflections); ++i){
		GtkTreeIter iter;
		HklSampleReflection *reflection = sample->reflections[i];

		gtk_list_store_append(model, &iter);
		gtk_list_store_set(model, &iter,
				   REFLECTION_COL_INDEX, i,
				   REFLECTION_COL_H, reflection->hkl.data[0],
				   REFLECTION_COL_K, reflection->hkl.data[1],
				   REFLECTION_COL_L, reflection->hkl.data[2],
				   REFLECTION_COL_FLAG, reflection->flag,
				   -1);
	}
}

void hkl_gui_update_tree_view_crystals(HklGuiWindow *self)
{
	LOG;

	size_t i;
	HklSample *sample;
	GtkListStore *reflections;
	GtkListStore *reflections_current;
	GtkTreeIter iter;
	GtkTreeIter iter_current;
	gboolean valid;
	const char *current_crystal_name;
	gboolean is_current_crystal_set = FALSE;

	/* clear all data in the sample model */
	if (self->store_samples){
		/* first the reflections models */
		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(self->store_samples), &iter);
		while(valid){
			gtk_tree_model_get(GTK_TREE_MODEL(self->store_samples), &iter,
					   SAMPLE_COL_REFLECTIONS, &reflections,
					   -1);
			g_object_unref(reflections);
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(self->store_samples), &iter);
		}
		g_object_unref(self->store_samples);
	}

	self->store_samples = gtk_list_store_new(SAMPLE_N_COLUMNS,
						 G_TYPE_POINTER, G_TYPE_POINTER,
						 G_TYPE_STRING, G_TYPE_DOUBLE,
						 G_TYPE_DOUBLE, G_TYPE_DOUBLE,
						 G_TYPE_DOUBLE, G_TYPE_DOUBLE,
						 G_TYPE_DOUBLE);

	if(self->samples->current){
		is_current_crystal_set = TRUE;
		current_crystal_name = self->samples->current->name;
	}

	/* Fill the models from the crystalList */
	for(i=0; i<HKL_LIST_LEN(self->samples->samples); ++i){
		HklLattice *lattice;

		sample = self->samples->samples[i];
		lattice = sample->lattice;

		/* create the attached reflections model */
		reflections = gtk_list_store_new(REFLECTION_N_COLUMNS,
						 G_TYPE_INT,
						 G_TYPE_DOUBLE,
						 G_TYPE_DOUBLE,
						 G_TYPE_DOUBLE,
						 G_TYPE_BOOLEAN);
		_hkl_gui_update_reflections(sample, reflections);

		gtk_list_store_append(self->store_samples, &iter);
		gtk_list_store_set(self->store_samples, &iter,
				   SAMPLE_COL_SAMPLE, sample,
				   SAMPLE_COL_REFLECTIONS, reflections,
				   SAMPLE_COL_NAME,sample->name,
				   SAMPLE_COL_A, hkl_parameter_get_value_unit(lattice->a),
				   SAMPLE_COL_B, hkl_parameter_get_value_unit(lattice->b),
				   SAMPLE_COL_C, hkl_parameter_get_value_unit(lattice->c),
				   SAMPLE_COL_ALPHA, hkl_parameter_get_value_unit(lattice->alpha),
				   SAMPLE_COL_BETA, hkl_parameter_get_value_unit(lattice->beta),
				   SAMPLE_COL_GAMMA, hkl_parameter_get_value_unit(lattice->gamma),
				   -1);

		/* save a few parameters if it is the current sample */
		if (is_current_crystal_set && current_crystal_name == sample->name){
			iter_current = iter;
			reflections_current = reflections;
		}
	}

	/* Set the model for the TreeView */
	gtk_tree_view_set_model(self->_treeview_crystals,
				GTK_TREE_MODEL(self->store_samples));
	if (is_current_crystal_set){
		GtkTreePath *path;

		path = gtk_tree_model_get_path(GTK_TREE_MODEL(self->store_samples), &iter_current);
		gtk_tree_view_set_cursor(self->_treeview_crystals, path, NULL, FALSE);
		gtk_tree_view_set_model(self->_treeview_reflections, GTK_TREE_MODEL(reflections_current));
		gtk_tree_path_free(path);
	}
}

void hkl_gui_update_reflections(HklGuiWindow *self, const HklSample *sample)
{
	gboolean valid;
	GtkTreeIter iter;

	LOG;

	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(self->store_samples), &iter);
	while(valid){
		const HklSample *sample_iter;
		GtkListStore *model;

		gtk_tree_model_get(GTK_TREE_MODEL(self->store_samples), &iter,
				   SAMPLE_COL_SAMPLE, &sample_iter,
				   SAMPLE_COL_REFLECTIONS, &model,
				   -1);
		if (sample == sample_iter){
			_hkl_gui_update_reflections(sample, model);
			break;
		}
		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(self->store_samples), &iter);
	}
}

void hkl_gui_update_status_bar(HklGuiWindow *self, const HklError *error)
{
	LOG;

	gtk_statusbar_push(self->_statusBar, 0, error->message);
}

void hkl_gui_update_crystal_model(HklGuiWindow *self, HklSample * sample)
{
	GtkTreeIter iter;
	gboolean valid;

	LOG;

	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(self->store_samples), &iter);
	while(valid){
		const char *name;

		gtk_tree_model_get(GTK_TREE_MODEL(self->store_samples), &iter,
				   SAMPLE_COL_NAME, &name,
				   -1);
		if (!strcmp(name, sample->name)){
			HklLattice *lattice;
				
			lattice = sample->lattice;
			gtk_list_store_set(self->store_samples, &iter,
					   SAMPLE_COL_A, hkl_parameter_get_value_unit(lattice->a),
					   SAMPLE_COL_B, hkl_parameter_get_value_unit(lattice->b),
					   SAMPLE_COL_C, hkl_parameter_get_value_unit(lattice->c),
					   SAMPLE_COL_ALPHA, hkl_parameter_get_value_unit(lattice->alpha),
					   SAMPLE_COL_BETA, hkl_parameter_get_value_unit(lattice->beta),
					   SAMPLE_COL_GAMMA, hkl_parameter_get_value_unit(lattice->gamma),
					   -1);
			break;
		}
		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(self->store_samples), &iter);
	}
}

void hkl_gui_update_pseudo_axes_frames(HklGuiWindow *self)
{
	LOG;

	size_t i;

	for(i=0; i<self->pseudoAxesFrames_len; ++i)
		hkl_gui_pseudo_axes_frame_update(self->pseudoAxesFrames[i]);
}

void hkl_gui_update_solutions(HklGuiWindow *self)
{
	LOG;

	size_t i;

	gtk_list_store_clear(self->store_solutions);
	for(i=0; i<hkl_geometry_list_len(self->engines->geometries); ++i){
		size_t j;
		HklGeometry *geometry;
		GtkTreeIter iter;

		geometry = self->engines->geometries->items[i]->geometry;

		gtk_list_store_append(self->store_solutions, &iter);
		gtk_list_store_set(self->store_solutions, &iter,
				   SOLUTION_COL_INDEX, i,
				   -1);
		for(j=0; j<HKL_LIST_LEN(geometry->axes); ++j)
			gtk_list_store_set(self->store_solutions, &iter,
					   SOLUTION_N_COLUMNS + j, hkl_parameter_get_value_unit((HklParameter *)&geometry->axes[j]),
					   -1);
	}
}
