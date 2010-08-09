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
#include "pseudoaxesframe.h"

enum
{
	MODE_COL_NAME = 0,
	MODE_NUM_COLS
};

enum
{
	PSEUDO_COL_NAME = 0,
	PSEUDO_COL_VALUE,
	PSEUDO_COL_PSEUDO,
	PSEUDO_NUM_COLS
};

/****************/
/* Non-Callback */
/****************/

static void update_pseudo_axis(HklGuiPseudoAxesFrame *self)
{
	size_t i;
	GtkTreeIter iter;

	gtk_list_store_clear(self->store_pseudo);
	for(i=0; i<HKL_LIST_LEN(self->engine->pseudoAxes); ++i){
		gtk_list_store_append(self->store_pseudo, &iter);

		gtk_list_store_set(self->store_pseudo, &iter,
				   PSEUDO_COL_NAME, ((HklParameter *)self->engine->pseudoAxes[i])->name,
				   PSEUDO_COL_VALUE, hkl_parameter_get_value_unit((HklParameter *)self->engine->pseudoAxes[i]),
				   PSEUDO_COL_PSEUDO, self->engine->pseudoAxes[i],
				   -1);
	}
}

static void update_mode(HklGuiPseudoAxesFrame *self)
{
	size_t i;
	GtkTreeIter iter;

	gtk_list_store_clear(self->store_mode);
	for(i=0; i<HKL_LIST_LEN(self->engine->modes); ++i){
		gtk_list_store_append(self->store_mode, &iter);
		gtk_list_store_set(self->store_mode, &iter,
				   MODE_COL_NAME, self->engine->modes[i]->name,
				   -1);
	}
}

static void update_mode_parameters(HklGuiPseudoAxesFrame *self)
{
	if(self->engine->mode){
		size_t len = HKL_LIST_LEN(self->engine->mode->parameters);
		if(len){
			size_t i;
			GtkTreeIter iter;
			HklParameter *parameters;

			gtk_list_store_clear(self->store_mode_parameter);
			parameters = self->engine->mode->parameters;
			for(i=0; i<len; ++i){
				gtk_list_store_append(self->store_mode_parameter, &iter);
				gtk_list_store_set(self->store_mode_parameter, &iter,
						   PSEUDO_COL_NAME, parameters[i].name,
						   PSEUDO_COL_VALUE, hkl_parameter_get_value_unit(&parameters[i]),
						   -1);
			}
			gtk_expander_set_expanded(self->expander1, 1);
			gtk_widget_show((GtkWidget *)self->expander1);
		}else
			gtk_widget_hide((GtkWidget *)self->expander1);
	}else
		gtk_widget_hide((GtkWidget *)self->expander1);
}

/************/
/* Callback */
/************/

static void on_combobox1_changed(GtkComboBox *combobox, gpointer user_data)
{
	HklGuiPseudoAxesFrame *self;
	size_t idx;

	self = (HklGuiPseudoAxesFrame *)user_data;
	idx = gtk_combobox_get_active_row_number(combobox);
	if(idx < HKL_LIST_LEN(self->engine->modes)){
		hkl_pseudo_axis_engine_select_mode(self->engine, idx);
		updateModeParameters(self);
	}
}

static void on_button1_clicked(GtkButton *button, gpointer user_data)
{
	HklGuiPseudoAxesFrame *self;

	self = (HklGuiPseudoAxesFrame *)user_data;
	if(hkl_pseudo_axis_engine_set(self->engine, NULL) == HKL_SUCCESS){
		hkl_geometry_init_geometry(self->engine->engines->geometry,
					   self->engine->engines->geometries->items[0]->geometry);
//		this->_signal_changed();
	}
}

static void on_button2_clicked(GtkButton *button, gpointer user_data)
{
	HklGuiPseudoAxesFrame *self;

	self = (HklGuiPseudoAxesFrame *)user_data;
	if(hkl_pseudo_axis_engine_initialize(self->engine, NULL) == HKL_SUCCESS){
		updateModeParameters(self); //some initialize function modify the parameters
		hkl_pseudo_axis_engine_fprintf(stdout, self->engine);
	}
}

void on_cell_tree_view_pseudo_axis_value_edited(GtkCellRendererText *renderer,
						gchar *path, gchar *new_text,
						gpointer *user_data)
{
	GtkTreeIter iter;
	GtkListStore *model;


	model = ((HklGuiPseudoAxesFrame *)user_data)->store_pseudo;
	if (gtk_tree_model_get_iter_from_string((GtkTreeModel *)model, &iter, path)){
		double value;
		HklPseudoAxis *pseudo;

		sscanf(new_text, "%lf", &value);
		gtk_tree_model_get((GtkTreeModel *)model, &iter, PSEUDO_COL_PSEUDO, &pseudo, -1);
		if(pseudo){
			g_object_set(renderer, "background", "red", NULL);
			hkl_parameter_set_value_unit((HklParameter *)pseudo, value);
			gtk_list_store_set(model, &iter, PSEUDO_COL_VALUE, value, -1);
		}
	}
}

HklGuiPseudoAxesFrame *hkl_gui_pseudo_axes_frame_new(HklPseudoAxisEngine *engine)
{
	HklGuiPseudoAxesFrame *self;	
	GtkBuilder *builder = NULL;

	self = HKL_MALLOC(HklGuiPseudoAxesFrame);

	self->engine = engine;
	builder = gtk_builder_new();
	if(builder){
		GError *error = NULL;

		gtk_builder_add_from_file(builder, "pseudo.ui", &error);
		if(error == NULL){
			GtkCellRenderer *renderer;

			// extract widgets from the builder
			self->frame1 = (GtkFrame *)gtk_builder_get_object(builder, "frame1");
			self->label2 = (GtkLabel *)gtk_builder_get_object(builder, "label2");
			self->combobox1 = (GtkComboBox *)gtk_builder_get_object(builder, "combobox1");
			self->expander1 = (GtkExpander *)gtk_builder_get_object(builder, "expander1");
			self->treeview1 = (GtkTreeView *)gtk_builder_get_object(builder, "treeview1");
			self->button1 = (GtkButton *)gtk_builder_get_object(builder, "button1");
			self->button2 = (GtkButton *)gtk_builder_get_object(builder, "button2");
			self->store_mode = (GtkListStore *)gtk_builder_get_object(builder, "liststore1");
			self->store_pseudo = (GtkListStore *)gtk_builder_get_object(builder, "liststore2");
			self->store_mode_parameter = (GtkListStore *)gtk_builder_get_object(builder, "liststore3");

			// title
			gtk_label_set_label(self->label2, self->engine->name);

			// update all the liststore
			update_pseudo_axis(self);
			update_mode(self);
			update_mode_parameters(self);

			gtk_builder_connect_signals(builder, self);

			// extra connect signals
			GList *cells = NULL;
			GtkTreeViewColumn *col = NULL;

			col = gtk_tree_view_get_column(self->treeview1, 1);
			cells = gtk_cell_layout_get_cells((GtkCellLayout *)col);
			g_signal_connect(G_OBJECT(cells->data), "edited", 
					 G_CALLBACK(on_cell_tree_view_pseudo_axis_value_edited), 
					 self);
		}else{
			/* Affichage du message d'erreur de GTK+ */
			g_error ("%s", error->message);
			g_error_free (error);
		}
	}
	return self;
}

void hkl_gui_pseudo_axes_frame_free(HklGuiPseudoAxesFrame *self)
{
	free(self);
}

void hkl_gui_pseudo_axes_frame_update(HklGuiPseudoAxesFrame *self)
{
	GList *cells = NULL;
	GtkTreeViewColumn *col = NULL;
	GtkCellRendererText *renderer = NULL;

	update_pseudo_axis(self);

	col = gtk_tree_view_get_column(self->treeview1, 1);
	cells = gtk_cell_layout_get_cells((GtkCellLayout *)col);
	g_object_set(cells->data, "background", NULL);
}
