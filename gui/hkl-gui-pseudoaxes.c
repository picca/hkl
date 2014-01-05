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

#include "hkl-gui-pseudoaxes.h"
#include "hkl.h"
#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>

enum {
  PROP_0,
  PROP_ENGINE,
};

struct _HklGuiEnginePrivate {
	/* Properties */
	HklEngine* engine;
	/* Properties */

	GtkBuilder *builder;
	GtkFrame* frame1;
	GtkLabel* label2;
	GtkComboBox* combobox1;
	GtkExpander* expander1;
	GtkTreeView* treeview1;
	GtkButton* button1;
	GtkButton* button2;
	GtkListStore* store_mode;
	GtkListStore* store_pseudo;
	GtkListStore* store_mode_parameter;
};

static void hkl_gui_engine_set_property (GObject      *object,
					 guint         param_id,
					 const GValue *value,
					 GParamSpec   *pspec);
static void hkl_gui_engine_get_property (GObject     *object,
					 guint        param_id,
					 GValue      *value,
					 GParamSpec  *pspec);

static void hkl_gui_engine_finalize (GObject* obj);

static void hkl_gui_engine_update_pseudo_axis (HklGuiEngine* self);

static void hkl_gui_engine_update_mode (HklGuiEngine* self);

static void hkl_gui_engine_update_mode_parameters (HklGuiEngine* self);

static void hkl_gui_engine_on_combobox1_changed (GtkComboBox* combobox, HklGuiEngine* self);

static void hkl_gui_engine_on_button1_clicked (GtkButton* button, HklGuiEngine* self);

static void hkl_gui_engine_on_button2_clicked (GtkButton* button, HklGuiEngine* self);

static void hkl_gui_engine_on_cell_tree_view_pseudo_axis_value_edited (GtkCellRendererText* renderer,
								       const gchar* path,
								       const gchar* new_text,
								       HklGuiEngine* self);

G_DEFINE_TYPE (HklGuiEngine, hkl_gui_engine, G_TYPE_OBJECT);

static void
hkl_gui_engine_class_init (HklGuiEngineClass * class)
{
	GObjectClass *gobject_class;
	
	gobject_class = (GObjectClass *) class;

	gobject_class->finalize = hkl_gui_engine_finalize;

	gobject_class->set_property = hkl_gui_engine_set_property;
	gobject_class->get_property = hkl_gui_engine_get_property;

	g_object_class_install_property (gobject_class,
					 PROP_ENGINE,
					 g_param_spec_pointer ("engine",
							       "Engine",
							       "The Hkl Engine used underneath",
							       G_PARAM_READABLE |
							       G_PARAM_WRITABLE));

	g_signal_new ("changed",
		      HKL_GUI_TYPE_ENGINE,
		      G_SIGNAL_RUN_LAST,
		      0, /* class offset */
		      NULL, /* accumulator */
		      NULL, /* accu_data */
		      g_cclosure_marshal_VOID__VOID,
		      G_TYPE_NONE, /* return_type */
		      0);

	g_type_class_add_private (class, sizeof (HklGuiEnginePrivate));
}

static void _connect_renderer(gpointer data, gpointer user_data)
{
	GtkCellRenderer *renderer = GTK_CELL_RENDERER(data);
	HklGuiEngine *self = HKL_GUI_ENGINE(user_data);
	
	g_signal_connect_object (renderer,
				 "edited",
				 (GCallback) hkl_gui_engine_on_cell_tree_view_pseudo_axis_value_edited,
				 self, 0);
}

static void hkl_gui_engine_init (HklGuiEngine * self)
{
	HklGuiEnginePrivate *priv;
	GtkBuilder *builder;
	GtkTreeViewColumn* col;
	GList* cells;

	self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self,
						  HKL_GUI_TYPE_ENGINE,
						  HklGuiEnginePrivate);
	priv = self->priv;

	priv->engine = NULL;
	priv->builder = builder = gtk_builder_new ();

	gtk_builder_add_from_file (builder, "pseudo.ui", NULL);

	priv->frame1 = GTK_FRAME(gtk_builder_get_object(builder, "frame1"));
	priv->label2 = GTK_LABEL(gtk_builder_get_object(builder, "label2"));
	priv->combobox1 = GTK_COMBO_BOX(gtk_builder_get_object(builder, "combobox1"));
	priv->expander1 = GTK_EXPANDER(gtk_builder_get_object(builder, "expander1"));
	priv->treeview1 = GTK_TREE_VIEW(gtk_builder_get_object(builder, "treeview1"));
	priv->button1 = GTK_BUTTON(gtk_builder_get_object(builder, "button1"));
	priv->button2 = GTK_BUTTON(gtk_builder_get_object(builder, "button2"));

	priv->store_mode = GTK_LIST_STORE(gtk_builder_get_object (builder, "liststore1"));
	priv->store_pseudo = GTK_LIST_STORE(gtk_builder_get_object (builder, "liststore2"));
	priv->store_mode_parameter = GTK_LIST_STORE(gtk_builder_get_object (builder, "liststore3"));

	gtk_builder_connect_signals (builder, self);

	g_signal_connect_object (self->priv->combobox1,
				 "changed",
				 (GCallback) hkl_gui_engine_on_combobox1_changed,
				 self, 0);

	g_signal_connect_object (self->priv->button1,
				 "clicked",
				 (GCallback) hkl_gui_engine_on_button1_clicked,
				 self, 0);

	g_signal_connect_object (self->priv->button2,
				 "clicked",
				 (GCallback) hkl_gui_engine_on_button2_clicked,
				 self, 0);

	col = gtk_tree_view_get_column (self->priv->treeview1, 1);
	cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(col));
	g_list_foreach(cells, _connect_renderer, self);
	g_list_free(cells);
}

static void hkl_gui_engine_set_property (GObject         *object,
					 guint            prop_id,
					 const GValue    *value,
					 GParamSpec      *pspec)
{
	HklGuiEngine *self = HKL_GUI_ENGINE (object);
	HklGuiEnginePrivate *priv = self->priv;
	
	switch (prop_id) {
	case PROP_ENGINE:
		hkl_gui_engine_set_engine(self, g_value_get_pointer (value));
		break;
	default:      
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void  hkl_gui_engine_get_property (GObject         *object,
					  guint            prop_id,
					  GValue          *value,
					  GParamSpec      *pspec)
{
	HklGuiEngine *self = HKL_GUI_ENGINE (object);
	HklGuiEnginePrivate *priv = self->priv;

	switch (prop_id)
	{
	case PROP_ENGINE:
		g_value_set_pointer (value, hkl_gui_engine_get_engine (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void hkl_gui_engine_finalize (GObject* object)
{
	HklGuiEngine * self = HKL_GUI_ENGINE(object);
	HklGuiEnginePrivate *priv = self->priv;

	g_object_unref(priv->builder);

	G_OBJECT_CLASS (hkl_gui_engine_parent_class)->finalize (object);
}


HklGuiEngine* hkl_gui_engine_new (HklEngine* engine)
{
	return g_object_new (HKL_GUI_TYPE_ENGINE,
			     "engine", engine,
			     NULL);
}


void hkl_gui_engine_set_engine (HklGuiEngine *self,
				HklEngine *engine)
{
	HklGuiEnginePrivate *priv;

	g_return_if_fail (self != NULL);
	g_return_if_fail (engine != NULL);
	
	priv = self->priv;

	priv->engine = engine;

	gtk_label_set_label (priv->label2,
			     hkl_engine_name(priv->engine));

	hkl_gui_engine_update_pseudo_axis (self);
	hkl_gui_engine_update_mode (self);
	hkl_gui_engine_update_mode_parameters (self);

	hkl_gui_engine_update(self);
}


HklEngine* hkl_gui_engine_get_engine (HklGuiEngine *self)
{
	HklGuiEnginePrivate *priv;

	priv = self->priv;

	return priv->engine;
}


GtkFrame *hkl_gui_engine_get_frame(HklGuiEngine *self)
{
	HklGuiEnginePrivate *priv;

	priv = self->priv;

	return priv->frame1;
}

void hkl_gui_engine_update (HklGuiEngine* self)
{
	GtkTreeViewColumn* col;
	GList* cells;

	g_return_if_fail (self != NULL);

	hkl_gui_engine_update_pseudo_axis (self);

	col = gtk_tree_view_get_column(self->priv->treeview1, 1);
	cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(col));
	g_object_set (G_OBJECT(cells->data), "background", NULL, NULL);
	g_list_free(cells);
}


static void hkl_gui_engine_update_pseudo_axis (HklGuiEngine* self) {
	GtkTreeIter iter = {0};
	darray_parameter *parameters;
	HklParameter **parameter;

	g_return_if_fail (self != NULL);

	gtk_list_store_clear (self->priv->store_pseudo);
	parameters = hkl_engine_pseudo_axes (self->priv->engine);
	darray_foreach(parameter, *parameters){
		gtk_list_store_append (self->priv->store_pseudo,
				       &iter);
		gtk_list_store_set (self->priv->store_pseudo,
				    &iter,
				    PSEUDO_COL_NAME, hkl_parameter_name_get (*parameter),
				    PSEUDO_COL_VALUE, hkl_parameter_value_unit_get (*parameter),
				    PSEUDO_COL_PSEUDO, *parameter,
				    -1);
	}
}


static void hkl_gui_engine_update_mode (HklGuiEngine* self) {
	GtkTreeIter iter = {0};
	darray_mode *modes;
	HklMode **mode;

	g_return_if_fail (self != NULL);

	modes = hkl_engine_modes(self->priv->engine);
	gtk_list_store_clear (self->priv->store_mode);
	darray_foreach(mode, *modes){
		gtk_list_store_append (self->priv->store_mode,
				       &iter);
		gtk_list_store_set (self->priv->store_mode,
				    &iter,
				    MODE_COL_NAME, hkl_mode_name(*mode),
				    -1);
	}
}


static void hkl_gui_engine_update_mode_parameters (HklGuiEngine* self) {
	HklMode *mode;

	g_return_if_fail (self != NULL);

	mode = hkl_engine_mode(self->priv->engine);
	if (mode){
		darray_parameter *parameters;
		HklParameter **parameter;

		parameters = hkl_mode_parameters(mode);
		if(darray_size(*parameters)){
			GtkTreeIter iter = {0};

			gtk_list_store_clear (self->priv->store_mode_parameter);
			darray_foreach(parameter, *parameters){
				gtk_list_store_append (self->priv->store_mode_parameter, &iter);
				gtk_list_store_set (self->priv->store_mode_parameter,
						    &iter,
						    PSEUDO_COL_NAME, hkl_parameter_name_get (*parameter),
						    PSEUDO_COL_VALUE, hkl_parameter_value_unit_get (*parameter),
						    -1);
			}
			gtk_expander_set_expanded (self->priv->expander1, TRUE);
			gtk_widget_show (GTK_WIDGET (self->priv->expander1));
		}else
			gtk_widget_hide (GTK_WIDGET (self->priv->expander1));
	}else
		gtk_widget_hide (GTK_WIDGET(self->priv->expander1));
}

/*************/
/* callbacks */
/*************/

static void hkl_gui_engine_on_combobox1_changed (GtkComboBox* combobox, HklGuiEngine* self)
{
	gchar *mode;
	GtkTreeIter iter = {0};

	g_return_if_fail (self != NULL);
	g_return_if_fail (combobox != NULL);

	if(gtk_combo_box_get_active_iter(combobox, &iter)){
		gtk_tree_model_get(GTK_TREE_MODEL(self->priv->store_mode),
				   &iter,
				   MODE_COL_NAME, &mode,
				   -1);
		hkl_engine_select_mode_by_name(self->priv->engine, mode);
		hkl_gui_engine_update_mode_parameters(self);
	}
}


static void hkl_gui_engine_on_button1_clicked (GtkButton* button, HklGuiEngine* self)
{
	g_return_if_fail (self != NULL);
	g_return_if_fail (button != NULL);

	if(hkl_engine_set(self->priv->engine, NULL)){
	        HklEngineList *engines;

		engines = hkl_engine_engines(self->priv->engine);
		hkl_engine_list_select_solution(engines, 0);
	}

	g_signal_emit_by_name (self, "changed");
}


static void hkl_gui_engine_on_button2_clicked (GtkButton* button, HklGuiEngine* self)
{
	g_return_if_fail (self != NULL);
	g_return_if_fail (button != NULL);

	if(hkl_engine_initialize(self->priv->engine, NULL)){
		/* some init method update the parameters */
		hkl_gui_engine_update_mode_parameters(self);
	}
}


static void hkl_gui_engine_on_cell_tree_view_pseudo_axis_value_edited (GtkCellRendererText* renderer,
								       const gchar* path,
								       const gchar* new_text,
								       HklGuiEngine* self)
{
	GtkTreeIter iter = {0};
	GtkListStore* model = NULL;

	g_return_if_fail (self != NULL);
	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);

	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(self->priv->store_pseudo),
						 &iter, path)) {
		gdouble value = 0.0;
		HklPseudoAxis* pseudo = NULL;

		value = atof(new_text);
		gtk_tree_model_get (GTK_TREE_MODEL(self->priv->store_pseudo),
				    &iter,
				    PSEUDO_COL_PSEUDO, &pseudo, -1);

		if (pseudo) {
			g_object_set (G_OBJECT(renderer), "background", "red", NULL, NULL);
			gtk_list_store_set (self->priv->store_pseudo,
					    &iter,
					    PSEUDO_COL_VALUE, value,
					    -1);
		}
	}
}
