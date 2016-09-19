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

#include "hkl-gui-macros.h"
#include "hkl-gui-pseudoaxes.h"
#include "hkl.h"
#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>

G_DEFINE_TYPE (HklGuiEngine, hkl_gui_engine, G_TYPE_OBJECT);

enum {
	PROP_0,

	PROP_ENGINE,
	PROP_LISTSTORE,

	N_PROPERTIES
};

/* Keep a pointer to the properties definition */
static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };

enum {
	CHANGED,

	N_SIGNALS
};

static guint signals[N_SIGNALS] = { 0 };

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

#define HKL_GUI_ENGINE_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HKL_GUI_TYPE_ENGINE, HklGuiEnginePrivate))

static void
set_property (GObject *object, guint prop_id,
	      const GValue *value, GParamSpec *pspec)
{
	HklGuiEngine *self = HKL_GUI_ENGINE (object);

	switch (prop_id) {
	case PROP_ENGINE:
		hkl_gui_engine_set_engine(self, g_value_get_pointer (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
get_property (GObject *object, guint prop_id,
	      GValue *value, GParamSpec *pspec)
{
	HklGuiEngine *self = HKL_GUI_ENGINE (object);

	switch (prop_id)
	{
	case PROP_ENGINE:
		g_value_set_pointer (value, hkl_gui_engine_get_engine (self));
		break;
	case PROP_LISTSTORE:
		g_value_set_object(value, hkl_gui_engine_get_liststore (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
finalize (GObject* object)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(object);

	g_object_unref(priv->builder);

	G_OBJECT_CLASS (hkl_gui_engine_parent_class)->finalize (object);
}


HklGuiEngine*
hkl_gui_engine_new (HklEngine* engine)
{
	return g_object_new (HKL_GUI_TYPE_ENGINE,
			     "engine", engine,
			     NULL);
}


HklEngine*
hkl_gui_engine_get_engine (HklGuiEngine *self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);

	return priv->engine;
}


GtkListStore*
hkl_gui_engine_get_liststore (HklGuiEngine *self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);

	return priv->store_pseudo;
}

GtkFrame*
hkl_gui_engine_get_frame(HklGuiEngine *self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);

	return priv->frame1;
}

static void
update_pseudo_axis (HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	GtkTreeIter iter = {0};
	const darray_string *parameters = hkl_engine_pseudo_axis_names_get (priv->engine);
	unsigned int n_values = darray_size(*parameters);
	double values[n_values];
	GError *error = NULL;
	unsigned int i;

	g_return_if_fail (self != NULL);

	gtk_list_store_clear (priv->store_pseudo);
	if(!hkl_engine_pseudo_axis_values_get(priv->engine, values, n_values, HKL_UNIT_USER, &error)){
		/* TODO check for the error */
		g_clear_error(&error);
		return;
	}

	for(i=0; i<n_values; ++i){
		gtk_list_store_append (priv->store_pseudo,
				       &iter);
		gtk_list_store_set (priv->store_pseudo,
				    &iter,
				    PSEUDO_COL_NAME, darray_item(*parameters, i),
				    PSEUDO_COL_IDX, i,
				    PSEUDO_COL_VALUE, values[i],
				    -1);
	}
}


static void
update_mode (HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	GtkTreeIter iter = {0};
	GtkTreeIter current = {0};
	const darray_string *modes;
	const char **mode;

	g_return_if_fail (self != NULL);

	modes = hkl_engine_modes_names_get(priv->engine);
	gtk_list_store_clear (priv->store_mode);
	darray_foreach(mode, *modes){
		gtk_list_store_append (priv->store_mode,
				       &iter);
		gtk_list_store_set (priv->store_mode,
				    &iter,
				    MODE_COL_NAME, *mode,
				    -1);
		if(*mode == hkl_engine_current_mode_get(priv->engine))
			current = iter;
	}

	/* now set the active row with the current mode */
	gtk_combo_box_set_active_iter(priv->combobox1, &current);
}


static void
update_mode_parameters (HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	const darray_string *parameters = hkl_engine_parameters_names_get(priv->engine);
	unsigned int n_values = darray_size(*parameters);
	double values[n_values];
	unsigned int i;

	if(n_values){
		GtkTreeIter iter = {0};

		gtk_list_store_clear (priv->store_mode_parameter);
		hkl_engine_parameters_values_get(priv->engine, values, n_values, HKL_UNIT_USER);
		for(i=0; i<n_values; ++i){
			gtk_list_store_append (priv->store_mode_parameter, &iter);
			gtk_list_store_set (priv->store_mode_parameter,
					    &iter,
					    PSEUDO_COL_NAME, darray_item(*parameters, i),
					    PSEUDO_COL_IDX, i,
					    PSEUDO_COL_VALUE, values[i],
					    -1);
		}
		gtk_expander_set_expanded (priv->expander1, TRUE);
		gtk_widget_show (GTK_WIDGET (priv->expander1));
	}else
		gtk_widget_hide (GTK_WIDGET (priv->expander1));
}


void
hkl_gui_engine_update (HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	GtkTreeViewColumn* col;
	GList* cells;

	g_return_if_fail (self != NULL);

	update_pseudo_axis (self);

	col = gtk_tree_view_get_column(priv->treeview1, 1);
	cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(col));
	g_object_set (G_OBJECT(cells->data), "background", NULL, NULL);
	g_list_free(cells);
}


void hkl_gui_engine_set_engine (HklGuiEngine *self,
				HklEngine *engine)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);
	g_return_if_fail (engine != NULL);

	priv->engine = engine;

	gtk_label_set_label (priv->label2,
			     hkl_engine_name_get(priv->engine));

	update_pseudo_axis (self);
	update_mode (self);
	update_mode_parameters (self);

	hkl_gui_engine_update(self);
}

static void
combobox1_changed_cb (GtkComboBox* combobox, HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	gchar *mode;
	GtkTreeIter iter = {0};

	g_return_if_fail (self != NULL);
	g_return_if_fail (combobox != NULL);

	if(gtk_combo_box_get_active_iter(combobox, &iter)){
		gtk_tree_model_get(GTK_TREE_MODEL(priv->store_mode),
				   &iter,
				   MODE_COL_NAME, &mode,
				   -1);
		if(!hkl_engine_current_mode_set(priv->engine, mode, NULL))
			return;
		update_mode_parameters(self);
	}
}

static void
button1_clicked_cb (GtkButton* button, HklGuiEngine* self)
{
	g_signal_emit(self, signals[CHANGED], 0);
}


static void
button2_clicked_cb (GtkButton* button, HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);

	if (HKL_ENGINE_CAPABILITIES_INITIALIZABLE & hkl_engine_capabilities_get(priv->engine)){
		if(hkl_engine_initialized_set(priv->engine, TRUE, NULL)){
			/* some init method update the parameters */
			update_mode_parameters(self);
		}
	}
}


static void
cell_tree_view_pseudo_axis_value_edited_cb (GtkCellRendererText* renderer,
					    const gchar* path,
					    const gchar* new_text,
					    HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	GtkTreeIter iter = {0};

	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->store_pseudo),
						 &iter, path)) {
		gdouble value = 0.0;

		value = atof(new_text);
		g_object_set (G_OBJECT(renderer), "background", "red", NULL, NULL);
		gtk_list_store_set (priv->store_pseudo,
				    &iter,
				    PSEUDO_COL_VALUE, value,
				    -1);
	}
}

/* mode parameters */
void
cellrendererspin2_edited_cb (GtkCellRendererText* renderer,
			     const gchar* path,
			     const gchar* new_text,
			     HklGuiEngine* self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	GtkTreeIter iter = {0};

	if (gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->store_mode_parameter),
						 &iter, path)) {
		const darray_string *parameters = hkl_engine_parameters_names_get(priv->engine);
		unsigned int n_values = darray_size(*parameters);
		double values[n_values];
		int idx;
		gdouble new_value = 0.0;

		new_value = atof(new_text);

		/* set the mode parameter value */
		hkl_engine_parameters_values_get(priv->engine, values, n_values, HKL_UNIT_USER);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->store_mode_parameter), &iter,
				    PSEUDO_COL_IDX, &idx,
				    -1);
		values[idx] = new_value;
		hkl_engine_parameters_values_set(priv->engine, values, n_values, HKL_UNIT_USER, NULL);

		/* set the store */
		gtk_list_store_set (priv->store_mode_parameter,
				    &iter,
				    PSEUDO_COL_VALUE, new_value,
				    -1);
	}
}

static void hkl_gui_engine_class_init (HklGuiEngineClass * class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (class);

	g_type_class_add_private (class, sizeof (HklGuiEnginePrivate));

	/* virtual methods */
	gobject_class->finalize = finalize;
	gobject_class->set_property = set_property;
	gobject_class->get_property = get_property;

	/* properties */
	obj_properties[PROP_ENGINE] =
		g_param_spec_pointer ("engine",
				      "Engine",
				      "The Hkl Engine used underneath",
				      G_PARAM_CONSTRUCT_ONLY |
				      G_PARAM_READWRITE |
				      G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_LISTSTORE] =
		g_param_spec_object ("liststore",
				     "Liststore",
				     "The liststore contaning all the pseudo axes values",
				     GTK_TYPE_LIST_STORE,
				     G_PARAM_READABLE |
				     G_PARAM_STATIC_STRINGS);

	g_object_class_install_properties (gobject_class,
					   N_PROPERTIES,
					   obj_properties);

	/* signals */
	signals[CHANGED] =
		g_signal_new ("changed",
			      HKL_GUI_TYPE_ENGINE,
			      G_SIGNAL_RUN_LAST,
			      0, /* class offset */
			      NULL, /* accumulator */
			      NULL, /* accu_data */
			      g_cclosure_marshal_VOID__VOID,
			      G_TYPE_NONE, /* return_type */
			      0);
}


static void _connect_renderer(gpointer data, gpointer user_data)
{
	GtkCellRenderer *renderer = GTK_CELL_RENDERER(data);
	HklGuiEngine *self = HKL_GUI_ENGINE(user_data);

	g_signal_connect_object (renderer,
				 "edited",
				 (GCallback) cell_tree_view_pseudo_axis_value_edited_cb,
				 self, 0);
}

static void hkl_gui_engine_init (HklGuiEngine * self)
{
	HklGuiEnginePrivate *priv = HKL_GUI_ENGINE_GET_PRIVATE(self);
	GtkBuilder *builder;
	GtkTreeViewColumn* col;
	GList* cells;

	priv->engine = NULL;
	priv->builder = builder = gtk_builder_new ();

	get_ui(builder, "pseudo.ui");

	get_object(builder, GTK_FRAME, priv, frame1);
	get_object(builder, GTK_LABEL, priv, label2);
	get_object(builder, GTK_COMBO_BOX, priv, combobox1);
	get_object(builder, GTK_EXPANDER, priv, expander1);
	get_object(builder, GTK_TREE_VIEW, priv, treeview1);
	get_object(builder, GTK_BUTTON, priv, button1);
	get_object(builder, GTK_BUTTON, priv, button2);

	get_object(builder, GTK_LIST_STORE, priv, store_mode);
	get_object(builder, GTK_LIST_STORE, priv, store_pseudo);
	get_object(builder, GTK_LIST_STORE, priv, store_mode_parameter);

	gtk_builder_connect_signals (builder, self);

	g_signal_connect_object (priv->combobox1,
				 "changed",
				 (GCallback) combobox1_changed_cb,
				 self, 0);

	g_signal_connect_object (priv->button1,
				 "clicked",
				 (GCallback) button1_clicked_cb,
				 self, 0);

	g_signal_connect_object (priv->button2,
				 "clicked",
				 (GCallback) button2_clicked_cb,
				 self, 0);

	col = gtk_tree_view_get_column (priv->treeview1, 1);
	cells = gtk_cell_layout_get_cells(GTK_CELL_LAYOUT(col));
	g_list_foreach(cells, _connect_renderer, self);
	g_list_free(cells);
}
