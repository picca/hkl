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

#include "hkl-gui.h"
#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include "hkl.h"
#include "hkl-gui-pseudoaxes.h"
#include <gdk/gdk.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include <stdio.h>

#define HKL_GUI_TYPE_WINDOW (hkl_gui_window_get_type ())
#define HKL_GUI_WINDOW(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), HKL_GUI_TYPE_WINDOW, HklGuiWindow))
#define HKL_GUI_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), HKL_GUI_TYPE_WINDOW, HklGuiWindowClass))
#define HKL_GUI_IS_WINDOW(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HKL_GUI_TYPE_WINDOW))
#define HKL_GUI_IS_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), HKL_GUI_TYPE_WINDOW))
#define HKL_GUI_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), HKL_GUI_TYPE_WINDOW, HklGuiWindowClass))

#define EMBED_BREAKPOINT  asm volatile ("int3;")

G_DEFINE_TYPE (HklGuiWindow, hkl_gui_window, G_TYPE_OBJECT);

typedef enum  {
	REFLECTION_COL_INDEX = 0,
	REFLECTION_COL_H,
	REFLECTION_COL_K,
	REFLECTION_COL_L,
	REFLECTION_COL_FLAG,
	REFLECTION_COL_REFLECTION,
	REFLECTION_COL_N_COLUMNS
} ReflectionCol;

typedef enum  {
	AXIS_COL_AXIS = 0,
	AXIS_COL_NAME,
	AXIS_COL_READ,
	AXIS_COL_WRITE,
	AXIS_COL_MIN,
	AXIS_COL_MAX,
	AXIS_COL_N_COLUMNS
} AxisCol;

typedef enum  {
	PSEUDO_AXIS_COL_PARAMETER = 0,
	PSEUDO_AXIS_COL_ENGINE,
	PSEUDO_AXIS_COL_NAME,
	PSEUDO_AXIS_COL_READ,
	PSEUDO_AXIS_COL_WRITE,
	PSEUDO_AXIS_COL_N_COLUMNS
} PseudoAxisCol;

typedef enum  {
	PARAMETER_COL_PARAMETER = 0,
	PARAMETER_COL_NAME,
	PARAMETER_COL_VALUE,
	PARAMETER_COL_N_COLUMNS
} ParameterCol;

typedef enum  {
	SAMPLE_COL_SAMPLE = 0,
	SAMPLE_COL_NAME,
	SAMPLE_COL_A,
	SAMPLE_COL_B,
	SAMPLE_COL_C,
	SAMPLE_COL_ALPHA,
	SAMPLE_COL_BETA,
	SAMPLE_COL_GAMMA,
	SAMPLE_COL_N_COLUMNS
} SampleCol;

typedef enum  {
	SOLUTION_COL_INDEX = 0,
	SOLUTION_COL_N_COLUMNS
} SolutionCol;

typedef enum  {
	DIFFRACTOMETER_COL_NAME = 0,
	DIFFRACTOMETER_COL_FACTORY,
	DIFFRACTOMETER_COL_DIFFRACTOMETER,
	DIFFRACTOMETER_COL_N_COLUMNS
} DiffractometerCol;

/******************/
/* Diffractometer */
/******************/

struct diffractometer_t {
	HklGeometry *geometry;
	HklDetector *detector;
	HklEngineList *engines;
};


static struct diffractometer_t *
create_diffractometer(HklFactory *factory)
{
	struct diffractometer_t *self;

	self = malloc(sizeof(*self));

	self->geometry = hkl_factory_create_new_geometry (factory);
	self->engines = hkl_factory_create_new_engine_list (factory);
	self->detector = hkl_detector_factory_new (HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set (self->detector, 1);

	return self;
}

static void
delete_diffractometer(struct diffractometer_t *self)
{
	hkl_geometry_free(self->geometry);
	hkl_engine_list_free(self->engines);
	hkl_detector_free(self->detector);
}


static void
dump_diffractometer(struct diffractometer_t *self)
{
	/* hkl_geometry_fprintf(stderr, self->geometry); */
	/* hkl_engine_list_fprintf(stderr, self->engines); */
	/* hkl_detector_fprintf(stderr, self->detector); */
}

static void
diffractometer_get_pseudo(struct diffractometer_t *self)
{
	hkl_engine_list_get(self->engines);
}

static void
diffractometer_engine_list_init(struct diffractometer_t *self,
				HklSample *sample)
{
	g_return_if_fail(self != NULL);
	g_return_if_fail(sample != NULL);

	hkl_engine_list_init(self->engines,
			     self->geometry,
			     self->detector,
			     sample);
	diffractometer_get_pseudo(self);
}

/****************/
/* HklGuiWindow */
/****************/

struct _HklGuiWindowPrivate {
	GtkBuilder* builder;
	GtkLabel* _label_UB11;
	GtkLabel* _label_UB12;
	GtkLabel* _label_UB13;
	GtkLabel* _label_UB21;
	GtkLabel* _label_UB22;
	GtkLabel* _label_UB23;
	GtkLabel* _label_UB31;
	GtkLabel* _label_UB32;
	GtkLabel* _label_UB33;
	GtkButton* _button2;
	GtkSpinButton* _spinbutton_a;
	GtkSpinButton* _spinbutton_b;
	GtkSpinButton* _spinbutton_c;
	GtkSpinButton* _spinbutton_alpha;
	GtkSpinButton* _spinbutton_beta;
	GtkSpinButton* _spinbutton_gamma;
	GtkSpinButton* _spinbutton_a_min;
	GtkSpinButton* _spinbutton_b_min;
	GtkSpinButton* _spinbutton_c_min;
	GtkSpinButton* _spinbutton_alpha_min;
	GtkSpinButton* _spinbutton_beta_min;
	GtkSpinButton* _spinbutton_gamma_min;
	GtkSpinButton* _spinbutton_a_max;
	GtkSpinButton* _spinbutton_b_max;
	GtkSpinButton* _spinbutton_c_max;
	GtkSpinButton* _spinbutton_alpha_max;
	GtkSpinButton* _spinbutton_beta_max;
	GtkSpinButton* _spinbutton_gamma_max;
	GtkSpinButton* _spinbutton_lambda;
	GtkSpinButton* _spinbutton_a_star;
	GtkSpinButton* _spinbutton_b_star;
	GtkSpinButton* _spinbutton_c_star;
	GtkSpinButton* _spinbutton_alpha_star;
	GtkSpinButton* _spinbutton_beta_star;
	GtkSpinButton* _spinbutton_gamma_star;
	GtkSpinButton* _spinbutton_ux;
	GtkSpinButton* _spinbutton_uy;
	GtkSpinButton* _spinbutton_uz;
	GtkSpinButton* _spinbutton_U11;
	GtkSpinButton* _spinbutton_U12;
	GtkSpinButton* _spinbutton_U13;
	GtkSpinButton* _spinbutton_U21;
	GtkSpinButton* _spinbutton_U22;
	GtkSpinButton* _spinbutton_U23;
	GtkSpinButton* _spinbutton_U31;
	GtkSpinButton* _spinbutton_U32;
	GtkSpinButton* _spinbutton_U33;
	GtkCheckButton* _checkbutton_a;
	GtkCheckButton* _checkbutton_b;
	GtkCheckButton* _checkbutton_c;
	GtkCheckButton* _checkbutton_alpha;
	GtkCheckButton* _checkbutton_beta;
	GtkCheckButton* _checkbutton_gamma;
	GtkCheckButton* _checkbutton_ux;
	GtkCheckButton* _checkbutton_uy;
	GtkCheckButton* _checkbutton_uz;
	GtkTreeView* _treeview_reflections;
	GtkTreeView* _treeview_crystals;
	GtkTreeView* _treeview_axes;
	GtkTreeView* _treeview_pseudo_axes;
	GtkTreeView* _treeview_pseudo_axes_parameters;
	GtkTreeView* _treeview_solutions;
	GtkToolButton* _toolbutton_add_reflection;
	GtkToolButton* _toolbutton_goto_reflection;
	GtkToolButton* _toolbutton_del_reflection;
	GtkToolButton* _toolbutton_setUB;
	GtkToolButton* _toolbutton_computeUB;
	GtkToolButton* _toolbutton_add_crystal;
	GtkToolButton* _toolbutton_copy_crystal;
	GtkToolButton* _toolbutton_del_crystal;
	GtkToolButton* _toolbutton_affiner;
	GtkStatusbar* _statusbar;
	GtkImageMenuItem* _menuitem5;
	GtkVBox* _box_info_bar; /* fake for the infor bar */
	GtkVBox* _vbox7;
	GtkVBox* _vbox2;
	GtkDialog* _dialog1;
	GtkButton* _button1;
	GtkComboBox* _combobox1;
	GtkListStore* _liststore_diffractometer;
	GtkListStore* _liststore_axis;
	GtkListStore* _liststore_pseudo_axes;
	GtkListStore* _liststore_solutions;
	GtkListStore* _liststore_reflections;
	GtkListStore* _liststore_crystals;

	GtkInfoBar *info_bar;
	GtkLabel *info_message;

	darray(HklGuiEngine *) pseudo_frames;

	struct diffractometer_t *diffractometer; /* unowned */
	HklSample *sample; /* unowned */
	HklLattice *reciprocal;
};

#define HKL_GUI_WINDOW_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HKL_GUI_TYPE_WINDOW, HklGuiWindowPrivate))

static gboolean
finalize_liststore_diffractometer(GtkTreeModel *model,
				  GtkTreePath *path,
				  GtkTreeIter *iter,
				  gpointer data)
{
	struct diffractometer_t *diffractometer;

	gtk_tree_model_get(model, iter,
			   DIFFRACTOMETER_COL_DIFFRACTOMETER, &diffractometer,
			   -1);
	delete_diffractometer(diffractometer);
	return FALSE;
}

static gboolean
finalize_liststore_samples(GtkTreeModel *model,
			   GtkTreePath *path,
			   GtkTreeIter *iter,
			   gpointer data)
{
	HklSample *sample = NULL;

	gtk_tree_model_get(model, iter,
			   SAMPLE_COL_SAMPLE, &sample,
			   -1);
	hkl_sample_free(sample);
	return FALSE;
}

static void
finalize (GObject* object)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(object);

	g_object_unref(priv->builder);

	darray_free(priv->pseudo_frames);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_diffractometer),
			       finalize_liststore_diffractometer,
			       NULL);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_crystals),
			       finalize_liststore_samples,
			       NULL);

	G_OBJECT_CLASS (hkl_gui_window_parent_class)->finalize (object);
}

HklGuiWindow* hkl_gui_window_new (void)
{
	return g_object_new (HKL_GUI_TYPE_WINDOW, NULL);
}


#define get_object(builder, type, priv, name) priv->_ ## name = type(gtk_builder_get_object(builder, #name))

static void
hkl_gui_window_get_widgets_and_objects_from_ui (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkBuilder* builder;

	g_return_if_fail (self != NULL);

	priv->builder = builder = gtk_builder_new ();
	gtk_builder_add_from_file (builder, "ghkl.ui", NULL);

	get_object(builder, GTK_LIST_STORE, priv, liststore_diffractometer);
	get_object(builder, GTK_LIST_STORE, priv, liststore_axis);
	get_object(builder, GTK_LIST_STORE, priv, liststore_pseudo_axes);
	get_object(builder, GTK_LIST_STORE, priv, liststore_reflections);
	get_object(builder, GTK_LIST_STORE, priv, liststore_crystals);

	get_object(builder, GTK_LABEL, priv, label_UB11);
	get_object(builder, GTK_LABEL, priv, label_UB12);
	get_object(builder, GTK_LABEL, priv, label_UB13);
	get_object(builder, GTK_LABEL, priv, label_UB21);
	get_object(builder, GTK_LABEL, priv, label_UB22);
	get_object(builder, GTK_LABEL, priv, label_UB23);
	get_object(builder, GTK_LABEL, priv, label_UB31);
	get_object(builder, GTK_LABEL, priv, label_UB32);
	get_object(builder, GTK_LABEL, priv, label_UB33);

	get_object(builder, GTK_BUTTON, priv, button2);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a_min);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a_max);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_a_star);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b_min);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b_max);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_b_star);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c_min);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c_max);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_c_star);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha_min);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha_max);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_alpha_star);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta_min);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta_max);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_beta_star);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma_min);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma_max);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_gamma_star);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_lambda);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_ux);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_uy);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_uz);

	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U11);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U12);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U13);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U21);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U22);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U23);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U31);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U32);
	get_object(builder, GTK_SPIN_BUTTON, priv, spinbutton_U33);


	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_a);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_b);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_c);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_alpha);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_beta);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_gamma);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_ux);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_uy);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_uz);


	get_object(builder, GTK_TREE_VIEW, priv, treeview_reflections);
	get_object(builder, GTK_TREE_VIEW, priv, treeview_crystals);
	get_object(builder, GTK_TREE_VIEW, priv, treeview_axes);
	get_object(builder, GTK_TREE_VIEW, priv, treeview_pseudo_axes);
	get_object(builder, GTK_TREE_VIEW, priv, treeview_solutions);
	priv->_treeview_pseudo_axes_parameters = GTK_TREE_VIEW(gtk_builder_get_object (builder, "treeview_pseudoAxes_parameters"));


	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_add_reflection);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_goto_reflection);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_del_reflection);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_setUB);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_computeUB);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_add_crystal);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_copy_crystal);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_del_crystal);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton_affiner);

	get_object(builder, GTK_STATUSBAR, priv, statusbar);

	get_object(builder, GTK_IMAGE_MENU_ITEM, priv, menuitem5);

	get_object(builder, GTK_VBOX, priv, vbox7);
	get_object(builder, GTK_VBOX, priv, vbox2);
	get_object(builder, GTK_VBOX, priv, box_info_bar);

	get_object(builder, GTK_DIALOG, priv, dialog1);

	get_object(builder, GTK_COMBO_BOX, priv, combobox1);

	gtk_builder_connect_signals (builder, self);
}

static void
update_pseudo_axes_frames (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklGuiEngine **engine;

	g_return_if_fail (self != NULL);

	darray_foreach(engine, priv->pseudo_frames){
		hkl_gui_engine_update(*engine);
	}
}

static void
raise_error(HklGuiWindow *self, HklError **error)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);
	g_return_if_fail (error != NULL);

	/* show an error message */
	gtk_label_set_text (GTK_LABEL (priv->info_message),
			    hkl_error_message_get(*error));
	gtk_info_bar_set_message_type (priv->info_bar,
				       GTK_MESSAGE_ERROR);
	gtk_widget_show (GTK_WIDGET(priv->info_bar));

	hkl_error_clear(error);
}

static void
clear_error(HklGuiWindow *self, HklError **error)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	gtk_widget_hide(GTK_WIDGET(priv->info_bar));
}

static gboolean
_update_axis (GtkTreeModel *model, GtkTreePath *path,
	      GtkTreeIter *iter, gpointer data)
{
	HklParameter *parameter;
	gdouble value, min, max;

	gtk_tree_model_get (model, iter,
			    AXIS_COL_AXIS, &parameter,
			    -1);

	hkl_parameter_min_max_unit_get(parameter, &min, &max);
	value = hkl_parameter_value_unit_get(parameter);

	gtk_list_store_set(GTK_LIST_STORE(model), iter,
			   AXIS_COL_READ, value,
			   AXIS_COL_WRITE, value,
			   AXIS_COL_MIN, min,
			   AXIS_COL_MAX, max,
			   -1);
	return FALSE;
}

static void
update_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_axis),
			       _update_axis,
			       self);
}

static gboolean
_update_pseudo_axes (GtkTreeModel *model, GtkTreePath *path,
		     GtkTreeIter *iter, gpointer data)
{
	HklParameter *parameter;
	gdouble value, min, max;

	gtk_tree_model_get (model, iter,
			    PSEUDO_AXIS_COL_PARAMETER, &parameter,
			    -1);

	hkl_parameter_min_max_unit_get(parameter, &min, &max);
	value = hkl_parameter_value_unit_get(parameter);

	gtk_list_store_set(GTK_LIST_STORE(model), iter,
			   PSEUDO_AXIS_COL_READ, value,
			   PSEUDO_AXIS_COL_WRITE, value,
			   -1);
	return FALSE;
}

static void
update_pseudo_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_pseudo_axes),
			       _update_pseudo_axes,
			       self);
}

static void
update_solutions (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	const HklGeometryList *geometries;
	const darray_item *items;
	GtkTreeIter iter = {0};

	g_return_if_fail (self != NULL);

	geometries = hkl_engine_list_geometries(priv->diffractometer->engines);

	gtk_list_store_clear(priv->_liststore_solutions);
	items = hkl_geometry_list_items_get(geometries);
	if (darray_size(*items)){
		gint n_values = gtk_tree_model_get_n_columns (GTK_TREE_MODEL(priv->_liststore_solutions));
		GValue *values = g_new0(GValue, n_values);
		gint *columns = g_new0(gint, n_values);
		gint i;

		/* prepare the GValue before using them */
		g_value_init(&values[0], G_TYPE_INT);
		for(i=1; i<n_values; ++i)
			g_value_init(&values[i], G_TYPE_DOUBLE);

		for(i=0; i<darray_size(*items);++i){
			gint column = 0;
			const HklGeometry *geometry;
			HklParameter **parameter;
			const darray_parameter *parameters;

			geometry = hkl_geometry_list_item_geometry_get(darray_item(*items, i));
			parameters = hkl_geometry_axes_get(geometry);

			g_value_set_int(&values[column], i);
			columns[0] = column;

			darray_foreach(parameter, *parameters){
				double value = hkl_parameter_value_unit_get(*parameter);

				column = column + 1;
				g_value_set_double(&values[column], value);
				columns[column] = column;
			}
			gtk_list_store_insert_with_valuesv(priv->_liststore_solutions,
							   &iter, i,
							   columns, values, n_values);
		}
		g_free(columns);
		g_free(values);
	}
}

static void
update_source (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	gtk_spin_button_set_value (priv->_spinbutton_lambda,
				   hkl_geometry_wavelength_get(priv->diffractometer->geometry));
}

static void
update_reflections (HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	gtk_list_store_clear (priv->_liststore_reflections);

	if(priv->sample){
		HklSampleReflection* reflection = NULL;
		guint index = 0;

		reflection = hkl_sample_first_reflection_get(priv->sample);
		while(reflection){
			GtkTreeIter iter = {0};
			gdouble h, k, l;
			gboolean flag;

			hkl_sample_reflection_hkl_get(reflection, &h, &k, &l);
			flag = hkl_sample_reflection_flag_get(reflection);

			gtk_list_store_append (priv->_liststore_reflections, &iter);

			gtk_list_store_set (priv->_liststore_reflections,
					    &iter,
					    REFLECTION_COL_INDEX, index++,
					    REFLECTION_COL_H, h,
					    REFLECTION_COL_K, k,
					    REFLECTION_COL_L, l,
					    REFLECTION_COL_FLAG, flag,
					    REFLECTION_COL_REFLECTION, reflection,
					    -1);
			reflection = hkl_sample_next_reflection_get(priv->sample,
								    reflection);
		}
	}
}

static gboolean
hkl_engine_to_axes(HklGuiWindow *self, HklEngine *engine)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklError *error = NULL;
	gboolean res = TRUE;

	g_return_val_if_fail (self != NULL, FALSE);
	g_return_val_if_fail (engine != NULL, FALSE);

	if(hkl_engine_set(engine, &error)){
		clear_error(self, &error);
		hkl_engine_list_select_solution(priv->diffractometer->engines, 0);
		hkl_engine_list_get(priv->diffractometer->engines);

		update_axes (self);
		update_pseudo_axes (self);
		update_pseudo_axes_frames (self);
	}else{
		raise_error(self, &error);
		dump_diffractometer(priv->diffractometer);
		res = FALSE;
	}
	update_solutions (self);
	return res;
}

static void
pseudo_axes_frame_changed_cb (HklGuiEngine *gui_engine, HklGuiWindow *self)
{
	HklEngine *engine;

	g_return_if_fail (self != NULL);

	g_object_get(G_OBJECT(gui_engine),
		     "engine", &engine,
		     NULL);

	hkl_engine_to_axes(self, engine);
}

static void
set_up_pseudo_axes_frames (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklGuiEngine **pseudo;
	GtkVBox* vbox2;
	HklEngine **engine;
	darray_engine *engines;

	g_return_if_fail (self != NULL);

	darray_foreach (pseudo, priv->pseudo_frames){
		gtk_container_remove(GTK_CONTAINER(priv->_vbox2),
				     GTK_WIDGET (hkl_gui_engine_get_frame (*pseudo)));
		g_object_unref(*pseudo);
	}
	darray_size (priv->pseudo_frames) = 0;

	engines = hkl_engine_list_engines (priv->diffractometer->engines);
	darray_foreach (engine, *engines){
		HklGuiEngine *pseudo;

		pseudo = hkl_gui_engine_new (*engine);
		darray_append(priv->pseudo_frames, pseudo);
		gtk_container_add (GTK_CONTAINER (priv->_vbox2),
				   GTK_WIDGET (hkl_gui_engine_get_frame(pseudo)));

		g_signal_connect_object (pseudo,
					 "changed",
					 G_CALLBACK(pseudo_axes_frame_changed_cb),
					 self, 0);
	}

	gtk_widget_show_all (GTK_WIDGET (priv->_vbox2));
}


static void
set_up_diffractometer_model (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	unsigned int i, n;
	HklFactory **factories;

	g_return_if_fail (self != NULL);

	factories = hkl_factory_get_all(&n);
	for(i=0; i<n; ++i){
		GtkTreeIter iter = {0};

		gtk_list_store_append (priv->_liststore_diffractometer, &iter);
		gtk_list_store_set (priv->_liststore_diffractometer,
				    &iter,
				    DIFFRACTOMETER_COL_NAME, hkl_factory_name(factories[i]),
				    DIFFRACTOMETER_COL_FACTORY, factories[i],
				    DIFFRACTOMETER_COL_DIFFRACTOMETER, NULL,
				    -1);
	}
}

static void
set_up_tree_view_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklParameter **parameter;
	const darray_parameter *parameters;
	GtkCellRenderer* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;

	g_return_if_fail (self != NULL);

	gtk_list_store_clear (priv->_liststore_axis);

	parameters = hkl_geometry_axes_get(priv->diffractometer->geometry);
	darray_foreach (parameter, *parameters){
		GtkTreeIter iter = {0};

		gtk_list_store_append (priv->_liststore_axis, &iter);
		gtk_list_store_set (priv->_liststore_axis, &iter,
				    AXIS_COL_AXIS, *parameter,
				    AXIS_COL_NAME, hkl_parameter_name_get(*parameter),
				    -1);
	}

	update_axes (self);
}

static void
set_up_tree_view_pseudo_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklParameter **parameter;
	const darray_parameter *parameters;
	HklEngine **engine;
	const darray_engine *engines;

	GtkCellRendererText* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;

	g_return_if_fail (self != NULL);

	gtk_list_store_clear(priv->_liststore_pseudo_axes);

	engines = hkl_engine_list_engines(priv->diffractometer->engines);
	darray_foreach(engine, *engines){
		parameters = hkl_engine_pseudo_axes(*engine);
		darray_foreach(parameter, *parameters){
			GtkTreeIter iter = {0};

			gtk_list_store_append (priv->_liststore_pseudo_axes, &iter);
			gtk_list_store_set (priv->_liststore_pseudo_axes, &iter,
					    PSEUDO_AXIS_COL_PARAMETER, *parameter,
					    PSEUDO_AXIS_COL_ENGINE, *engine,
					    PSEUDO_AXIS_COL_NAME, hkl_parameter_name_get(*parameter),
					    -1);
		}
	}

	update_pseudo_axes (self);
}

static void
_delete_column(gpointer data,
	       gpointer user_data)
{
	gtk_tree_view_remove_column (GTK_TREE_VIEW(user_data),
				     GTK_TREE_VIEW_COLUMN(data));
}

static void
set_up_tree_view_solutions (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	const darray_parameter *parameters;
	int i;
	GtkCellRenderer* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;
	GType* types;
	gint n_columns;

	g_return_if_fail (self != NULL);

	parameters = hkl_geometry_axes_get(priv->diffractometer->geometry);

	n_columns = SOLUTION_COL_N_COLUMNS + darray_size(*parameters);

	/* prepare types for the liststore */
	types = g_new0 (GType, n_columns);

	/* first remove all the columns */
	columns = gtk_tree_view_get_columns (priv->_treeview_solutions);
	g_list_foreach(columns, _delete_column, priv->_treeview_solutions);
	g_list_free(columns);

	/* now add the index column */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("index",
							   renderer, "text",
							   SOLUTION_COL_INDEX, NULL);

	gtk_tree_view_append_column (priv->_treeview_solutions, column);
	types[0] = G_TYPE_INT;

	/* add the axes column */
	for(i=1; i<n_columns; ++i){
		HklParameter *parameter;

		parameter = darray_item(*parameters, i - SOLUTION_COL_N_COLUMNS);
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes (hkl_parameter_name_get(parameter),
								   renderer, "text",
								   i, NULL);

		gtk_tree_view_append_column (priv->_treeview_solutions, column);
		types[i] = G_TYPE_DOUBLE;
	}

	if (priv->_liststore_solutions)
		g_object_unref(priv->_liststore_solutions);
	priv->_liststore_solutions = gtk_list_store_newv (n_columns, types);
	g_free (types);

	gtk_tree_view_set_model (priv->_treeview_solutions,
				 GTK_TREE_MODEL(priv->_liststore_solutions));

	update_solutions (self);
}

void
set_up_info_bar(HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkWidget *content_area;

	g_return_if_fail (self != NULL);

	/* set up info bar until we can use glade for this purpose or
	 * switch to gtk3 */
	if (priv->info_bar)
		return;

	priv->info_bar = GTK_INFO_BAR(gtk_info_bar_new ());
	gtk_widget_set_no_show_all (GTK_WIDGET(priv->info_bar), TRUE);

	priv->info_message = GTK_LABEL(gtk_label_new (""));
	gtk_widget_show (GTK_WIDGET(priv->info_message));

	content_area = gtk_info_bar_get_content_area (GTK_INFO_BAR (priv->info_bar));
	gtk_container_add (GTK_CONTAINER (content_area),
			   GTK_WIDGET(priv->info_message));
	gtk_info_bar_add_button (priv->info_bar,
				 GTK_STOCK_OK, GTK_RESPONSE_OK);
	g_signal_connect (priv->info_bar, "response",
			  G_CALLBACK (gtk_widget_hide), NULL);

	gtk_box_pack_start(GTK_BOX(priv->_box_info_bar),
			   GTK_WIDGET(priv->info_bar),
			   TRUE, TRUE, 0);
}

void
hkl_gui_window_combobox1_changed_cb(GtkComboBox *combobox, gpointer *user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	HklFactory *factory;
	struct diffractometer_t *dif = NULL;

	GtkTreeIter iter = {0};

	if(gtk_combo_box_get_active_iter (combobox, &iter)){
		gtk_tree_model_get(GTK_TREE_MODEL(priv->_liststore_diffractometer),
				   &iter,
				   DIFFRACTOMETER_COL_FACTORY, &factory,
				   DIFFRACTOMETER_COL_DIFFRACTOMETER, &dif,
				   -1);

		if (!dif){
			dif = create_diffractometer(factory);
			gtk_list_store_set(priv->_liststore_diffractometer,
					   &iter,
					   DIFFRACTOMETER_COL_DIFFRACTOMETER, dif,
					   -1);
		}
		printf("toto\n");
	}
	priv->diffractometer = dif;
	/* TODO check if this is the right place for this */
	hkl_engine_list_init(dif->engines, dif->geometry, dif->detector, priv->sample);

	set_up_pseudo_axes_frames(self);
	set_up_tree_view_axes(self);
	//hkl_gui_window_set_up_tree_view_pseudo_axes_parameters(self);
	set_up_tree_view_pseudo_axes(self);

	/* FIXME create the right solution Model Column */
	/* this._solutionModelColumns = 0; */
	set_up_tree_view_solutions(self);
	set_up_info_bar(self);
#if HKL3D
	set_up_3D(self);
#endif
}


/* axis read cb */
void
hkl_gui_window_cellrendererspin1_edited_cb(GtkCellRendererText *renderer,
					   gchar *path,
					   gchar *new_text,
					   gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter = NULL;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_axis),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_axis), &iter,
					   AXIS_COL_AXIS, &parameter,
					   -1);

	value = atof(new_text); /* TODO need to check for the right conversion */
	hkl_parameter_value_unit_set (parameter, value, NULL);
	hkl_geometry_axis_set(priv->diffractometer->geometry,
			      parameter);

	hkl_engine_list_get(priv->diffractometer->engines);

	/* ok so set the model with the new value */
	gtk_list_store_set (priv->_liststore_axis, &iter,
			    AXIS_COL_READ, value,
			    AXIS_COL_WRITE, value,
			    -1);

	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
}


/* axis min cb */
void
hkl_gui_window_cellrendererspin3_edited_cb(GtkCellRendererText *renderer,
					   gchar *path,
					   gchar *new_text,
					   gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter = NULL;
	gdouble shit, max;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_axis),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_axis), &iter,
					   AXIS_COL_AXIS, &parameter,
					   -1);

	value = atof(new_text); /* TODO need to check for the right conversion */
	hkl_parameter_min_max_unit_get (parameter, &shit, &max);
	hkl_parameter_min_max_unit_set (parameter, value, max);

	gtk_list_store_set (priv->_liststore_axis, &iter,
			    AXIS_COL_MIN, value,
			    -1);

	update_pseudo_axes (self);
}


/* axis max cb */
void
hkl_gui_window_cellrendererspin4_edited_cb(GtkCellRendererText *renderer,
					   gchar *path,
					   gchar *new_text,
					   gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter = NULL;
	gdouble shit, min;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_axis),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_axis), &iter,
					   AXIS_COL_AXIS, &parameter,
					   -1);

	value = atof(new_text); /* TODO need to check for the right conversion */
	hkl_parameter_min_max_unit_get (parameter, &min, &shit);
	hkl_parameter_min_max_unit_set (parameter, min, value);

	gtk_list_store_set (priv->_liststore_axis, &iter,
			    AXIS_COL_MAX, value,
			    -1);

	update_pseudo_axes (self);
}


/* pseudo axis write */
void
hkl_gui_window_cellrenderertext5_edited_cb(GtkCellRendererText *renderer,
					   gchar *path,
					   gchar *new_text,
					   gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	gdouble old_value;
	HklParameter* parameter = NULL;
	HklEngine *engine = NULL;
	HklError *error = NULL;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_pseudo_axes),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_pseudo_axes), &iter,
			    PSEUDO_AXIS_COL_PARAMETER, &parameter,
			    PSEUDO_AXIS_COL_ENGINE, &engine,
			    -1);

	value = atof(new_text); /* TODO need to check for the right conversion */
	old_value = hkl_parameter_value_unit_get(parameter);

	g_assert(error != NULL || error == NULL);
	hkl_parameter_value_unit_set (parameter, value, &error);
	if(error != NULL){
		raise_error(self, &error);
	}

	if (hkl_engine_to_axes(self, engine)){
		gtk_list_store_set (priv->_liststore_pseudo_axes,
				    &iter,
				    PSEUDO_AXIS_COL_WRITE, value,
				    -1);
	}else{
		hkl_parameter_value_unit_set(parameter, old_value, NULL);
	}
}


void
hkl_gui_window_treeview_solutions_cursor_changed_cb (GtkTreeView *tree_view,
						     gpointer     user_data)
{
	HklGuiWindow* self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	GtkTreePath* path = NULL;
	GtkTreeViewColumn* focus_column = NULL;
	GtkTreeIter iter = {0};
	gsize index = 0UL;

	g_return_if_fail (tree_view != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_view_get_cursor (tree_view, &path, &focus_column);
	gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->_liststore_solutions), &iter, path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_solutions), &iter,
			    SOLUTION_COL_INDEX, &index,
			    -1);

	hkl_engine_list_select_solution (priv->diffractometer->engines, index);
	hkl_engine_list_get (priv->diffractometer->engines);

	update_axes (self);
	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);

	gtk_tree_path_free (path);
}

/* reflection h */
void
hkl_gui_window_cellrenderertext7_edited_cb(GtkCellRendererText* _sender, const gchar* path,
					   const gchar* new_text, gpointer self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);

	if (priv->sample){
		gdouble h = 0.0;
		gdouble k = 0.0;
		gdouble l = 0.0;
		HklSampleReflection* reflection = NULL;
		GtkTreeIter iter = {0};

		gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_reflections),
						     &iter, path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_reflections),
				    &iter,
				    REFLECTION_COL_REFLECTION, &reflection,
				    -1);

		hkl_sample_reflection_hkl_get (reflection, &h, &k, &l);
		h = atof(new_text);
		hkl_sample_reflection_hkl_set (reflection, h, k, l);
		gtk_list_store_set (priv->_liststore_reflections,
				    &iter,
				    REFLECTION_COL_H, h,
				    -1);
	}
}

/* reflection k */
void
hkl_gui_window_cellrenderertext8_edited_cb (GtkCellRendererText* _sender, const gchar* path,
					    const gchar* new_text, gpointer self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);

	if (priv->sample){
		gdouble h = 0.0;
		gdouble k = 0.0;
		gdouble l = 0.0;
		HklSampleReflection* reflection = NULL;
		GtkTreeIter iter = {0};

		gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_reflections),
						     &iter, path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_reflections),
				    &iter,
				    REFLECTION_COL_REFLECTION, &reflection,
				    -1);

		hkl_sample_reflection_hkl_get (reflection, &h, &k, &l);
		k = atof(new_text);
		hkl_sample_reflection_hkl_set (reflection, h, k, l);
		gtk_list_store_set (priv->_liststore_reflections,
				    &iter,
				    REFLECTION_COL_K, k,
				    -1);
	}
}

/* reflection l */
void
hkl_gui_window_cellrenderertext9_edited_cb (GtkCellRendererText* _sender, const gchar* path,
					    const gchar* new_text, gpointer self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);

	if (priv->sample){
		gdouble h = 0.0;
		gdouble k = 0.0;
		gdouble l = 0.0;
		HklSampleReflection* reflection = NULL;
		GtkTreeIter iter = {0};

		gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_reflections),
						     &iter, path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_reflections),
				    &iter,
				    REFLECTION_COL_REFLECTION, &reflection,
				    -1);

		hkl_sample_reflection_hkl_get (reflection, &h, &k, &l);
		l = atof(new_text);
		hkl_sample_reflection_hkl_set (reflection, h, k, l);
		gtk_list_store_set (priv->_liststore_reflections,
				    &iter,
				    REFLECTION_COL_L, l,
				    -1);
	}
}

/* reflection flag */
void
hkl_gui_window_cellrenderertoggle1_toggled_cb (GtkCellRendererToggle* renderer, const gchar* path,
					       gpointer self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);
	g_return_if_fail (path != NULL);

	if (priv->sample){
		gboolean flag;
		HklSampleReflection* reflection = NULL;
		GtkTreeIter iter = {0};

		gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_liststore_reflections),
						     &iter, path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_reflections),
				    &iter,
				    REFLECTION_COL_REFLECTION, &reflection,
				    -1);

		flag = gtk_cell_renderer_toggle_get_active(renderer);
		hkl_sample_reflection_flag_set (reflection, flag);
		gtk_list_store_set (priv->_liststore_reflections,
				    &iter,
				    REFLECTION_COL_FLAG, flag,
				    -1);
	}
}

gboolean
hkl_gui_window_treeview_reflections_key_press_event_cb (GtkWidget* _sender, GdkEventKey* event,
							gpointer self)
{
	return TRUE;
}

void
hkl_gui_window_toolbutton_add_reflection_clicked_cb(GtkToolButton* _sender,
						    gpointer self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	if (priv->diffractometer == NULL){
		gtk_statusbar_push (priv->_statusbar, 0,
				    "Please select a diffractometer before adding reflections");
		return;
	}

	if (priv->sample) {
		HklSampleReflection *reflection = NULL;
		GtkTreeIter iter = {0};
		gboolean flag;
		gint n_rows;

		reflection = hkl_sample_reflection_new(priv->diffractometer->geometry,
						       priv->diffractometer->detector,
						       0, 0, 0);
		hkl_sample_add_reflection(priv->sample, reflection);
		flag = hkl_sample_reflection_flag_get(reflection);

		n_rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(priv->_liststore_reflections),
							NULL);
		gtk_list_store_insert_with_values (priv->_liststore_reflections,
						   &iter, -1,
						   REFLECTION_COL_INDEX, n_rows,
						   REFLECTION_COL_H, 0.,
						   REFLECTION_COL_K, 0.,
						   REFLECTION_COL_L, 0.,
						   REFLECTION_COL_FLAG, flag,
						   REFLECTION_COL_REFLECTION, reflection,
						   -1);
	}
}

void
hkl_gui_window_toolbutton_goto_reflection_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	g_return_if_fail (self != NULL);

	if (priv->sample) {
		GtkTreeSelection* selection = NULL;
		guint nb_rows = 0U;

		selection = gtk_tree_view_get_selection (priv->_treeview_reflections);
		nb_rows = gtk_tree_selection_count_selected_rows (selection);

		if (nb_rows == 1) {
			HklSampleReflection *reflection;
			GtkTreeIter iter = {0};
			GtkTreeModel* model = NULL;
			GtkTreePath *treepath;
			GList* list;

			model = GTK_TREE_MODEL(priv->_liststore_reflections);

			list = gtk_tree_selection_get_selected_rows (selection,
								     &model);

			treepath = g_list_nth_data(list, 0);

			gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->_liststore_reflections),
						 &iter, treepath);

			gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_reflections),
					    &iter,
					    REFLECTION_COL_REFLECTION, &reflection,
					    -1);

			hkl_geometry_set (priv->diffractometer->geometry,
					  hkl_sample_reflection_geometry_get(reflection));

			update_source (self);

			update_axes (self);

			update_pseudo_axes (self);

			g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);
		} else
			if (nb_rows > 1)
				gtk_statusbar_push (priv->_statusbar, 0,
						    "Please select only one reflection.");
			else
				gtk_statusbar_push (priv->_statusbar, 0,
						    "Please select at least one reflection.");
	}
}

static void
_del_reflection(gpointer data, gpointer user_data)
{
	HklSampleReflection *reflection;
	GtkTreeIter iter = {0};
	GtkTreePath *treepath = data;
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->_liststore_reflections),
				 &iter, treepath);

	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_reflections),
			    &iter,
			    REFLECTION_COL_REFLECTION, &reflection,
			    -1);
	hkl_sample_del_reflection(priv->sample, reflection);
}

void
hkl_gui_window_toolbutton_del_reflection_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	HklSample* sample = NULL;

	g_return_if_fail (self != NULL);

	if (priv->sample) {
		GtkTreeSelection* selection = NULL;
		guint nb_rows = 0U;

		selection = gtk_tree_view_get_selection (priv->_treeview_reflections);
		nb_rows = gtk_tree_selection_count_selected_rows (selection);
		if (nb_rows > 0) {
			GtkTreeModel* model = NULL;
			GList* list;
			guint* indexes;
			gint i;
			GtkMessageDialog* dialog;

			model = GTK_TREE_MODEL(priv->_liststore_reflections);
			list = gtk_tree_selection_get_selected_rows (selection, &model);


			dialog = GTK_MESSAGE_DIALOG(
				gtk_message_dialog_new (NULL,
							GTK_DIALOG_DESTROY_WITH_PARENT,
							GTK_MESSAGE_WARNING,
							GTK_BUTTONS_YES_NO,
							"Are you sure that you want to delete reflections"));

			switch (gtk_dialog_run (GTK_DIALOG(dialog))) {
			case GTK_RESPONSE_YES:
			{
				g_list_foreach(list, _del_reflection, self);
				update_reflections (self);
				break;
			}
			default:
				break;
			}
			gtk_widget_destroy (GTK_WIDGET(dialog));
			g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);
		} else {
			gtk_statusbar_push (priv->_statusbar, 0,
					    "Please select at least one reflection.");
		}
	}
}

/* crystal name */
void
hkl_gui_window_cellrenderertext10_edited_cb(GtkCellRendererText* _sender, const gchar* path,
					    const gchar* new_text, gpointer user_data)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	GtkTreeModel* model = NULL;
	GtkTreeIter iter = {0};
	HklSample* sample = NULL;
	gchar* name = NULL;

	g_return_if_fail (user_data != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);

	model = GTK_TREE_MODEL(priv->_liststore_crystals);

	gtk_tree_model_get_iter_from_string (model, &iter, path);

	gtk_tree_model_get (model, &iter,
			    SAMPLE_COL_SAMPLE, &sample,
			    -1);

	hkl_sample_name_set (sample, new_text);

	gtk_list_store_set(priv->_liststore_crystals, &iter,
			   SAMPLE_COL_NAME, new_text,
			   -1);
}

#define set_lattice(lattice, parameter) do{				\
		const HklParameter *p;					\
		gdouble min, max, value;				\
		gboolean fit;						\
		p = hkl_lattice_## parameter ##_get((lattice));		\
			value = hkl_parameter_value_unit_get(p);	\
			hkl_parameter_min_max_unit_get(p, &min, &max);	\
			fit = hkl_parameter_fit_get(p);			\
			gtk_spin_button_set_value(priv->_spinbutton_## parameter, value); \
			gtk_spin_button_set_value(priv->_spinbutton_## parameter ##_min, min); \
			gtk_spin_button_set_value(priv->_spinbutton_## parameter ##_max, max); \
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(priv->_checkbutton_ ## parameter), fit); \
	}while(0)

static void
update_lattice (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	if (priv->sample != NULL) {
		const HklLattice *lattice;
		lattice = hkl_sample_lattice_get(priv->sample);
		set_lattice(lattice, a);
		set_lattice(lattice, b);
		set_lattice(lattice, c);
		set_lattice(lattice, alpha);
		set_lattice(lattice, beta);
		set_lattice(lattice, gamma);
	}
}

#define set_reciprocal_lattice(lattice, parameter) do{			\
		const HklParameter *p;					\
		gdouble value;						\
		p = hkl_lattice_## parameter ##_get((lattice));		\
			value = hkl_parameter_value_unit_get(p);	\
			gtk_spin_button_set_value(priv->_spinbutton_## parameter ##_star, value); \
	}while(0)

static void
update_reciprocal_lattice (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	if (priv->sample != NULL) {
		hkl_lattice_reciprocal (hkl_sample_lattice_get(priv->sample),
					priv->reciprocal);

		set_reciprocal_lattice(priv->reciprocal, a);
		set_reciprocal_lattice(priv->reciprocal, b);
		set_reciprocal_lattice(priv->reciprocal, c);
		set_reciprocal_lattice(priv->reciprocal, alpha);
		set_reciprocal_lattice(priv->reciprocal, beta);
		set_reciprocal_lattice(priv->reciprocal, gamma);
	}
}

#define set_ux_uy_uz(sample, parameter) do {				\
		const HklParameter *p;					\
		p = hkl_sample_## parameter ##_get((sample));		\
			gboolean fit = hkl_parameter_fit_get(p);	\
			gtk_spin_button_set_value(priv->_spinbutton_## parameter, \
						  hkl_parameter_value_unit_get(p)); \
			gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(priv->_checkbutton_## parameter), fit); \
	}while(0)

static void
update_ux_uy_uz (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	if (priv->sample != NULL) {
		set_ux_uy_uz(priv->sample, ux);
		set_ux_uy_uz(priv->sample, uy);
		set_ux_uy_uz(priv->sample, uz);
	}
}

static void
update_UB (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	if (priv->sample != NULL) {
		const HklMatrix *UB = hkl_sample_UB_get (priv->sample);
		gchar *text = g_new0 (gchar, G_ASCII_DTOSTR_BUF_SIZE);

		gtk_label_set_text(priv->_label_UB11,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 0, 0)));
		gtk_label_set_text(priv->_label_UB12,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 0, 1)));
		gtk_label_set_text(priv->_label_UB13,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 0, 2)));
		gtk_label_set_text(priv->_label_UB21,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 1, 0)));
		gtk_label_set_text(priv->_label_UB22,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 1, 1)));
		gtk_label_set_text(priv->_label_UB23,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 1, 2)));
		gtk_label_set_text(priv->_label_UB31,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 2, 0)));
		gtk_label_set_text(priv->_label_UB32,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 2, 1)));
		gtk_label_set_text(priv->_label_UB33,
				   g_ascii_dtostr(text,
						  G_ASCII_DTOSTR_BUF_SIZE,
						  hkl_matrix_get(UB, 2, 2)));
		g_free(text);
	}
}

void
hkl_gui_window_treeview_crystals_cursor_changed_cb (GtkTreeView* _sender, gpointer user_data)
{
	GtkTreePath* path = NULL;
	GtkTreeViewColumn* column = NULL;
	GtkTreeIter iter = {0};

	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	HklSample *sample;

	g_return_if_fail (user_data != NULL);

	gtk_tree_view_get_cursor (priv->_treeview_crystals, &path, &column);
	if(path){
		if (gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->_liststore_crystals),
					     &iter, path) == TRUE){
			gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_crystals),
					    &iter,
					    SAMPLE_COL_SAMPLE, &sample,
					    -1);

			if(sample){
				priv->sample = sample;
				diffractometer_engine_list_init(priv->diffractometer,
								priv->sample);
				update_reflections(self);
				update_lattice(self);
				update_reciprocal_lattice (self);
				update_ux_uy_uz (self);
				update_UB (self);
				update_pseudo_axes (self);
				update_pseudo_axes_frames (self);
			}
		}
		gtk_tree_path_free (path);
	}
}



static GtkTreeIter
_add_sample(HklGuiWindow *self, HklSample *sample)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkTreeIter iter = {0};
	const HklLattice *lattice;
	gdouble a, b, c, alpha, beta, gamma;

	g_return_val_if_fail (self != NULL, iter);

	lattice = hkl_sample_lattice_get(sample);
	a = hkl_parameter_value_unit_get(hkl_lattice_a_get(lattice));
	b = hkl_parameter_value_unit_get(hkl_lattice_b_get(lattice));
	c = hkl_parameter_value_unit_get(hkl_lattice_c_get(lattice));
	alpha = hkl_parameter_value_unit_get(hkl_lattice_alpha_get(lattice));
	beta = hkl_parameter_value_unit_get(hkl_lattice_beta_get(lattice));
	gamma = hkl_parameter_value_unit_get(hkl_lattice_gamma_get(lattice));

	gtk_list_store_insert_with_values(priv->_liststore_crystals,
					  &iter, -1,
					  SAMPLE_COL_SAMPLE, sample,
					  SAMPLE_COL_NAME, hkl_sample_name_get(sample),
					  SAMPLE_COL_A, a,
					  SAMPLE_COL_B, b,
					  SAMPLE_COL_C, c,
					  SAMPLE_COL_ALPHA, alpha,
					  SAMPLE_COL_BETA, beta,
					  SAMPLE_COL_GAMMA, gamma,
					  -1);
	return iter;
}

static void
set_up_tree_view_crystals (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkTreeIter iter = {0};
	GtkTreePath *path = NULL;

	g_return_if_fail (self != NULL);

	iter = _add_sample(self, priv->sample);

	path = gtk_tree_model_get_path(GTK_TREE_MODEL(priv->_liststore_crystals),
				       &iter);

	gtk_tree_view_set_cursor(priv->_treeview_crystals, path, NULL, FALSE);

	gtk_tree_path_free(path);
}

static void
_add_sample_and_edit_name(HklGuiWindow *self, HklSample *sample)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkTreeIter iter = {0};
	GtkTreePath* path = NULL;
	GtkTreeViewColumn* column = NULL;

	iter = _add_sample(self, sample);

	path = gtk_tree_model_get_path(GTK_TREE_MODEL(priv->_liststore_crystals),
				       &iter);
	column = gtk_tree_view_get_column (priv->_treeview_crystals, 0);
	gtk_tree_view_set_cursor (priv->_treeview_crystals, path, column, TRUE);

	gtk_tree_path_free(path);
}

void
hkl_gui_window_toolbutton_add_crystal_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	HklSample *sample;

	g_return_if_fail (user_data != NULL);

	sample = hkl_sample_new ("new");
	if(sample)
		_add_sample_and_edit_name(self, sample);
}

void
hkl_gui_window_toolbutton_copy_crystal_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	HklSample *copy = NULL;

	g_return_if_fail (self != NULL);

	if(priv->sample) {
		copy = hkl_sample_new_copy(priv->sample);
		if (copy)
			_add_sample_and_edit_name(self, copy);
	}else
		gtk_statusbar_push (priv->_statusbar, (guint) 0, "Please select a crystal to copy.");
}

void
hkl_gui_window_toolbutton_del_crystal_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	g_return_if_fail (user_data != NULL);

	if (priv->sample != NULL) {
		guint n_rows;

		n_rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(priv->_liststore_crystals),
							NULL );
		if (n_rows == 1)
			return;
		else {
			GtkTreeIter iter = {0};
			GtkTreePath *path = NULL;
			GtkTreeViewColumn *column = NULL;

			gtk_tree_view_get_cursor(priv->_treeview_crystals,
						 &path, &column);
			if (path){
				if (gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->_liststore_crystals),
							     &iter, path) == TRUE) {
					gtk_tree_path_free(path);

					hkl_sample_free(priv->sample);
					if (gtk_list_store_remove(priv->_liststore_crystals,
								  &iter) == TRUE){
						path = gtk_tree_model_get_path(GTK_TREE_MODEL(priv->_liststore_crystals),
									       &iter);
						gtk_tree_view_set_cursor(priv->_treeview_crystals,
									 path, NULL, FALSE);
					}
				}
			}
		}
	}
}

#define get_lattice_parameter(lattice, parameter) do{			\
		HklParameter *p = hkl_parameter_new_copy(hkl_lattice_## parameter ##_get(lattice)); \
		hkl_parameter_min_max_unit_set(p,			\
					       gtk_spin_button_get_value(priv->_spinbutton_## parameter ##_min), \
					       gtk_spin_button_get_value(priv->_spinbutton_## parameter ##_max)); \
		hkl_parameter_fit_set(p, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(priv->_checkbutton_## parameter))); \
		hkl_lattice_## parameter ##_set(lattice, p);		\
		hkl_parameter_free(p);					\
	} while(0)

#define get_ux_uy_uz(sample, parameter) do {				\
		HklParameter *p;					\
		p = hkl_parameter_new_copy(hkl_sample_## parameter ##_get(sample)); \
		hkl_parameter_value_unit_set(p,				\
					     gtk_spin_button_get_value (priv->_spinbutton_## parameter), \
					     NULL);			\
		hkl_parameter_fit_set(p,				\
				      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(priv->_checkbutton_## parameter))); \
		hkl_sample_## parameter ##_set(sample, p);	\
		hkl_parameter_free(p);					\
	}while(0)


static gboolean
_update_crystal_model(GtkTreeModel *model,
		      GtkTreePath *path,
		      GtkTreeIter *iter,
		      gpointer data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(data);
	HklSample *sample = NULL;

	gtk_tree_model_get(model, iter,
			   SAMPLE_COL_SAMPLE, &sample,
			   -1);
	if(priv->sample == sample){
		const HklLattice *lattice;
		gdouble a, b, c, alpha, beta, gamma;

		lattice = hkl_sample_lattice_get(sample);
		a = hkl_parameter_value_unit_get(hkl_lattice_a_get(lattice));
		b = hkl_parameter_value_unit_get(hkl_lattice_b_get(lattice));
		c = hkl_parameter_value_unit_get(hkl_lattice_c_get(lattice));
		alpha = hkl_parameter_value_unit_get(hkl_lattice_alpha_get(lattice));
		beta = hkl_parameter_value_unit_get(hkl_lattice_beta_get(lattice));
		gamma = hkl_parameter_value_unit_get(hkl_lattice_gamma_get(lattice));

		gtk_list_store_set(priv->_liststore_crystals,
				   iter,
				   SAMPLE_COL_NAME, hkl_sample_name_get(sample),
				   SAMPLE_COL_A, a,
				   SAMPLE_COL_B, b,
				   SAMPLE_COL_C, c,
				   SAMPLE_COL_ALPHA, alpha,
				   SAMPLE_COL_BETA, beta,
				   SAMPLE_COL_GAMMA, gamma,
				   -1);
		diffractometer_get_pseudo(priv->diffractometer);

		return TRUE;
	}
	return FALSE;
}

static void
update_crystal_model(HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_crystals),
			       _update_crystal_model,
			       self);
}

/* apply crystal parameters */
void
hkl_gui_window_button2_clicked_cb (GtkButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	g_return_if_fail (self != NULL);

	if (priv->sample != NULL) {
		gdouble a, b, c, alpha, beta, gamma;
		gdouble ux, uy, uz;
		HklLattice *lattice;
		HklParameter *p;

		fprintf(stderr, "%s\n", __func__);
		/* lattice parameters */
		a = gtk_spin_button_get_value (priv->_spinbutton_a);
		b = gtk_spin_button_get_value (priv->_spinbutton_b);
		c = gtk_spin_button_get_value (priv->_spinbutton_c);
		alpha = gtk_spin_button_get_value (priv->_spinbutton_alpha);
		beta = gtk_spin_button_get_value (priv->_spinbutton_beta);
		gamma = gtk_spin_button_get_value (priv->_spinbutton_gamma);

		lattice = hkl_lattice_new(a, b, c,
					  alpha * HKL_DEGTORAD,
					  beta * HKL_DEGTORAD,
					  gamma * HKL_DEGTORAD);

		get_lattice_parameter(lattice, a);
		get_lattice_parameter(lattice, b);
		get_lattice_parameter(lattice, c);
		get_lattice_parameter(lattice, alpha);
		get_lattice_parameter(lattice, beta);
		get_lattice_parameter(lattice, gamma);

		hkl_sample_lattice_set(priv->sample, lattice);

		hkl_lattice_free(lattice);

		/* UB */
		get_ux_uy_uz(priv->sample, ux);
		get_ux_uy_uz(priv->sample, uy);
		get_ux_uy_uz(priv->sample, uz);

		update_crystal_model (self);
		update_reciprocal_lattice (self);
		update_UB (self);
		update_pseudo_axes (self);
		update_pseudo_axes_frames (self);
	}
}



/*


static void _hkl_gui_window_on_spinbutton_lambda_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_lambda_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_uxuyuz_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_uxuyuz_value_changed (self);

}


static void _hkl_gui_window_on_checkbutton_a_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_a_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_b_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_b_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_c_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_c_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_alpha_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_alpha_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_beta_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_beta_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_gamma_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_gamma_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_Ux_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_Ux_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_Uy_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_Uy_toggled (self);

}


static void _hkl_gui_window_on_checkbutton_Uz_toggled_gtk_toggle_button_toggled (GtkToggleButton* _sender, gpointer self) {

	hkl_gui_window_on_checkbutton_Uz_toggled (self);

}


static void _hkl_gui_window_on_tree_view_pseudo_axes_cursor_changed_gtk_tree_view_cursor_changed (GtkTreeView* _sender, gpointer self) {

	hkl_gui_window_on_tree_view_pseudo_axes_cursor_changed (self);

}


static gboolean _hkl_gui_window_on_tree_view_crystals_key_press_event_gtk_widget_key_press_event (GtkWidget* _sender, GdkEventKey* event, gpointer self) {
	gboolean result;
	result = hkl_gui_window_on_tree_view_crystals_key_press_event (event, self);

	return result;

}




static void _hkl_gui_window_on_toolbutton_setUB_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_setUB_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_computeUB_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_computeUB_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_affiner_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_affiner_clicked (self);

}


static void _hkl_gui_window_on_menuitem5_activate_gtk_menu_item_activate (GtkMenuItem* _sender, gpointer self) {

	hkl_gui_window_on_menuitem5_activate (self);

}


static void _hkl_gui_window_on_button1_clicked_gtk_button_clicked (GtkButton* _sender, gpointer self) {

	hkl_gui_window_on_button1_clicked (self);

}


static void _hkl_gui_window_on_combobox1_changed_gtk_combo_box_changed (GtkComboBox* _sender, gpointer self) {

	hkl_gui_window_on_combobox1_changed (self);
}

static void _hkl_gui_window_on_cell_tree_view_pseudo_axes_is_initialized_toggled_gtk_cell_renderer_toggle_toggled (GtkCellRendererToggle* _sender, const gchar* path, gpointer self) {

	hkl_gui_window_on_cell_tree_view_pseudo_axes_is_initialized_toggled (path, self);

}


static void _hkl_gui_window_on_cell_tree_view_pseudo_axes_parameters_value_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_pseudo_axes_parameters_value_edited (path, new_text, self);

}


static void hkl_gui_window_set_up_tree_view_pseudo_axes_parameters (HklGuiWindow* self) {
	GtkCellRendererText* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GtkTreeView* _tmp0_;
	GList* _tmp1_ = NULL;
	GList* columns;
	GList* _tmp2_;
	GtkCellRendererText* _tmp5_;
	GtkCellRendererText* _tmp6_;
	GtkTreeViewColumn* _tmp7_;
	GtkTreeView* _tmp8_;
	GtkTreeViewColumn* _tmp9_;
	GtkCellRendererText* _tmp10_;
	GtkCellRendererText* _tmp11_;
	GtkCellRendererText* _tmp12_;
	GtkCellRendererText* _tmp13_;
	GtkTreeViewColumn* _tmp14_;
	GtkTreeView* _tmp15_;
	GtkTreeViewColumn* _tmp16_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_treeview_pseudo_axes_parameters;

	_tmp1_ = gtk_tree_view_get_columns (_tmp0_);

	columns = _tmp1_;

	_tmp2_ = columns;

	{
		GList* col_collection = NULL;
		GList* col_it = NULL;

		col_collection = _tmp2_;

		for (col_it = col_collection; col_it != NULL; col_it = col_it->next) {

			GtkTreeViewColumn* col = NULL;

			col = (GtkTreeViewColumn*) col_it->data;

			{
				GtkTreeView* _tmp3_;
				GtkTreeViewColumn* _tmp4_;

				_tmp3_ = priv->_treeview_pseudo_axes_parameters;

				_tmp4_ = col;

				gtk_tree_view_remove_column (_tmp3_, _tmp4_);

			}
		}
	}

	_tmp5_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp5_);

	_g_object_unref0 (renderer);

	renderer = _tmp5_;

	_tmp6_ = renderer;

	_tmp7_ = gtk_tree_view_column_new_with_attributes ("name", (GtkCellRenderer*) _tmp6_, "text", PARAMETER_COL_NAME, NULL);

	g_object_ref_sink (_tmp7_);

	_g_object_unref0 (column);

	column = _tmp7_;

	_tmp8_ = priv->_treeview_pseudo_axes_parameters;

	_tmp9_ = column;

	gtk_tree_view_append_column (_tmp8_, _tmp9_);

	_tmp10_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp10_);

	_g_object_unref0 (renderer);

	renderer = _tmp10_;

	_tmp11_ = renderer;

	g_signal_connect_object (_tmp11_, "edited", (GCallback) _hkl_gui_window_on_cell_tree_view_pseudo_axes_parameters_value_edited_gtk_cell_renderer_text_edited, self, 0);

	_tmp12_ = renderer;

	g_object_set (_tmp12_, "editable", TRUE, NULL);

	_tmp13_ = renderer;

	_tmp14_ = gtk_tree_view_column_new_with_attributes ("read", (GtkCellRenderer*) _tmp13_, "text", PARAMETER_COL_VALUE, NULL);

	g_object_ref_sink (_tmp14_);

	_g_object_unref0 (column);

	column = _tmp14_;

	_tmp15_ = priv->_treeview_pseudo_axes_parameters;

	_tmp16_ = column;

	gtk_tree_view_append_column (_tmp15_, _tmp16_);

	_g_list_free0 (columns);

	_g_object_unref0 (column);

	_g_object_unref0 (renderer);

}


static void _hkl_gui_window_on_tree_view1_cursor_changed_gtk_tree_view_cursor_changed (GtkTreeView* _sender, gpointer self) {

	hkl_gui_window_on_tree_view1_cursor_changed (self);

}







static void hkl_gui_window_set_up_3D (HklGuiWindow* self) {
	HklGeometry* _tmp0_;
	HklGeometryConfig* _tmp1_;
	HklGeometryType _tmp2_;
	GtkVBox* _tmp7_;
	HklGui3DFrame* _tmp8_;
	GtkFrame* _tmp9_ = NULL;
	GtkFrame* _tmp10_;
	GtkVBox* _tmp11_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->geometry;

	_tmp1_ = _tmp0_->config;

	_tmp2_ = (*_tmp1_).type;

	switch (_tmp2_) {

	case HKL_GEOMETRY_TYPE_KAPPA6C:

	{
		HklGeometry* _tmp3_;
		HklGui3DFrame* _tmp4_;

		_tmp3_ = priv->geometry;

		_tmp4_ = hkl_gui_3d_frame_new ("../data/diffabs.yaml", _tmp3_);

		_g_object_unref0 (priv->Frame3D);

		priv->Frame3D = _tmp4_;

		break;

	}

	case HKL_GEOMETRY_TYPE_KAPPA4C_VERTICAL:

	{
		HklGeometry* _tmp5_;
		HklGui3DFrame* _tmp6_;

		_tmp5_ = priv->geometry;

		_tmp6_ = hkl_gui_3d_frame_new ("../data/cristal4C.yaml", _tmp5_);

		_g_object_unref0 (priv->Frame3D);

		priv->Frame3D = _tmp6_;

		break;

	}
	default:

		break;

	}

	_tmp7_ = priv->_vbox7;

	_tmp8_ = priv->Frame3D;

	_tmp9_ = hkl_gui_3d_frame_frame (_tmp8_);

	_tmp10_ = _tmp9_;

	gtk_box_pack_start ((GtkBox*) _tmp7_, (GtkWidget*) _tmp10_, TRUE, TRUE, (guint) 0);

	_g_object_unref0 (_tmp10_);

	_tmp11_ = priv->_vbox7;

	gtk_widget_show_all ((GtkWidget*) _tmp11_);

}



static void hkl_gui_window_update_pseudo_axes_parameters (HklGuiWindow* self) {
	GeeHashMap* _tmp0_;
	GeeMapIterator* _tmp1_ = NULL;
	GeeMapIterator* iter;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->hash_store_pseudo_axis_parameter;

	_tmp1_ = gee_abstract_map_map_iterator ((GeeAbstractMap*) _tmp0_);

	iter = _tmp1_;

	while (TRUE) {

		GeeMapIterator* _tmp2_;
		gboolean _tmp3_ = FALSE;
		GtkListStore* model = NULL;
		GtkTreeIter iter2 = {0};
		gboolean valid = FALSE;
		GeeMapIterator* _tmp4_;
		gpointer _tmp5_ = NULL;
		GtkListStore* _tmp6_;
		GtkTreeIter _tmp7_ = {0};
		gboolean _tmp8_ = FALSE;

		_tmp2_ = iter;

		_tmp3_ = gee_map_iterator_next (_tmp2_);

		if (!_tmp3_) {

			break;

		}

		_tmp4_ = iter;

		_tmp5_ = gee_map_iterator_get_value (_tmp4_);

		_g_object_unref0 (model);

		model = (GtkListStore*) _tmp5_;

		_tmp6_ = model;

		_tmp8_ = gtk_tree_model_get_iter_first ((GtkTreeModel*) _tmp6_, &_tmp7_);

		iter2 = _tmp7_;

		valid = _tmp8_;

		while (TRUE) {

			gboolean _tmp9_;
			HklParameter* parameter = NULL;
			GtkListStore* _tmp10_;
			GtkTreeIter _tmp11_;
			GtkListStore* _tmp12_;
			GtkTreeIter _tmp13_;
			HklParameter* _tmp14_;
			const gchar* _tmp15_;
			HklParameter* _tmp16_;
			HklParameter _tmp17_;
			gdouble _tmp18_ = 0.0;
			GtkListStore* _tmp19_;
			gboolean _tmp20_ = FALSE;

			_tmp9_ = valid;

			if (!_tmp9_) {

				break;

			}

			_tmp10_ = model;

			_tmp11_ = iter2;

			gtk_tree_model_get ((GtkTreeModel*) _tmp10_, &_tmp11_, PARAMETER_COL_PARAMETER, &parameter, -1);

			_tmp12_ = model;

			_tmp13_ = iter2;

			_tmp14_ = parameter;

			_tmp15_ = (*_tmp14_).name;

			_tmp16_ = parameter;

			_tmp17_ = *_tmp16_;

			_tmp18_ = hkl_parameter_get_value_unit (&_tmp17_);

			gtk_list_store_set (_tmp12_, &_tmp13_, PARAMETER_COL_NAME, _tmp15_, PARAMETER_COL_VALUE, _tmp18_, -1);

			_tmp19_ = model;

			_tmp20_ = gtk_tree_model_iter_next ((GtkTreeModel*) _tmp19_, &iter2);

			valid = _tmp20_;

		}

		_g_object_unref0 (model);

	}

	_g_object_unref0 (iter);

}










static void hkl_gui_window_on_tree_view_pseudo_axes_cursor_changed (HklGuiWindow* self) {
	GtkTreePath* path = NULL;
	GtkTreeViewColumn* focus_column = NULL;
	GtkListStore* model = NULL;
	GtkTreeIter iter = {0};
	HklPseudoAxis* pseudoAxis = NULL;
	GtkTreeView* _tmp0_;
	GtkTreePath* _tmp1_ = NULL;
	GtkTreeViewColumn* _tmp2_ = NULL;
	GtkTreeViewColumn* _tmp3_;
	GtkListStore* _tmp4_;
	GtkTreeIter _tmp5_ = {0};
	GtkListStore* _tmp6_;
	GtkTreeIter _tmp7_;
	GeeHashMap* _tmp8_;
	gpointer _tmp9_ = NULL;
	GtkTreeView* _tmp10_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_treeview_pseudo_axes;

	gtk_tree_view_get_cursor (_tmp0_, &_tmp1_, &_tmp2_);

	_gtk_tree_path_free0 (path);

	path = _tmp1_;

	_g_object_unref0 (focus_column);

	_tmp3_ = _g_object_ref0 (_tmp2_);

	focus_column = _tmp3_;

	_tmp4_ = priv->store_pseudo_axis;

	gtk_tree_model_get_iter ((GtkTreeModel*) _tmp4_, &_tmp5_, path);

	iter = _tmp5_;

	_tmp6_ = priv->store_pseudo_axis;

	_tmp7_ = iter;

	gtk_tree_model_get ((GtkTreeModel*) _tmp6_, &_tmp7_, PSEUDO_AXIS_COL_PSEUDOAXIS, &pseudoAxis, -1);

	_tmp8_ = priv->hash_store_pseudo_axis_parameter;

	_tmp9_ = gee_abstract_map_get ((GeeAbstractMap*) _tmp8_, pseudoAxis);

	_g_object_unref0 (model);

	model = (GtkListStore*) _tmp9_;

	_tmp10_ = priv->_treeview_pseudo_axes_parameters;

	gtk_tree_view_set_model (_tmp10_, (GtkTreeModel*) model);

	_g_object_unref0 (model);

	_g_object_unref0 (focus_column);

	_gtk_tree_path_free0 (path);

}

static void hkl_gui_window_on_spinbutton_lambda_value_changed (HklGuiWindow* self) {
	HklGeometry* _tmp0_;
	GtkSpinButton* _tmp1_;
	gdouble _tmp2_ = 0.0;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->geometry;

	_tmp1_ = priv->_spinbutton_lambda;

	_tmp2_ = gtk_spin_button_get_value (_tmp1_);

	_tmp0_->source.wave_length = _tmp2_;

	hkl_gui_window_update_pseudo_axes (self);

	hkl_gui_window_update_pseudo_axes_frames (self);

}


static void hkl_gui_window_on_spinbutton_uxuyuz_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}




static void hkl_gui_window_on_checkbutton_a_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		GtkCheckButton* _tmp6_;
		gboolean _tmp7_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->a;

		_tmp6_ = priv->_checkbutton_a;

		_tmp7_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp6_);

		(*_tmp5_).fit = _tmp7_;

	}
}


static void hkl_gui_window_on_checkbutton_b_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		GtkCheckButton* _tmp6_;
		gboolean _tmp7_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->b;

		_tmp6_ = priv->_checkbutton_b;

		_tmp7_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp6_);

		(*_tmp5_).fit = _tmp7_;

	}
}


static void hkl_gui_window_on_checkbutton_c_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		GtkCheckButton* _tmp6_;
		gboolean _tmp7_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->c;

		_tmp6_ = priv->_checkbutton_c;

		_tmp7_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp6_);

		(*_tmp5_).fit = _tmp7_;

	}
}


static void hkl_gui_window_on_checkbutton_alpha_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		GtkCheckButton* _tmp6_;
		gboolean _tmp7_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->alpha;

		_tmp6_ = priv->_checkbutton_alpha;

		_tmp7_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp6_);

		(*_tmp5_).fit = _tmp7_;

	}
}


static void hkl_gui_window_on_checkbutton_beta_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		GtkCheckButton* _tmp6_;
		gboolean _tmp7_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->beta;

		_tmp6_ = priv->_checkbutton_beta;

		_tmp7_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp6_);

		(*_tmp5_).fit = _tmp7_;

	}
}


static void hkl_gui_window_on_checkbutton_gamma_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		GtkCheckButton* _tmp6_;
		gboolean _tmp7_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->gamma;

		_tmp6_ = priv->_checkbutton_gamma;

		_tmp7_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp6_);

		(*_tmp5_).fit = _tmp7_;

	}
}


static void hkl_gui_window_on_checkbutton_Ux_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklParameter* _tmp4_;
		GtkCheckButton* _tmp5_;
		gboolean _tmp6_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->ux;

		_tmp5_ = priv->_checkbutton_Ux;

		_tmp6_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp5_);

		(*_tmp4_).fit = _tmp6_;

	}
}


static void hkl_gui_window_on_checkbutton_Uy_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklParameter* _tmp4_;
		GtkCheckButton* _tmp5_;
		gboolean _tmp6_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->uy;

		_tmp5_ = priv->_checkbutton_Uy;

		_tmp6_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp5_);

		(*_tmp4_).fit = _tmp6_;

	}
}


static void hkl_gui_window_on_checkbutton_Uz_toggled (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;
		HklParameter* _tmp4_;
		GtkCheckButton* _tmp5_;
		gboolean _tmp6_ = FALSE;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->uz;

		_tmp5_ = priv->_checkbutton_Uz;

		_tmp6_ = gtk_toggle_button_get_active ((GtkToggleButton*) _tmp5_);

		(*_tmp4_).fit = _tmp6_;

	}
}

static void hkl_gui_window_on_cell_tree_view_pseudo_axes_is_initialized_toggled (const gchar* path, HklGuiWindow* self) {
	GtkTreeModel* model = NULL;
	GtkTreeIter iter = {0};
	HklPseudoAxis* pseudoAxis = NULL;
	gboolean old_flag = FALSE;
	GtkTreeView* _tmp0_;
	GtkTreeModel* _tmp1_ = NULL;
	GtkTreeModel* _tmp2_;
	GtkTreeModel* _tmp3_;
	const gchar* _tmp4_;
	GtkTreeIter _tmp5_ = {0};
	GtkTreeModel* _tmp6_;
	GtkTreeIter _tmp7_;
	gboolean _tmp8_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	_tmp0_ = priv->_treeview_pseudo_axes;

	_tmp1_ = gtk_tree_view_get_model (_tmp0_);

	_tmp2_ = _g_object_ref0 (_tmp1_);

	_g_object_unref0 (model);

	model = _tmp2_;

	_tmp3_ = model;

	_tmp4_ = path;

	gtk_tree_model_get_iter_from_string (_tmp3_, &_tmp5_, _tmp4_);

	iter = _tmp5_;

	_tmp6_ = model;

	_tmp7_ = iter;

	gtk_tree_model_get (_tmp6_, &_tmp7_, PSEUDO_AXIS_COL_PSEUDOAXIS, &pseudoAxis, PSEUDO_AXIS_COL_INITIALIZED, &old_flag, -1);

	_tmp8_ = old_flag;

	if (!_tmp8_) {

		HklPseudoAxis* _tmp9_;
		HklPseudoAxisEngine* _tmp10_;
		gboolean _tmp11_ = FALSE;

		_tmp9_ = pseudoAxis;

		_tmp10_ = _tmp9_->engine;

		_tmp11_ = hkl_pseudo_axis_engine_initialize (_tmp10_, NULL);

		if (_tmp11_) {

			hkl_gui_window_update_pseudo_axes (self);

		}
	}

	_g_object_unref0 (model);

}


static void hkl_gui_window_on_cell_tree_view_pseudo_axes_parameters_value_edited (const gchar* path, const gchar* new_text, HklGuiWindow* self) {
	GtkListStore* model = NULL;
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter;
	GtkTreeView* _tmp0_;
	GtkTreeModel* _tmp1_ = NULL;
	GtkListStore* _tmp2_;
	const gchar* _tmp3_;
	GtkTreeIter _tmp4_ = {0};
	GtkTreeIter _tmp5_;
	const gchar* _tmp6_;
	gdouble _tmp7_ = 0.0;
	HklParameter _tmp8_;
	GtkTreeIter _tmp9_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	g_return_if_fail (new_text != NULL);

	parameter = NULL;

	_tmp0_ = priv->_treeview_pseudo_axes_parameters;

	_tmp1_ = gtk_tree_view_get_model (_tmp0_);

	_tmp2_ = _g_object_ref0 (G_TYPE_CHECK_INSTANCE_TYPE (_tmp1_, GTK_TYPE_LIST_STORE) ? ((GtkListStore*) _tmp1_) : NULL);

	_g_object_unref0 (model);

	model = _tmp2_;

	_tmp3_ = path;

	gtk_tree_model_get_iter_from_string ((GtkTreeModel*) model, &_tmp4_, _tmp3_);

	iter = _tmp4_;

	_tmp5_ = iter;

	gtk_tree_model_get ((GtkTreeModel*) model, &_tmp5_, PARAMETER_COL_PARAMETER, parameter, -1);

	_tmp6_ = new_text;

	_tmp7_ = double_parse (_tmp6_);

	value = _tmp7_;

	_tmp8_ = *parameter;

	hkl_parameter_set_value_unit (&_tmp8_, value);

	_tmp9_ = iter;

	gtk_list_store_set (model, &_tmp9_, PARAMETER_COL_VALUE, value, -1);

	hkl_gui_window_update_pseudo_axes (self);

	hkl_gui_window_update_pseudo_axes_parameters (self);

	_g_object_unref0 (model);

}








static void _gtk_tree_path_free0_ (gpointer var) {

	(var == NULL) ? NULL : (var = (gtk_tree_path_free (var), NULL));

}


static void _g_list_free__gtk_tree_path_free0_ (GList* self) {

	g_list_foreach (self, (GFunc) _gtk_tree_path_free0_, NULL);

	g_list_free (self);

}




static gpointer _gtk_tree_path_copy0 (gpointer self) {

	return self ? gtk_tree_path_copy (self) : NULL;

}




static void hkl_gui_window_on_toolbutton_setUB_clicked (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	if (_tmp1_ != NULL) {

		HklMatrix UB = {0};
		HklSampleList* _tmp2_;
		HklSample* _tmp3_;
		HklMatrix _tmp4_ = {0};
		HklMatrix _tmp5_;
		gdouble** _tmp6_;
		gint _tmp6__length1;
		gdouble* _tmp7_;
		gint _tmp7__length1;
		GtkSpinButton* _tmp8_;
		gdouble _tmp9_ = 0.0;
		gdouble _tmp10_;
		HklMatrix _tmp11_;
		gdouble** _tmp12_;
		gint _tmp12__length1;
		gdouble* _tmp13_;
		gint _tmp13__length1;
		GtkSpinButton* _tmp14_;
		gdouble _tmp15_ = 0.0;
		gdouble _tmp16_;
		HklMatrix _tmp17_;
		gdouble** _tmp18_;
		gint _tmp18__length1;
		gdouble* _tmp19_;
		gint _tmp19__length1;
		GtkSpinButton* _tmp20_;
		gdouble _tmp21_ = 0.0;
		gdouble _tmp22_;
		HklMatrix _tmp23_;
		gdouble** _tmp24_;
		gint _tmp24__length1;
		gdouble* _tmp25_;
		gint _tmp25__length1;
		GtkSpinButton* _tmp26_;
		gdouble _tmp27_ = 0.0;
		gdouble _tmp28_;
		HklMatrix _tmp29_;
		gdouble** _tmp30_;
		gint _tmp30__length1;
		gdouble* _tmp31_;
		gint _tmp31__length1;
		GtkSpinButton* _tmp32_;
		gdouble _tmp33_ = 0.0;
		gdouble _tmp34_;
		HklMatrix _tmp35_;
		gdouble** _tmp36_;
		gint _tmp36__length1;
		gdouble* _tmp37_;
		gint _tmp37__length1;
		GtkSpinButton* _tmp38_;
		gdouble _tmp39_ = 0.0;
		gdouble _tmp40_;
		HklMatrix _tmp41_;
		gdouble** _tmp42_;
		gint _tmp42__length1;
		gdouble* _tmp43_;
		gint _tmp43__length1;
		GtkSpinButton* _tmp44_;
		gdouble _tmp45_ = 0.0;
		gdouble _tmp46_;
		HklMatrix _tmp47_;
		gdouble** _tmp48_;
		gint _tmp48__length1;
		gdouble* _tmp49_;
		gint _tmp49__length1;
		GtkSpinButton* _tmp50_;
		gdouble _tmp51_ = 0.0;
		gdouble _tmp52_;
		HklMatrix _tmp53_;
		gdouble** _tmp54_;
		gint _tmp54__length1;
		gdouble* _tmp55_;
		gint _tmp55__length1;
		GtkSpinButton* _tmp56_;
		gdouble _tmp57_ = 0.0;
		gdouble _tmp58_;
		HklSampleList* _tmp59_;
		HklSample* _tmp60_;
		HklSampleList* _tmp61_;
		HklSample* _tmp62_;
		FILE* _tmp63_;
		HklSampleList* _tmp64_;
		HklSample* _tmp65_;

		_tmp2_ = priv->samples;

		_tmp3_ = _tmp2_->current;

		hkl_sample_get_UB (_tmp3_, &_tmp4_);

		(&UB);

		UB = _tmp4_;

		_tmp5_ = UB;

		_tmp6_ = _tmp5_.data;

		_tmp6__length1 = -1;

		_tmp7_ = _tmp6_[0];

		_tmp7__length1 = -1;

		_tmp8_ = priv->_spinbutton_U11;

		_tmp9_ = gtk_spin_button_get_value (_tmp8_);

		_tmp7_[0] = _tmp9_;

		_tmp10_ = _tmp7_[0];

		_tmp11_ = UB;

		_tmp12_ = _tmp11_.data;

		_tmp12__length1 = -1;

		_tmp13_ = _tmp12_[0];

		_tmp13__length1 = -1;

		_tmp14_ = priv->_spinbutton_U12;

		_tmp15_ = gtk_spin_button_get_value (_tmp14_);

		_tmp13_[1] = _tmp15_;

		_tmp16_ = _tmp13_[1];

		_tmp17_ = UB;

		_tmp18_ = _tmp17_.data;

		_tmp18__length1 = -1;

		_tmp19_ = _tmp18_[0];

		_tmp19__length1 = -1;

		_tmp20_ = priv->_spinbutton_U13;

		_tmp21_ = gtk_spin_button_get_value (_tmp20_);

		_tmp19_[2] = _tmp21_;

		_tmp22_ = _tmp19_[2];

		_tmp23_ = UB;

		_tmp24_ = _tmp23_.data;

		_tmp24__length1 = -1;

		_tmp25_ = _tmp24_[1];

		_tmp25__length1 = -1;

		_tmp26_ = priv->_spinbutton_U21;

		_tmp27_ = gtk_spin_button_get_value (_tmp26_);

		_tmp25_[0] = _tmp27_;

		_tmp28_ = _tmp25_[0];

		_tmp29_ = UB;

		_tmp30_ = _tmp29_.data;

		_tmp30__length1 = -1;

		_tmp31_ = _tmp30_[1];

		_tmp31__length1 = -1;

		_tmp32_ = priv->_spinbutton_U22;

		_tmp33_ = gtk_spin_button_get_value (_tmp32_);

		_tmp31_[1] = _tmp33_;

		_tmp34_ = _tmp31_[1];

		_tmp35_ = UB;

		_tmp36_ = _tmp35_.data;

		_tmp36__length1 = -1;

		_tmp37_ = _tmp36_[1];

		_tmp37__length1 = -1;

		_tmp38_ = priv->_spinbutton_U23;

		_tmp39_ = gtk_spin_button_get_value (_tmp38_);

		_tmp37_[2] = _tmp39_;

		_tmp40_ = _tmp37_[2];

		_tmp41_ = UB;

		_tmp42_ = _tmp41_.data;

		_tmp42__length1 = -1;

		_tmp43_ = _tmp42_[2];

		_tmp43__length1 = -1;

		_tmp44_ = priv->_spinbutton_U31;

		_tmp45_ = gtk_spin_button_get_value (_tmp44_);

		_tmp43_[0] = _tmp45_;

		_tmp46_ = _tmp43_[0];

		_tmp47_ = UB;

		_tmp48_ = _tmp47_.data;

		_tmp48__length1 = -1;

		_tmp49_ = _tmp48_[2];

		_tmp49__length1 = -1;

		_tmp50_ = priv->_spinbutton_U32;

		_tmp51_ = gtk_spin_button_get_value (_tmp50_);

		_tmp49_[1] = _tmp51_;

		_tmp52_ = _tmp49_[1];

		_tmp53_ = UB;

		_tmp54_ = _tmp53_.data;

		_tmp54__length1 = -1;

		_tmp55_ = _tmp54_[2];

		_tmp55__length1 = -1;

		_tmp56_ = priv->_spinbutton_U33;

		_tmp57_ = gtk_spin_button_get_value (_tmp56_);

		_tmp55_[2] = _tmp57_;

		_tmp58_ = _tmp55_[2];

		_tmp59_ = priv->samples;

		_tmp60_ = _tmp59_->current;

		hkl_sample_set_UB (_tmp60_, &UB);

		_tmp61_ = priv->samples;

		_tmp62_ = _tmp61_->current;

		_tmp63_ = stdout;

		hkl_sample_fprintf (_tmp63_, _tmp62_);

		hkl_gui_window_update_lattice (self);

		hkl_gui_window_update_lattice_parameters (self);

		hkl_gui_window_update_reciprocal_lattice (self);

		_tmp64_ = priv->samples;

		_tmp65_ = _tmp64_->current;

		hkl_gui_window_update_crystal_model (self, _tmp65_);

		hkl_gui_window_update_UB (self);

		hkl_gui_window_update_UxUyUz (self);

		hkl_gui_window_update_pseudo_axes (self);

		hkl_gui_window_update_pseudo_axes_frames (self);

		(&UB);

	}
}


static void hkl_gui_window_on_toolbutton_computeUB_clicked (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;

		_tmp3_ = sample;

		hkl_sample_compute_UB_busing_levy (_tmp3_, (gsize) 0, (gsize) 1);

		hkl_gui_window_update_UB (self);

		hkl_gui_window_update_UxUyUz (self);

		hkl_gui_window_update_pseudo_axes (self);

		hkl_gui_window_update_pseudo_axes_frames (self);

	}
}


static void hkl_gui_window_on_toolbutton_affiner_clicked (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;
	HklSample* _tmp4_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		HklSample* _tmp3_;

		_tmp3_ = sample;

		hkl_sample_affine (_tmp3_);

	}

	_tmp4_ = sample;

	hkl_gui_window_update_crystal_model (self, _tmp4_);

	hkl_gui_window_update_lattice (self);

	hkl_gui_window_update_reciprocal_lattice (self);

	hkl_gui_window_update_UB (self);

	hkl_gui_window_update_UxUyUz (self);

}




static gboolean hkl_gui_window_on_tree_view_crystals_key_press_event (GdkEventKey* event, HklGuiWindow* self) {
	gboolean result = FALSE;

	g_return_val_if_fail (self != NULL, FALSE);

	g_return_val_if_fail (event != NULL, FALSE);

	result = TRUE;

	return result;

}


static void hkl_gui_window_on_menuitem5_activate (HklGuiWindow* self) {
	GtkDialog* _tmp0_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_dialog1;

	gtk_widget_show ((GtkWidget*) _tmp0_);

}


void hkl_gui_window_on_button1_clicked (HklGuiWindow* self) {
	GtkDialog* _tmp0_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_dialog1;

	gtk_widget_hide ((GtkWidget*) _tmp0_);

}

*/

static void
hkl_gui_window_class_init (HklGuiWindowClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (class);

	g_type_class_add_private (class, sizeof (HklGuiWindowPrivate));

	/* virtual method */
	gobject_class->finalize = finalize;
}


static void hkl_gui_window_init (HklGuiWindow * self)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);

	priv->diffractometer = NULL;

	darray_init(priv->pseudo_frames);

	priv->sample = hkl_sample_new ("test");
	priv->reciprocal = hkl_lattice_new_default ();

	hkl_gui_window_get_widgets_and_objects_from_ui (self);

	set_up_diffractometer_model (self);

	set_up_tree_view_crystals (self);
}

int main (int argc, char ** argv)
{
	gtk_init (&argc, &argv);

	hkl_gui_window_new ();

	gtk_main ();

	return 0;
}
