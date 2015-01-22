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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <stdlib.h>
#include <string.h>
#include <float.h>
#include <math.h>
#include <stdio.h>

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>

#include "hkl.h"
#include "hkl-gui.h"
#include "hkl-gui-macros.h"
#if HKL3D
# include <gtk/gtkgl.h>
# include "hkl-gui-3d.h"
#endif
#include "hkl-gui-pseudoaxes.h"

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
	PSEUDO_AXIS_COL_IDX = 0,
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
	SOLUTION_COL_HKL_GEOMETRY_LIST_ITEM,
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
	HklFactory *factory;
	HklGeometry *geometry;
	HklDetector *detector;
	HklEngineList *engines;
	HklGeometryList *solutions;
};


static struct diffractometer_t *
create_diffractometer(HklFactory *factory)
{
	struct diffractometer_t *self;

	self = malloc(sizeof(*self));

	self->factory = factory;
	self->geometry = hkl_factory_create_new_geometry (factory);
	self->engines = hkl_factory_create_new_engine_list (factory);
	self->detector = hkl_detector_factory_new (HKL_DETECTOR_TYPE_0D);
	self->solutions = NULL;

	return self;
}

static void
delete_diffractometer(struct diffractometer_t *self)
{
	hkl_geometry_free(self->geometry);
	hkl_engine_list_free(self->engines);
	hkl_detector_free(self->detector);
	if(self->solutions)
		hkl_geometry_list_free(self->solutions);
}


static void
dump_diffractometer(struct diffractometer_t *self)
{
	/* hkl_geometry_fprintf(stderr, self->geometry); */
	/* hkl_engine_list_fprintf(stderr, self->engines); */
	/* hkl_detector_fprintf(stderr, self->detector); */
}

static void
diffractometer_set_sample(struct diffractometer_t *self,
			  HklSample *sample)
{
	hkl_engine_list_init(self->engines,
			     self->geometry,
			     self->detector,
			     sample);
	hkl_engine_list_get(self->engines);
}

static void
diffractometer_set_wavelength(struct diffractometer_t *self,
			      double wavelength)
{
	if(hkl_geometry_wavelength_set(self->geometry,
				       wavelength, HKL_UNIT_USER, NULL))
		hkl_engine_list_get(self->engines);
}

static gboolean
diffractometer_set_solutions(struct diffractometer_t *self, HklGeometryList *solutions)
{
	if(solutions){
		if(self->solutions)
			hkl_geometry_list_free(self->solutions);
		self->solutions = solutions;
	}

	return NULL != solutions;
}

static gboolean
diffractometer_pseudo_axis_values_set(struct diffractometer_t *self,
				      HklEngine *engine, gdouble values[], guint n_values,
				      GError **error)
{
	HklGeometryList *solutions;


	solutions = hkl_engine_pseudo_axis_values_set(engine, values, n_values, HKL_UNIT_USER, error);

	return diffractometer_set_solutions(self, solutions);
}

static void
diffractometer_set_solution(struct diffractometer_t *self,
			    const HklGeometryListItem *item)
{
	hkl_engine_list_select_solution(self->engines, item);
}


/****************/
/* HklGuiWindow */
/****************/

struct _HklGuiWindowPrivate {
	GtkBuilder* builder;
	GtkLabel* label_UB11;
	GtkLabel* label_UB12;
	GtkLabel* label_UB13;
	GtkLabel* label_UB21;
	GtkLabel* label_UB22;
	GtkLabel* label_UB23;
	GtkLabel* label_UB31;
	GtkLabel* label_UB32;
	GtkLabel* label_UB33;
	GtkButton* button2;
	GtkSpinButton* spinbutton_a;
	GtkSpinButton* spinbutton_b;
	GtkSpinButton* spinbutton_c;
	GtkSpinButton* spinbutton_alpha;
	GtkSpinButton* spinbutton_beta;
	GtkSpinButton* spinbutton_gamma;
	GtkSpinButton* spinbutton_a_min;
	GtkSpinButton* spinbutton_b_min;
	GtkSpinButton* spinbutton_c_min;
	GtkSpinButton* spinbutton_alpha_min;
	GtkSpinButton* spinbutton_beta_min;
	GtkSpinButton* spinbutton_gamma_min;
	GtkSpinButton* spinbutton_a_max;
	GtkSpinButton* spinbutton_b_max;
	GtkSpinButton* spinbutton_c_max;
	GtkSpinButton* spinbutton_alpha_max;
	GtkSpinButton* spinbutton_beta_max;
	GtkSpinButton* spinbutton_gamma_max;
	GtkSpinButton* spinbutton_lambda;
	GtkSpinButton* spinbutton_a_star;
	GtkSpinButton* spinbutton_b_star;
	GtkSpinButton* spinbutton_c_star;
	GtkSpinButton* spinbutton_alpha_star;
	GtkSpinButton* spinbutton_beta_star;
	GtkSpinButton* spinbutton_gamma_star;
	GtkSpinButton* spinbutton_ux;
	GtkSpinButton* spinbutton_uy;
	GtkSpinButton* spinbutton_uz;
	GtkSpinButton* spinbutton_U11;
	GtkSpinButton* spinbutton_U12;
	GtkSpinButton* spinbutton_U13;
	GtkSpinButton* spinbutton_U21;
	GtkSpinButton* spinbutton_U22;
	GtkSpinButton* spinbutton_U23;
	GtkSpinButton* spinbutton_U31;
	GtkSpinButton* spinbutton_U32;
	GtkSpinButton* spinbutton_U33;
	GtkCheckButton* checkbutton_a;
	GtkCheckButton* checkbutton_b;
	GtkCheckButton* checkbutton_c;
	GtkCheckButton* checkbutton_alpha;
	GtkCheckButton* checkbutton_beta;
	GtkCheckButton* checkbutton_gamma;
	GtkCheckButton* checkbutton_ux;
	GtkCheckButton* checkbutton_uy;
	GtkCheckButton* checkbutton_uz;
	GtkTreeView* treeview_reflections;
	GtkTreeView* treeview_crystals;
	GtkTreeView* treeview_axes;
	GtkTreeView* treeview_pseudo_axes;
	GtkTreeView* treeview_solutions;
	GtkToolButton* toolbutton_add_reflection;
	GtkToolButton* toolbutton_goto_reflection;
	GtkToolButton* toolbutton_del_reflection;
	GtkToolButton* toolbutton_setUB;
	GtkToolButton* toolbutton_computeUB;
	GtkToolButton* toolbutton_add_crystal;
	GtkToolButton* toolbutton_copy_crystal;
	GtkToolButton* toolbutton_del_crystal;
	GtkToolButton* toolbutton_affiner;
	GtkStatusbar* statusbar;
	GtkImageMenuItem* menuitem5;
	GtkVBox* box_info_bar; /* fake for the infor bar */
	GtkVBox* vbox7;
	GtkVBox* vbox2;
	GtkDialog* dialog1;
	GtkButton* button1;
	GtkComboBox* combobox1;
	GtkListStore* liststore_diffractometer;
	GtkListStore* liststore_axis;
	GtkListStore* liststore_pseudo_axes;
	GtkListStore* liststore_solutions;
	GtkListStore* liststore_reflections;
	GtkListStore* liststore_crystals;

	GtkInfoBar *info_bar;
	GtkLabel *info_message;

	darray(HklGuiEngine *) pseudo_frames;

#if HKL3D
	HklGui3D *frame3d;
#endif
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

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->liststore_diffractometer),
			       finalize_liststore_diffractometer,
			       NULL);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->liststore_crystals),
			       finalize_liststore_samples,
			       NULL);

	G_OBJECT_CLASS (hkl_gui_window_parent_class)->finalize (object);
}

HklGuiWindow* hkl_gui_window_new (void)
{
	return g_object_new (HKL_GUI_TYPE_WINDOW, NULL);
}

static void
hkl_gui_window_get_widgets_and_objects_from_ui (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkBuilder* builder;

	g_return_if_fail (self != NULL);

	priv->builder = builder = gtk_builder_new ();
	get_ui(builder, "ghkl.ui");

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
raise_error(HklGuiWindow *self, GError **error)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (error != NULL);

	/* show an error message */
	gtk_label_set_text (GTK_LABEL (priv->info_message),
			    (*error)->message);
	gtk_info_bar_set_message_type (priv->info_bar,
				       GTK_MESSAGE_ERROR);
	gtk_widget_show (GTK_WIDGET(priv->info_bar));

	g_clear_error(error);
}

static void
clear_error(HklGuiWindow *self, GError **error)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	gtk_widget_hide(GTK_WIDGET(priv->info_bar));
}

static gboolean
_update_axis (GtkTreeModel *model, GtkTreePath *path,
	      GtkTreeIter *iter, gpointer data)
{
	HklGuiWindowPrivate *priv =  HKL_GUI_WINDOW_GET_PRIVATE(data);
	const char *name;
	const HklParameter *axis;
	gdouble value, min, max;

	gtk_tree_model_get (model, iter,
			    AXIS_COL_NAME, &name,
			    -1);

	axis = hkl_geometry_axis_get(priv->diffractometer->geometry, name, NULL);
	hkl_parameter_min_max_get(axis, &min, &max, HKL_UNIT_USER);
	value = hkl_parameter_value_get(axis, HKL_UNIT_USER);

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

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->liststore_axis),
			       _update_axis,
			       self);
}

static gboolean
_update_pseudo_axes (GtkTreeModel *model, GtkTreePath *path,
		     GtkTreeIter *iter, gpointer data)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(data);
	const char *name;
	const HklEngine *engine;
	const HklParameter *pseudo_axis;
	gdouble value, min, max;

	gtk_tree_model_get (model, iter,
			    PSEUDO_AXIS_COL_ENGINE, &engine,
			    PSEUDO_AXIS_COL_NAME, &name,
			    -1);

	pseudo_axis = hkl_engine_pseudo_axis_get(engine, name, NULL);
	hkl_parameter_min_max_get(pseudo_axis, &min, &max, HKL_UNIT_USER);
	value = hkl_parameter_value_get(pseudo_axis, HKL_UNIT_USER);

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

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->liststore_pseudo_axes),
			       _update_pseudo_axes,
			       self);
}

static void
update_solutions (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkTreeIter iter = {0};

	g_return_if_fail (self != NULL);
	g_return_if_fail (priv->diffractometer->solutions != NULL);

	const HklGeometryListItem *item;
	gtk_list_store_clear(priv->liststore_solutions);

	gint n_values = gtk_tree_model_get_n_columns (GTK_TREE_MODEL(priv->liststore_solutions));
	GValue *values = g_new0(GValue, n_values);
	gint *columns = g_new0(gint, n_values);
	gint i;

	/* prepare the GValue before using them */
	g_value_init(&values[SOLUTION_COL_INDEX], G_TYPE_INT);
	g_value_init(&values[SOLUTION_COL_HKL_GEOMETRY_LIST_ITEM], G_TYPE_POINTER);
	for(i=SOLUTION_COL_N_COLUMNS; i<n_values; ++i)
		g_value_init(&values[i], G_TYPE_DOUBLE);

	i=0;
	HKL_GEOMETRY_LIST_FOREACH(item, priv->diffractometer->solutions){
		const HklGeometry *geometry = hkl_geometry_list_item_geometry_get(item);
		unsigned int n_v = darray_size(*hkl_geometry_axis_names_get(geometry));
		double v[n_v];

		hkl_geometry_axis_values_get(geometry, v, n_v, HKL_UNIT_USER);

		g_value_set_int(&values[SOLUTION_COL_INDEX], i);
		g_value_set_pointer(&values[SOLUTION_COL_HKL_GEOMETRY_LIST_ITEM], (gpointer)item);
		columns[SOLUTION_COL_INDEX] = SOLUTION_COL_INDEX;
		columns[SOLUTION_COL_HKL_GEOMETRY_LIST_ITEM] = SOLUTION_COL_HKL_GEOMETRY_LIST_ITEM;

		for(unsigned int j=0; j<n_v; ++j){
			g_value_set_double(&values[SOLUTION_COL_N_COLUMNS + j], v[j]);
			columns[SOLUTION_COL_N_COLUMNS + j] = SOLUTION_COL_N_COLUMNS + j;
		}
		gtk_list_store_insert_with_valuesv(priv->liststore_solutions,
						   &iter, i,
						   columns, values, n_values);
		i++;
	}
	g_free(columns);
	g_free(values);
}

static void
update_source (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	gtk_spin_button_set_value (priv->spinbutton_lambda,
				   hkl_geometry_wavelength_get(priv->diffractometer->geometry,
							       HKL_UNIT_USER));
}

static void
update_reflections (HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	gtk_list_store_clear (priv->liststore_reflections);

	if(priv->sample){
		HklSampleReflection* reflection;
		guint index = 0;

		HKL_SAMPLE_REFLECTIONS_FOREACH(reflection, priv->sample){
			GtkTreeIter iter = {0};
			gdouble h, k, l;
			gboolean flag;

			hkl_sample_reflection_hkl_get(reflection, &h, &k, &l);
			flag = hkl_sample_reflection_flag_get(reflection);

			gtk_list_store_append (priv->liststore_reflections, &iter);

			gtk_list_store_set (priv->liststore_reflections,
					    &iter,
					    REFLECTION_COL_INDEX, index++,
					    REFLECTION_COL_H, h,
					    REFLECTION_COL_K, k,
					    REFLECTION_COL_L, l,
					    REFLECTION_COL_FLAG, flag,
					    REFLECTION_COL_REFLECTION, reflection,
					    -1);
		}
	}
}

static void
update_3d(HklGuiWindow *self)
{
#ifdef HKL3D
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	if(priv->frame3d){
		hkl_gui_3d_is_colliding(priv->frame3d);
		hkl_gui_3d_invalidate(priv->frame3d);
	}
#endif
}

static void
pseudo_axes_frame_changed_cb (HklGuiEngine *gui_engine, HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklEngine *engine;
	GtkListStore *liststore;
	guint n_values;
	GtkTreeIter iter = {0};
	gboolean valid;
	GError *error = NULL;

	g_object_get(gui_engine,
		     "engine", &engine,
		     "liststore", &liststore,
		     NULL);

	n_values = darray_size(*hkl_engine_pseudo_axis_names_get(engine));
	gdouble values[n_values];

	/* extract all the values from the listore */
	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(liststore), &iter);
	while(valid){
		guint it_idx;
		gdouble it_value;

		gtk_tree_model_get (GTK_TREE_MODEL(liststore), &iter,
				    PSEUDO_COL_IDX, &it_idx,
				    PSEUDO_COL_VALUE, &it_value,
				    -1);

		values[it_idx] = it_value;

		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(liststore), &iter);
	}

	if(diffractometer_pseudo_axis_values_set(priv->diffractometer, engine,
						 values, n_values, &error)){
		update_axes (self);
		update_pseudo_axes (self);
		update_pseudo_axes_frames (self);
		update_solutions (self);
		update_3d(self);
	}else
		raise_error(self, &error);

	g_object_unref(liststore);
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
		gtk_container_remove(GTK_CONTAINER(priv->vbox2),
				     GTK_WIDGET (hkl_gui_engine_get_frame (*pseudo)));
		g_object_unref(*pseudo);
	}
	darray_size (priv->pseudo_frames) = 0;

	engines = hkl_engine_list_engines_get (priv->diffractometer->engines);
	darray_foreach (engine, *engines){
		HklGuiEngine *pseudo;

		pseudo = hkl_gui_engine_new (*engine);
		darray_append(priv->pseudo_frames, pseudo);
		gtk_container_add (GTK_CONTAINER (priv->vbox2),
				   GTK_WIDGET (hkl_gui_engine_get_frame(pseudo)));

		g_signal_connect_object (pseudo,
					 "changed",
					 G_CALLBACK(pseudo_axes_frame_changed_cb),
					 self, 0);
	}

	gtk_widget_show_all (GTK_WIDGET (priv->vbox2));
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

		gtk_list_store_append (priv->liststore_diffractometer, &iter);
		gtk_list_store_set (priv->liststore_diffractometer, &iter,
				    DIFFRACTOMETER_COL_NAME, hkl_factory_name_get(factories[i]),
				    DIFFRACTOMETER_COL_FACTORY, factories[i],
				    DIFFRACTOMETER_COL_DIFFRACTOMETER, NULL,
				    -1);
	}
}

static void
set_up_tree_view_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	const darray_string *axes;
	const char **axis;
	GtkCellRenderer* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;
	GtkTreeIter iter = {0};

	gtk_list_store_clear (priv->liststore_axis);

	axes = hkl_geometry_axis_names_get(priv->diffractometer->geometry);
	darray_foreach (axis, *axes){
		gtk_list_store_append (priv->liststore_axis, &iter);
		gtk_list_store_set (priv->liststore_axis, &iter,
				    AXIS_COL_NAME, *axis,
				    -1);
	}

	update_axes (self);
}

static void
set_up_tree_view_pseudo_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	HklParameter **parameter;
	HklEngine **engine;
	const darray_engine *engines;

	gtk_list_store_clear(priv->liststore_pseudo_axes);

	engines = hkl_engine_list_engines_get(priv->diffractometer->engines);
	darray_foreach(engine, *engines){
		const darray_string *pseudo_axes = hkl_engine_pseudo_axis_names_get(*engine);
		GtkTreeIter iter = {0};
		guint idx;

		for(idx=0; idx<darray_size(*pseudo_axes); ++idx){
			gtk_list_store_append (priv->liststore_pseudo_axes, &iter);
			gtk_list_store_set (priv->liststore_pseudo_axes, &iter,
					    PSEUDO_AXIS_COL_IDX, idx,
					    PSEUDO_AXIS_COL_ENGINE, *engine,
					    PSEUDO_AXIS_COL_NAME, darray_item(*pseudo_axes, idx),
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
	const darray_string *axes;
	int i;
	GtkCellRenderer* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;
	GType* types;
	gint n_columns;

	axes = hkl_geometry_axis_names_get(priv->diffractometer->geometry);

	n_columns = SOLUTION_COL_N_COLUMNS + darray_size(*axes);

	/* prepare types for the liststore */
	types = g_new0 (GType, n_columns);

	/* first remove all the columns */
	columns = gtk_tree_view_get_columns (priv->treeview_solutions);
	g_list_foreach(columns, _delete_column, priv->treeview_solutions);
	g_list_free(columns);

	/* now add the index column */
	renderer = gtk_cell_renderer_text_new ();
	column = gtk_tree_view_column_new_with_attributes ("index",
							   renderer, "text",
							   SOLUTION_COL_INDEX, NULL);

	gtk_tree_view_append_column (priv->treeview_solutions, column);

	types[0] = G_TYPE_INT;
	types[1] = G_TYPE_POINTER;

	/* add the axes column */
	for(i=SOLUTION_COL_N_COLUMNS; i<n_columns; ++i){
		const char *axis;

		axis = darray_item(*axes, i - SOLUTION_COL_N_COLUMNS);
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes (axis,
								   renderer, "text",
								   i, NULL);

		gtk_tree_view_append_column (priv->treeview_solutions, column);
		types[i] = G_TYPE_DOUBLE;
	}

	if (priv->liststore_solutions)
		g_object_unref(priv->liststore_solutions);
	priv->liststore_solutions = gtk_list_store_newv (n_columns, types);
	g_free (types);

	gtk_tree_view_set_model (priv->treeview_solutions,
				 GTK_TREE_MODEL(priv->liststore_solutions));

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

	gtk_box_pack_start(GTK_BOX(priv->box_info_bar),
			   GTK_WIDGET(priv->info_bar),
			   TRUE, TRUE, 0);
}

static void
set_up_lambda(HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_object_set(G_OBJECT(priv->spinbutton_lambda),
		     "sensitive", TRUE,
		     NULL);

	gtk_spin_button_set_value(priv->spinbutton_lambda,
				  hkl_geometry_wavelength_get(priv->diffractometer->geometry,
							      HKL_UNIT_USER));
}

static void
set_up_3D (HklGuiWindow* self)
{
#if HKL3D

	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	const char *filename = NULL;
	const char *name = hkl_factory_name_get(priv->diffractometer->factory);

	if(!strcmp("K6C", name))
		filename = "../data/diffabs.yaml";
	else if (!strcmp("K4CV", name))	
		filename = "../data/cristal4C.yaml";

	if(priv->frame3d){
		gtk_widget_destroy(GTK_WIDGET(hkl_gui_3d_frame_get(priv->frame3d)));
		g_object_unref(priv->frame3d);
		priv->frame3d = NULL;
	}

	if (filename){
		priv->frame3d = hkl_gui_3d_new(filename, priv->diffractometer->geometry);

		gtk_box_pack_start (GTK_BOX(priv->vbox7),
				    GTK_WIDGET(hkl_gui_3d_frame_get(priv->frame3d)),
				    TRUE, TRUE, (guint) 0);

		gtk_widget_show_all (GTK_WIDGET(priv->vbox7));
	}
#endif
}

/* select diffractometer */
void
hkl_gui_window_combobox1_changed_cb(GtkComboBox *combobox, gpointer *user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	HklFactory *factory;
	struct diffractometer_t *dif = NULL;

	GtkTreeIter iter = {0};

	if(gtk_combo_box_get_active_iter (combobox, &iter)){
		gtk_tree_model_get(GTK_TREE_MODEL(priv->liststore_diffractometer),
				   &iter,
				   DIFFRACTOMETER_COL_FACTORY, &factory,
				   DIFFRACTOMETER_COL_DIFFRACTOMETER, &dif,
				   -1);

		if (!dif){
			dif = create_diffractometer(factory);
			gtk_list_store_set(priv->liststore_diffractometer,
					   &iter,
					   DIFFRACTOMETER_COL_DIFFRACTOMETER, dif,
					   -1);
		}
	}
	if(dif != priv->diffractometer){
		priv->diffractometer = dif;

		diffractometer_set_sample(dif, priv->sample);

		set_up_lambda(self);
		set_up_pseudo_axes_frames(self);
		set_up_tree_view_axes(self);
		set_up_tree_view_pseudo_axes(self);
		set_up_tree_view_solutions(self);
		set_up_info_bar(self);
		set_up_3D(self);
	}
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
	const char *axis;
	HklParameter *parameter;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->liststore_axis),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_axis), &iter,
					   AXIS_COL_NAME, &axis,
					   -1);

	value = atof(new_text); /* TODO need to check for the right conversion */

	/* set the axis value */
	parameter = hkl_parameter_new_copy(hkl_geometry_axis_get(priv->diffractometer->geometry,
								 axis, NULL));
	hkl_parameter_value_set (parameter, value, HKL_UNIT_USER, NULL);
	hkl_geometry_axis_set(priv->diffractometer->geometry,
			      axis, parameter, NULL);
	hkl_parameter_free(parameter);

	hkl_engine_list_get(priv->diffractometer->engines);

	/* ok so set the model with the new value */
	gtk_list_store_set (priv->liststore_axis, &iter,
			    AXIS_COL_READ, value,
			    AXIS_COL_WRITE, value,
			    -1);

	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
	update_3d(self);
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
	const char *axis;
	HklParameter* parameter = NULL;
	gdouble shit, max;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->liststore_axis),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_axis), &iter,
					   AXIS_COL_NAME, &axis,
					   -1);

	value = atof(new_text); /* TODO need to check for the right conversion */

	parameter = hkl_parameter_new_copy(hkl_geometry_axis_get(priv->diffractometer->geometry,
								 axis, NULL));
	hkl_parameter_min_max_get (parameter, &shit, &max, HKL_UNIT_USER);
	hkl_parameter_min_max_set (parameter, value, max, HKL_UNIT_USER, NULL); /* TODO error */
	hkl_geometry_axis_set(priv->diffractometer->geometry,
			      axis, parameter, NULL);
	hkl_parameter_free(parameter);

	gtk_list_store_set (priv->liststore_axis, &iter,
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
	const char *axis;
	HklParameter* parameter = NULL;
	gdouble shit, min;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->liststore_axis),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_axis), &iter,
					   AXIS_COL_NAME, &axis,
					   -1);

	value = atof(new_text); /* TODO need to check for the right conversion */


	parameter = hkl_parameter_new_copy(hkl_geometry_axis_get(priv->diffractometer->geometry,
								 axis, NULL));
	hkl_parameter_min_max_get (parameter, &min, &shit, HKL_UNIT_USER);
	hkl_parameter_min_max_set (parameter, min, value, HKL_UNIT_USER, NULL);
	hkl_geometry_axis_set(priv->diffractometer->geometry,
			      axis, parameter, NULL);
	hkl_parameter_free(parameter);


	gtk_list_store_set (priv->liststore_axis, &iter,
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
	unsigned int idx;
	HklEngine *engine = NULL;
	gboolean valid;
	GtkTreeIter it = {0};
	guint n_values;
	GError *error = NULL;

	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->liststore_pseudo_axes),
					     &iter,
					     path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_pseudo_axes), &iter,
			    PSEUDO_AXIS_COL_IDX, &idx,
			    PSEUDO_AXIS_COL_ENGINE, &engine,
			    PSEUDO_AXIS_COL_WRITE, &old_value,
			    -1);

	n_values = darray_size(*hkl_engine_pseudo_axis_names_get(engine));
	gdouble values[n_values];

	/* extract all the values from the listore */

	valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(priv->liststore_pseudo_axes), &it);
	while(valid){
		guint it_idx;
		HklEngine *it_engine;
		gdouble it_value;

		gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_pseudo_axes), &it,
				    PSEUDO_AXIS_COL_IDX, &it_idx,
				    PSEUDO_AXIS_COL_ENGINE, &it_engine,
				    PSEUDO_AXIS_COL_WRITE, &it_value,
				    -1);

		if(engine == it_engine)
			values[it_idx] = it_value;

		valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(priv->liststore_pseudo_axes), &it);
	}

	/* replace with the new value */
	value = atof(new_text); /* TODO need to check for the right conversion */
	values[idx] = value;

	if(diffractometer_pseudo_axis_values_set(priv->diffractometer, engine,
						 values, n_values, &error)){
		gtk_list_store_set (priv->liststore_pseudo_axes,
				    &iter,
				    PSEUDO_AXIS_COL_WRITE, value,
				    -1);

		update_axes (self);
		update_pseudo_axes (self);
		update_pseudo_axes_frames (self);
		update_solutions (self);
		update_3d(self);
	}else
		raise_error(self, &error);
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
	const HklGeometryListItem *solution;

	gtk_tree_view_get_cursor (tree_view, &path, &focus_column);
	gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_solutions), &iter, path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_solutions), &iter,
			    SOLUTION_COL_HKL_GEOMETRY_LIST_ITEM, &solution,
			    -1);

	diffractometer_set_solution(priv->diffractometer, solution);

	update_axes (self);
	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
	update_3d(self);

	gtk_tree_path_free (path);
}

/* reflection h k l */
#define HKL_GUI_WINDOW_CELLRENDERERTEXT_HKL_EDITED_CB(_number, _hkl, _HKL) \
	void								\
	hkl_gui_window_cellrenderertext ## _number ## _edited_cb(GtkCellRendererText* _sender, const gchar* path, \
								 const gchar* new_text, gpointer user_data) \
	{								\
		HklGuiWindow *self = HKL_GUI_WINDOW(user_data);		\
		HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data); \
									\
		g_return_if_fail (self != NULL);			\
		g_return_if_fail (path != NULL);			\
		g_return_if_fail (new_text != NULL);			\
									\
		if (priv->sample){					\
			gdouble h = 0.0;				\
			gdouble k = 0.0;				\
			gdouble l = 0.0;				\
			HklSampleReflection* reflection = NULL;		\
			GtkTreeIter iter = {0};				\
			GError *error = NULL;				\
									\
			gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->liststore_reflections), \
							     &iter, path); \
			gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_reflections), \
					    &iter,			\
					    REFLECTION_COL_REFLECTION, &reflection, \
					    -1);			\
									\
			hkl_sample_reflection_hkl_get (reflection, &h, &k, &l);	\
			_hkl = atof(new_text);				\
			if(!hkl_sample_reflection_hkl_set (reflection, h, k, l, NULL)) \
				raise_error(self, &error);		\
			else						\
				gtk_list_store_set (priv->liststore_reflections, \
						    &iter,		\
						    REFLECTION_COL_ ## _HKL, _hkl, \
						    -1);		\
		}							\
	}

HKL_GUI_WINDOW_CELLRENDERERTEXT_HKL_EDITED_CB(7, h, H);
HKL_GUI_WINDOW_CELLRENDERERTEXT_HKL_EDITED_CB(8, k, K);
HKL_GUI_WINDOW_CELLRENDERERTEXT_HKL_EDITED_CB(9, l, L);


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

		gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->liststore_reflections),
						     &iter, path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_reflections),
				    &iter,
				    REFLECTION_COL_REFLECTION, &reflection,
				    -1);

		flag = gtk_cell_renderer_toggle_get_active(renderer);
		hkl_sample_reflection_flag_set (reflection, flag);
		gtk_list_store_set (priv->liststore_reflections,
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
		gtk_statusbar_push (priv->statusbar, 0,
				    "Please select a diffractometer before adding reflections");
		return;
	}

	if (priv->sample) {
		HklSampleReflection *reflection = NULL;
		GtkTreeIter iter = {0};
		gboolean flag;
		gint n_rows;
		GError *error = NULL;

		reflection = hkl_sample_reflection_new(priv->diffractometer->geometry,
						       priv->diffractometer->detector,
						       0, 0, 0, &error);
		if(!reflection)
			raise_error(self, &error);
		else{
			hkl_sample_add_reflection(priv->sample, reflection);
			flag = hkl_sample_reflection_flag_get(reflection);

			n_rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(priv->liststore_reflections),
							NULL);
			gtk_list_store_insert_with_values (priv->liststore_reflections,
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

		selection = gtk_tree_view_get_selection (priv->treeview_reflections);
		nb_rows = gtk_tree_selection_count_selected_rows (selection);

		if (nb_rows == 1) {
			HklSampleReflection *reflection;
			GtkTreeIter iter = {0};
			GtkTreeModel* model = NULL;
			GtkTreePath *treepath;
			GList* list;

			model = GTK_TREE_MODEL(priv->liststore_reflections);

			list = gtk_tree_selection_get_selected_rows (selection,
								     &model);

			treepath = g_list_nth_data(list, 0);

			gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_reflections),
						 &iter, treepath);

			gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_reflections),
					    &iter,
					    REFLECTION_COL_REFLECTION, &reflection,
					    -1);

			hkl_geometry_set (priv->diffractometer->geometry,
					  hkl_sample_reflection_geometry_get(reflection));

			update_source (self);
			update_axes (self);
			update_pseudo_axes (self);
			update_3d(self);

			g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);
		} else
			if (nb_rows > 1)
				gtk_statusbar_push (priv->statusbar, 0,
						    "Please select only one reflection.");
			else
				gtk_statusbar_push (priv->statusbar, 0,
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

	gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_reflections),
				 &iter, treepath);

	gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_reflections),
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

		selection = gtk_tree_view_get_selection (priv->treeview_reflections);
		nb_rows = gtk_tree_selection_count_selected_rows (selection);
		if (nb_rows > 0) {
			GtkTreeModel* model = NULL;
			GList* list;
			guint* indexes;
			gint i;
			GtkMessageDialog* dialog;

			model = GTK_TREE_MODEL(priv->liststore_reflections);
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
			gtk_statusbar_push (priv->statusbar, 0,
					    "Please select at least one reflection.");
		}
	}
}

static void
set_up_tree_view_reflections(HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);
	GtkTreeSelection* selection = NULL;

	selection = gtk_tree_view_get_selection (priv->treeview_reflections);
	gtk_tree_selection_set_mode(selection, GTK_SELECTION_MULTIPLE);
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

	model = GTK_TREE_MODEL(priv->liststore_crystals);

	gtk_tree_model_get_iter_from_string (model, &iter, path);

	gtk_tree_model_get (model, &iter,
			    SAMPLE_COL_SAMPLE, &sample,
			    -1);

	hkl_sample_name_set (sample, new_text);

	gtk_list_store_set(priv->liststore_crystals, &iter,
			   SAMPLE_COL_NAME, new_text,
			   -1);
}

#define set_lattice(lattice, parameter) do{				\
		const HklParameter *p;					\
		gdouble min, max, value;				\
		gboolean fit;						\
		p = hkl_lattice_## parameter ##_get((lattice));		\
		value = hkl_parameter_value_get(p, HKL_UNIT_USER);	\
		hkl_parameter_min_max_get(p, &min, &max, HKL_UNIT_USER); \
		fit = hkl_parameter_fit_get(p);				\
		gtk_spin_button_set_value(priv->spinbutton_## parameter, value); \
		gtk_spin_button_set_value(priv->spinbutton_## parameter ##_min, min); \
		gtk_spin_button_set_value(priv->spinbutton_## parameter ##_max, max); \
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(priv->checkbutton_ ## parameter), fit); \
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
		value = hkl_parameter_value_get(p, HKL_UNIT_USER);	\
		gtk_spin_button_set_value(priv->spinbutton_## parameter ##_star, value); \
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
		gboolean fit = hkl_parameter_fit_get(p);		\
		gtk_spin_button_set_value(priv->spinbutton_## parameter, \
					  hkl_parameter_value_get(p, HKL_UNIT_USER)); \
		gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(priv->checkbutton_## parameter), fit); \
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

#define set_UB(i, j) do{						\
		gtk_label_set_markup(priv->label_UB ## i ## j,		\
				     g_ascii_formatd(text,		\
						     G_ASCII_DTOSTR_BUF_SIZE, \
						     "<tt> %+.4f </tt>",	\
						     hkl_matrix_get(UB, i - 1, j - 1))); \
	}while(0)

static void
update_UB (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	g_return_if_fail (self != NULL);

	if (priv->sample != NULL) {
		const HklMatrix *UB = hkl_sample_UB_get (priv->sample);
		gchar *text = g_new0 (gchar, G_ASCII_DTOSTR_BUF_SIZE);

		set_UB(1, 1);
		set_UB(1, 2);
		set_UB(1, 3);
		set_UB(2, 1);
		set_UB(2, 2);
		set_UB(2, 3);
		set_UB(3, 1);
		set_UB(3, 2);
		set_UB(3, 3);

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

	gtk_tree_view_get_cursor (priv->treeview_crystals, &path, &column);
	if(path){
		if (gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_crystals),
					     &iter, path) == TRUE){
			gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_crystals),
					    &iter,
					    SAMPLE_COL_SAMPLE, &sample,
					    -1);

			if(sample && sample != priv->sample){
				priv->sample = sample;

				update_reflections(self);
				update_lattice(self);
				update_reciprocal_lattice (self);
				update_ux_uy_uz (self);
				update_UB (self);

				if(priv->diffractometer){
					diffractometer_set_sample(priv->diffractometer,
								  priv->sample);

					update_pseudo_axes (self);
					update_pseudo_axes_frames (self);
					update_solutions(self);
				}
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
	a = hkl_parameter_value_get(hkl_lattice_a_get(lattice), HKL_UNIT_USER);
	b = hkl_parameter_value_get(hkl_lattice_b_get(lattice), HKL_UNIT_USER);
	c = hkl_parameter_value_get(hkl_lattice_c_get(lattice), HKL_UNIT_USER);
	alpha = hkl_parameter_value_get(hkl_lattice_alpha_get(lattice),
					HKL_UNIT_USER);
	beta = hkl_parameter_value_get(hkl_lattice_beta_get(lattice),
				       HKL_UNIT_USER);
	gamma = hkl_parameter_value_get(hkl_lattice_gamma_get(lattice),
					HKL_UNIT_USER);

	gtk_list_store_insert_with_values(priv->liststore_crystals,
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

	iter = _add_sample(self, hkl_sample_new("default"));

	path = gtk_tree_model_get_path(GTK_TREE_MODEL(priv->liststore_crystals),
				       &iter);

	gtk_tree_view_set_cursor(priv->treeview_crystals, path, NULL, FALSE);

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

	path = gtk_tree_model_get_path(GTK_TREE_MODEL(priv->liststore_crystals),
				       &iter);
	column = gtk_tree_view_get_column (priv->treeview_crystals, 0);
	gtk_tree_view_set_cursor (priv->treeview_crystals, path, column, TRUE);

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
		gtk_statusbar_push (priv->statusbar, (guint) 0, "Please select a crystal to copy.");
}

void
hkl_gui_window_toolbutton_del_crystal_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	g_return_if_fail (user_data != NULL);

	if (priv->sample != NULL) {
		guint n_rows;

		n_rows = gtk_tree_model_iter_n_children(GTK_TREE_MODEL(priv->liststore_crystals),
							NULL );
		if (n_rows == 1)
			return;
		else {
			GtkTreeIter iter = {0};
			GtkTreePath *path = NULL;
			GtkTreeViewColumn *column = NULL;

			gtk_tree_view_get_cursor(priv->treeview_crystals,
						 &path, &column);
			if (path){
				if (gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_crystals),
							     &iter, path) == TRUE) {
					gtk_tree_path_free(path);

					hkl_sample_free(priv->sample);
					if (gtk_list_store_remove(priv->liststore_crystals,
								  &iter) == TRUE){
						path = gtk_tree_model_get_path(GTK_TREE_MODEL(priv->liststore_crystals),
									       &iter);
						gtk_tree_view_set_cursor(priv->treeview_crystals,
									 path, NULL, FALSE);
					}
				}
			}
		}
	}
}

#define get_lattice_parameter(lattice, parameter, _error) do{		\
		HklParameter *p = hkl_parameter_new_copy(hkl_lattice_## parameter ##_get(lattice)); \
		if(!hkl_parameter_min_max_set(p,			\
					      gtk_spin_button_get_value(priv->spinbutton_## parameter ##_min), \
					      gtk_spin_button_get_value(priv->spinbutton_## parameter ##_max),	\
					      HKL_UNIT_USER, _error)){	\
			raise_error(self, _error);			\
			hkl_parameter_free(p);				\
			return;						\
		}else{							\
			hkl_parameter_fit_set(p, gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(priv->checkbutton_## parameter))); \
			if(!hkl_lattice_ ## parameter ## _set(lattice, p, _error)){ \
				raise_error(self, _error);		\
				hkl_parameter_free(p);			\
				return;					\
			}						\
		}							\
		hkl_parameter_free(p);					\
	} while(0)

#define get_ux_uy_uz(sample, parameter, _error) do {			\
		HklParameter *p;					\
		p = hkl_parameter_new_copy(hkl_sample_## parameter ##_get(sample)); \
		if(!hkl_parameter_value_set(p,				\
					    gtk_spin_button_get_value (priv->spinbutton_## parameter), \
					    HKL_UNIT_USER, _error)){	\
			raise_error(self, _error);			\
			hkl_parameter_free(p);				\
			return;						\
		}else{							\
			hkl_parameter_fit_set(p,			\
					      gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(priv->checkbutton_## parameter))); \
			if(!hkl_sample_ ## parameter ## _set(sample, p, _error)){ \
				raise_error(self, _error);		\
				hkl_parameter_free(p);			\
				return;					\
			}						\
		}							\
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
		a = hkl_parameter_value_get(hkl_lattice_a_get(lattice),
					    HKL_UNIT_USER);
		b = hkl_parameter_value_get(hkl_lattice_b_get(lattice),
					    HKL_UNIT_USER);
		c = hkl_parameter_value_get(hkl_lattice_c_get(lattice),
					    HKL_UNIT_USER);
		alpha = hkl_parameter_value_get(hkl_lattice_alpha_get(lattice),
						HKL_UNIT_USER);
		beta = hkl_parameter_value_get(hkl_lattice_beta_get(lattice),
					       HKL_UNIT_USER);
		gamma = hkl_parameter_value_get(hkl_lattice_gamma_get(lattice),
						HKL_UNIT_USER);

		gtk_list_store_set(priv->liststore_crystals,
				   iter,
				   SAMPLE_COL_NAME, hkl_sample_name_get(sample),
				   SAMPLE_COL_A, a,
				   SAMPLE_COL_B, b,
				   SAMPLE_COL_C, c,
				   SAMPLE_COL_ALPHA, alpha,
				   SAMPLE_COL_BETA, beta,
				   SAMPLE_COL_GAMMA, gamma,
				   -1);
		return TRUE;
	}
	return FALSE;
}

static void
update_crystal_model(HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(self);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->liststore_crystals),
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
		GError *error = NULL;

		fprintf(stderr, "%s\n", __func__);
		/* lattice parameters */
		a = gtk_spin_button_get_value (priv->spinbutton_a);
		b = gtk_spin_button_get_value (priv->spinbutton_b);
		c = gtk_spin_button_get_value (priv->spinbutton_c);
		alpha = gtk_spin_button_get_value (priv->spinbutton_alpha);
		beta = gtk_spin_button_get_value (priv->spinbutton_beta);
		gamma = gtk_spin_button_get_value (priv->spinbutton_gamma);

		lattice = hkl_lattice_new(a, b, c,
					  alpha * HKL_DEGTORAD,
					  beta * HKL_DEGTORAD,
					  gamma * HKL_DEGTORAD, &error);
		if(!lattice)
			raise_error(self, &error);
		else{

			get_lattice_parameter(lattice, a, &error);
			get_lattice_parameter(lattice, b, &error);
			get_lattice_parameter(lattice, c, &error);
			get_lattice_parameter(lattice, alpha, &error);
			get_lattice_parameter(lattice, beta, &error);
			get_lattice_parameter(lattice, gamma, &error);

			hkl_sample_lattice_set(priv->sample, lattice);

			hkl_lattice_free(lattice);

			/* UB */
			get_ux_uy_uz(priv->sample, ux, &error);
			get_ux_uy_uz(priv->sample, uy, &error);
			get_ux_uy_uz(priv->sample, uz, &error);

			if(priv->diffractometer)
				diffractometer_set_sample(priv->diffractometer,
							  priv->sample);

			update_crystal_model (self);
			update_reciprocal_lattice (self);
			update_UB (self);
			update_pseudo_axes (self);
			update_pseudo_axes_frames (self);
		}
	}
}

void
hkl_gui_window_spinbutton_lambda_value_changed_cb (GtkSpinButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	diffractometer_set_wavelength(priv->diffractometer,
				      gtk_spin_button_get_value(_sender));
	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
}

void
hkl_gui_window_spinbutton_ux_value_changed_cb (GtkSpinButton *_senser, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GError *error = NULL;

	get_ux_uy_uz(priv->sample, ux, &error);

	if(priv->diffractometer)
		diffractometer_set_sample(priv->diffractometer,
					  priv->sample);

	update_UB (self);
	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
}

void
hkl_gui_window_spinbutton_uy_value_changed_cb (GtkSpinButton *_senser, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GError *error = NULL;

	get_ux_uy_uz(priv->sample, uy, &error);

	if(priv->diffractometer)
		diffractometer_set_sample(priv->diffractometer,
					  priv->sample);

	update_UB (self);
	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
}

void
hkl_gui_window_spinbutton_uz_value_changed_cb (GtkSpinButton *_senser, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GError *error = NULL;

	get_ux_uy_uz(priv->sample, uz, &error);

	if(priv->diffractometer)
		diffractometer_set_sample(priv->diffractometer,
					  priv->sample);

	update_UB (self);
	update_pseudo_axes (self);
	update_pseudo_axes_frames (self);
}

void
hkl_gui_window_toolbutton_setUB_clicked_cb(GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);

	HklMatrix *UB;
	GError *error = NULL;

	UB = hkl_matrix_new_full(gtk_spin_button_get_value(priv->spinbutton_U11),
				 gtk_spin_button_get_value(priv->spinbutton_U12),
				 gtk_spin_button_get_value(priv->spinbutton_U13),
				 gtk_spin_button_get_value(priv->spinbutton_U21),
				 gtk_spin_button_get_value(priv->spinbutton_U22),
				 gtk_spin_button_get_value(priv->spinbutton_U23),
				 gtk_spin_button_get_value(priv->spinbutton_U31),
				 gtk_spin_button_get_value(priv->spinbutton_U32),
				 gtk_spin_button_get_value(priv->spinbutton_U33));

	if(!hkl_sample_UB_set (priv->sample, UB, &error))
		raise_error(self, &error);
	else{
		if(priv->diffractometer){
			diffractometer_set_sample(priv->diffractometer,
						  priv->sample);

			update_lattice (self);
			update_crystal_model (self);
			update_reciprocal_lattice (self);
			update_UB (self);
			update_ux_uy_uz (self);
			update_pseudo_axes (self);
			update_pseudo_axes_frames (self);
		}
	}

	hkl_matrix_free(UB);
}

void
hkl_gui_window_toolbutton_computeUB_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GtkTreeSelection* selection = NULL;
	guint nb_rows = 0U;

	selection = gtk_tree_view_get_selection (priv->treeview_reflections);
	nb_rows = gtk_tree_selection_count_selected_rows (selection);
	if (nb_rows > 1) {
		GtkTreeModel* model = NULL;
		GList* list;
		GtkTreeIter iter = {0};
		GtkTreePath *path;
		HklSampleReflection *ref1, *ref2;
		GError *error = NULL;

		model = GTK_TREE_MODEL(priv->liststore_reflections);
		list = gtk_tree_selection_get_selected_rows (selection, &model);

		/* get the first reflection */
		path = g_list_nth_data(list, 0);
		gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_reflections),
					 &iter,
					 path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_reflections), &iter,
				    REFLECTION_COL_REFLECTION, &ref1,
				    -1);

		/* get the second one */
		path = g_list_nth_data(list, 1);
		gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->liststore_reflections),
					 &iter,
					 path);
		gtk_tree_model_get (GTK_TREE_MODEL(priv->liststore_reflections), &iter,
				    REFLECTION_COL_REFLECTION, &ref2,
				    -1);

		if(!hkl_sample_compute_UB_busing_levy(priv->sample,
						      ref1, ref2, &error)){
			raise_error(self, &error);
		}else{
			if(priv->diffractometer)
				diffractometer_set_sample(priv->diffractometer,
							  priv->sample);

			update_UB (self);
			update_ux_uy_uz (self);
			update_pseudo_axes (self);
			update_pseudo_axes_frames (self);
		}
		g_list_free_full (list, (GDestroyNotify) gtk_tree_path_free);
	} else {
		gtk_statusbar_push (priv->statusbar, 0,
				    "Please select at least two reflection.");
	}
}

void
hkl_gui_window_toolbutton_affiner_clicked_cb (GtkToolButton* _sender, gpointer user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data);
	GError *error = NULL;

	if(!hkl_sample_affine (priv->sample, &error)){
		raise_error(self, &error);
	}else{
		if(priv->diffractometer)
			diffractometer_set_sample(priv->diffractometer,
						  priv->sample);

		update_lattice (self);
		update_crystal_model (self);
		update_reciprocal_lattice (self);
		update_UB (self);
		update_ux_uy_uz (self);
		update_pseudo_axes (self);
		update_pseudo_axes_frames (self);
	}
}

#define TOGGLE_LATTICE_CB(_parameter)					\
	void hkl_gui_window_checkbutton_ ## _parameter ## _toggled_cb(GtkCheckButton *checkbutton, \
								      gpointer user_data) \
	{								\
		HklGuiWindow *self = HKL_GUI_WINDOW(user_data);		\
		HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data); \
		HklLattice *lattice;					\
		HklParameter *p;					\
		GError *error = NULL;					\
		lattice = hkl_lattice_new_copy(hkl_sample_lattice_get(priv->sample)); \
		p = hkl_parameter_new_copy(hkl_lattice_## _parameter ##_get(lattice)); \
		hkl_parameter_fit_set(p,				\
				      gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(checkbutton))); \
		if(!hkl_lattice_## _parameter ##_set(lattice, p, &error)){ \
			raise_error(self, &error);			\
		}else{							\
			hkl_sample_lattice_set(priv->sample, lattice);	\
		}							\
		hkl_parameter_free(p);					\
		hkl_lattice_free(lattice);				\
	}

TOGGLE_LATTICE_CB(a);
TOGGLE_LATTICE_CB(b);
TOGGLE_LATTICE_CB(c);
TOGGLE_LATTICE_CB(alpha);
TOGGLE_LATTICE_CB(beta);
TOGGLE_LATTICE_CB(gamma);

#define TOGGLE_UX_UY_UZ(_parameter)					\
	void hkl_gui_window_checkbutton_ ## _parameter ## _toggled_cb(GtkCheckButton *checkbutton, \
								      gpointer user_data) \
	{								\
		HklGuiWindow *self = HKL_GUI_WINDOW(user_data);		\
		HklGuiWindowPrivate *priv = HKL_GUI_WINDOW_GET_PRIVATE(user_data); \
		HklParameter *p;					\
		GError *error = NULL;					\
		p = hkl_parameter_new_copy(hkl_sample_ ## _parameter ## _get(priv->sample)); \
		hkl_parameter_fit_set(p,				\
				      gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(checkbutton))); \
		if(!hkl_sample_ ## _parameter ## _set(priv->sample, p, &error)){ \
			raise_error(self, &error);			\
		}							\
		hkl_parameter_free(p);					\
	}

TOGGLE_UX_UY_UZ(ux);
TOGGLE_UX_UY_UZ(uy);
TOGGLE_UX_UY_UZ(uz);

/*

static gboolean _hkl_gui_window_on_tree_view_crystals_key_press_event_gtk_widget_key_press_event (GtkWidget* _sender, GdkEventKey* event, gpointer self) {
	gboolean result;
	result = hkl_gui_window_on_tree_view_crystals_key_press_event (event, self);

	return result;

}

static gboolean hkl_gui_window_on_tree_view_crystals_key_press_event (GdkEventKey* event, HklGuiWindow* self) {
	gboolean result = FALSE;

	g_return_val_if_fail (self != NULL, FALSE);

	g_return_val_if_fail (event != NULL, FALSE);

	result = TRUE;

	return result;

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
	priv->sample = NULL;

	darray_init(priv->pseudo_frames);

	priv->reciprocal = hkl_lattice_new_default ();

	hkl_gui_window_get_widgets_and_objects_from_ui (self);

	set_up_diffractometer_model (self);

	set_up_tree_view_crystals (self);

	set_up_tree_view_reflections(self);
}

int main (int argc, char ** argv)
{
	gtk_init (&argc, &argv);
#ifdef HKL3D
	gtk_gl_init(&argc, &argv);
#endif
	hkl_gui_window_new ();

	gtk_main ();

	return 0;
}
