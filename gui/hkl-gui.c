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

typedef enum  {
	REFLECTION_COL_INDEX = 0,
	REFLECTION_COL_H,
	REFLECTION_COL_K,
	REFLECTION_COL_L,
	REFLECTION_COL_FLAG,
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
	SAMPLE_COL_REFLECTIONS,
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

struct diffractometer_t {
	HklGeometry *geometry;
	HklDetector *detector;
	HklEngineList *engines;
};

static struct diffractometer_t *create_diffractometer(HklFactory *factory);
static void delete_diffractometer(struct diffractometer_t *self);

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
	GtkCheckButton* _checkbutton_Ux;
	GtkCheckButton* _checkbutton_Uy;
	GtkCheckButton* _checkbutton_Uz;
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

	GtkListStore* store_samples;
	GtkInfoBar *info_bar;
	GtkLabel *info_message;

	GList *pseudo_frames;

	struct diffractometer_t *diffractometer; /* unowned */
	HklSample *sample; /* unowned */
	HklLattice *reciprocal;
};

#define SPINBUTTON_VALUE_CHANGED_CB(name) hkl_gui_on_ ## name ## _value_changed_cb
#define SPINBUTTON_VALUE_CHANGED_CB_DECL(name)				\
	static void SPINBUTTON_VALUE_CHANGED_CB(name)(GtkSpinButton *spinbutton, gpointer data)	\
	{								\
		/* TODO change the color of the cell */			\
	}

#define CHECKBUTTON_TOGGLED_CB(name) _hkl_gui_window_on_ ## name ## _toggled_gtk_toggle_button_toggled

SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_a);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_a_min);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_a_max);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_a_star);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_b);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_b_min);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_b_max);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_b_star);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_c);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_c_min);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_c_max);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_c_star);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_alpha);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_alpha_min);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_alpha_max);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_alpha_star);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_beta);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_beta_min);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_beta_max);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_beta_star);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_gamma);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_gamma_min);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_gamma_max);
SPINBUTTON_VALUE_CHANGED_CB_DECL(spinbutton_gamma_star);
static void hkl_gui_window_get_widgets_and_objects_from_ui (HklGuiWindow* self);
static void hkl_gui_window_set_up_diffractometer_model (HklGuiWindow* self);
static void hkl_gui_window_connect_all_signals (HklGuiWindow* self);
HklGuiWindow* hkl_gui_window_construct (GType object_type);
static void hkl_gui_window_set_up_tree_view_reflections (HklGuiWindow* self);
static void hkl_gui_window_set_up_tree_view_crystals (HklGuiWindow* self);
static void hkl_gui_window_update_tree_view_crystals (HklGuiWindow* self);
static void hkl_gui_window_update_source (HklGuiWindow* self);
static void hkl_gui_window_update_lattice (HklGuiWindow* self);
static void hkl_gui_window_update_lattice_parameters (HklGuiWindow* self);
static void hkl_gui_window_update_reciprocal_lattice (HklGuiWindow* self);
static void hkl_gui_window_update_UxUyUz (HklGuiWindow* self);
static void hkl_gui_window_update_UB (HklGuiWindow* self);
static void hkl_gui_window_set_up_pseudo_axes_frames (HklGuiWindow* self);
static void hkl_gui_window_set_up_tree_view_axes (HklGuiWindow* self);
static void hkl_gui_window_update_axes (HklGuiWindow* self);
static void hkl_gui_window_set_up_tree_view_pseudo_axes (HklGuiWindow* self);
static void hkl_gui_window_update_pseudo_axes (HklGuiWindow* self);
static void hkl_gui_window_set_up_tree_view_pseudo_axes_parameters (HklGuiWindow* self);
static void hkl_gui_window_set_up_tree_view_solutions (HklGuiWindow* self);
static void hkl_gui_window_update_solutions (HklGuiWindow* self);
static void hkl_gui_window_set_up_3D (HklGuiWindow* self);
static void hkl_gui_window_update_pseudo_axes_parameters (HklGuiWindow* self);
static void _hkl_gui_window_update_reflections (HklSample* sample, GtkListStore* model);
static void hkl_gui_window_update_reflections (HklGuiWindow* self, HklSample* sample);
static void hkl_gui_window_update_crystal_model (HklGuiWindow* self, HklSample* sample);
static void hkl_gui_window_update_pseudo_axes_frames (HklGuiWindow* self);
void hkl_gui_window_main (gchar** args, int args_length1);
static void hkl_gui_window_finalize (GObject* obj);
static void hkl_gui_window_set_up_info_bar(HklGuiWindow *self);

/* callbacks */

void combobox1_changed_cb(GtkComboBox *combobox, gpointer *user_data);
void hkl_gui_window_cellrendererspin1_edited_cb(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data);


static struct diffractometer_t *create_diffractometer(HklFactory *factory)
{
	struct diffractometer_t *self;

	self = malloc(sizeof(*self));

	self->geometry = hkl_factory_create_new_geometry (factory);
	self->engines = hkl_factory_create_new_engine_list (factory);
	self->detector = hkl_detector_factory_new (HKL_DETECTOR_TYPE_0D);
	hkl_detector_idx_set (self->detector, 1);

	return self;
}


static void delete_diffractometer(struct diffractometer_t *self)
{
	hkl_geometry_free(self->geometry);
	hkl_engine_list_free(self->engines);
	hkl_detector_free(self->detector);
}


static void dump_diffractometer(struct diffractometer_t *self)
{
	hkl_geometry_fprintf(stderr, self->geometry);
	hkl_engine_list_fprintf(stderr, self->engines);
	hkl_detector_fprintf(stderr, self->detector);
}

G_DEFINE_TYPE (HklGuiWindow, hkl_gui_window, G_TYPE_OBJECT);

static void 
hkl_gui_window_class_init (HklGuiWindowClass * class)
{
	GObjectClass *gobject_class;
	
	gobject_class = (GObjectClass *) class;

	gobject_class->finalize = hkl_gui_window_finalize;

	g_type_class_add_private (class, sizeof (HklGuiWindowPrivate));
}

static void
hkl_gui_window_init (HklGuiWindow * self)
{
	HklGuiWindowPrivate *priv;

	self->priv = G_TYPE_INSTANCE_GET_PRIVATE (self,
						  HKL_GUI_TYPE_WINDOW,
						  HklGuiWindowPrivate);

	priv = self->priv;
	priv->diffractometer = NULL;

	priv->sample = hkl_sample_new ("test");
	priv->reciprocal = hkl_lattice_new_default ();

	hkl_gui_window_get_widgets_and_objects_from_ui (self);

	hkl_gui_window_set_up_diffractometer_model (self);

	//hkl_gui_window_set_up_tree_view_reflections (self);

	//hkl_gui_window_set_up_tree_view_crystals (self);

	//hkl_gui_window_update_tree_view_crystals (self);

	//hkl_gui_window_update_source (self);

	//hkl_gui_window_update_lattice (self);

	//hkl_gui_window_update_lattice_parameters (self);

	//hkl_gui_window_update_reciprocal_lattice (self);

	//hkl_gui_window_update_UxUyUz (self);

	//hkl_gui_window_update_UB (self);

	//hkl_gui_window_connect_all_signals (self);
}

static gboolean _finalize_liststore_diffractometer(GtkTreeModel *model,
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

static void
hkl_gui_window_finalize (GObject* object)
{
	HklGuiWindow * self = HKL_GUI_WINDOW(object);
	HklGuiWindowPrivate *priv = self->priv;

	g_object_unref(priv->builder);

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_diffractometer),
			       _finalize_liststore_diffractometer,
			       NULL);

	G_OBJECT_CLASS (hkl_gui_window_parent_class)->finalize (object);
}

HklGuiWindow* 
hkl_gui_window_new (void)
{
	return g_object_new (HKL_GUI_TYPE_WINDOW, NULL);
}


#define get_object(builder, type, priv, name) priv->_ ## name = type(gtk_builder_get_object(builder, #name))

static void
hkl_gui_window_get_widgets_and_objects_from_ui (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = self->priv;

	GtkBuilder* builder;

	g_return_if_fail (self != NULL);

	priv->builder = builder = gtk_builder_new ();
	gtk_builder_add_from_file (builder, "ghkl.ui", NULL);

	get_object(builder, GTK_LIST_STORE, priv, liststore_diffractometer);
	get_object(builder, GTK_LIST_STORE, priv, liststore_axis);
	get_object(builder, GTK_LIST_STORE, priv, liststore_pseudo_axes);

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
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_Ux);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_Uy);
	get_object(builder, GTK_CHECK_BUTTON, priv, checkbutton_Uz);


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
hkl_gui_window_set_up_diffractometer_model (HklGuiWindow* self)
{
	unsigned int i, n;
	HklFactory **factories;

	g_return_if_fail (self != NULL);

	factories = hkl_factory_get_all(&n);
	for(i=0; i<n; ++i){
		GtkTreeIter iter = {0};

		gtk_list_store_append (self->priv->_liststore_diffractometer, &iter);
		gtk_list_store_set (self->priv->_liststore_diffractometer,
				    &iter,
				    DIFFRACTOMETER_COL_NAME, hkl_factory_name(factories[i]),
				    DIFFRACTOMETER_COL_FACTORY, factories[i],
				    DIFFRACTOMETER_COL_DIFFRACTOMETER, NULL,
				    -1);
	}
}

void hkl_gui_window_combobox1_changed_cb(GtkComboBox *combobox, gpointer *user_data)
{
	HklGuiWindow *self = HKL_GUI_WINDOW(user_data);
	HklGuiWindowPrivate *priv = self->priv;
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

	hkl_gui_window_set_up_pseudo_axes_frames(self);
	hkl_gui_window_set_up_tree_view_axes(self);
	//hkl_gui_window_set_up_tree_view_pseudo_axes_parameters(self);
	hkl_gui_window_set_up_tree_view_pseudo_axes(self);

	/* FIXME create the right solution Model Column */
	/* this._solutionModelColumns = 0; */
	hkl_gui_window_set_up_tree_view_solutions(self);
	hkl_gui_window_set_up_info_bar(self);
#if HKL3D
	hkl_gui_window_set_up_3D(self);
#endif
}

/* axis read cb */
void hkl_gui_window_cellrendererspin1_edited_cb(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data)
{
	HklGuiWindow *self;
	HklGuiWindowPrivate *priv;
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter = NULL;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	self = user_data;
	priv = self->priv;

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

	hkl_gui_window_update_pseudo_axes (self);
	//hkl_gui_window_update_pseudo_axes_frames (self);
}

/* axis min cb */
void hkl_gui_window_cellrendererspin3_edited_cb(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data)
{
	HklGuiWindow *self;
	HklGuiWindowPrivate *priv;
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter = NULL;
	gdouble shit, max;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	self = user_data;
	priv = self->priv;

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

	hkl_gui_window_update_pseudo_axes (self);
}

/* axis max cb */
void hkl_gui_window_cellrendererspin4_edited_cb(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data)
{
	HklGuiWindow *self;
	HklGuiWindowPrivate *priv;
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklParameter* parameter = NULL;
	gdouble shit, min;

	g_return_if_fail (renderer != NULL);
	g_return_if_fail (path != NULL);
	g_return_if_fail (new_text != NULL);
	g_return_if_fail (user_data != NULL);

	self = user_data;
	priv = self->priv;

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

	hkl_gui_window_update_pseudo_axes (self);
}


static void raise_error(HklGuiWindow *self, HklError **error)
{
	HklGuiWindowPrivate *priv;

	g_return_if_fail (self != NULL);
	g_return_if_fail (error != NULL);

	priv = self->priv;

	/* show an error message */
	gtk_label_set_text (GTK_LABEL (priv->info_message),
			    hkl_error_message_get(*error));
	gtk_info_bar_set_message_type (priv->info_bar,
				       GTK_MESSAGE_ERROR);
	gtk_widget_show (GTK_WIDGET(priv->info_bar));

	hkl_error_clear(error);
}

static void clear_error(HklGuiWindow *self, HklError **error)
{
	HklGuiWindowPrivate *priv;

	g_return_if_fail (self != NULL);

	priv = self->priv;

	gtk_widget_hide(GTK_WIDGET(priv->info_bar));
}

/* pseudo axis write */
void hkl_gui_window_cellrenderertext5_edited_cb(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data)
{
	HklGuiWindow *self;
	HklGuiWindowPrivate *priv;
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

	self = user_data;
	priv = self->priv;

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

	if(hkl_engine_set(engine, &error)){
		clear_error(self, &error);
		hkl_engine_list_select_solution(priv->diffractometer->engines, 0);
		hkl_engine_list_get(priv->diffractometer->engines);

		gtk_list_store_set (priv->_liststore_pseudo_axes,
				    &iter,
				    PSEUDO_AXIS_COL_WRITE, value,
				    -1);

		hkl_gui_window_update_axes (self);
		hkl_gui_window_update_pseudo_axes (self);
		//hkl_gui_window_update_pseudo_axes_frames (self);
	}else{
		hkl_parameter_value_unit_set(parameter, old_value, NULL);
		raise_error(self, &error);
		dump_diffractometer(priv->diffractometer);
	}
	hkl_gui_window_update_solutions (self);
}


static void _destroy_pseudo_frame_cb(GtkWidget *widget, gpointer data)
{
	g_return_if_fail (widget != NULL);
	g_return_if_fail (data != NULL);

	gtk_widget_destroy(widget);
}


static void _update_pseudo_frame_cb(GtkWidget *widget, gpointer data)
{
	g_return_if_fail (widget != NULL);
	g_return_if_fail (data != NULL);

	hkl_gui_engine_update(data);
}


static void hkl_gui_window_update_pseudo_axes_frames (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;

	g_return_if_fail (self != NULL);

	priv = self->priv;

	gtk_container_foreach (GTK_CONTAINER(priv->_vbox2),
			       _update_pseudo_frame_cb, self);
}

static void hkl_gui_window_set_up_pseudo_axes_frames (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;
	GtkVBox* vbox2;
	HklEngine **engine;
	darray_engine *engines;

	g_return_if_fail (self != NULL);

	priv = self->priv;

	gtk_container_foreach (GTK_CONTAINER(priv->_vbox2),
			       _destroy_pseudo_frame_cb, self);

	engines = hkl_engine_list_engines (priv->diffractometer->engines);
	darray_foreach (engine, *engines){
		HklGuiEngine *frame;

		frame = hkl_gui_engine_new (*engine);
		gtk_container_add (GTK_CONTAINER (priv->_vbox2),
				   GTK_WIDGET (hkl_gui_engine_get_frame(frame)));
		/*
		g_signal_connect_object (frame,
					 "changed",
					 (GCallback) _hkl_gui_window_on_pseudo_axes_frame_changed_hkl_gui_pseudo_axes_frame_changed,
					 self, 0);
		*/
	}

	gtk_widget_show_all (GTK_WIDGET (priv->_vbox2));
}


gboolean _update_axis (GtkTreeModel *model,
		       GtkTreePath *path,
		       GtkTreeIter *iter,
		       gpointer data)
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

static void hkl_gui_window_update_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;
	gboolean valid = FALSE;
	GtkTreeIter iter = {0};

	g_return_if_fail (self != NULL);

	priv = self->priv;

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_axis),
			       _update_axis,
			       self);
}

static void hkl_gui_window_set_up_tree_view_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;
	HklParameter **parameter;
	const darray_parameter *parameters;
	GtkCellRenderer* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;

	g_return_if_fail (self != NULL);

	priv = self->priv;

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

	hkl_gui_window_update_axes (self);
}

static gboolean _update_pseudo_axes (GtkTreeModel *model,
				     GtkTreePath *path,
				     GtkTreeIter *iter,
				     gpointer data)
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

static void hkl_gui_window_update_pseudo_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;

	g_return_if_fail (self != NULL);

	priv = self->priv;

	gtk_tree_model_foreach(GTK_TREE_MODEL(priv->_liststore_pseudo_axes),
			       _update_pseudo_axes,
			       self);
}


static void hkl_gui_window_set_up_tree_view_pseudo_axes (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;
	HklParameter **parameter;
	const darray_parameter *parameters;
	HklEngine **engine;
	const darray_engine *engines;

	GtkCellRendererText* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;

	g_return_if_fail (self != NULL);

	priv = self->priv;

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

	hkl_gui_window_update_pseudo_axes (self);
}

void hkl_gui_window_treeview_solutions_cursor_changed_cb (GtkTreeView *tree_view,
							  gpointer     user_data)
{
	HklGuiWindow* self;
	HklGuiWindowPrivate *priv;

	GtkTreePath* path = NULL;
	GtkTreeViewColumn* focus_column = NULL;
	GtkTreeIter iter = {0};
	gsize index = 0UL;

	g_return_if_fail (tree_view != NULL);
	g_return_if_fail (user_data != NULL);
	
	self = user_data;
	priv = self->priv;

	gtk_tree_view_get_cursor (tree_view, &path, &focus_column);
	gtk_tree_model_get_iter (GTK_TREE_MODEL(priv->_liststore_solutions), &iter, path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_liststore_solutions), &iter,
			    SOLUTION_COL_INDEX, &index,
			    -1);

	hkl_engine_list_select_solution (priv->diffractometer->engines, index);
	hkl_engine_list_get (priv->diffractometer->engines);

	hkl_gui_window_update_axes (self);
	hkl_gui_window_update_pseudo_axes (self);
	//hkl_gui_window_update_pseudo_axes_frames (self);

	gtk_tree_path_free (path);
}


static void hkl_gui_window_update_solutions (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;
	const HklGeometryList *geometries;
	const darray_item *items;
	GtkTreeIter iter = {0};

	g_return_if_fail (self != NULL);

	priv = self->priv;

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


static void _delete_column(gpointer data,
			    gpointer user_data)
{
	GtkTreeViewColumn *column;
	GtkTreeView *treeview;

	g_return_if_fail (data != NULL);
	g_return_if_fail (user_data != NULL);

	column = data;
	treeview = user_data;

	gtk_tree_view_remove_column (treeview, column);
}


static void hkl_gui_window_set_up_tree_view_solutions (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv;
	const darray_parameter *parameters;
	int i;
	GtkCellRenderer* renderer = NULL;
	GtkTreeViewColumn* column = NULL;
	GList* columns;
	GType* types;
	gint n_columns;

	g_return_if_fail (self != NULL);

	priv = self->priv;

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

	hkl_gui_window_update_solutions (self);
}

void hkl_gui_window_set_up_info_bar(HklGuiWindow *self)
{
	HklGuiWindowPrivate *priv;
	GtkWidget *content_area;

	g_return_if_fail (self != NULL);

	priv = self->priv;

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

/*
static void _hkl_gui_window_on_spinbutton_a_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_a_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_b_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_b_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_c_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_c_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_alpha_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_alpha_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_beta_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_beta_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_gamma_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_gamma_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_a_min_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_a_min_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_b_min_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_b_min_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_c_min_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_c_min_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_alpha_min_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_alpha_min_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_beta_min_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_beta_min_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_gamma_min_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_gamma_min_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_a_max_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_a_max_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_b_max_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_b_max_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_c_max_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_c_max_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_alpha_max_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_alpha_max_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_beta_max_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_beta_max_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_gamma_max_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_gamma_max_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_lambda_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_lambda_value_changed (self);

}


static void _hkl_gui_window_on_spinbutton_uxuyuz_value_changed_gtk_spin_button_value_changed (GtkSpinButton* _sender, gpointer self) {

	hkl_gui_window_on_spinbutton_uxuyuz_value_changed (self);

}


static void _hkl_gui_window_on_button2_clicked_gtk_button_clicked (GtkButton* _sender, gpointer self) {

	hkl_gui_window_on_button2_clicked (self);

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


static gboolean _hkl_gui_window_on_tree_view_reflections_key_press_event_gtk_widget_key_press_event (GtkWidget* _sender, GdkEventKey* event, gpointer self) {
	gboolean result;
	result = hkl_gui_window_on_tree_view_reflections_key_press_event (event, self);

	return result;

}


static void _hkl_gui_window_on_tree_view_pseudo_axes_cursor_changed_gtk_tree_view_cursor_changed (GtkTreeView* _sender, gpointer self) {

	hkl_gui_window_on_tree_view_pseudo_axes_cursor_changed (self);

}


static void _hkl_gui_window_on_tree_view_crystals_cursor_changed_gtk_tree_view_cursor_changed (GtkTreeView* _sender, gpointer self) {

	hkl_gui_window_on_tree_view_crystals_cursor_changed (self);

}


static gboolean _hkl_gui_window_on_tree_view_crystals_key_press_event_gtk_widget_key_press_event (GtkWidget* _sender, GdkEventKey* event, gpointer self) {
	gboolean result;
	result = hkl_gui_window_on_tree_view_crystals_key_press_event (event, self);

	return result;

}


static void _hkl_gui_window_on_toolbutton_add_reflection_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_add_reflection_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_goto_reflection_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_goto_reflection_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_del_reflection_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_del_reflection_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_setUB_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_setUB_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_computeUB_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_computeUB_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_add_crystal_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_add_crystal_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_copy_crystal_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_copy_crystal_clicked (self);

}


static void _hkl_gui_window_on_toolbutton_del_crystal_clicked_gtk_tool_button_clicked (GtkToolButton* _sender, gpointer self) {

	hkl_gui_window_on_toolbutton_del_crystal_clicked (self);

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

#define CONNECT_SPINBUTTON(name)					\
	g_signal_connect_object (priv->_ ## name,			\
				 "value-changed",			\
				 (GCallback) SPINBUTTON_VALUE_CHANGED_CB(name),	\
				 self, 0);

#define CONNECT_UX_UY_UZ(name)						\
	g_signal_connect_object (priv->_ ## name,			\
				 "value-changed",			\
				 (GCallback) _hkl_gui_window_on_spinbutton_uxuyuz_value_changed_gtk_spin_button_value_changed, \
				 self, 0);

#define CONNECT_CHECKBUTTON(name)					\
	g_signal_connect_object (priv->_ ## name,			\
				 "toggled",				\
				 (GCallback) CHECKBUTTON_TOGGLED_CB(name), \
				 self, 0);
static void
hkl_gui_window_connect_all_signals (HklGuiWindow* self)
{
	HklGuiWindowPrivate *priv = self->priv;

	g_return_if_fail (self != NULL);

	CONNECT_SPINBUTTON(spinbutton_a);
	CONNECT_SPINBUTTON(spinbutton_a_min);
	CONNECT_SPINBUTTON(spinbutton_a_max);
	CONNECT_SPINBUTTON(spinbutton_a_star);

	CONNECT_SPINBUTTON(spinbutton_b);
	CONNECT_SPINBUTTON(spinbutton_b_min);
	CONNECT_SPINBUTTON(spinbutton_b_max);
	CONNECT_SPINBUTTON(spinbutton_b_star);

	CONNECT_SPINBUTTON(spinbutton_c);
	CONNECT_SPINBUTTON(spinbutton_c_min);
	CONNECT_SPINBUTTON(spinbutton_c_max);
	CONNECT_SPINBUTTON(spinbutton_c_star);

	CONNECT_SPINBUTTON(spinbutton_alpha);
	CONNECT_SPINBUTTON(spinbutton_alpha_min);
	CONNECT_SPINBUTTON(spinbutton_alpha_max);
	CONNECT_SPINBUTTON(spinbutton_alpha_star);

	CONNECT_SPINBUTTON(spinbutton_beta);
	CONNECT_SPINBUTTON(spinbutton_beta_min);
	CONNECT_SPINBUTTON(spinbutton_beta_max);
	CONNECT_SPINBUTTON(spinbutton_beta_star);

	CONNECT_SPINBUTTON(spinbutton_alpha);
	CONNECT_SPINBUTTON(spinbutton_alpha_min);
	CONNECT_SPINBUTTON(spinbutton_alpha_max);
	CONNECT_SPINBUTTON(spinbutton_alpha_star);

	g_signal_connect_object (priv->_spinbutton_lambda,
				 "value-changed",
				 (GCallback) _hkl_gui_window_on_spinbutton_lambda_value_changed_gtk_spin_button_value_changed,
				 self, 0);

	CONNECT_UX_UY_UZ(spinbutton_ux);
	CONNECT_UX_UY_UZ(spinbutton_uy);
	CONNECT_UX_UY_UZ(spinbutton_uz);

	g_signal_connect_object (priv->_button2,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_button2_clicked_gtk_button_clicked,
				 self, 0);
	
	CONNECT_CHECKBUTTON(checkbutton_a);
	CONNECT_CHECKBUTTON(checkbutton_b);
	CONNECT_CHECKBUTTON(checkbutton_c);
	CONNECT_CHECKBUTTON(checkbutton_alpha);
	CONNECT_CHECKBUTTON(checkbutton_beta);
	CONNECT_CHECKBUTTON(checkbutton_gamma);
	CONNECT_CHECKBUTTON(checkbutton_Ux);
	CONNECT_CHECKBUTTON(checkbutton_Uy);
	CONNECT_CHECKBUTTON(checkbutton_Uz);

	g_signal_connect_object (GTK_WIDGET(priv->_treeview_reflections),
				 "key-press-event",
				 (GCallback) _hkl_gui_window_on_tree_view_reflections_key_press_event_gtk_widget_key_press_event,
				 self, 0);

	g_signal_connect_object (priv->_treeview_pseudo_axes,
				 "cursor-changed",
				 (GCallback) _hkl_gui_window_on_tree_view_pseudo_axes_cursor_changed_gtk_tree_view_cursor_changed,
				 self, 0);

	g_signal_connect_object (priv->_treeview_crystals,
				 "cursor-changed",
				 (GCallback) _hkl_gui_window_on_tree_view_crystals_cursor_changed_gtk_tree_view_cursor_changed, self, 0);

	g_signal_connect_object (GTK_WIDGET(priv->_treeview_crystals),
				 "key-press-event",
				 (GCallback) _hkl_gui_window_on_tree_view_crystals_key_press_event_gtk_widget_key_press_event,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_add_reflection,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_add_reflection_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_goto_reflection,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_goto_reflection_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_del_reflection,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_del_reflection_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_setUB,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_setUB_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_computeUB,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_computeUB_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_add_crystal,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_add_crystal_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_copy_crystal,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_copy_crystal_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_del_crystal,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_del_crystal_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_toolbutton_affiner,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_toolbutton_affiner_clicked_gtk_tool_button_clicked,
				 self, 0);

	g_signal_connect_object (GTK_MENU_ITEM(priv->_menuitem5),
				 "activate",
				 (GCallback) _hkl_gui_window_on_menuitem5_activate_gtk_menu_item_activate,
				 self, 0);

	g_signal_connect_object (priv->_button1,
				 "clicked",
				 (GCallback) _hkl_gui_window_on_button1_clicked_gtk_button_clicked,
				 self, 0);

	g_signal_connect_object (priv->_combobox1,
				 "changed",
				 (GCallback) _hkl_gui_window_on_combobox1_changed_gtk_combo_box_changed,
				 self, 0);
}


static void _hkl_gui_window_on_pseudo_axes_frame_changed_hkl_gui_pseudo_axes_frame_changed (HklGuiEngine* _sender, gpointer self)
{
	hkl_gui_window_on_pseudo_axes_frame_changed (self);
}




static void _hkl_gui_window_on_cell_tree_view_axes_read_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_axes_read_edited (path, new_text, self);

}


static void _hkl_gui_window_on_cell_tree_view_axes_write_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_axes_write_edited (path, new_text, self);

}


static void _hkl_gui_window_on_cell_tree_view_axes_min_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_axes_min_edited (path, new_text, self);

}


static void _hkl_gui_window_on_cell_tree_view_axes_max_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_axes_max_edited (path, new_text, self);

}



static void _hkl_gui_window_on_cell_tree_view_pseudo_axes_write_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_pseudo_axes_write_edited (path, new_text, self);

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




static void _hkl_gui_window_on_cell_tree_view_reflections_h_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_reflections_h_edited (path, new_text, self);

}


static void _hkl_gui_window_on_cell_tree_view_reflections_k_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_reflections_k_edited (path, new_text, self);

}


static void _hkl_gui_window_on_cell_tree_view_reflections_l_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_reflections_l_edited (path, new_text, self);

}


static void _hkl_gui_window_on_cell_tree_view_reflections_flag_toggled_gtk_cell_renderer_toggle_toggled (GtkCellRendererToggle* _sender, const gchar* path, gpointer self) {

	hkl_gui_window_on_cell_tree_view_reflections_flag_toggled (path, self);

}


static void hkl_gui_window_set_up_tree_view_reflections (HklGuiWindow* self) {
	GtkTreeViewColumn* column = NULL;
	GtkCellRendererText* renderer = NULL;
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
	GtkCellRendererText* _tmp17_;
	GtkCellRendererText* _tmp18_;
	GtkCellRendererText* _tmp19_;
	GtkCellRendererText* _tmp20_;
	GtkTreeViewColumn* _tmp21_;
	GtkTreeView* _tmp22_;
	GtkTreeViewColumn* _tmp23_;
	GtkCellRendererText* _tmp24_;
	GtkCellRendererText* _tmp25_;
	GtkCellRendererText* _tmp26_;
	GtkCellRendererText* _tmp27_;
	GtkTreeViewColumn* _tmp28_;
	GtkTreeView* _tmp29_;
	GtkTreeViewColumn* _tmp30_;
	GtkCellRendererToggle* _tmp31_;
	GtkCellRendererToggle* toggle;
	GtkTreeViewColumn* _tmp32_;
	GtkTreeView* _tmp33_;
	GtkTreeViewColumn* _tmp34_;
	GtkTreeView* _tmp35_;
	GtkTreeSelection* _tmp36_ = NULL;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_treeview_reflections;

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

				_tmp3_ = priv->_treeview_reflections;

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

	_tmp7_ = gtk_tree_view_column_new_with_attributes ("index", (GtkCellRenderer*) _tmp6_, "text", REFLECTION_COL_INDEX, NULL);

	g_object_ref_sink (_tmp7_);

	_g_object_unref0 (column);

	column = _tmp7_;

	_tmp8_ = priv->_treeview_reflections;

	_tmp9_ = column;

	gtk_tree_view_append_column (_tmp8_, _tmp9_);

	_tmp10_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp10_);

	_g_object_unref0 (renderer);

	renderer = _tmp10_;

	_tmp11_ = renderer;

	g_signal_connect_object (_tmp11_, "edited", (GCallback) _hkl_gui_window_on_cell_tree_view_reflections_h_edited_gtk_cell_renderer_text_edited, self, 0);

	_tmp12_ = renderer;

	g_object_set (_tmp12_, "editable", TRUE, NULL);

	_tmp13_ = renderer;

	_tmp14_ = gtk_tree_view_column_new_with_attributes ("h", (GtkCellRenderer*) _tmp13_, "text", REFLECTION_COL_H, NULL);

	g_object_ref_sink (_tmp14_);

	_g_object_unref0 (column);

	column = _tmp14_;

	_tmp15_ = priv->_treeview_reflections;

	_tmp16_ = column;

	gtk_tree_view_append_column (_tmp15_, _tmp16_);

	_tmp17_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp17_);

	_g_object_unref0 (renderer);

	renderer = _tmp17_;

	_tmp18_ = renderer;

	g_signal_connect_object (_tmp18_, "edited", (GCallback) _hkl_gui_window_on_cell_tree_view_reflections_k_edited_gtk_cell_renderer_text_edited, self, 0);

	_tmp19_ = renderer;

	g_object_set (_tmp19_, "editable", TRUE, NULL);

	_tmp20_ = renderer;

	_tmp21_ = gtk_tree_view_column_new_with_attributes ("k", (GtkCellRenderer*) _tmp20_, "text", REFLECTION_COL_K, NULL);

	g_object_ref_sink (_tmp21_);

	_g_object_unref0 (column);

	column = _tmp21_;

	_tmp22_ = priv->_treeview_reflections;

	_tmp23_ = column;

	gtk_tree_view_append_column (_tmp22_, _tmp23_);

	_tmp24_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp24_);

	_g_object_unref0 (renderer);

	renderer = _tmp24_;

	_tmp25_ = renderer;

	g_signal_connect_object (_tmp25_, "edited", (GCallback) _hkl_gui_window_on_cell_tree_view_reflections_l_edited_gtk_cell_renderer_text_edited, self, 0);

	_tmp26_ = renderer;

	g_object_set (_tmp26_, "editable", TRUE, NULL);

	_tmp27_ = renderer;

	_tmp28_ = gtk_tree_view_column_new_with_attributes ("l", (GtkCellRenderer*) _tmp27_, "text", REFLECTION_COL_L, NULL);

	g_object_ref_sink (_tmp28_);

	_g_object_unref0 (column);

	column = _tmp28_;

	_tmp29_ = priv->_treeview_reflections;

	_tmp30_ = column;

	gtk_tree_view_append_column (_tmp29_, _tmp30_);

	_tmp31_ = (GtkCellRendererToggle*) gtk_cell_renderer_toggle_new ();

	g_object_ref_sink (_tmp31_);

	toggle = _tmp31_;

	g_signal_connect_object (toggle, "toggled", (GCallback) _hkl_gui_window_on_cell_tree_view_reflections_flag_toggled_gtk_cell_renderer_toggle_toggled, self, 0);

	_tmp32_ = gtk_tree_view_column_new_with_attributes ("flag", (GtkCellRenderer*) toggle, "active", REFLECTION_COL_FLAG, NULL);

	g_object_ref_sink (_tmp32_);

	_g_object_unref0 (column);

	column = _tmp32_;

	_tmp33_ = priv->_treeview_reflections;

	_tmp34_ = column;

	gtk_tree_view_append_column (_tmp33_, _tmp34_);

	_tmp35_ = priv->_treeview_reflections;

	_tmp36_ = gtk_tree_view_get_selection (_tmp35_);

	gtk_tree_selection_set_mode (_tmp36_, GTK_SELECTION_MULTIPLE);

	_g_object_unref0 (toggle);

	_g_list_free0 (columns);

	_g_object_unref0 (renderer);

	_g_object_unref0 (column);

}


static void _hkl_gui_window_on_cell_tree_view_crystals_name_edited_gtk_cell_renderer_text_edited (GtkCellRendererText* _sender, const gchar* path, const gchar* new_text, gpointer self) {

	hkl_gui_window_on_cell_tree_view_crystals_name_edited (path, new_text, self);

}


static void hkl_gui_window_set_up_tree_view_crystals (HklGuiWindow* self) {
	GtkTreeViewColumn* column = NULL;
	GtkCellRendererText* renderer = NULL;
	GtkTreeView* _tmp0_;
	GList* _tmp1_ = NULL;
	GList* columns;
	GList* _tmp2_;
	GtkCellRendererText* _tmp5_;
	GtkCellRendererText* _tmp6_;
	GtkCellRendererText* _tmp7_;
	GtkCellRendererText* _tmp8_;
	GtkTreeViewColumn* _tmp9_;
	GtkTreeView* _tmp10_;
	GtkTreeViewColumn* _tmp11_;
	GtkCellRendererText* _tmp12_;
	GtkCellRendererText* _tmp13_;
	GtkTreeViewColumn* _tmp14_;
	GtkTreeView* _tmp15_;
	GtkTreeViewColumn* _tmp16_;
	GtkCellRendererText* _tmp17_;
	GtkCellRendererText* _tmp18_;
	GtkTreeViewColumn* _tmp19_;
	GtkTreeView* _tmp20_;
	GtkTreeViewColumn* _tmp21_;
	GtkCellRendererText* _tmp22_;
	GtkCellRendererText* _tmp23_;
	GtkTreeViewColumn* _tmp24_;
	GtkTreeView* _tmp25_;
	GtkTreeViewColumn* _tmp26_;
	GtkCellRendererText* _tmp27_;
	GtkCellRendererText* _tmp28_;
	GtkTreeViewColumn* _tmp29_;
	GtkTreeView* _tmp30_;
	GtkTreeViewColumn* _tmp31_;
	GtkCellRendererText* _tmp32_;
	GtkCellRendererText* _tmp33_;
	GtkTreeViewColumn* _tmp34_;
	GtkTreeView* _tmp35_;
	GtkTreeViewColumn* _tmp36_;
	GtkCellRendererText* _tmp37_;
	GtkCellRendererText* _tmp38_;
	GtkTreeViewColumn* _tmp39_;
	GtkTreeView* _tmp40_;
	GtkTreeViewColumn* _tmp41_;
	GtkTreeView* _tmp42_;
	GtkTreeSelection* _tmp43_ = NULL;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_treeview_crystals;

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

				_tmp3_ = priv->_treeview_crystals;

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

	g_signal_connect_object (_tmp6_, "edited", (GCallback) _hkl_gui_window_on_cell_tree_view_crystals_name_edited_gtk_cell_renderer_text_edited, self, 0);

	_tmp7_ = renderer;

	g_object_set (_tmp7_, "editable", TRUE, NULL);

	_tmp8_ = renderer;

	_tmp9_ = gtk_tree_view_column_new_with_attributes ("name", (GtkCellRenderer*) _tmp8_, "text", SAMPLE_COL_NAME, NULL);

	g_object_ref_sink (_tmp9_);

	_g_object_unref0 (column);

	column = _tmp9_;

	_tmp10_ = priv->_treeview_crystals;

	_tmp11_ = column;

	gtk_tree_view_append_column (_tmp10_, _tmp11_);

	_tmp12_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp12_);

	_g_object_unref0 (renderer);

	renderer = _tmp12_;

	_tmp13_ = renderer;

	_tmp14_ = gtk_tree_view_column_new_with_attributes ("a", (GtkCellRenderer*) _tmp13_, "text", SAMPLE_COL_A, NULL);

	g_object_ref_sink (_tmp14_);

	_g_object_unref0 (column);

	column = _tmp14_;

	_tmp15_ = priv->_treeview_crystals;

	_tmp16_ = column;

	gtk_tree_view_append_column (_tmp15_, _tmp16_);

	_tmp17_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp17_);

	_g_object_unref0 (renderer);

	renderer = _tmp17_;

	_tmp18_ = renderer;

	_tmp19_ = gtk_tree_view_column_new_with_attributes ("b", (GtkCellRenderer*) _tmp18_, "text", SAMPLE_COL_B, NULL);

	g_object_ref_sink (_tmp19_);

	_g_object_unref0 (column);

	column = _tmp19_;

	_tmp20_ = priv->_treeview_crystals;

	_tmp21_ = column;

	gtk_tree_view_append_column (_tmp20_, _tmp21_);

	_tmp22_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp22_);

	_g_object_unref0 (renderer);

	renderer = _tmp22_;

	_tmp23_ = renderer;

	_tmp24_ = gtk_tree_view_column_new_with_attributes ("c", (GtkCellRenderer*) _tmp23_, "text", SAMPLE_COL_C, NULL);

	g_object_ref_sink (_tmp24_);

	_g_object_unref0 (column);

	column = _tmp24_;

	_tmp25_ = priv->_treeview_crystals;

	_tmp26_ = column;

	gtk_tree_view_append_column (_tmp25_, _tmp26_);

	_tmp27_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp27_);

	_g_object_unref0 (renderer);

	renderer = _tmp27_;

	_tmp28_ = renderer;

	_tmp29_ = gtk_tree_view_column_new_with_attributes ("alpha", (GtkCellRenderer*) _tmp28_, "text", SAMPLE_COL_ALPHA, NULL);

	g_object_ref_sink (_tmp29_);

	_g_object_unref0 (column);

	column = _tmp29_;

	_tmp30_ = priv->_treeview_crystals;

	_tmp31_ = column;

	gtk_tree_view_append_column (_tmp30_, _tmp31_);

	_tmp32_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp32_);

	_g_object_unref0 (renderer);

	renderer = _tmp32_;

	_tmp33_ = renderer;

	_tmp34_ = gtk_tree_view_column_new_with_attributes ("beta", (GtkCellRenderer*) _tmp33_, "text", SAMPLE_COL_BETA, NULL);

	g_object_ref_sink (_tmp34_);

	_g_object_unref0 (column);

	column = _tmp34_;

	_tmp35_ = priv->_treeview_crystals;

	_tmp36_ = column;

	gtk_tree_view_append_column (_tmp35_, _tmp36_);

	_tmp37_ = (GtkCellRendererText*) gtk_cell_renderer_text_new ();

	g_object_ref_sink (_tmp37_);

	_g_object_unref0 (renderer);

	renderer = _tmp37_;

	_tmp38_ = renderer;

	_tmp39_ = gtk_tree_view_column_new_with_attributes ("gamma", (GtkCellRenderer*) _tmp38_, "text", SAMPLE_COL_GAMMA, NULL);

	g_object_ref_sink (_tmp39_);

	_g_object_unref0 (column);

	column = _tmp39_;

	_tmp40_ = priv->_treeview_crystals;

	_tmp41_ = column;

	gtk_tree_view_append_column (_tmp40_, _tmp41_);

	_tmp42_ = priv->_treeview_crystals;

	_tmp43_ = gtk_tree_view_get_selection (_tmp42_);

	gtk_tree_selection_set_mode (_tmp43_, GTK_SELECTION_MULTIPLE);

	_g_list_free0 (columns);

	_g_object_unref0 (renderer);

	_g_object_unref0 (column);

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


static void hkl_gui_window_update_source (HklGuiWindow* self) {
	HklGeometry* _tmp0_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->geometry;

	if (_tmp0_ != NULL) {

		HklGeometry* _tmp1_;
		gdouble _tmp2_ = 0.0;
		gdouble lambda;
		GtkSpinButton* _tmp3_;
		gdouble _tmp4_;

		_tmp1_ = priv->geometry;

		_tmp2_ = hkl_source_get_wavelength (&_tmp1_->source);

		lambda = _tmp2_;

		_tmp3_ = priv->_spinbutton_lambda;

		_tmp4_ = lambda;

		gtk_spin_button_set_value (_tmp3_, _tmp4_);

	}
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


static void hkl_gui_window_update_lattice (HklGuiWindow* self) {
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

		GtkSpinButton* _tmp3_;
		HklSample* _tmp4_;
		HklLattice* _tmp5_;
		HklParameter* _tmp6_;
		HklParameter _tmp7_;
		gdouble _tmp8_ = 0.0;
		GtkSpinButton* _tmp9_;
		HklSample* _tmp10_;
		HklLattice* _tmp11_;
		HklParameter* _tmp12_;
		HklParameter _tmp13_;
		gdouble _tmp14_ = 0.0;
		GtkSpinButton* _tmp15_;
		HklSample* _tmp16_;
		HklLattice* _tmp17_;
		HklParameter* _tmp18_;
		HklParameter _tmp19_;
		gdouble _tmp20_ = 0.0;
		GtkSpinButton* _tmp21_;
		HklSample* _tmp22_;
		HklLattice* _tmp23_;
		HklParameter* _tmp24_;
		HklParameter _tmp25_;
		gdouble _tmp26_ = 0.0;
		GtkSpinButton* _tmp27_;
		HklSample* _tmp28_;
		HklLattice* _tmp29_;
		HklParameter* _tmp30_;
		HklParameter _tmp31_;
		gdouble _tmp32_ = 0.0;
		GtkSpinButton* _tmp33_;
		HklSample* _tmp34_;
		HklLattice* _tmp35_;
		HklParameter* _tmp36_;
		HklParameter _tmp37_;
		gdouble _tmp38_ = 0.0;

		_tmp3_ = priv->_spinbutton_a;

		_tmp4_ = sample;

		_tmp5_ = _tmp4_->lattice;

		_tmp6_ = _tmp5_->a;

		_tmp7_ = *_tmp6_;

		_tmp8_ = hkl_parameter_get_value_unit (&_tmp7_);

		gtk_spin_button_set_value (_tmp3_, _tmp8_);

		_tmp9_ = priv->_spinbutton_b;

		_tmp10_ = sample;

		_tmp11_ = _tmp10_->lattice;

		_tmp12_ = _tmp11_->b;

		_tmp13_ = *_tmp12_;

		_tmp14_ = hkl_parameter_get_value_unit (&_tmp13_);

		gtk_spin_button_set_value (_tmp9_, _tmp14_);

		_tmp15_ = priv->_spinbutton_c;

		_tmp16_ = sample;

		_tmp17_ = _tmp16_->lattice;

		_tmp18_ = _tmp17_->c;

		_tmp19_ = *_tmp18_;

		_tmp20_ = hkl_parameter_get_value_unit (&_tmp19_);

		gtk_spin_button_set_value (_tmp15_, _tmp20_);

		_tmp21_ = priv->_spinbutton_alpha;

		_tmp22_ = sample;

		_tmp23_ = _tmp22_->lattice;

		_tmp24_ = _tmp23_->alpha;

		_tmp25_ = *_tmp24_;

		_tmp26_ = hkl_parameter_get_value_unit (&_tmp25_);

		gtk_spin_button_set_value (_tmp21_, _tmp26_);

		_tmp27_ = priv->_spinbutton_beta;

		_tmp28_ = sample;

		_tmp29_ = _tmp28_->lattice;

		_tmp30_ = _tmp29_->beta;

		_tmp31_ = *_tmp30_;

		_tmp32_ = hkl_parameter_get_value_unit (&_tmp31_);

		gtk_spin_button_set_value (_tmp27_, _tmp32_);

		_tmp33_ = priv->_spinbutton_gamma;

		_tmp34_ = sample;

		_tmp35_ = _tmp34_->lattice;

		_tmp36_ = _tmp35_->gamma;

		_tmp37_ = *_tmp36_;

		_tmp38_ = hkl_parameter_get_value_unit (&_tmp37_);

		gtk_spin_button_set_value (_tmp33_, _tmp38_);

	}
}


static void hkl_gui_window_update_lattice_parameters (HklGuiWindow* self) {
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

		gdouble min = 0.0;
		gdouble max = 0.0;
		HklParameter* parameter = NULL;
		HklSample* _tmp3_;
		HklLattice* _tmp4_;
		HklParameter* _tmp5_;
		HklParameter* _tmp6_;
		HklParameter _tmp7_;
		gdouble _tmp8_ = 0.0;
		gdouble _tmp9_ = 0.0;
		GtkSpinButton* _tmp10_;
		gdouble _tmp11_;
		GtkSpinButton* _tmp12_;
		gdouble _tmp13_;
		GtkCheckButton* _tmp14_;
		HklParameter* _tmp15_;
		gboolean _tmp16_;
		HklSample* _tmp17_;
		HklLattice* _tmp18_;
		HklParameter* _tmp19_;
		HklParameter* _tmp20_;
		HklParameter _tmp21_;
		gdouble _tmp22_ = 0.0;
		gdouble _tmp23_ = 0.0;
		GtkSpinButton* _tmp24_;
		gdouble _tmp25_;
		GtkSpinButton* _tmp26_;
		gdouble _tmp27_;
		GtkCheckButton* _tmp28_;
		HklParameter* _tmp29_;
		gboolean _tmp30_;
		HklSample* _tmp31_;
		HklLattice* _tmp32_;
		HklParameter* _tmp33_;
		HklParameter* _tmp34_;
		HklParameter _tmp35_;
		gdouble _tmp36_ = 0.0;
		gdouble _tmp37_ = 0.0;
		GtkSpinButton* _tmp38_;
		gdouble _tmp39_;
		GtkSpinButton* _tmp40_;
		gdouble _tmp41_;
		GtkCheckButton* _tmp42_;
		HklParameter* _tmp43_;
		gboolean _tmp44_;
		HklSample* _tmp45_;
		HklLattice* _tmp46_;
		HklParameter* _tmp47_;
		HklParameter* _tmp48_;
		HklParameter _tmp49_;
		gdouble _tmp50_ = 0.0;
		gdouble _tmp51_ = 0.0;
		GtkSpinButton* _tmp52_;
		gdouble _tmp53_;
		GtkSpinButton* _tmp54_;
		gdouble _tmp55_;
		GtkCheckButton* _tmp56_;
		HklParameter* _tmp57_;
		gboolean _tmp58_;
		HklSample* _tmp59_;
		HklLattice* _tmp60_;
		HklParameter* _tmp61_;
		HklParameter* _tmp62_;
		HklParameter _tmp63_;
		gdouble _tmp64_ = 0.0;
		gdouble _tmp65_ = 0.0;
		GtkSpinButton* _tmp66_;
		gdouble _tmp67_;
		GtkSpinButton* _tmp68_;
		gdouble _tmp69_;
		GtkCheckButton* _tmp70_;
		HklParameter* _tmp71_;
		gboolean _tmp72_;
		HklSample* _tmp73_;
		HklLattice* _tmp74_;
		HklParameter* _tmp75_;
		HklParameter* _tmp76_;
		HklParameter _tmp77_;
		gdouble _tmp78_ = 0.0;
		gdouble _tmp79_ = 0.0;
		GtkSpinButton* _tmp80_;
		gdouble _tmp81_;
		GtkSpinButton* _tmp82_;
		gdouble _tmp83_;
		GtkCheckButton* _tmp84_;
		HklParameter* _tmp85_;
		gboolean _tmp86_;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = _tmp4_->a;

		parameter = _tmp5_;

		_tmp6_ = parameter;

		_tmp7_ = *_tmp6_;

		hkl_parameter_get_range_unit (&_tmp7_, &_tmp8_, &_tmp9_);

		min = _tmp8_;

		max = _tmp9_;

		_tmp10_ = priv->_spinbutton_a_min;

		_tmp11_ = min;

		gtk_spin_button_set_value (_tmp10_, _tmp11_);

		_tmp12_ = priv->_spinbutton_a_max;

		_tmp13_ = max;

		gtk_spin_button_set_value (_tmp12_, _tmp13_);

		_tmp14_ = priv->_checkbutton_a;

		_tmp15_ = parameter;

		_tmp16_ = (*_tmp15_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp14_, _tmp16_);

		_tmp17_ = sample;

		_tmp18_ = _tmp17_->lattice;

		_tmp19_ = _tmp18_->b;

		parameter = _tmp19_;

		_tmp20_ = parameter;

		_tmp21_ = *_tmp20_;

		hkl_parameter_get_range_unit (&_tmp21_, &_tmp22_, &_tmp23_);

		min = _tmp22_;

		max = _tmp23_;

		_tmp24_ = priv->_spinbutton_b_min;

		_tmp25_ = min;

		gtk_spin_button_set_value (_tmp24_, _tmp25_);

		_tmp26_ = priv->_spinbutton_b_max;

		_tmp27_ = max;

		gtk_spin_button_set_value (_tmp26_, _tmp27_);

		_tmp28_ = priv->_checkbutton_b;

		_tmp29_ = parameter;

		_tmp30_ = (*_tmp29_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp28_, _tmp30_);

		_tmp31_ = sample;

		_tmp32_ = _tmp31_->lattice;

		_tmp33_ = _tmp32_->c;

		parameter = _tmp33_;

		_tmp34_ = parameter;

		_tmp35_ = *_tmp34_;

		hkl_parameter_get_range_unit (&_tmp35_, &_tmp36_, &_tmp37_);

		min = _tmp36_;

		max = _tmp37_;

		_tmp38_ = priv->_spinbutton_c_min;

		_tmp39_ = min;

		gtk_spin_button_set_value (_tmp38_, _tmp39_);

		_tmp40_ = priv->_spinbutton_c_max;

		_tmp41_ = max;

		gtk_spin_button_set_value (_tmp40_, _tmp41_);

		_tmp42_ = priv->_checkbutton_c;

		_tmp43_ = parameter;

		_tmp44_ = (*_tmp43_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp42_, _tmp44_);

		_tmp45_ = sample;

		_tmp46_ = _tmp45_->lattice;

		_tmp47_ = _tmp46_->alpha;

		parameter = _tmp47_;

		_tmp48_ = parameter;

		_tmp49_ = *_tmp48_;

		hkl_parameter_get_range_unit (&_tmp49_, &_tmp50_, &_tmp51_);

		min = _tmp50_;

		max = _tmp51_;

		_tmp52_ = priv->_spinbutton_alpha_min;

		_tmp53_ = min;

		gtk_spin_button_set_value (_tmp52_, _tmp53_);

		_tmp54_ = priv->_spinbutton_alpha_max;

		_tmp55_ = max;

		gtk_spin_button_set_value (_tmp54_, _tmp55_);

		_tmp56_ = priv->_checkbutton_alpha;

		_tmp57_ = parameter;

		_tmp58_ = (*_tmp57_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp56_, _tmp58_);

		_tmp59_ = sample;

		_tmp60_ = _tmp59_->lattice;

		_tmp61_ = _tmp60_->beta;

		parameter = _tmp61_;

		_tmp62_ = parameter;

		_tmp63_ = *_tmp62_;

		hkl_parameter_get_range_unit (&_tmp63_, &_tmp64_, &_tmp65_);

		min = _tmp64_;

		max = _tmp65_;

		_tmp66_ = priv->_spinbutton_beta_min;

		_tmp67_ = min;

		gtk_spin_button_set_value (_tmp66_, _tmp67_);

		_tmp68_ = priv->_spinbutton_beta_max;

		_tmp69_ = max;

		gtk_spin_button_set_value (_tmp68_, _tmp69_);

		_tmp70_ = priv->_checkbutton_beta;

		_tmp71_ = parameter;

		_tmp72_ = (*_tmp71_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp70_, _tmp72_);

		_tmp73_ = sample;

		_tmp74_ = _tmp73_->lattice;

		_tmp75_ = _tmp74_->gamma;

		parameter = _tmp75_;

		_tmp76_ = parameter;

		_tmp77_ = *_tmp76_;

		hkl_parameter_get_range_unit (&_tmp77_, &_tmp78_, &_tmp79_);

		min = _tmp78_;

		max = _tmp79_;

		_tmp80_ = priv->_spinbutton_gamma_min;

		_tmp81_ = min;

		gtk_spin_button_set_value (_tmp80_, _tmp81_);

		_tmp82_ = priv->_spinbutton_gamma_max;

		_tmp83_ = max;

		gtk_spin_button_set_value (_tmp82_, _tmp83_);

		_tmp84_ = priv->_checkbutton_gamma;

		_tmp85_ = parameter;

		_tmp86_ = (*_tmp85_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp84_, _tmp86_);

	}
}


static void hkl_gui_window_update_reciprocal_lattice (HklGuiWindow* self) {
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
		HklLattice* _tmp5_;
		GtkSpinButton* _tmp6_;
		HklLattice* _tmp7_;
		HklParameter* _tmp8_;
		HklParameter _tmp9_;
		gdouble _tmp10_ = 0.0;
		GtkSpinButton* _tmp11_;
		HklLattice* _tmp12_;
		HklParameter* _tmp13_;
		HklParameter _tmp14_;
		gdouble _tmp15_ = 0.0;
		GtkSpinButton* _tmp16_;
		HklLattice* _tmp17_;
		HklParameter* _tmp18_;
		HklParameter _tmp19_;
		gdouble _tmp20_ = 0.0;
		GtkSpinButton* _tmp21_;
		HklLattice* _tmp22_;
		HklParameter* _tmp23_;
		HklParameter _tmp24_;
		gdouble _tmp25_ = 0.0;
		GtkSpinButton* _tmp26_;
		HklLattice* _tmp27_;
		HklParameter* _tmp28_;
		HklParameter _tmp29_;
		gdouble _tmp30_ = 0.0;
		GtkSpinButton* _tmp31_;
		HklLattice* _tmp32_;
		HklParameter* _tmp33_;
		HklParameter _tmp34_;
		gdouble _tmp35_ = 0.0;

		_tmp3_ = sample;

		_tmp4_ = _tmp3_->lattice;

		_tmp5_ = priv->reciprocal;

		hkl_lattice_reciprocal (_tmp4_, _tmp5_);

		_tmp6_ = priv->_spinbutton_a_star;

		_tmp7_ = priv->reciprocal;

		_tmp8_ = _tmp7_->a;

		_tmp9_ = *_tmp8_;

		_tmp10_ = hkl_parameter_get_value_unit (&_tmp9_);

		gtk_spin_button_set_value (_tmp6_, _tmp10_);

		_tmp11_ = priv->_spinbutton_b_star;

		_tmp12_ = priv->reciprocal;

		_tmp13_ = _tmp12_->b;

		_tmp14_ = *_tmp13_;

		_tmp15_ = hkl_parameter_get_value_unit (&_tmp14_);

		gtk_spin_button_set_value (_tmp11_, _tmp15_);

		_tmp16_ = priv->_spinbutton_c_star;

		_tmp17_ = priv->reciprocal;

		_tmp18_ = _tmp17_->c;

		_tmp19_ = *_tmp18_;

		_tmp20_ = hkl_parameter_get_value_unit (&_tmp19_);

		gtk_spin_button_set_value (_tmp16_, _tmp20_);

		_tmp21_ = priv->_spinbutton_alpha_star;

		_tmp22_ = priv->reciprocal;

		_tmp23_ = _tmp22_->alpha;

		_tmp24_ = *_tmp23_;

		_tmp25_ = hkl_parameter_get_value_unit (&_tmp24_);

		gtk_spin_button_set_value (_tmp21_, _tmp25_);

		_tmp26_ = priv->_spinbutton_beta_star;

		_tmp27_ = priv->reciprocal;

		_tmp28_ = _tmp27_->beta;

		_tmp29_ = *_tmp28_;

		_tmp30_ = hkl_parameter_get_value_unit (&_tmp29_);

		gtk_spin_button_set_value (_tmp26_, _tmp30_);

		_tmp31_ = priv->_spinbutton_gamma_star;

		_tmp32_ = priv->reciprocal;

		_tmp33_ = _tmp32_->gamma;

		_tmp34_ = *_tmp33_;

		_tmp35_ = hkl_parameter_get_value_unit (&_tmp34_);

		gtk_spin_button_set_value (_tmp31_, _tmp35_);

	}
}


static gchar* double_to_string (gdouble self) {
	gchar* result = NULL;
	gchar* _tmp0_ = NULL;
	gchar* _tmp1_;
	gint _tmp1__length1;
	const gchar* _tmp2_ = NULL;
	gchar* _tmp3_;
	gchar* _tmp4_;

	_tmp0_ = g_new0 (gchar, G_ASCII_DTOSTR_BUF_SIZE);

	_tmp1_ = _tmp0_;

	_tmp1__length1 = G_ASCII_DTOSTR_BUF_SIZE;

	_tmp2_ = g_ascii_dtostr (_tmp1_, G_ASCII_DTOSTR_BUF_SIZE, self);

	_tmp3_ = g_strdup (_tmp2_);

	_tmp4_ = _tmp3_;

	_tmp1_ = (g_free (_tmp1_), NULL);

	result = _tmp4_;

	return result;

}


static void hkl_gui_window_update_UB (HklGuiWindow* self) {
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

		HklMatrix UB = {0};
		HklSample* _tmp3_;
		HklMatrix _tmp4_ = {0};
		GtkLabel* _tmp5_;
		HklMatrix _tmp6_;
		gdouble** _tmp7_;
		gint _tmp7__length1;
		gdouble* _tmp8_;
		gint _tmp8__length1;
		gdouble _tmp9_;
		gchar* _tmp10_ = NULL;
		gchar* _tmp11_;
		GtkLabel* _tmp12_;
		HklMatrix _tmp13_;
		gdouble** _tmp14_;
		gint _tmp14__length1;
		gdouble* _tmp15_;
		gint _tmp15__length1;
		gdouble _tmp16_;
		gchar* _tmp17_ = NULL;
		gchar* _tmp18_;
		GtkLabel* _tmp19_;
		HklMatrix _tmp20_;
		gdouble** _tmp21_;
		gint _tmp21__length1;
		gdouble* _tmp22_;
		gint _tmp22__length1;
		gdouble _tmp23_;
		gchar* _tmp24_ = NULL;
		gchar* _tmp25_;
		GtkLabel* _tmp26_;
		HklMatrix _tmp27_;
		gdouble** _tmp28_;
		gint _tmp28__length1;
		gdouble* _tmp29_;
		gint _tmp29__length1;
		gdouble _tmp30_;
		gchar* _tmp31_ = NULL;
		gchar* _tmp32_;
		GtkLabel* _tmp33_;
		HklMatrix _tmp34_;
		gdouble** _tmp35_;
		gint _tmp35__length1;
		gdouble* _tmp36_;
		gint _tmp36__length1;
		gdouble _tmp37_;
		gchar* _tmp38_ = NULL;
		gchar* _tmp39_;
		GtkLabel* _tmp40_;
		HklMatrix _tmp41_;
		gdouble** _tmp42_;
		gint _tmp42__length1;
		gdouble* _tmp43_;
		gint _tmp43__length1;
		gdouble _tmp44_;
		gchar* _tmp45_ = NULL;
		gchar* _tmp46_;
		GtkLabel* _tmp47_;
		HklMatrix _tmp48_;
		gdouble** _tmp49_;
		gint _tmp49__length1;
		gdouble* _tmp50_;
		gint _tmp50__length1;
		gdouble _tmp51_;
		gchar* _tmp52_ = NULL;
		gchar* _tmp53_;
		GtkLabel* _tmp54_;
		HklMatrix _tmp55_;
		gdouble** _tmp56_;
		gint _tmp56__length1;
		gdouble* _tmp57_;
		gint _tmp57__length1;
		gdouble _tmp58_;
		gchar* _tmp59_ = NULL;
		gchar* _tmp60_;
		GtkLabel* _tmp61_;
		HklMatrix _tmp62_;
		gdouble** _tmp63_;
		gint _tmp63__length1;
		gdouble* _tmp64_;
		gint _tmp64__length1;
		gdouble _tmp65_;
		gchar* _tmp66_ = NULL;
		gchar* _tmp67_;

		_tmp3_ = sample;

		hkl_sample_get_UB (_tmp3_, &_tmp4_);

		(&UB);

		UB = _tmp4_;

		_tmp5_ = priv->_label_UB11;

		_tmp6_ = UB;

		_tmp7_ = _tmp6_.data;

		_tmp7__length1 = -1;

		_tmp8_ = _tmp7_[0];

		_tmp8__length1 = -1;

		_tmp9_ = _tmp8_[0];

		_tmp10_ = double_to_string (_tmp9_);

		_tmp11_ = _tmp10_;

		gtk_label_set_text (_tmp5_, _tmp11_);

		_g_free0 (_tmp11_);

		_tmp12_ = priv->_label_UB12;

		_tmp13_ = UB;

		_tmp14_ = _tmp13_.data;

		_tmp14__length1 = -1;

		_tmp15_ = _tmp14_[0];

		_tmp15__length1 = -1;

		_tmp16_ = _tmp15_[1];

		_tmp17_ = double_to_string (_tmp16_);

		_tmp18_ = _tmp17_;

		gtk_label_set_text (_tmp12_, _tmp18_);

		_g_free0 (_tmp18_);

		_tmp19_ = priv->_label_UB13;

		_tmp20_ = UB;

		_tmp21_ = _tmp20_.data;

		_tmp21__length1 = -1;

		_tmp22_ = _tmp21_[0];

		_tmp22__length1 = -1;

		_tmp23_ = _tmp22_[2];

		_tmp24_ = double_to_string (_tmp23_);

		_tmp25_ = _tmp24_;

		gtk_label_set_text (_tmp19_, _tmp25_);

		_g_free0 (_tmp25_);

		_tmp26_ = priv->_label_UB21;

		_tmp27_ = UB;

		_tmp28_ = _tmp27_.data;

		_tmp28__length1 = -1;

		_tmp29_ = _tmp28_[1];

		_tmp29__length1 = -1;

		_tmp30_ = _tmp29_[0];

		_tmp31_ = double_to_string (_tmp30_);

		_tmp32_ = _tmp31_;

		gtk_label_set_text (_tmp26_, _tmp32_);

		_g_free0 (_tmp32_);

		_tmp33_ = priv->_label_UB22;

		_tmp34_ = UB;

		_tmp35_ = _tmp34_.data;

		_tmp35__length1 = -1;

		_tmp36_ = _tmp35_[1];

		_tmp36__length1 = -1;

		_tmp37_ = _tmp36_[1];

		_tmp38_ = double_to_string (_tmp37_);

		_tmp39_ = _tmp38_;

		gtk_label_set_text (_tmp33_, _tmp39_);

		_g_free0 (_tmp39_);

		_tmp40_ = priv->_label_UB23;

		_tmp41_ = UB;

		_tmp42_ = _tmp41_.data;

		_tmp42__length1 = -1;

		_tmp43_ = _tmp42_[1];

		_tmp43__length1 = -1;

		_tmp44_ = _tmp43_[2];

		_tmp45_ = double_to_string (_tmp44_);

		_tmp46_ = _tmp45_;

		gtk_label_set_text (_tmp40_, _tmp46_);

		_g_free0 (_tmp46_);

		_tmp47_ = priv->_label_UB31;

		_tmp48_ = UB;

		_tmp49_ = _tmp48_.data;

		_tmp49__length1 = -1;

		_tmp50_ = _tmp49_[2];

		_tmp50__length1 = -1;

		_tmp51_ = _tmp50_[0];

		_tmp52_ = double_to_string (_tmp51_);

		_tmp53_ = _tmp52_;

		gtk_label_set_text (_tmp47_, _tmp53_);

		_g_free0 (_tmp53_);

		_tmp54_ = priv->_label_UB32;

		_tmp55_ = UB;

		_tmp56_ = _tmp55_.data;

		_tmp56__length1 = -1;

		_tmp57_ = _tmp56_[2];

		_tmp57__length1 = -1;

		_tmp58_ = _tmp57_[1];

		_tmp59_ = double_to_string (_tmp58_);

		_tmp60_ = _tmp59_;

		gtk_label_set_text (_tmp54_, _tmp60_);

		_g_free0 (_tmp60_);

		_tmp61_ = priv->_label_UB33;

		_tmp62_ = UB;

		_tmp63_ = _tmp62_.data;

		_tmp63__length1 = -1;

		_tmp64_ = _tmp63_[2];

		_tmp64__length1 = -1;

		_tmp65_ = _tmp64_[2];

		_tmp66_ = double_to_string (_tmp65_);

		_tmp67_ = _tmp66_;

		gtk_label_set_text (_tmp61_, _tmp67_);

		_g_free0 (_tmp67_);

		(&UB);

	}
}


static void hkl_gui_window_update_UxUyUz (HklGuiWindow* self) {
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

		GtkSpinButton* _tmp3_;
		HklSample* _tmp4_;
		HklParameter* _tmp5_;
		gdouble _tmp6_ = 0.0;
		GtkSpinButton* _tmp7_;
		HklSample* _tmp8_;
		HklParameter* _tmp9_;
		gdouble _tmp10_ = 0.0;
		GtkSpinButton* _tmp11_;
		HklSample* _tmp12_;
		HklParameter* _tmp13_;
		gdouble _tmp14_ = 0.0;
		GtkCheckButton* _tmp15_;
		HklSample* _tmp16_;
		HklParameter* _tmp17_;
		gboolean _tmp18_;
		GtkCheckButton* _tmp19_;
		HklSample* _tmp20_;
		HklParameter* _tmp21_;
		gboolean _tmp22_;
		GtkCheckButton* _tmp23_;
		HklSample* _tmp24_;
		HklParameter* _tmp25_;
		gboolean _tmp26_;

		_tmp3_ = priv->_spinbutton_ux;

		_tmp4_ = sample;

		_tmp5_ = _tmp4_->ux;

		_tmp6_ = hkl_parameter_get_value_unit (_tmp5_);

		gtk_spin_button_set_value (_tmp3_, _tmp6_);

		_tmp7_ = priv->_spinbutton_uy;

		_tmp8_ = sample;

		_tmp9_ = _tmp8_->uy;

		_tmp10_ = hkl_parameter_get_value_unit (_tmp9_);

		gtk_spin_button_set_value (_tmp7_, _tmp10_);

		_tmp11_ = priv->_spinbutton_uz;

		_tmp12_ = sample;

		_tmp13_ = _tmp12_->uz;

		_tmp14_ = hkl_parameter_get_value_unit (_tmp13_);

		gtk_spin_button_set_value (_tmp11_, _tmp14_);

		_tmp15_ = priv->_checkbutton_Ux;

		_tmp16_ = sample;

		_tmp17_ = _tmp16_->ux;

		_tmp18_ = (*_tmp17_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp15_, _tmp18_);

		_tmp19_ = priv->_checkbutton_Uy;

		_tmp20_ = sample;

		_tmp21_ = _tmp20_->uy;

		_tmp22_ = (*_tmp21_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp19_, _tmp22_);

		_tmp23_ = priv->_checkbutton_Uz;

		_tmp24_ = sample;

		_tmp25_ = _tmp24_->uz;

		_tmp26_ = (*_tmp25_).fit;

		gtk_toggle_button_set_active ((GtkToggleButton*) _tmp23_, _tmp26_);

	}
}


static void _hkl_gui_window_update_reflections (HklSample* sample, GtkListStore* model) {
	gsize i = 0UL;
	GtkListStore* _tmp0_;
	HklSample* _tmp1_;
	HklSampleReflection** _tmp2_;
	gint _tmp2__length1;

	g_return_if_fail (sample != NULL);

	g_return_if_fail (model != NULL);

	_tmp0_ = model;

	gtk_list_store_clear (_tmp0_);

	i = (gsize) 0;

	_tmp1_ = sample;

	_tmp2_ = _tmp1_->reflections;

	_tmp2__length1 = _tmp1_->reflections_len;

	{
		HklSampleReflection** reflection_collection = NULL;
		gint reflection_collection_length1 = 0;
		gint _reflection_collection_size_ = 0;
		gint reflection_it = 0;

		reflection_collection = _tmp2_;

		reflection_collection_length1 = _tmp2__length1;

		for (reflection_it = 0; reflection_it < _tmp2__length1; reflection_it = reflection_it + 1) {

			HklSampleReflection* reflection = NULL;

			reflection = reflection_collection[reflection_it];

			{
				GtkTreeIter iter = {0};
				GtkListStore* _tmp3_;
				GtkTreeIter _tmp4_ = {0};
				GtkListStore* _tmp5_;
				GtkTreeIter _tmp6_;
				gsize _tmp7_;
				HklSampleReflection* _tmp8_;
				HklVector _tmp9_;
				gdouble* _tmp10_;
				gint _tmp10__length1;
				gdouble _tmp11_;
				HklSampleReflection* _tmp12_;
				HklVector _tmp13_;
				gdouble* _tmp14_;
				gint _tmp14__length1;
				gdouble _tmp15_;
				HklSampleReflection* _tmp16_;
				HklVector _tmp17_;
				gdouble* _tmp18_;
				gint _tmp18__length1;
				gdouble _tmp19_;
				HklSampleReflection* _tmp20_;
				gint _tmp21_;

				_tmp3_ = model;

				gtk_list_store_append (_tmp3_, &_tmp4_);

				iter = _tmp4_;

				_tmp5_ = model;

				_tmp6_ = iter;

				_tmp7_ = i;

				i = _tmp7_ + 1;

				_tmp8_ = reflection;

				_tmp9_ = _tmp8_->hkl;

				_tmp10_ = _tmp9_.data;

				_tmp10__length1 = _tmp9_.data_length1;

				_tmp11_ = _tmp10_[0];

				_tmp12_ = reflection;

				_tmp13_ = _tmp12_->hkl;

				_tmp14_ = _tmp13_.data;

				_tmp14__length1 = _tmp13_.data_length1;

				_tmp15_ = _tmp14_[1];

				_tmp16_ = reflection;

				_tmp17_ = _tmp16_->hkl;

				_tmp18_ = _tmp17_.data;

				_tmp18__length1 = _tmp17_.data_length1;

				_tmp19_ = _tmp18_[2];

				_tmp20_ = reflection;

				_tmp21_ = _tmp20_->flag;

				gtk_list_store_set (_tmp5_, &_tmp6_, REFLECTION_COL_INDEX, _tmp7_, REFLECTION_COL_H, _tmp11_, REFLECTION_COL_K, _tmp15_, REFLECTION_COL_L, _tmp19_, REFLECTION_COL_FLAG, _tmp21_, -1);

			}
		}
	}
}


static void hkl_gui_window_update_tree_view_crystals (HklGuiWindow* self) {
	GtkListStore* reflections = NULL;
	GtkTreeIter iter = {0};
	GtkListStore* _tmp0_;
	GtkTreeView* _tmp1_;
	GtkListStore* _tmp2_;
	HklSampleList* _tmp3_;
	HklSample** _tmp4_;
	gint _tmp4__length1;

	g_return_if_fail (self != NULL);

	_tmp0_ = gtk_list_store_new ((gint) SAMPLE_COL_N_COLUMNS, G_TYPE_POINTER, GTK_TYPE_LIST_STORE, G_TYPE_STRING, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE);

	_g_object_unref0 (priv->store_samples);

	priv->store_samples = _tmp0_;

	_tmp1_ = priv->_treeview_crystals;

	_tmp2_ = priv->store_samples;

	gtk_tree_view_set_model (_tmp1_, (GtkTreeModel*) _tmp2_);

	_tmp3_ = priv->samples;

	_tmp4_ = _tmp3_->samples;

	_tmp4__length1 = _tmp3_->len;

	{
		HklSample** sample_collection = NULL;
		gint sample_collection_length1 = 0;
		gint _sample_collection_size_ = 0;
		gint sample_it = 0;

		sample_collection = _tmp4_;

		sample_collection_length1 = _tmp4__length1;

		for (sample_it = 0; sample_it < _tmp4__length1; sample_it = sample_it + 1) {

			HklSample* sample = NULL;

			sample = sample_collection[sample_it];

			{
				HklSample* _tmp5_;
				HklLattice* _tmp6_;
				HklLattice* lattice;
				GtkListStore* _tmp7_;
				HklSample* _tmp8_;
				GtkListStore* _tmp9_;
				GtkListStore* _tmp10_;
				GtkTreeIter _tmp11_ = {0};
				GtkListStore* _tmp12_;
				GtkTreeIter _tmp13_;
				HklSample* _tmp14_;
				GtkListStore* _tmp15_;
				HklSample* _tmp16_;
				const gchar* _tmp17_;
				HklLattice* _tmp18_;
				HklParameter* _tmp19_;
				HklParameter _tmp20_;
				gdouble _tmp21_ = 0.0;
				HklLattice* _tmp22_;
				HklParameter* _tmp23_;
				HklParameter _tmp24_;
				gdouble _tmp25_ = 0.0;
				HklLattice* _tmp26_;
				HklParameter* _tmp27_;
				HklParameter _tmp28_;
				gdouble _tmp29_ = 0.0;
				HklLattice* _tmp30_;
				HklParameter* _tmp31_;
				HklParameter _tmp32_;
				gdouble _tmp33_ = 0.0;
				HklLattice* _tmp34_;
				HklParameter* _tmp35_;
				HklParameter _tmp36_;
				gdouble _tmp37_ = 0.0;
				HklLattice* _tmp38_;
				HklParameter* _tmp39_;
				HklParameter _tmp40_;
				gdouble _tmp41_ = 0.0;
				gboolean _tmp42_ = FALSE;
				HklSampleList* _tmp43_;
				HklSample* _tmp44_;
				gboolean _tmp50_;

				_tmp5_ = sample;

				_tmp6_ = _tmp5_->lattice;

				lattice = _tmp6_;

				_tmp7_ = gtk_list_store_new ((gint) REFLECTION_COL_N_COLUMNS, G_TYPE_INT, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_DOUBLE, G_TYPE_BOOLEAN);

				_g_object_unref0 (reflections);

				reflections = _tmp7_;

				_tmp8_ = sample;

				_tmp9_ = reflections;

				_hkl_gui_window_update_reflections (_tmp8_, _tmp9_);

				_tmp10_ = priv->store_samples;

				gtk_list_store_append (_tmp10_, &_tmp11_);

				iter = _tmp11_;

				_tmp12_ = priv->store_samples;

				_tmp13_ = iter;

				_tmp14_ = sample;

				_tmp15_ = reflections;

				_tmp16_ = sample;

				_tmp17_ = _tmp16_->name;

				_tmp18_ = lattice;

				_tmp19_ = _tmp18_->a;

				_tmp20_ = *_tmp19_;

				_tmp21_ = hkl_parameter_get_value_unit (&_tmp20_);

				_tmp22_ = lattice;

				_tmp23_ = _tmp22_->b;

				_tmp24_ = *_tmp23_;

				_tmp25_ = hkl_parameter_get_value_unit (&_tmp24_);

				_tmp26_ = lattice;

				_tmp27_ = _tmp26_->c;

				_tmp28_ = *_tmp27_;

				_tmp29_ = hkl_parameter_get_value_unit (&_tmp28_);

				_tmp30_ = lattice;

				_tmp31_ = _tmp30_->alpha;

				_tmp32_ = *_tmp31_;

				_tmp33_ = hkl_parameter_get_value_unit (&_tmp32_);

				_tmp34_ = lattice;

				_tmp35_ = _tmp34_->beta;

				_tmp36_ = *_tmp35_;

				_tmp37_ = hkl_parameter_get_value_unit (&_tmp36_);

				_tmp38_ = lattice;

				_tmp39_ = _tmp38_->gamma;

				_tmp40_ = *_tmp39_;

				_tmp41_ = hkl_parameter_get_value_unit (&_tmp40_);

				gtk_list_store_set (_tmp12_, &_tmp13_, SAMPLE_COL_SAMPLE, _tmp14_, SAMPLE_COL_REFLECTIONS, _tmp15_, SAMPLE_COL_NAME, _tmp17_, SAMPLE_COL_A, _tmp21_, SAMPLE_COL_B, _tmp25_, SAMPLE_COL_C, _tmp29_, SAMPLE_COL_ALPHA, _tmp33_, SAMPLE_COL_BETA, _tmp37_, SAMPLE_COL_GAMMA, _tmp41_, -1);

				_tmp43_ = priv->samples;

				_tmp44_ = _tmp43_->current;

				if (_tmp44_ != NULL) {

					HklSampleList* _tmp45_;
					HklSample* _tmp46_;
					const gchar* _tmp47_;
					HklSample* _tmp48_;
					const gchar* _tmp49_;

					_tmp45_ = priv->samples;

					_tmp46_ = _tmp45_->current;

					_tmp47_ = _tmp46_->name;

					_tmp48_ = sample;

					_tmp49_ = _tmp48_->name;

					_tmp42_ = g_strcmp0 (_tmp47_, _tmp49_) == 0;

				} else {

					_tmp42_ = FALSE;

				}

				_tmp50_ = _tmp42_;

				if (_tmp50_) {

					GtkTreePath* path = NULL;
					GtkListStore* _tmp51_;
					GtkTreeIter _tmp52_;
					GtkTreePath* _tmp53_ = NULL;
					GtkTreeView* _tmp54_;
					GtkTreePath* _tmp55_;
					GtkTreeView* _tmp56_;
					GtkListStore* _tmp57_;

					_tmp51_ = priv->store_samples;

					_tmp52_ = iter;

					_tmp53_ = gtk_tree_model_get_path ((GtkTreeModel*) _tmp51_, &_tmp52_);

					_gtk_tree_path_free0 (path);

					path = _tmp53_;

					_tmp54_ = priv->_treeview_crystals;

					_tmp55_ = path;

					gtk_tree_view_set_cursor (_tmp54_, _tmp55_, NULL, FALSE);

					_tmp56_ = priv->_treeview_reflections;

					_tmp57_ = reflections;

					gtk_tree_view_set_model (_tmp56_, (GtkTreeModel*) _tmp57_);

					_gtk_tree_path_free0 (path);

				}
			}
		}
	}

	_g_object_unref0 (reflections);

}


static void hkl_gui_window_update_reflections (HklGuiWindow* self, HklSample* sample) {
	gboolean valid = FALSE;
	GtkTreeIter iter = {0};
	GtkListStore* _tmp0_;
	GtkTreeIter _tmp1_ = {0};
	gboolean _tmp2_ = FALSE;

	g_return_if_fail (self != NULL);

	g_return_if_fail (sample != NULL);

	_tmp0_ = priv->store_samples;

	_tmp2_ = gtk_tree_model_get_iter_first ((GtkTreeModel*) _tmp0_, &_tmp1_);

	iter = _tmp1_;

	valid = _tmp2_;

	while (TRUE) {

		gboolean _tmp3_;
		HklSample* sample_iter = NULL;
		GtkListStore* model = NULL;
		GtkListStore* _tmp4_;
		GtkTreeIter _tmp5_;
		HklSample* _tmp6_;
		HklSample* _tmp7_;
		GtkListStore* _tmp10_;
		gboolean _tmp11_ = FALSE;

		_tmp3_ = valid;

		if (!_tmp3_) {

			break;

		}

		_tmp4_ = priv->store_samples;

		_tmp5_ = iter;

		gtk_tree_model_get ((GtkTreeModel*) _tmp4_, &_tmp5_, SAMPLE_COL_SAMPLE, &sample_iter, SAMPLE_COL_REFLECTIONS, &model, -1);

		_tmp6_ = sample;

		_tmp7_ = sample_iter;

		if (_tmp6_ == _tmp7_) {

			HklSample* _tmp8_;
			GtkListStore* _tmp9_;

			_tmp8_ = sample;

			_tmp9_ = model;

			_hkl_gui_window_update_reflections (_tmp8_, _tmp9_);

			_g_object_unref0 (model);

			break;

		}

		_tmp10_ = priv->store_samples;

		_tmp11_ = gtk_tree_model_iter_next ((GtkTreeModel*) _tmp10_, &iter);

		valid = _tmp11_;

		_g_object_unref0 (model);

	}
}


static void hkl_gui_window_update_crystal_model (HklGuiWindow* self, HklSample* sample) {
	GtkTreeIter iter = {0};
	gboolean valid = FALSE;
	GtkListStore* _tmp0_;
	GtkTreeIter _tmp1_ = {0};
	gboolean _tmp2_ = FALSE;

	g_return_if_fail (self != NULL);

	g_return_if_fail (sample != NULL);

	_tmp0_ = priv->store_samples;

	_tmp2_ = gtk_tree_model_get_iter_first ((GtkTreeModel*) _tmp0_, &_tmp1_);

	iter = _tmp1_;

	valid = _tmp2_;

	while (TRUE) {

		gboolean _tmp3_;
		gchar* name = NULL;
		GtkListStore* _tmp4_;
		GtkTreeIter _tmp5_;
		const gchar* _tmp6_;
		HklSample* _tmp7_;
		const gchar* _tmp8_;
		GtkListStore* _tmp37_;
		gboolean _tmp38_ = FALSE;

		_tmp3_ = valid;

		if (!_tmp3_) {

			break;

		}

		_tmp4_ = priv->store_samples;

		_tmp5_ = iter;

		gtk_tree_model_get ((GtkTreeModel*) _tmp4_, &_tmp5_, SAMPLE_COL_NAME, &name, -1);

		_tmp6_ = name;

		_tmp7_ = sample;

		_tmp8_ = _tmp7_->name;

		if (g_strcmp0 (_tmp6_, _tmp8_) == 0) {

			HklSample* _tmp9_;
			HklLattice* _tmp10_;
			HklLattice* lattice;
			GtkListStore* _tmp11_;
			GtkTreeIter _tmp12_;
			HklLattice* _tmp13_;
			HklParameter* _tmp14_;
			HklParameter _tmp15_;
			gdouble _tmp16_ = 0.0;
			HklLattice* _tmp17_;
			HklParameter* _tmp18_;
			HklParameter _tmp19_;
			gdouble _tmp20_ = 0.0;
			HklLattice* _tmp21_;
			HklParameter* _tmp22_;
			HklParameter _tmp23_;
			gdouble _tmp24_ = 0.0;
			HklLattice* _tmp25_;
			HklParameter* _tmp26_;
			HklParameter _tmp27_;
			gdouble _tmp28_ = 0.0;
			HklLattice* _tmp29_;
			HklParameter* _tmp30_;
			HklParameter _tmp31_;
			gdouble _tmp32_ = 0.0;
			HklLattice* _tmp33_;
			HklParameter* _tmp34_;
			HklParameter _tmp35_;
			gdouble _tmp36_ = 0.0;

			_tmp9_ = sample;

			_tmp10_ = _tmp9_->lattice;

			lattice = _tmp10_;

			_tmp11_ = priv->store_samples;

			_tmp12_ = iter;

			_tmp13_ = lattice;

			_tmp14_ = _tmp13_->a;

			_tmp15_ = *_tmp14_;

			_tmp16_ = hkl_parameter_get_value_unit (&_tmp15_);

			_tmp17_ = lattice;

			_tmp18_ = _tmp17_->b;

			_tmp19_ = *_tmp18_;

			_tmp20_ = hkl_parameter_get_value_unit (&_tmp19_);

			_tmp21_ = lattice;

			_tmp22_ = _tmp21_->c;

			_tmp23_ = *_tmp22_;

			_tmp24_ = hkl_parameter_get_value_unit (&_tmp23_);

			_tmp25_ = lattice;

			_tmp26_ = _tmp25_->alpha;

			_tmp27_ = *_tmp26_;

			_tmp28_ = hkl_parameter_get_value_unit (&_tmp27_);

			_tmp29_ = lattice;

			_tmp30_ = _tmp29_->beta;

			_tmp31_ = *_tmp30_;

			_tmp32_ = hkl_parameter_get_value_unit (&_tmp31_);

			_tmp33_ = lattice;

			_tmp34_ = _tmp33_->gamma;

			_tmp35_ = *_tmp34_;

			_tmp36_ = hkl_parameter_get_value_unit (&_tmp35_);

			gtk_list_store_set (_tmp11_, &_tmp12_, SAMPLE_COL_A, _tmp16_, SAMPLE_COL_B, _tmp20_, SAMPLE_COL_C, _tmp24_, SAMPLE_COL_ALPHA, _tmp28_, SAMPLE_COL_BETA, _tmp32_, SAMPLE_COL_GAMMA, _tmp36_, -1);

			_g_free0 (name);

			break;

		}

		_tmp37_ = priv->store_samples;

		_tmp38_ = gtk_tree_model_iter_next ((GtkTreeModel*) _tmp37_, &iter);

		valid = _tmp38_;

		_g_free0 (name);

	}
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


static void hkl_gui_window_on_tree_view_crystals_cursor_changed (HklGuiWindow* self) {
	GtkTreePath* path = NULL;
	GtkTreeViewColumn* focus_column = NULL;
	GtkTreeModel* model = NULL;
	GtkListStore* reflections = NULL;
	GtkTreeIter iter = {0};
	gchar* name = NULL;
	GtkTreeView* _tmp0_;
	GtkTreePath* _tmp1_ = NULL;
	GtkTreeViewColumn* _tmp2_ = NULL;
	GtkTreeViewColumn* _tmp3_;
	GtkTreeView* _tmp4_;
	GtkTreeModel* _tmp5_ = NULL;
	GtkTreeModel* _tmp6_;
	GtkTreeIter _tmp7_ = {0};
	GtkTreeIter _tmp8_;
	HklSampleList* _tmp9_;
	HklPseudoAxisEngineList* _tmp10_;
	HklGeometry* _tmp11_;
	HklDetector* _tmp12_;
	HklSampleList* _tmp13_;
	HklSample* _tmp14_;
	GtkTreeView* _tmp15_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_treeview_crystals;

	gtk_tree_view_get_cursor (_tmp0_, &_tmp1_, &_tmp2_);

	_gtk_tree_path_free0 (path);

	path = _tmp1_;

	_g_object_unref0 (focus_column);

	_tmp3_ = _g_object_ref0 (_tmp2_);

	focus_column = _tmp3_;

	_tmp4_ = priv->_treeview_crystals;

	_tmp5_ = gtk_tree_view_get_model (_tmp4_);

	_tmp6_ = _g_object_ref0 (_tmp5_);

	_g_object_unref0 (model);

	model = _tmp6_;

	gtk_tree_model_get_iter (model, &_tmp7_, path);

	iter = _tmp7_;

	_tmp8_ = iter;

	gtk_tree_model_get (model, &_tmp8_, SAMPLE_COL_NAME, &name, SAMPLE_COL_REFLECTIONS, &reflections, -1);

	_tmp9_ = priv->samples;

	hkl_sample_list_select_current (_tmp9_, name);

	_tmp10_ = priv->engines;

	_tmp11_ = priv->geometry;

	_tmp12_ = priv->detector;

	_tmp13_ = priv->samples;

	_tmp14_ = _tmp13_->current;

	hkl_pseudo_axis_engine_list_init (_tmp10_, _tmp11_, _tmp12_, _tmp14_);

	_tmp15_ = priv->_treeview_reflections;

	gtk_tree_view_set_model (_tmp15_, (GtkTreeModel*) reflections);

	hkl_gui_window_update_lattice (self);

	hkl_gui_window_update_lattice_parameters (self);

	hkl_gui_window_update_reciprocal_lattice (self);

	hkl_gui_window_update_UxUyUz (self);

	hkl_gui_window_update_UB (self);

	hkl_gui_window_update_pseudo_axes (self);

	hkl_gui_window_update_pseudo_axes_frames (self);

	_g_free0 (name);

	_g_object_unref0 (reflections);

	_g_object_unref0 (model);

	_g_object_unref0 (focus_column);

	_gtk_tree_path_free0 (path);

}


static void hkl_gui_window_on_spinbutton_a_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_b_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_c_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_alpha_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_beta_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_gamma_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_a_min_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_b_min_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_c_min_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_alpha_min_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_beta_min_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_gamma_min_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_a_max_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_b_max_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_c_max_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_alpha_max_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_beta_max_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

}


static void hkl_gui_window_on_spinbutton_gamma_max_value_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

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


static void hkl_gui_window_on_button2_clicked (HklGuiWindow* self) {
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
		GtkSpinButton* _tmp4_;
		gdouble _tmp5_ = 0.0;
		GtkSpinButton* _tmp6_;
		gdouble _tmp7_ = 0.0;
		GtkSpinButton* _tmp8_;
		gdouble _tmp9_ = 0.0;
		GtkSpinButton* _tmp10_;
		gdouble _tmp11_ = 0.0;
		GtkSpinButton* _tmp12_;
		gdouble _tmp13_ = 0.0;
		GtkSpinButton* _tmp14_;
		gdouble _tmp15_ = 0.0;
		HklSample* _tmp16_;
		GtkSpinButton* _tmp17_;
		gdouble _tmp18_ = 0.0;
		GtkSpinButton* _tmp19_;
		gdouble _tmp20_ = 0.0;
		GtkSpinButton* _tmp21_;
		gdouble _tmp22_ = 0.0;
		HklSample* _tmp23_;
		HklLattice* _tmp24_;
		HklParameter* _tmp25_;
		GtkSpinButton* _tmp26_;
		gdouble _tmp27_ = 0.0;
		GtkSpinButton* _tmp28_;
		gdouble _tmp29_ = 0.0;
		HklParameter _tmp30_;
		HklSample* _tmp31_;
		HklLattice* _tmp32_;
		HklParameter* _tmp33_;
		GtkSpinButton* _tmp34_;
		gdouble _tmp35_ = 0.0;
		GtkSpinButton* _tmp36_;
		gdouble _tmp37_ = 0.0;
		HklParameter _tmp38_;
		HklSample* _tmp39_;
		HklLattice* _tmp40_;
		HklParameter* _tmp41_;
		GtkSpinButton* _tmp42_;
		gdouble _tmp43_ = 0.0;
		GtkSpinButton* _tmp44_;
		gdouble _tmp45_ = 0.0;
		HklParameter _tmp46_;
		HklSample* _tmp47_;
		HklLattice* _tmp48_;
		HklParameter* _tmp49_;
		GtkSpinButton* _tmp50_;
		gdouble _tmp51_ = 0.0;
		GtkSpinButton* _tmp52_;
		gdouble _tmp53_ = 0.0;
		HklParameter _tmp54_;
		HklSample* _tmp55_;
		HklLattice* _tmp56_;
		HklParameter* _tmp57_;
		GtkSpinButton* _tmp58_;
		gdouble _tmp59_ = 0.0;
		GtkSpinButton* _tmp60_;
		gdouble _tmp61_ = 0.0;
		HklParameter _tmp62_;
		HklSample* _tmp63_;
		HklLattice* _tmp64_;
		HklParameter* _tmp65_;
		GtkSpinButton* _tmp66_;
		gdouble _tmp67_ = 0.0;
		GtkSpinButton* _tmp68_;
		gdouble _tmp69_ = 0.0;
		HklParameter _tmp70_;
		HklSample* _tmp71_;

		_tmp3_ = sample;

		_tmp4_ = priv->_spinbutton_a;

		_tmp5_ = gtk_spin_button_get_value (_tmp4_);

		_tmp6_ = priv->_spinbutton_b;

		_tmp7_ = gtk_spin_button_get_value (_tmp6_);

		_tmp8_ = priv->_spinbutton_c;

		_tmp9_ = gtk_spin_button_get_value (_tmp8_);

		_tmp10_ = priv->_spinbutton_alpha;

		_tmp11_ = gtk_spin_button_get_value (_tmp10_);

		_tmp12_ = priv->_spinbutton_beta;

		_tmp13_ = gtk_spin_button_get_value (_tmp12_);

		_tmp14_ = priv->_spinbutton_gamma;

		_tmp15_ = gtk_spin_button_get_value (_tmp14_);

		hkl_sample_set_lattice (_tmp3_, _tmp5_, _tmp7_, _tmp9_, _tmp11_ * HKL_DEGTORAD, _tmp13_ * HKL_DEGTORAD, _tmp15_ * HKL_DEGTORAD);

		_tmp16_ = sample;

		_tmp17_ = priv->_spinbutton_ux;

		_tmp18_ = gtk_spin_button_get_value (_tmp17_);

		_tmp19_ = priv->_spinbutton_uy;

		_tmp20_ = gtk_spin_button_get_value (_tmp19_);

		_tmp21_ = priv->_spinbutton_uz;

		_tmp22_ = gtk_spin_button_get_value (_tmp21_);

		hkl_sample_set_U_from_euler (_tmp16_, _tmp18_ * HKL_DEGTORAD, _tmp20_ * HKL_DEGTORAD, _tmp22_ * HKL_DEGTORAD);

		_tmp23_ = sample;

		_tmp24_ = _tmp23_->lattice;

		_tmp25_ = _tmp24_->a;

		_tmp26_ = priv->_spinbutton_a_min;

		_tmp27_ = gtk_spin_button_get_value (_tmp26_);

		_tmp28_ = priv->_spinbutton_a_max;

		_tmp29_ = gtk_spin_button_get_value (_tmp28_);

		_tmp30_ = *_tmp25_;

		hkl_parameter_set_range_unit (&_tmp30_, _tmp27_, _tmp29_);

		_tmp31_ = sample;

		_tmp32_ = _tmp31_->lattice;

		_tmp33_ = _tmp32_->b;

		_tmp34_ = priv->_spinbutton_b_min;

		_tmp35_ = gtk_spin_button_get_value (_tmp34_);

		_tmp36_ = priv->_spinbutton_b_max;

		_tmp37_ = gtk_spin_button_get_value (_tmp36_);

		_tmp38_ = *_tmp33_;

		hkl_parameter_set_range_unit (&_tmp38_, _tmp35_, _tmp37_);

		_tmp39_ = sample;

		_tmp40_ = _tmp39_->lattice;

		_tmp41_ = _tmp40_->c;

		_tmp42_ = priv->_spinbutton_c_min;

		_tmp43_ = gtk_spin_button_get_value (_tmp42_);

		_tmp44_ = priv->_spinbutton_c_max;

		_tmp45_ = gtk_spin_button_get_value (_tmp44_);

		_tmp46_ = *_tmp41_;

		hkl_parameter_set_range_unit (&_tmp46_, _tmp43_, _tmp45_);

		_tmp47_ = sample;

		_tmp48_ = _tmp47_->lattice;

		_tmp49_ = _tmp48_->alpha;

		_tmp50_ = priv->_spinbutton_alpha_min;

		_tmp51_ = gtk_spin_button_get_value (_tmp50_);

		_tmp52_ = priv->_spinbutton_alpha_max;

		_tmp53_ = gtk_spin_button_get_value (_tmp52_);

		_tmp54_ = *_tmp49_;

		hkl_parameter_set_range_unit (&_tmp54_, _tmp51_, _tmp53_);

		_tmp55_ = sample;

		_tmp56_ = _tmp55_->lattice;

		_tmp57_ = _tmp56_->beta;

		_tmp58_ = priv->_spinbutton_beta_min;

		_tmp59_ = gtk_spin_button_get_value (_tmp58_);

		_tmp60_ = priv->_spinbutton_beta_max;

		_tmp61_ = gtk_spin_button_get_value (_tmp60_);

		_tmp62_ = *_tmp57_;

		hkl_parameter_set_range_unit (&_tmp62_, _tmp59_, _tmp61_);

		_tmp63_ = sample;

		_tmp64_ = _tmp63_->lattice;

		_tmp65_ = _tmp64_->gamma;

		_tmp66_ = priv->_spinbutton_gamma_min;

		_tmp67_ = gtk_spin_button_get_value (_tmp66_);

		_tmp68_ = priv->_spinbutton_gamma_max;

		_tmp69_ = gtk_spin_button_get_value (_tmp68_);

		_tmp70_ = *_tmp65_;

		hkl_parameter_set_range_unit (&_tmp70_, _tmp67_, _tmp69_);

		_tmp71_ = sample;

		hkl_gui_window_update_crystal_model (self, _tmp71_);

		hkl_gui_window_update_reciprocal_lattice (self);

		hkl_gui_window_update_UB (self);

		hkl_gui_window_update_pseudo_axes (self);

		hkl_gui_window_update_pseudo_axes_frames (self);

	}
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


static gdouble double_parse (const gchar* str) {
	gdouble result = 0.0;
	const gchar* _tmp0_;
	gdouble _tmp1_ = 0.0;

	g_return_val_if_fail (str != NULL, 0.0);

	_tmp0_ = str;

	_tmp1_ = g_ascii_strtod (_tmp0_, NULL);

	result = _tmp1_;

	return result;

}




static void hkl_gui_window_on_cell_tree_view_axes_write_edited (const gchar* path, const gchar* new_text, HklGuiWindow* self) {
	GtkTreeIter iter = {0};
	gdouble value = 0.0;
	HklAxis* axis = NULL;
	GtkListStore* _tmp0_;
	const gchar* _tmp1_;
	GtkTreeIter _tmp2_ = {0};
	GtkListStore* _tmp3_;
	GtkTreeIter _tmp4_;
	const gchar* _tmp5_;
	gdouble _tmp6_ = 0.0;
	HklAxis _tmp7_;
	HklGeometry* _tmp8_;
	GtkListStore* _tmp9_;
	GtkTreeIter _tmp10_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	g_return_if_fail (new_text != NULL);

	_tmp0_ = priv->store_axis;

	_tmp1_ = path;

	gtk_tree_model_get_iter_from_string ((GtkTreeModel*) _tmp0_, &_tmp2_, _tmp1_);

	iter = _tmp2_;

	_tmp3_ = priv->store_axis;

	_tmp4_ = iter;

	gtk_tree_model_get ((GtkTreeModel*) _tmp3_, &_tmp4_, AXIS_COL_AXIS, &axis, -1);

	_tmp5_ = new_text;

	_tmp6_ = double_parse (_tmp5_);

	value = _tmp6_;

	_tmp7_ = *axis;

	hkl_axis_set_value_unit (&_tmp7_, value);

	_tmp8_ = priv->geometry;
	hkl_geometry_update (_tmp8_);

	_tmp9_ = priv->store_axis;

	_tmp10_ = iter;

	gtk_list_store_set (_tmp9_, &_tmp10_, AXIS_COL_WRITE, value, -1);

	hkl_gui_window_update_pseudo_axes (self);

	hkl_gui_window_update_pseudo_axes_frames (self);

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


static void hkl_gui_window_on_cell_tree_view_crystals_name_edited (const gchar* path, const gchar* new_text, HklGuiWindow* self) {
	GtkTreeModel* model = NULL;
	GtkTreeIter iter = {0};
	HklSample* sample = NULL;
	gchar* name = NULL;
	GtkTreeView* _tmp0_;
	GtkTreeModel* _tmp1_ = NULL;
	GtkTreeModel* _tmp2_;
	GtkTreeModel* _tmp3_;
	const gchar* _tmp4_;
	GtkTreeIter _tmp5_ = {0};
	GtkTreeModel* _tmp6_;
	GtkTreeIter _tmp7_;
	HklSampleList* _tmp8_;
	const gchar* _tmp9_;
	HklSample* _tmp10_ = NULL;
	HklSample* _tmp11_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	g_return_if_fail (new_text != NULL);

	_tmp0_ = priv->_treeview_crystals;

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

	gtk_tree_model_get (_tmp6_, &_tmp7_, SAMPLE_COL_NAME, &name, -1);

	_tmp8_ = priv->samples;

	_tmp9_ = name;

	_tmp10_ = hkl_sample_list_get_by_name (_tmp8_, _tmp9_);

	sample = _tmp10_;

	_tmp11_ = sample;

	if (_tmp11_ != NULL) {

		HklSample* _tmp12_;
		const gchar* _tmp13_;

		_tmp12_ = sample;

		_tmp13_ = new_text;

		hkl_sample_set_name (_tmp12_, _tmp13_);

		hkl_gui_window_update_tree_view_crystals (self);

	}

	_g_free0 (name);

	_g_object_unref0 (model);

}


static void hkl_gui_window_on_cell_tree_view_reflections_h_edited (const gchar* path, const gchar* new_text, HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	g_return_if_fail (new_text != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		gint index = 0;
		gdouble h = 0.0;
		gdouble k = 0.0;
		gdouble l = 0.0;
		HklSampleReflection* reflection = NULL;
		GtkListStore* model = NULL;
		GtkTreeIter iter = {0};
		GtkTreeView* _tmp3_;
		GtkTreeModel* _tmp4_ = NULL;
		GtkListStore* _tmp5_;
		GtkListStore* _tmp6_;
		const gchar* _tmp7_;
		GtkTreeIter _tmp8_ = {0};
		GtkListStore* _tmp9_;
		GtkTreeIter _tmp10_;
		HklSample* _tmp11_;
		HklSampleReflection** _tmp12_;
		gint _tmp12__length1;
		gint _tmp13_;
		HklSampleReflection* _tmp14_;
		const gchar* _tmp15_;
		gdouble _tmp16_ = 0.0;
		HklSampleReflection* _tmp17_;
		HklVector _tmp18_;
		gdouble* _tmp19_;
		gint _tmp19__length1;
		gdouble _tmp20_;
		HklSampleReflection* _tmp21_;
		HklVector _tmp22_;
		gdouble* _tmp23_;
		gint _tmp23__length1;
		gdouble _tmp24_;
		HklSampleReflection* _tmp25_;
		gdouble _tmp26_;
		gdouble _tmp27_;
		gdouble _tmp28_;
		GtkListStore* _tmp29_;
		GtkTreeIter _tmp30_;
		gdouble _tmp31_;
		HklSampleReflection* _tmp32_;
		gint _tmp33_;
		HklSample* _tmp34_;

		_tmp3_ = priv->_treeview_reflections;

		_tmp4_ = gtk_tree_view_get_model (_tmp3_);

		_tmp5_ = _g_object_ref0 (G_TYPE_CHECK_INSTANCE_TYPE (_tmp4_, GTK_TYPE_LIST_STORE) ? ((GtkListStore*) _tmp4_) : NULL);

		_g_object_unref0 (model);

		model = _tmp5_;

		_tmp6_ = model;

		_tmp7_ = path;

		gtk_tree_model_get_iter_from_string ((GtkTreeModel*) _tmp6_, &_tmp8_, _tmp7_);

		iter = _tmp8_;

		_tmp9_ = model;

		_tmp10_ = iter;

		gtk_tree_model_get ((GtkTreeModel*) _tmp9_, &_tmp10_, REFLECTION_COL_INDEX, &index, -1);

		_tmp11_ = sample;

		_tmp12_ = _tmp11_->reflections;

		_tmp12__length1 = _tmp11_->reflections_len;

		_tmp13_ = index;

		_tmp14_ = _tmp12_[_tmp13_];

		reflection = _tmp14_;

		_tmp15_ = new_text;

		_tmp16_ = double_parse (_tmp15_);

		h = _tmp16_;

		_tmp17_ = reflection;

		_tmp18_ = _tmp17_->hkl;

		_tmp19_ = _tmp18_.data;

		_tmp19__length1 = _tmp18_.data_length1;

		_tmp20_ = _tmp19_[1];

		k = _tmp20_;

		_tmp21_ = reflection;

		_tmp22_ = _tmp21_->hkl;

		_tmp23_ = _tmp22_.data;

		_tmp23__length1 = _tmp22_.data_length1;

		_tmp24_ = _tmp23_[2];

		l = _tmp24_;

		_tmp25_ = reflection;

		_tmp26_ = h;

		_tmp27_ = k;

		_tmp28_ = l;

		hkl_sample_reflection_set_hkl (_tmp25_, _tmp26_, _tmp27_, _tmp28_);

		_tmp29_ = model;

		_tmp30_ = iter;

		_tmp31_ = h;

		_tmp32_ = reflection;

		_tmp33_ = _tmp32_->flag;

		gtk_list_store_set (_tmp29_, &_tmp30_, REFLECTION_COL_H, _tmp31_, REFLECTION_COL_FLAG, _tmp33_, -1);

		_tmp34_ = sample;

		hkl_gui_window_update_crystal_model (self, _tmp34_);

		_g_object_unref0 (model);

	}
}


static void hkl_gui_window_on_cell_tree_view_reflections_k_edited (const gchar* path, const gchar* new_text, HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	g_return_if_fail (new_text != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		gint index = 0;
		gdouble h = 0.0;
		gdouble k = 0.0;
		gdouble l = 0.0;
		GtkListStore* model = NULL;
		GtkTreeIter iter = {0};
		HklSampleReflection* reflection = NULL;
		GtkTreeView* _tmp3_;
		GtkTreeModel* _tmp4_ = NULL;
		GtkListStore* _tmp5_;
		GtkListStore* _tmp6_;
		const gchar* _tmp7_;
		GtkTreeIter _tmp8_ = {0};
		GtkListStore* _tmp9_;
		GtkTreeIter _tmp10_;
		HklSample* _tmp11_;
		HklSampleReflection** _tmp12_;
		gint _tmp12__length1;
		gint _tmp13_;
		HklSampleReflection* _tmp14_;
		HklSampleReflection* _tmp15_;
		HklVector _tmp16_;
		gdouble* _tmp17_;
		gint _tmp17__length1;
		gdouble _tmp18_;
		const gchar* _tmp19_;
		gdouble _tmp20_ = 0.0;
		HklSampleReflection* _tmp21_;
		HklVector _tmp22_;
		gdouble* _tmp23_;
		gint _tmp23__length1;
		gdouble _tmp24_;
		HklSampleReflection* _tmp25_;
		gdouble _tmp26_;
		gdouble _tmp27_;
		gdouble _tmp28_;
		GtkListStore* _tmp29_;
		GtkTreeIter _tmp30_;
		gdouble _tmp31_;
		HklSampleReflection* _tmp32_;
		gint _tmp33_;
		HklSample* _tmp34_;

		_tmp3_ = priv->_treeview_reflections;

		_tmp4_ = gtk_tree_view_get_model (_tmp3_);

		_tmp5_ = _g_object_ref0 (G_TYPE_CHECK_INSTANCE_TYPE (_tmp4_, GTK_TYPE_LIST_STORE) ? ((GtkListStore*) _tmp4_) : NULL);

		_g_object_unref0 (model);

		model = _tmp5_;

		_tmp6_ = model;

		_tmp7_ = path;

		gtk_tree_model_get_iter_from_string ((GtkTreeModel*) _tmp6_, &_tmp8_, _tmp7_);

		iter = _tmp8_;

		_tmp9_ = model;

		_tmp10_ = iter;

		gtk_tree_model_get ((GtkTreeModel*) _tmp9_, &_tmp10_, REFLECTION_COL_INDEX, &index, -1);

		_tmp11_ = sample;

		_tmp12_ = _tmp11_->reflections;

		_tmp12__length1 = _tmp11_->reflections_len;

		_tmp13_ = index;

		_tmp14_ = _tmp12_[_tmp13_];

		reflection = _tmp14_;

		_tmp15_ = reflection;

		_tmp16_ = _tmp15_->hkl;

		_tmp17_ = _tmp16_.data;

		_tmp17__length1 = _tmp16_.data_length1;

		_tmp18_ = _tmp17_[0];

		h = _tmp18_;

		_tmp19_ = new_text;

		_tmp20_ = double_parse (_tmp19_);

		k = _tmp20_;

		_tmp21_ = reflection;

		_tmp22_ = _tmp21_->hkl;

		_tmp23_ = _tmp22_.data;

		_tmp23__length1 = _tmp22_.data_length1;

		_tmp24_ = _tmp23_[2];

		l = _tmp24_;

		_tmp25_ = reflection;

		_tmp26_ = h;

		_tmp27_ = k;

		_tmp28_ = l;

		hkl_sample_reflection_set_hkl (_tmp25_, _tmp26_, _tmp27_, _tmp28_);

		_tmp29_ = model;

		_tmp30_ = iter;

		_tmp31_ = k;

		_tmp32_ = reflection;

		_tmp33_ = _tmp32_->flag;

		gtk_list_store_set (_tmp29_, &_tmp30_, REFLECTION_COL_K, _tmp31_, REFLECTION_COL_FLAG, _tmp33_, -1);

		_tmp34_ = sample;

		hkl_gui_window_update_crystal_model (self, _tmp34_);

		_g_object_unref0 (model);

	}
}


static void hkl_gui_window_on_cell_tree_view_reflections_l_edited (const gchar* path, const gchar* new_text, HklGuiWindow* self) {
	GtkTreeIter iter = {0};
	GtkListStore* model = NULL;
	GtkTreeView* _tmp0_;
	GtkTreeModel* _tmp1_ = NULL;
	GtkListStore* _tmp2_;
	GtkListStore* _tmp3_;
	const gchar* _tmp4_;
	GtkTreeIter _tmp5_ = {0};
	HklSampleList* _tmp6_;
	HklSample* _tmp7_;
	HklSample* sample;
	HklSample* _tmp8_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	g_return_if_fail (new_text != NULL);

	_tmp0_ = priv->_treeview_reflections;

	_tmp1_ = gtk_tree_view_get_model (_tmp0_);

	_tmp2_ = _g_object_ref0 (G_TYPE_CHECK_INSTANCE_TYPE (_tmp1_, GTK_TYPE_LIST_STORE) ? ((GtkListStore*) _tmp1_) : NULL);

	_g_object_unref0 (model);

	model = _tmp2_;

	_tmp3_ = model;

	_tmp4_ = path;

	gtk_tree_model_get_iter_from_string ((GtkTreeModel*) _tmp3_, &_tmp5_, _tmp4_);

	iter = _tmp5_;

	_tmp6_ = priv->samples;

	_tmp7_ = _tmp6_->current;

	sample = _tmp7_;

	_tmp8_ = sample;

	if (_tmp8_ != NULL) {

		gint index = 0;
		gdouble h = 0.0;
		gdouble k = 0.0;
		gdouble l = 0.0;
		HklSampleReflection* reflection = NULL;
		GtkListStore* _tmp9_;
		GtkTreeIter _tmp10_;
		HklSample* _tmp11_;
		HklSampleReflection** _tmp12_;
		gint _tmp12__length1;
		gint _tmp13_;
		HklSampleReflection* _tmp14_;
		HklSampleReflection* _tmp15_;
		HklVector _tmp16_;
		gdouble* _tmp17_;
		gint _tmp17__length1;
		gdouble _tmp18_;
		HklSampleReflection* _tmp19_;
		HklVector _tmp20_;
		gdouble* _tmp21_;
		gint _tmp21__length1;
		gdouble _tmp22_;
		const gchar* _tmp23_;
		gdouble _tmp24_ = 0.0;
		HklSampleReflection* _tmp25_;
		gdouble _tmp26_;
		gdouble _tmp27_;
		gdouble _tmp28_;
		GtkListStore* _tmp29_;
		GtkTreeIter _tmp30_;
		gdouble _tmp31_;
		HklSampleReflection* _tmp32_;
		gint _tmp33_;
		HklSample* _tmp34_;

		_tmp9_ = model;

		_tmp10_ = iter;

		gtk_tree_model_get ((GtkTreeModel*) _tmp9_, &_tmp10_, REFLECTION_COL_INDEX, &index, -1);

		_tmp11_ = sample;

		_tmp12_ = _tmp11_->reflections;

		_tmp12__length1 = _tmp11_->reflections_len;

		_tmp13_ = index;

		_tmp14_ = _tmp12_[_tmp13_];

		reflection = _tmp14_;

		_tmp15_ = reflection;

		_tmp16_ = _tmp15_->hkl;

		_tmp17_ = _tmp16_.data;

		_tmp17__length1 = _tmp16_.data_length1;

		_tmp18_ = _tmp17_[0];

		h = _tmp18_;

		_tmp19_ = reflection;

		_tmp20_ = _tmp19_->hkl;

		_tmp21_ = _tmp20_.data;

		_tmp21__length1 = _tmp20_.data_length1;

		_tmp22_ = _tmp21_[1];

		k = _tmp22_;

		_tmp23_ = new_text;

		_tmp24_ = double_parse (_tmp23_);

		l = _tmp24_;

		_tmp25_ = reflection;

		_tmp26_ = h;

		_tmp27_ = k;

		_tmp28_ = l;

		hkl_sample_reflection_set_hkl (_tmp25_, _tmp26_, _tmp27_, _tmp28_);

		_tmp29_ = model;

		_tmp30_ = iter;

		_tmp31_ = l;

		_tmp32_ = reflection;

		_tmp33_ = _tmp32_->flag;

		gtk_list_store_set (_tmp29_, &_tmp30_, REFLECTION_COL_L, _tmp31_, REFLECTION_COL_FLAG, _tmp33_, -1);

		_tmp34_ = sample;

		hkl_gui_window_update_crystal_model (self, _tmp34_);

	}

	_g_object_unref0 (model);

}


static void hkl_gui_window_on_cell_tree_view_reflections_flag_toggled (const gchar* path, HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* sample;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	g_return_if_fail (path != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		gint index = 0;
		gint flag = 0;
		HklSampleReflection* reflection = NULL;
		GtkListStore* model = NULL;
		GtkTreeIter iter = {0};
		GtkTreeView* _tmp3_;
		GtkTreeModel* _tmp4_ = NULL;
		GtkListStore* _tmp5_;
		GtkListStore* _tmp6_;
		const gchar* _tmp7_;
		GtkTreeIter _tmp8_ = {0};
		GtkListStore* _tmp9_;
		GtkTreeIter _tmp10_;
		HklSample* _tmp11_;
		HklSampleReflection** _tmp12_;
		gint _tmp12__length1;
		gint _tmp13_;
		HklSampleReflection* _tmp14_;
		HklSampleReflection* _tmp15_;
		gint _tmp16_;
		HklSampleReflection* _tmp17_;
		gint _tmp18_;
		GtkListStore* _tmp19_;
		GtkTreeIter _tmp20_;
		gint _tmp21_;

		_tmp3_ = priv->_treeview_reflections;

		_tmp4_ = gtk_tree_view_get_model (_tmp3_);

		_tmp5_ = _g_object_ref0 (G_TYPE_CHECK_INSTANCE_TYPE (_tmp4_, GTK_TYPE_LIST_STORE) ? ((GtkListStore*) _tmp4_) : NULL);

		_g_object_unref0 (model);

		model = _tmp5_;

		_tmp6_ = model;

		_tmp7_ = path;

		gtk_tree_model_get_iter_from_string ((GtkTreeModel*) _tmp6_, &_tmp8_, _tmp7_);

		iter = _tmp8_;

		_tmp9_ = model;

		_tmp10_ = iter;

		gtk_tree_model_get ((GtkTreeModel*) _tmp9_, &_tmp10_, REFLECTION_COL_INDEX, &index, -1);

		_tmp11_ = sample;

		_tmp12_ = _tmp11_->reflections;

		_tmp12__length1 = _tmp11_->reflections_len;

		_tmp13_ = index;

		_tmp14_ = _tmp12_[_tmp13_];

		reflection = _tmp14_;

		_tmp15_ = reflection;

		_tmp16_ = _tmp15_->flag;

		if (_tmp16_ == 0) {

			flag = 1;

		} else {

			flag = 0;

		}

		_tmp17_ = reflection;

		_tmp18_ = flag;

		hkl_sample_reflection_set_flag (_tmp17_, _tmp18_);

		_tmp19_ = model;

		_tmp20_ = iter;

		_tmp21_ = flag;

		gtk_list_store_set (_tmp19_, &_tmp20_, REFLECTION_COL_FLAG, _tmp21_, -1);

		_g_object_unref0 (model);

	}
}


static void hkl_gui_window_on_toolbutton_add_reflection_clicked (HklGuiWindow* self) {
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

		gdouble h;
		gdouble k;
		gdouble l;
		HklSample* _tmp3_;
		HklGeometry* _tmp4_;
		HklDetector* _tmp5_;
		gdouble _tmp6_;
		gdouble _tmp7_;
		gdouble _tmp8_;
		HklSampleReflection* _tmp9_ = NULL;
		HklSample* _tmp10_;

		h = (gdouble) 0;

		k = (gdouble) 0;

		l = (gdouble) 0;

		_tmp3_ = sample;

		_tmp4_ = priv->geometry;

		_tmp5_ = priv->detector;

		_tmp6_ = h;

		_tmp7_ = k;

		_tmp8_ = l;

		_tmp9_ = hkl_sample_add_reflection (_tmp3_, _tmp4_, _tmp5_, _tmp6_, _tmp7_, _tmp8_);

		_tmp10_ = sample;

		hkl_gui_window_update_reflections (self, _tmp10_);

	}
}


static void _gtk_tree_path_free0_ (gpointer var) {

	(var == NULL) ? NULL : (var = (gtk_tree_path_free (var), NULL));

}


static void _g_list_free__gtk_tree_path_free0_ (GList* self) {

	g_list_foreach (self, (GFunc) _gtk_tree_path_free0_, NULL);

	g_list_free (self);

}


static void hkl_gui_window_on_toolbutton_goto_reflection_clicked (HklGuiWindow* self) {
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

		GtkTreeSelection* selection = NULL;
		guint nb_rows = 0U;
		GtkTreeView* _tmp3_;
		GtkTreeSelection* _tmp4_ = NULL;
		GtkTreeSelection* _tmp5_;
		GtkTreeSelection* _tmp6_;
		gint _tmp7_ = 0;
		guint _tmp8_;

		_tmp3_ = priv->_treeview_reflections;

		_tmp4_ = gtk_tree_view_get_selection (_tmp3_);

		_tmp5_ = _g_object_ref0 (_tmp4_);

		_g_object_unref0 (selection);

		selection = _tmp5_;

		_tmp6_ = selection;

		_tmp7_ = gtk_tree_selection_count_selected_rows (_tmp6_);

		nb_rows = (guint) _tmp7_;

		_tmp8_ = nb_rows;

		if (_tmp8_ == ((guint) 1)) {

			guint index = 0U;
			GtkTreeIter iter = {0};
			GtkTreeModel* model = NULL;
			GtkTreeSelection* _tmp9_;
			GtkTreeModel* _tmp10_ = NULL;
			GList* _tmp11_ = NULL;
			GtkTreeModel* _tmp12_;
			GList* list;
			GtkTreeModel* _tmp13_;
			GList* _tmp14_;
			gconstpointer _tmp15_;
			GtkTreeIter _tmp16_ = {0};
			GtkTreeModel* _tmp17_;
			GtkTreeIter _tmp18_;
			HklGeometry* _tmp19_;
			HklSample* _tmp20_;
			HklSampleReflection** _tmp21_;
			gint _tmp21__length1;
			guint _tmp22_;
			HklSampleReflection* _tmp23_;
			HklGeometry* _tmp24_;

			_tmp9_ = selection;

			_tmp11_ = gtk_tree_selection_get_selected_rows (_tmp9_, &_tmp10_);

			_g_object_unref0 (model);

			_tmp12_ = _g_object_ref0 (_tmp10_);

			model = _tmp12_;

			list = _tmp11_;

			_tmp13_ = model;

			_tmp14_ = list;

			_tmp15_ = _tmp14_->data;

			gtk_tree_model_get_iter (_tmp13_, &_tmp16_, (GtkTreePath*) _tmp15_);

			iter = _tmp16_;

			_tmp17_ = model;

			_tmp18_ = iter;

			gtk_tree_model_get (_tmp17_, &_tmp18_, REFLECTION_COL_INDEX, &index, -1);

			_tmp19_ = priv->geometry;

			_tmp20_ = sample;

			_tmp21_ = _tmp20_->reflections;

			_tmp21__length1 = _tmp20_->reflections_len;

			_tmp22_ = index;

			_tmp23_ = _tmp21_[_tmp22_];

			_tmp24_ = _tmp23_->geometry;

			hkl_geometry_init_geometry (_tmp19_, _tmp24_);

			hkl_gui_window_update_source (self);

			hkl_gui_window_update_axes (self);

			hkl_gui_window_update_pseudo_axes (self);

			__g_list_free__gtk_tree_path_free0_0 (list);

			_g_object_unref0 (model);

		} else {
			guint _tmp25_;

			_tmp25_ = nb_rows;

			if (_tmp25_ > ((guint) 0)) {

				GtkStatusbar* _tmp26_;

				_tmp26_ = priv->_statusBar;

				gtk_statusbar_push (_tmp26_, (guint) 0, "Please select only one reflection.");

			} else {
				GtkStatusbar* _tmp27_;

				_tmp27_ = priv->_statusBar;

				gtk_statusbar_push (_tmp27_, (guint) 0, "Please select at least one reflection.");

			}
		}

		_g_object_unref0 (selection);

	}
}


static gpointer _gtk_tree_path_copy0 (gpointer self) {

	return self ? gtk_tree_path_copy (self) : NULL;

}


static void hkl_gui_window_on_toolbutton_del_reflection_clicked (HklGuiWindow* self) {
	HklSample* sample = NULL;
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* _tmp2_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	sample = _tmp1_;

	_tmp2_ = sample;

	if (_tmp2_ != NULL) {

		GtkTreeSelection* selection = NULL;
		guint nb_rows = 0U;
		GtkTreeView* _tmp3_;
		GtkTreeSelection* _tmp4_ = NULL;
		GtkTreeSelection* _tmp5_;
		GtkTreeSelection* _tmp6_;
		gint _tmp7_ = 0;
		guint _tmp8_;

		_tmp3_ = priv->_treeview_reflections;

		_tmp4_ = gtk_tree_view_get_selection (_tmp3_);

		_tmp5_ = _g_object_ref0 (_tmp4_);

		_g_object_unref0 (selection);

		selection = _tmp5_;

		_tmp6_ = selection;

		_tmp7_ = gtk_tree_selection_count_selected_rows (_tmp6_);

		nb_rows = (guint) _tmp7_;

		_tmp8_ = nb_rows;

		if (_tmp8_ > ((guint) 0)) {

			GtkTreeModel* model = NULL;
			GtkTreeIter iter = {0};
			GtkTreeSelection* _tmp9_;
			GtkTreeModel* _tmp10_ = NULL;
			GList* _tmp11_ = NULL;
			GtkTreeModel* _tmp12_;
			GList* list;
			guint _tmp13_;
			guint* _tmp14_ = NULL;
			guint* indexes;
			gint indexes_length1;
			gint _indexes_size_;
			gint i;
			GList* _tmp15_;
			GtkMessageDialog* _tmp24_;
			GtkMessageDialog* dialog;
			GtkMessageDialog* _tmp25_;
			gint _tmp26_ = 0;
			gint respons;
			gint _tmp27_;

			_tmp9_ = selection;

			_tmp11_ = gtk_tree_selection_get_selected_rows (_tmp9_, &_tmp10_);

			_g_object_unref0 (model);

			_tmp12_ = _g_object_ref0 (_tmp10_);

			model = _tmp12_;

			list = _tmp11_;

			_tmp13_ = nb_rows;

			_tmp14_ = g_new0 (guint, _tmp13_);

			indexes = _tmp14_;

			indexes_length1 = _tmp13_;

			_indexes_size_ = indexes_length1;

			i = 0;

			_tmp15_ = list;

			{
				GList* path_collection = NULL;
				GList* path_it = NULL;

				path_collection = _tmp15_;

				for (path_it = path_collection; path_it != NULL; path_it = path_it->next) {

					GtkTreePath* _tmp16_;
					GtkTreePath* path = NULL;

					_tmp16_ = _gtk_tree_path_copy0 ((GtkTreePath*) path_it->data);

					path = _tmp16_;

					{
						GtkTreeModel* _tmp17_;
						GtkTreePath* _tmp18_;
						GtkTreeIter _tmp19_ = {0};
						GtkTreeModel* _tmp20_;
						GtkTreeIter _tmp21_;
						guint* _tmp22_;
						gint _tmp22__length1;
						gint _tmp23_;

						_tmp17_ = model;

						_tmp18_ = path;

						gtk_tree_model_get_iter (_tmp17_, &_tmp19_, _tmp18_);

						iter = _tmp19_;

						_tmp20_ = model;

						_tmp21_ = iter;

						_tmp22_ = indexes;

						_tmp22__length1 = indexes_length1;

						_tmp23_ = i;

						i = _tmp23_ + 1;

						gtk_tree_model_get (_tmp20_, &_tmp21_, REFLECTION_COL_INDEX, &_tmp22_[_tmp23_], -1);

						_gtk_tree_path_free0 (path);

					}
				}
			}

			_tmp24_ = (GtkMessageDialog*) gtk_message_dialog_new (NULL, GTK_DIALOG_DESTROY_WITH_PARENT, GTK_MESSAGE_WARNING, GTK_BUTTONS_YES_NO, "Are you sure that you want to delete reflections");

			g_object_ref_sink (_tmp24_);

			dialog = _tmp24_;

			_tmp25_ = dialog;

			_tmp26_ = gtk_dialog_run ((GtkDialog*) _tmp25_);

			respons = _tmp26_;

			_tmp27_ = respons;

			switch (_tmp27_) {

			case GTK_RESPONSE_YES:

			{
				HklSample* _tmp39_;
				{
					gboolean _tmp28_;

					i = 0;

					_tmp28_ = TRUE;

					while (TRUE) {

						gboolean _tmp29_;
						gint _tmp31_;
						guint _tmp32_;
						guint* _tmp33_;
						gint _tmp33__length1;
						gint _tmp34_;
						guint _tmp35_;
						gint _tmp36_;
						guint index;
						HklSample* _tmp37_;
						guint _tmp38_;

						_tmp29_ = _tmp28_;

						if (!_tmp29_) {

							gint _tmp30_;

							_tmp30_ = i;

							i = _tmp30_ + 1;

						}

						_tmp28_ = FALSE;

						_tmp31_ = i;

						_tmp32_ = nb_rows;

						if (!(((guint) _tmp31_) < _tmp32_)) {

							break;

						}

						_tmp33_ = indexes;

						_tmp33__length1 = indexes_length1;

						_tmp34_ = i;

						_tmp35_ = _tmp33_[_tmp34_];

						_tmp36_ = i;

						index = _tmp35_ - _tmp36_;

						_tmp37_ = sample;

						_tmp38_ = index;

						hkl_sample_del_reflection (_tmp37_, (gsize) _tmp38_);

					}
				}

				_tmp39_ = sample;

				hkl_gui_window_update_reflections (self, _tmp39_);

				break;

			}
			default:

				break;

			}

			_g_object_unref0 (dialog);

			indexes = (g_free (indexes), NULL);

			__g_list_free__gtk_tree_path_free0_0 (list);

			_g_object_unref0 (model);

		} else {
			GtkStatusbar* _tmp40_;

			_tmp40_ = priv->_statusBar;

			gtk_statusbar_push (_tmp40_, (guint) 0, "Please select at least one reflection.");

		}

		_g_object_unref0 (selection);

	}
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


static void hkl_gui_window_on_toolbutton_add_crystal_clicked (HklGuiWindow* self) {
	HklSample* sample = NULL;
	HklSample* _tmp0_;
	HklSample* _tmp1_;

	g_return_if_fail (self != NULL);

	_tmp0_ = hkl_sample_new ("new_sample", HKL_SAMPLE_TYPE_MONOCRYSTAL);

	sample = _tmp0_;

	_tmp1_ = sample;

	if (_tmp1_ != NULL) {

		GtkTreePath* path = NULL;
		GtkTreeViewColumn* column = NULL;
		HklSampleList* _tmp2_;
		HklSample* _tmp3_;
		HklSampleList* _tmp4_;
		GtkTreeView* _tmp5_;
		GtkTreePath* _tmp6_ = NULL;
		GtkTreeViewColumn* _tmp7_ = NULL;
		GtkTreeViewColumn* _tmp8_;
		GtkTreeView* _tmp9_;
		GtkTreeViewColumn* _tmp10_ = NULL;
		GtkTreeViewColumn* _tmp11_;
		GtkTreeView* _tmp12_;
		GtkTreePath* _tmp13_;
		GtkTreeViewColumn* _tmp14_;

		_tmp2_ = priv->samples;

		_tmp3_ = sample;

		sample = NULL;

		hkl_sample_list_append (_tmp2_, _tmp3_);

		_tmp4_ = priv->samples;

		hkl_sample_list_select_current (_tmp4_, "new_sample");

		hkl_gui_window_update_tree_view_crystals (self);

		_tmp5_ = priv->_treeview_crystals;

		gtk_tree_view_get_cursor (_tmp5_, &_tmp6_, &_tmp7_);

		_gtk_tree_path_free0 (path);

		path = _tmp6_;

		_g_object_unref0 (column);

		_tmp8_ = _g_object_ref0 (_tmp7_);

		column = _tmp8_;

		_tmp9_ = priv->_treeview_crystals;

		_tmp10_ = gtk_tree_view_get_column (_tmp9_, 0);

		_tmp11_ = _g_object_ref0 (_tmp10_);

		_g_object_unref0 (column);

		column = _tmp11_;

		_tmp12_ = priv->_treeview_crystals;

		_tmp13_ = path;

		_tmp14_ = column;

		gtk_tree_view_set_cursor (_tmp12_, _tmp13_, _tmp14_, TRUE);

		_g_object_unref0 (column);

		_gtk_tree_path_free0 (path);

	}
}


static void hkl_gui_window_on_toolbutton_copy_crystal_clicked (HklGuiWindow* self) {
	GtkTreePath* path = NULL;
	GtkTreeViewColumn* column = NULL;
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;
	HklSample* old_sample;
	HklSample* _tmp2_;
	HklSampleList* _tmp4_;
	HklSample* _tmp5_;
	HklSample* _tmp6_;
	HklSample* sample;
	HklSample* _tmp7_;
	HklSampleList* _tmp8_;
	HklSample* _tmp9_;
	HklSampleList* _tmp10_;
	GtkTreeView* _tmp11_;
	GtkTreePath* _tmp12_ = NULL;
	GtkTreeViewColumn* _tmp13_ = NULL;
	GtkTreeViewColumn* _tmp14_;
	GtkTreeView* _tmp15_;
	GtkTreeViewColumn* _tmp16_ = NULL;
	GtkTreeViewColumn* _tmp17_;
	GtkTreeView* _tmp18_;
	GtkTreePath* _tmp19_;
	GtkTreeViewColumn* _tmp20_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	old_sample = _tmp1_;

	_tmp2_ = old_sample;

	if (_tmp2_ == NULL) {

		GtkStatusbar* _tmp3_;

		_tmp3_ = priv->_statusBar;

		gtk_statusbar_push (_tmp3_, (guint) 0, "Please select a crystal to copy.");

		_g_object_unref0 (column);

		_gtk_tree_path_free0 (path);

		return;

	}

	_tmp4_ = priv->samples;

	_tmp5_ = _tmp4_->current;

	_tmp6_ = hkl_sample_new_copy (_tmp5_);

	sample = _tmp6_;

	_tmp7_ = sample;

	hkl_sample_set_name (_tmp7_, "copy");

	_tmp8_ = priv->samples;

	_tmp9_ = sample;

	sample = NULL;

	hkl_sample_list_append (_tmp8_, _tmp9_);

	_tmp10_ = priv->samples;

	hkl_sample_list_select_current (_tmp10_, "copy");

	hkl_gui_window_update_tree_view_crystals (self);

	_tmp11_ = priv->_treeview_crystals;

	gtk_tree_view_get_cursor (_tmp11_, &_tmp12_, &_tmp13_);

	_gtk_tree_path_free0 (path);

	path = _tmp12_;

	_g_object_unref0 (column);

	_tmp14_ = _g_object_ref0 (_tmp13_);

	column = _tmp14_;

	_tmp15_ = priv->_treeview_crystals;

	_tmp16_ = gtk_tree_view_get_column (_tmp15_, 0);

	_tmp17_ = _g_object_ref0 (_tmp16_);

	_g_object_unref0 (column);

	column = _tmp17_;

	_tmp18_ = priv->_treeview_crystals;

	_tmp19_ = path;

	_tmp20_ = column;

	gtk_tree_view_set_cursor (_tmp18_, _tmp19_, _tmp20_, TRUE);

	_g_object_unref0 (column);

	_gtk_tree_path_free0 (path);

}


static void hkl_gui_window_on_toolbutton_del_crystal_clicked (HklGuiWindow* self) {
	HklSampleList* _tmp0_;
	HklSample* _tmp1_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->samples;

	_tmp1_ = _tmp0_->current;

	if (_tmp1_ != NULL) {

		HklSampleList* _tmp2_;
		HklSampleList* _tmp3_;
		HklSample* _tmp4_;

		_tmp2_ = priv->samples;

		_tmp3_ = priv->samples;

		_tmp4_ = _tmp3_->current;

		hkl_sample_list_del (_tmp2_, _tmp4_);

		hkl_gui_window_update_tree_view_crystals (self);

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


static gboolean hkl_gui_window_on_tree_view_reflections_key_press_event (GdkEventKey* event, HklGuiWindow* self) {
	gboolean result = FALSE;

	g_return_val_if_fail (self != NULL, FALSE);

	g_return_val_if_fail (event != NULL, FALSE);

	result = TRUE;

	return result;

}


static gboolean hkl_gui_window_on_tree_view_crystals_key_press_event (GdkEventKey* event, HklGuiWindow* self) {
	gboolean result = FALSE;

	g_return_val_if_fail (self != NULL, FALSE);

	g_return_val_if_fail (event != NULL, FALSE);

	result = TRUE;

	return result;

}


static void hkl_gui_window_on_pseudo_axes_frame_changed (HklGuiWindow* self) {

	g_return_if_fail (self != NULL);

	hkl_gui_window_update_axes (self);

	hkl_gui_window_update_pseudo_axes (self);

	hkl_gui_window_update_pseudo_axes_frames (self);

	hkl_gui_window_update_solutions (self);

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


void hkl_gui_window_on_combobox1_changed (HklGuiWindow* self) {
	gsize idx = 0UL;
	HklGeometryConfig config = {0};
	GtkComboBox* _tmp0_;
	gint _tmp1_ = 0;
	HklGeometryConfig _tmp2_;
	HklGeometryConfig _tmp3_;
	HklGeometry* _tmp4_ = NULL;
	HklGeometryConfig _tmp5_;
	HklPseudoAxisEngineList* _tmp6_ = NULL;
	HklPseudoAxisEngineList* _tmp7_;
	HklGeometry* _tmp8_;
	HklDetector* _tmp9_;
	HklSampleList* _tmp10_;
	HklSample* _tmp11_;

	g_return_if_fail (self != NULL);

	_tmp0_ = priv->_combobox1;

	_tmp1_ = gtk_combo_box_get_active (_tmp0_);

	idx = (gsize) _tmp1_;

	_tmp2_ = hkl_geometry_factory_configs[idx];

	config = _tmp2_;

	_tmp3_ = config;

	_tmp4_ = hkl_geometry_factory_new (&_tmp3_, 50 * HKL_DEGTORAD);

	_hkl_geometry_free0 (priv->geometry);

	priv->geometry = _tmp4_;

	_tmp5_ = config;

	_tmp6_ = hkl_pseudo_axis_engine_list_factory (&_tmp5_);

	_hkl_pseudo_axis_engine_list_free0 (priv->engines);

	priv->engines = _tmp6_;

	_tmp7_ = priv->engines;

	_tmp8_ = priv->geometry;

	_tmp9_ = priv->detector;

	_tmp10_ = priv->samples;

	_tmp11_ = _tmp10_->current;

	hkl_pseudo_axis_engine_list_init (_tmp7_, _tmp8_, _tmp9_, _tmp11_);

	hkl_gui_window_set_up_pseudo_axes_frames (self);

	hkl_gui_window_set_up_tree_view_axes (self);

	hkl_gui_window_set_up_tree_view_pseudo_axes_parameters (self);

	hkl_gui_window_set_up_tree_view_pseudo_axes (self);

	hkl_gui_window_set_up_tree_view_treeview1 (self);

	hkl_gui_window_set_up_3D (self);

}

*/


int main (int argc, char ** argv)
{
	gtk_init (&argc, &argv);

	hkl_gui_window_new ();

	gtk_main ();

	return 0;
}
