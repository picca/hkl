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
#ifndef __GHKL_H__
#define __GHKL_H__

#include <stdio.h>

#include "pseudoaxesframe.h"

#define LOG fprintf(stdout, "%s\n", __func__);

typedef struct _HklGuiWindow HklGuiWindow;

enum
{
	REFLECTION_COL_INDEX = 0,
	REFLECTION_COL_H,
	REFLECTION_COL_K,
	REFLECTION_COL_L,
	REFLECTION_COL_FLAG,
	REFLECTION_N_COLUMNS
};

enum
{
	AXIS_COL_AXIS = 0,
	AXIS_COL_NAME,
	AXIS_COL_READ,
	AXIS_COL_WRITE,
	AXIS_COL_MIN,
	AXIS_COL_MAX,
	AXIS_N_COLUMNS
};

enum
{
	PSEUDOAXIS_COL_PSEUDOAXIS = 0,
	PSEUDOAXIS_COL_NAME,
	PSEUDOAXIS_COL_READ,
	PSEUDOAXIS_COL_WRITE,
	PSEUDOAXIS_COL_MIN,
	PSEUDOAXIS_COL_MAX,
	PSEUDOAXIS_COL_INITIALIZED,
	PSEUDOAXIS_N_COLUMNS
};

enum
{
	PARAMETER_COL_PARAMETER = 0,
	PARAMETER_COL_NAME,
	PARAMETER_COL_VALUE,
	PARAMETER_N_COLUMNS
};

enum
{
	SAMPLE_COL_NAME = 0,
	SAMPLE_COL_A,
	SAMPLE_COL_B,
	SAMPLE_COL_C,
	SAMPLE_COL_ALPHA,
	SAMPLE_COL_BETA,
	SAMPLE_COL_GAMMA,
	SAMPLE_N_COLUMNS
};

enum
{
	SOLUTION_COL_INDEX = 0,
	SOLUTION_N_COLUMNS,
	/* SOLUTION_COL_AXIS1..N */
};

enum
{
	DIFFRACTOMETER_COL_NAME = 0,
};

struct _HklGuiWindow
{
	GtkBuilder *builder;

	// pointers on usefull widgets.
	GtkWindow *window;
	GtkLabel *_label_UB11;
	GtkLabel *_label_UB12;
	GtkLabel *_label_UB13;
	GtkLabel *_label_UB21;
	GtkLabel *_label_UB22;
	GtkLabel *_label_UB23;
	GtkLabel *_label_UB31;
	GtkLabel *_label_UB32;
	GtkLabel *_label_UB33;
	GtkButton *_button2;
	GtkSpinButton *_spinbutton_a;
	GtkSpinButton *_spinbutton_b;
	GtkSpinButton *_spinbutton_c;
	GtkSpinButton *_spinbutton_alpha;
	GtkSpinButton *_spinbutton_beta;
	GtkSpinButton *_spinbutton_gamma;
	GtkSpinButton *_spinbutton_a_min;
	GtkSpinButton *_spinbutton_b_min;
	GtkSpinButton *_spinbutton_c_min;
	GtkSpinButton *_spinbutton_alpha_min;
	GtkSpinButton *_spinbutton_beta_min;
	GtkSpinButton *_spinbutton_gamma_min;
	GtkSpinButton *_spinbutton_a_max;
	GtkSpinButton *_spinbutton_b_max;
	GtkSpinButton *_spinbutton_c_max;
	GtkSpinButton *_spinbutton_alpha_max;
	GtkSpinButton *_spinbutton_beta_max;
	GtkSpinButton *_spinbutton_gamma_max;
	GtkSpinButton *_spinbutton_lambda;
	GtkSpinButton *_spinbutton_a_star;
	GtkSpinButton *_spinbutton_b_star;
	GtkSpinButton *_spinbutton_c_star;
	GtkSpinButton *_spinbutton_alpha_star;
	GtkSpinButton *_spinbutton_beta_star;
	GtkSpinButton *_spinbutton_gamma_star;
	GtkSpinButton *_spinbutton_ux;
	GtkSpinButton *_spinbutton_uy;
	GtkSpinButton *_spinbutton_uz;
	GtkSpinButton *_spinbutton_U11;
	GtkSpinButton *_spinbutton_U12;
	GtkSpinButton *_spinbutton_U13;
	GtkSpinButton *_spinbutton_U21;
	GtkSpinButton *_spinbutton_U22;
	GtkSpinButton *_spinbutton_U23;
	GtkSpinButton *_spinbutton_U31;
	GtkSpinButton *_spinbutton_U32;
	GtkSpinButton *_spinbutton_U33;
	GtkCheckButton *_checkbutton_a;
	GtkCheckButton *_checkbutton_b;
	GtkCheckButton *_checkbutton_c;
	GtkCheckButton *_checkbutton_alpha;
	GtkCheckButton *_checkbutton_beta;
	GtkCheckButton *_checkbutton_gamma;
	GtkCheckButton *_checkbutton_Ux;
	GtkCheckButton *_checkbutton_Uy;
	GtkCheckButton *_checkbutton_Uz;
	GtkTreeView *_treeview_reflections;
	GtkTreeView *_treeview_crystals;
	GtkTreeView *_treeview_axes;
	GtkTreeView *_treeview_pseudo_axes;
	GtkTreeView *_treeview_pseudo_axes_parameters;
	GtkTreeView *_treeview1; // attached to the _solutionModel
	GtkToolButton *_toolbutton_add_reflection;
	GtkToolButton *_toolbutton_goto_reflection;
	GtkToolButton *_toolbutton_del_reflection;
	GtkToolButton *_toolbutton_setUB;
	GtkToolButton *_toolbutton_computeUB;
	GtkToolButton *_toolbutton_add_crystal;
	GtkToolButton *_toolbutton_copy_crystal;
	GtkToolButton *_toolbutton_del_crystal;
	GtkToolButton *_toolbutton_affiner;
	GtkStatusbar *_statusBar;
	GtkImageMenuItem *_menuitem5; // menu preferences

	// dialog1 preferences
	GtkDialog *_dialog1;
	GtkButton *_button1; // close
	GtkComboBox *_combobox1; // select diffractometer type

	HklGeometry *geometry;
	HklDetector *detector;
	HklSampleList *samples;
	HklLattice *reciprocal;
	HklPseudoAxisEngineList *engines;

	unsigned int _nb_axes;
	unsigned int _nb_sampleAxes;
	unsigned int _nb_detectorAxes;
	const char **_sampleAxesNames;
	const char **_detectorAxesNames;

	unsigned int _nb_pseudoAxes;
	const char **_pseudoAxesNames;

	//ReflectionModelColumns _reflectionModelColumns;
	//stdmap<Glibustring, GlibRefPtr<GtkListStore> > _mapReflectionModel;

	//CrystalModelColumns _crystalModelColumns;
	//GlibRefPtr<GtkListStore> _crystalModel;

	GtkListStore *store_diffractometer;
	GtkListStore *store_axis;
	GtkListStore *store_pseudo_axis;
	GtkListStore *store_solutions;
	GHashTable *hash_store_pseudo_axis_parameter; /* use to store the pseudo_axis_parameters liststore */

	GtkListStore **reflections_stores;
	size_t reflections_stores_len;

	GtkMessageDialog *_message;

	HklGuiPseudoAxesFrame **pseudoAxesFrames;
	size_t pseudoAxesFrames_len;
};

extern HklGuiWindow *hkl_gui_window_new(void);

/* Non-Signal handlers */
extern void hkl_gui_set_up_tree_view_axes(HklGuiWindow *self);
extern void hkl_gui_set_up_tree_view_pseudo_axes(HklGuiWindow *self);
extern void hkl_gui_set_up_tree_view_pseudo_axes_parameters(HklGuiWindow *self);
/* STATIC extern void hkl_gui_set_up_tree_view_treeview1(HklGuiWindow *self); */
extern void hkl_gui_set_up_tree_view_reflections(HklGuiWindow *self);
extern void hkl_gui_set_up_tree_view_crystals(HklGuiWindow *self);
extern void hkl_gui_update_source(HklGuiWindow *self);
extern void hkl_gui_update_axes(HklGuiWindow *self);
extern void hkl_gui_update_pseudo_axes(HklGuiWindow *self);
extern void hkl_gui_update_pseudo_axes_parameters(HklGuiWindow *self);
extern void hkl_gui_update_lattice(HklGuiWindow *self);
extern void hkl_gui_update_lattice_parameters(HklGuiWindow *self);
extern void hkl_gui_update_reciprocal_lattice(HklGuiWindow *self);
extern void hkl_gui_update_tree_view_crystals(HklGuiWindow *self);
extern void hkl_gui_update_UB(HklGuiWindow *self);
extern void hkl_gui_update_UxUyUz(HklGuiWindow *self);
//	void updateReflections(const HklSample *sample, GtkListStore &);
extern void hkl_gui_update_status_bar(HklGuiWindow *self, const HklError *error);
extern void hkl_gui_update_crystal_model(HklGuiWindow *self, HklSample *sample);
extern void hkl_gui_update_pseudo_axes_frames(HklGuiWindow *self);
extern void hkl_gui_update_solutions(HklGuiWindow *self);

/* STATIC extern void hkl_gui_get_widgets_and_objects_from_ui(HklGuiWindow *self); */
/* STATIC extern void hkl_gui_connect_all_signals(HklGuiWindow *self); */
extern void hkl_gui_set_up_pseudo_axes_frames(HklGuiWindow *self);
/* STATIC extern void hkl_gui_set_up_diffractometer_model(HklGuiWindow *self); */

#endif // __GHKL_H__
