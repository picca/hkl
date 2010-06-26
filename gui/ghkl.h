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

struct _HklGuiWindow
{
	//variables
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
	GtkTreeView *_treeview_pseudoAxes;
	GtkTreeView *_treeview_pseudoAxes_parameters;
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

	//AxeModelColumns _axeModelColumns;
	//GlibRefPtr<GtkListStore> _axeModel;

	//PseudoAxeModelColumns _pseudoAxeModelColumns;
	//GlibRefPtr<GtkListStore> _pseudoAxeModel;

	//ParameterModelColumns _parameterModelColumns;
	//stdmap<HklPseudoAxis *, GlibRefPtr<GtkListStore> > _mapPseudoAxeParameterModel;

	//SolutionModelColumns *_solutionModelColumns;
	//GlibRefPtr<GtkListStore> _solutionModel;

	GtkListStore *store_diffractometer;

	GtkMessageDialog *_message;

	//stdvector<PseudoAxesFrame *> _pseudoAxesFrames;
};

HklGuiWindow *hkl_gui_window_new(void);

//Signal handlers
void on_button2_clicked(void);
void on_spinbutton_a_value_changed(void);
void on_spinbutton_b_value_changed(void);
void on_spinbutton_c_value_changed(void);
void on_spinbutton_alpha_value_changed(void);
void on_spinbutton_beta_value_changed(void);
void on_spinbutton_gamma_value_changed(void);
void on_spinbutton_a_min_value_changed(void);
void on_spinbutton_b_min_value_changed(void);
void on_spinbutton_c_min_value_changed(void);
void on_spinbutton_alpha_min_value_changed(void);
void on_spinbutton_beta_min_value_changed(void);
void on_spinbutton_gamma_min_value_changed(void);
void on_spinbutton_a_max_value_changed(void);
void on_spinbutton_b_max_value_changed(void);
void on_spinbutton_c_max_value_changed(void);
void on_spinbutton_alpha_max_value_changed(void);
void on_spinbutton_beta_max_value_changed(void);
void on_spinbutton_gamma_max_value_changed(void);
void on_spinbutton_lambda_value_changed(void);
void on_spinbutton_uxuyuz_value_changed(void);
void on_checkbutton_a_toggled(void);
void on_checkbutton_b_toggled(void);
void on_checkbutton_c_toggled(void);
void on_checkbutton_alpha_toggled(void);
void on_checkbutton_beta_toggled(void);
void on_checkbutton_gamma_toggled(void);
void on_checkbutton_Ux_toggled(void);
void on_checkbutton_Uy_toggled(void);
void on_checkbutton_Uz_toggled(void);

/*
  void on_cell_TreeView_axes_read_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_axes_write_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_axes_min_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_axes_max_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_pseudoAxes_write_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_pseudoAxes_is_initialized_toggled(Glibustring const &);
  void on_cell_TreeView_pseudoAxes_parameters_value_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_crystals_name_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_reflections_h_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_reflections_k_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_reflections_l_edited(Glibustring const &,
  Glibustring const &);
  void on_cell_TreeView_reflections_flag_toggled(Glibustring const &);
*/
void on_toolbutton_add_reflection_clicked(void);
void on_toolbutton_goto_reflection_clicked(void);
void on_toolbutton_del_reflection_clicked(void);
void on_toolbutton_setUB_clicked(void);
void on_toolbutton_computeUB_clicked(void);
void on_toolbutton_add_crystal_clicked(void);
void on_toolbutton_copy_crystal_clicked(void);
void on_toolbutton_del_crystal_clicked(void);
void on_toolbutton_affiner_clicked(void);
//	bool on_treeViewReflections_key_press_event(GdkEventKey *);
void on_treeViewCrystals_cursor_changed(void);
void on_treeView_pseudoAxes_cursor_changed(void);
//	bool on_treeViewCrystals_key_press_event(GdkEventKey *);
void on_treeview1_cursor_changed(void);
void on_pseudoAxesFrame_changed(void);
void on_menuitem5_activate(void);

// dialog1
void on_button1_clicked(void);
void on_combobox1_changed(void);

//Non-Signal handlers
void set_up_TreeView_axes(void);
void set_up_TreeView_pseudoAxes(void);
void set_up_TreeView_pseudoAxes_parameters(void);
void set_up_TreeView_treeview1(void);
void set_up_TreeView_reflections(void);
void set_up_TreeView_crystals(void);
void updateSource(void);
void updateAxes(void);
void updatePseudoAxes(void);
void update_pseudoAxes_parameters(void);
void updateLattice(void);
void updateLatticeParameters(void);
void updateReciprocalLattice(void);
void updateTreeViewCrystals(void);
void updateUB(void);
void updateUxUyUz(void);
//	void updateReflections(const HklSample *sample, GtkListStore &);
void updateStatusBar(const HklError *error);
void updateCrystalModel(HklSample *sample);
void updatePseudoAxesFrames(void);
void updateSolutions(void);

void get_widgets_and_objects_from_ui(void);
void connect_all_signals(void);
void set_up_pseudo_axes_frames(void);
void set_up_diffractometer_model(void);

#endif // __GHKL_H__
