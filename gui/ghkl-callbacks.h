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
#ifndef __GHKL_CALLBACK_H__
#define __GHKL_CALLBACK_H__

//Signal handlers
extern void on_button2_clicked(void);
extern void on_spinbutton_a_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType  arg1,
					  gpointer       user_datavoidvoid);
extern void on_spinbutton_b_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType  arg1,
					  gpointer       user_datavoidvoid);
extern void on_spinbutton_c_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType  arg1,
					  gpointer       user_datavoidvoid);
extern void on_spinbutton_alpha_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_beta_value_changed(GtkSpinButton *spinbutton,
					     GtkScrollType  arg1,
					     gpointer       user_datavoidvoid);
extern void on_spinbutton_gamma_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_a_min_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_b_min_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_c_min_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_alpha_min_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType  arg1,
						  gpointer       user_datavoidvoid);
extern void on_spinbutton_beta_min_value_changed(GtkSpinButton *spinbutton,
						 GtkScrollType  arg1,
						 gpointer       user_datavoidvoid);
extern void on_spinbutton_gamma_min_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType  arg1,
						  gpointer       user_datavoidvoid);
extern void on_spinbutton_a_max_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_b_max_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_c_max_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType  arg1,
					      gpointer       user_datavoidvoid);
extern void on_spinbutton_alpha_max_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType  arg1,
						  gpointer       user_datavoidvoid);
extern void on_spinbutton_beta_max_value_changed(GtkSpinButton *spinbutton,
						 GtkScrollType  arg1,
						 gpointer       user_datavoidvoid);
extern void on_spinbutton_gamma_max_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType  arg1,
						  gpointer       user_datavoidvoid);
extern void on_spinbutton_lambda_value_changed(GtkSpinButton *spinbutton,
					       GtkScrollType  arg1,
					       gpointer       user_datavoidvoid);
extern void on_spinbutton_uxuyuz_value_changed(GtkSpinButton *spinbutton,
					       GtkScrollType  arg1,
					       gpointer       user_datavoidvoid);
extern void on_checkbutton_a_toggled(void);
extern void on_checkbutton_b_toggled(void);
extern void on_checkbutton_c_toggled(void);
extern void on_checkbutton_alpha_toggled(void);
extern void on_checkbutton_beta_toggled(void);
extern void on_checkbutton_gamma_toggled(void);
extern void on_checkbutton_Ux_toggled(void);
extern void on_checkbutton_Uy_toggled(void);
extern void on_checkbutton_Uz_toggled(void);

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
extern void on_toolbutton_add_reflection_clicked(void);
extern void on_toolbutton_goto_reflection_clicked(void);
extern void on_toolbutton_del_reflection_clicked(void);
extern void on_toolbutton_setUB_clicked(void);
extern void on_toolbutton_computeUB_clicked(void);
extern void on_toolbutton_add_crystal_clicked(void);
extern void on_toolbutton_copy_crystal_clicked(void);
extern void on_toolbutton_del_crystal_clicked(void);
extern void on_toolbutton_affiner_clicked(void);
extern void on_treeview_reflections_key_press_event(GdkEventKey *);
extern void on_treeview_crystals_cursor_changed(void);
extern void on_treeview_pseudoAxes_cursor_changed(void);
extern void on_treeview_crystals_key_press_event(GdkEventKey *);
extern void on_treeview1_cursor_changed(void);
extern void on_pseudoAxesFrame_changed(void);
extern void on_menuitem5_activate(void);

// dialog1
extern void on_button1_clicked(void);
extern void on_combobox1_changed(void);

#endif // __GHKL_CALLBACK_H__
