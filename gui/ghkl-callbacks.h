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

extern void on_button2_clicked(GtkButton *button,
			       gpointer user_data);

extern void on_spinbutton_a_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType arg1,
					  gpointer user_data);
extern void on_spinbutton_b_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType arg1,
					  gpointer user_data);
extern void on_spinbutton_c_value_changed(GtkSpinButton *spinbutton,
					  GtkScrollType arg1,
					  gpointer user_data);
extern void on_spinbutton_alpha_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_beta_value_changed(GtkSpinButton *spinbutton,
					     GtkScrollType arg1,
					     gpointer user_data);
extern void on_spinbutton_gamma_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_a_min_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_b_min_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_c_min_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_alpha_min_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType arg1,
						  gpointer user_data);
extern void on_spinbutton_beta_min_value_changed(GtkSpinButton *spinbutton,
						 GtkScrollType arg1,
						 gpointer user_data);
extern void on_spinbutton_gamma_min_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType arg1,
						  gpointer user_data);
extern void on_spinbutton_a_max_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_b_max_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_c_max_value_changed(GtkSpinButton *spinbutton,
					      GtkScrollType arg1,
					      gpointer user_data);
extern void on_spinbutton_alpha_max_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType arg1,
						  gpointer user_data);
extern void on_spinbutton_beta_max_value_changed(GtkSpinButton *spinbutton,
						 GtkScrollType arg1,
						 gpointer user_data);
extern void on_spinbutton_gamma_max_value_changed(GtkSpinButton *spinbutton,
						  GtkScrollType arg1,
						  gpointer user_data);
extern void on_spinbutton_lambda_value_changed(GtkSpinButton *spinbutton,
					       GtkScrollType arg1,
					       gpointer user_data);
extern void on_spinbutton_uxuyuz_value_changed(GtkSpinButton *spinbutton,
					       GtkScrollType arg1,
					       gpointer user_data);
extern void on_checkbutton_a_toggled(GtkToggleButton *togglebutton,
				     gpointer user_data);
extern void on_checkbutton_b_toggled(GtkToggleButton *togglebutton,
				     gpointer user_data);
extern void on_checkbutton_c_toggled(GtkToggleButton *togglebutton,
				     gpointer user_data);
extern void on_checkbutton_alpha_toggled(GtkToggleButton *togglebutton,
					 gpointer user_data);
extern void on_checkbutton_beta_toggled(GtkToggleButton *togglebutton,
					gpointer user_data);
extern void on_checkbutton_gamma_toggled(GtkToggleButton *togglebutton,
					 gpointer user_data);
extern void on_checkbutton_Ux_toggled(GtkToggleButton *togglebutton,
				      gpointer user_data);
extern void on_checkbutton_Uy_toggled(GtkToggleButton *togglebutton,
				      gpointer user_data);
extern void on_checkbutton_Uz_toggled(GtkToggleButton *togglebutton,
				      gpointer user_data);

void on_cell_tree_view_axes_read_edited(GtkCellRendererText *renderer,
					gchar *path,
					gchar *new_text,
					gpointer user_data);
void on_cell_tree_view_axes_write_edited(GtkCellRendererText *renderer,
					 gchar *path,
					 gchar *new_text,
					 gpointer user_data);
void on_cell_tree_view_axes_min_edited(GtkCellRendererText *renderer,
				       gchar *path,
				       gchar *new_text,
				       gpointer user_data);
void on_cell_tree_view_axes_max_edited(GtkCellRendererText *renderer,
				       gchar *path,
				       gchar *new_text,
				       gpointer user_data);
void on_cell_tree_view_pseudo_axes_write_edited(GtkCellRendererText *renderer,
						gchar *path,
						gchar *new_text,
						gpointer user_data);
void on_cell_tree_view_pseudo_axes_parameters_value_edited(GtkCellRendererText *renderer,
							   gchar *path,
							   gchar *new_text,
							   gpointer user_data);
void on_cell_tree_view_crystals_name_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data);
void on_cell_tree_view_reflections_h_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data);
void on_cell_tree_view_reflections_k_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data);
void on_cell_tree_view_reflections_l_edited(GtkCellRendererText *renderer,
					    gchar *path,
					    gchar *new_text,
					    gpointer user_data);
void on_cell_tree_view_pseudo_axes_is_initialized_toggled(GtkCellRendererToggle *cell_renderer,
							  gchar *path,
							  gpointer user_data);
void on_cell_tree_view_reflections_flag_toggled(GtkCellRendererToggle *cell_renderer,
						gchar *path,
						gpointer user_data);

extern void on_toolbutton_add_reflection_clicked(GtkToolButton *toolbutton,
						 gpointer user_data);
extern void on_toolbutton_goto_reflection_clicked(GtkToolButton *toolbutton,
						 gpointer user_data);
extern void on_toolbutton_del_reflection_clicked(GtkToolButton *toolbutton,
						 gpointer user_data);
extern void on_toolbutton_setUB_clicked(GtkToolButton *toolbutton,
					gpointer user_data);
extern void on_toolbutton_computeUB_clicked(GtkToolButton *toolbutton,
					    gpointer user_data);
extern void on_toolbutton_add_crystal_clicked(GtkToolButton *toolbutton,
					      gpointer user_data);
extern void on_toolbutton_copy_crystal_clicked(GtkToolButton *toolbutton,
					       gpointer user_data);
extern void on_toolbutton_del_crystal_clicked(GtkToolButton *toolbutton,
					      gpointer user_data);
extern void on_toolbutton_affiner_clicked(GtkToolButton *toolbutton,
					  gpointer user_data);

extern gboolean on_tree_view_reflections_key_press_event(GtkWidget *widget,
							 GdkEventKey *event,
							 gpointer user_data);
extern gboolean on_tree_view_crystals_key_press_event(GtkWidget *widget,
						      GdkEventKey *event,
						      gpointer user_data);

extern void on_tree_view_crystals_cursor_changed(GtkTreeView *tree_view,
						 gpointer user_data);
extern void on_tree_view_pseudo_axes_cursor_changed(GtkTreeView *tree_view,
						    gpointer user_data);
extern void on_tree_view1_cursor_changed(GtkTreeView *tree_view,
					 gpointer user_data);

extern void on_pseudo_axes_frame_changed(gpointer user_data);

extern void on_menuitem5_activate(GtkMenuItem *menuitem,
				  gpointer user_data);

// dialog1
extern void on_button1_clicked(GtkButton *button,
			       gpointer user_data);

extern void on_combobox1_changed(GtkComboBox *widget,
				 gpointer user_data);

#endif // __GHKL_CALLBACK_H__
