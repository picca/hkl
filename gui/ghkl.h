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

#include <map>
#include <iostream>

#include <gtkmm.h>

#include "pseudoaxesframe.h"
#include "modelcolumns.h"

#define LOG std::cout << __func__ << std::endl;

class HKLWindow : public Gtk::Window
{
public:
	HKLWindow(void);
	virtual ~HKLWindow(void);

protected:
	//Signal handlers
	virtual void on_button2_clicked(void);
	virtual void on_spinbutton_a_value_changed(void);
	virtual void on_spinbutton_b_value_changed(void);
	virtual void on_spinbutton_c_value_changed(void);
	virtual void on_spinbutton_alpha_value_changed(void);
	virtual void on_spinbutton_beta_value_changed(void);
	virtual void on_spinbutton_gamma_value_changed(void);
	virtual void on_spinbutton_a_min_value_changed(void);
	virtual void on_spinbutton_b_min_value_changed(void);
	virtual void on_spinbutton_c_min_value_changed(void);
	virtual void on_spinbutton_alpha_min_value_changed(void);
	virtual void on_spinbutton_beta_min_value_changed(void);
	virtual void on_spinbutton_gamma_min_value_changed(void);
	virtual void on_spinbutton_a_max_value_changed(void);
	virtual void on_spinbutton_b_max_value_changed(void);
	virtual void on_spinbutton_c_max_value_changed(void);
	virtual void on_spinbutton_alpha_max_value_changed(void);
	virtual void on_spinbutton_beta_max_value_changed(void);
	virtual void on_spinbutton_gamma_max_value_changed(void);
	virtual void on_spinbutton_lambda_value_changed(void);
	virtual void on_spinbutton_uxuyuz_value_changed(void);
	virtual void on_checkbutton_a_toggled(void);
	virtual void on_checkbutton_b_toggled(void);
	virtual void on_checkbutton_c_toggled(void);
	virtual void on_checkbutton_alpha_toggled(void);
	virtual void on_checkbutton_beta_toggled(void);
	virtual void on_checkbutton_gamma_toggled(void);
	virtual void on_checkbutton_Ux_toggled(void);
	virtual void on_checkbutton_Uy_toggled(void);
	virtual void on_checkbutton_Uz_toggled(void);

	virtual void on_cell_TreeView_axes_read_edited(Glib::ustring const &,
						       Glib::ustring const &);
	virtual void on_cell_TreeView_axes_write_edited(Glib::ustring const &,
							Glib::ustring const &);
	virtual void on_cell_TreeView_axes_min_edited(Glib::ustring const &,
						      Glib::ustring const &);
	virtual void on_cell_TreeView_axes_max_edited(Glib::ustring const &,
						      Glib::ustring const &);
	virtual void on_cell_TreeView_pseudoAxes_write_edited(Glib::ustring const &,
							      Glib::ustring const &);
	virtual void on_cell_TreeView_pseudoAxes_is_initialized_toggled(Glib::ustring const &);
	virtual void on_cell_TreeView_pseudoAxes_parameters_value_edited(Glib::ustring const &,
									 Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_name_edited(Glib::ustring const &,
							   Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_h_edited(Glib::ustring const &,
							   Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_k_edited(Glib::ustring const &,
							   Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_l_edited(Glib::ustring const &,
							   Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_flag_toggled(Glib::ustring const &);
	virtual void on_toolbutton_add_reflection_clicked(void);
	virtual void on_toolbutton_goto_reflection_clicked(void);
	virtual void on_toolbutton_del_reflection_clicked(void);
	virtual void on_toolbutton_setUB_clicked(void);
	virtual void on_toolbutton_computeUB_clicked(void);
	virtual void on_toolbutton_add_crystal_clicked(void);
	virtual void on_toolbutton_copy_crystal_clicked(void);
	virtual void on_toolbutton_del_crystal_clicked(void);
	virtual void on_toolbutton_affiner_clicked(void);
	virtual bool on_treeViewReflections_key_press_event(GdkEventKey *);
	virtual void on_treeViewCrystals_cursor_changed(void);
	virtual void on_treeView_pseudoAxes_cursor_changed(void);
	virtual bool on_treeViewCrystals_key_press_event(GdkEventKey *);
	virtual void on_treeview1_cursor_changed(void);
	virtual void on_pseudoAxesFrame_changed(void);
	virtual void on_menuitem5_activate(void);

	// dialog1
	virtual void on_button1_clicked(void);
	virtual void on_combobox1_changed(void);

protected:
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
	void updateReflections(const HklSample *sample, Glib::RefPtr<Gtk::ListStore> &);
	void updateStatusBar(const HklError *error);
	void updateCrystalModel(HklSample *sample);
	void updatePseudoAxesFrames(void);
	void updateSolutions(void);

	void get_widgets_and_objects_from_ui(void);
	void connect_all_signals(void);
	void set_up_pseudo_axes_frames(void);
	void set_up_diffractometer_model(void);

private:
	//variables
	Glib::RefPtr<Gtk::Builder> _refGlade;
	// pointers on usefull widgets.
	Gtk::Label *_label_UB11;
	Gtk::Label *_label_UB12;
	Gtk::Label *_label_UB13;
	Gtk::Label *_label_UB21;
	Gtk::Label *_label_UB22;
	Gtk::Label *_label_UB23;
	Gtk::Label *_label_UB31;
	Gtk::Label *_label_UB32;
	Gtk::Label *_label_UB33;
	Gtk::Button *_button2;
	Gtk::SpinButton *_spinbutton_a;
	Gtk::SpinButton *_spinbutton_b;
	Gtk::SpinButton *_spinbutton_c;
	Gtk::SpinButton *_spinbutton_alpha;
	Gtk::SpinButton *_spinbutton_beta;
	Gtk::SpinButton *_spinbutton_gamma;
	Gtk::SpinButton *_spinbutton_a_min;
	Gtk::SpinButton *_spinbutton_b_min;
	Gtk::SpinButton *_spinbutton_c_min;
	Gtk::SpinButton *_spinbutton_alpha_min;
	Gtk::SpinButton *_spinbutton_beta_min;
	Gtk::SpinButton *_spinbutton_gamma_min;
	Gtk::SpinButton *_spinbutton_a_max;
	Gtk::SpinButton *_spinbutton_b_max;
	Gtk::SpinButton *_spinbutton_c_max;
	Gtk::SpinButton *_spinbutton_alpha_max;
	Gtk::SpinButton *_spinbutton_beta_max;
	Gtk::SpinButton *_spinbutton_gamma_max;
	Gtk::SpinButton *_spinbutton_lambda;
	Gtk::SpinButton *_spinbutton_a_star;
	Gtk::SpinButton *_spinbutton_b_star;
	Gtk::SpinButton *_spinbutton_c_star;
	Gtk::SpinButton *_spinbutton_alpha_star;
	Gtk::SpinButton *_spinbutton_beta_star;
	Gtk::SpinButton *_spinbutton_gamma_star;
	Gtk::SpinButton *_spinbutton_ux;
	Gtk::SpinButton *_spinbutton_uy;
	Gtk::SpinButton *_spinbutton_uz;
	Gtk::SpinButton *_spinbutton_U11;
	Gtk::SpinButton *_spinbutton_U12;
	Gtk::SpinButton *_spinbutton_U13;
	Gtk::SpinButton *_spinbutton_U21;
	Gtk::SpinButton *_spinbutton_U22;
	Gtk::SpinButton *_spinbutton_U23;
	Gtk::SpinButton *_spinbutton_U31;
	Gtk::SpinButton *_spinbutton_U32;
	Gtk::SpinButton *_spinbutton_U33;
	Gtk::CheckButton *_checkbutton_a;
	Gtk::CheckButton *_checkbutton_b;
	Gtk::CheckButton *_checkbutton_c;
	Gtk::CheckButton *_checkbutton_alpha;
	Gtk::CheckButton *_checkbutton_beta;
	Gtk::CheckButton *_checkbutton_gamma;
	Gtk::CheckButton *_checkbutton_Ux;
	Gtk::CheckButton *_checkbutton_Uy;
	Gtk::CheckButton *_checkbutton_Uz;
	Gtk::TreeView *_treeViewReflections;
	Gtk::TreeView *_treeViewCrystals;
	Gtk::TreeView *_TreeView_axes;
	Gtk::TreeView *_TreeView_pseudoAxes;
	Gtk::TreeView *_TreeView_pseudoAxes_parameters;
	Gtk::TreeView *_treeview1; // attached to the _solutionModel
	Gtk::ToolButton *_toolbutton_add_reflection;
	Gtk::ToolButton *_toolbutton_goto_reflection;
	Gtk::ToolButton *_toolbutton_del_reflection;
	Gtk::ToolButton *_toolbutton_setUB;
	Gtk::ToolButton *_toolbutton_computeUB;
	Gtk::ToolButton *_toolbutton_add_crystal;
	Gtk::ToolButton *_toolbutton_copy_crystal;
	Gtk::ToolButton *_toolbutton_del_crystal;
	Gtk::ToolButton *_toolbutton_affiner;
	Gtk::Statusbar *_statusBar;
	Gtk::ImageMenuItem *_menuitem5; // menu preferences

	// dialog1 preferences
	Gtk::Dialog *_dialog1;
	Gtk::Button *_button1; // close
	Gtk::ComboBox *_combobox1; // select diffractometer type

	HklGeometry *_geometry;
	HklDetector *_detector;
	HklSampleList *_samples;
	HklLattice *_reciprocal;
	HklPseudoAxisEngineList *_engines;

	unsigned int _nb_axes;
	unsigned int _nb_sampleAxes;
	unsigned int _nb_detectorAxes;
	std::vector<std::string> _sampleAxesNames;
	std::vector<std::string> _detectorAxesNames;

	unsigned int _nb_pseudoAxes;
	std::vector<std::string> _pseudoAxesNames;

	ReflectionModelColumns _reflectionModelColumns;
	std::map<Glib::ustring, Glib::RefPtr<Gtk::ListStore> > _mapReflectionModel;

	CrystalModelColumns _crystalModelColumns;
	Glib::RefPtr<Gtk::ListStore> _crystalModel;

	AxeModelColumns _axeModelColumns;
	Glib::RefPtr<Gtk::ListStore> _axeModel;

	PseudoAxeModelColumns _pseudoAxeModelColumns;
	Glib::RefPtr<Gtk::ListStore> _pseudoAxeModel;

	ParameterModelColumns _parameterModelColumns;
	std::map<HklPseudoAxis *, Glib::RefPtr<Gtk::ListStore> > _mapPseudoAxeParameterModel;

	SolutionModelColumns *_solutionModelColumns;
	Glib::RefPtr<Gtk::ListStore> _solutionModel;

	DiffractometerModelColumns *_diffractometerModelColumns;
	Glib::RefPtr<Gtk::ListStore> _diffractometerModel;

	Gtk::MessageDialog *_message;

	std::vector<PseudoAxesFrame *> _pseudoAxesFrames;
};

#endif // __GHKL_H__
