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
#ifndef GTKMM_HKL_WINDOW_H
#define GTKMM_HKL_WINDOW_H

#include <map>

#include <gtkmm.h>

#include "axespinbutton.h"
#include "pseudoaxesframe.h"
#include "modelcolumns.h"

class HKLWindow : public Gtk::Window
{
public:
	HKLWindow(HklGeometryType type);
	virtual ~HKLWindow(void);

	HklAxis *get_axe(Glib::ustring const & name);

protected:
	//Signal handlers:
	virtual void on_button_goto_hkl_clicked(void);
	virtual void on_comboboxentrytext_modes_changed(void);
	virtual void on_comboboxentrytext_affinement_changed(void);
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
	virtual void on_spinbutton_max_iteration_value_changed(void);
	virtual void on_checkbutton_a_toggled(void);
	virtual void on_checkbutton_b_toggled(void);
	virtual void on_checkbutton_c_toggled(void);
	virtual void on_checkbutton_alpha_toggled(void);
	virtual void on_checkbutton_beta_toggled(void);
	virtual void on_checkbutton_gamma_toggled(void);
	virtual void on_checkbutton_U_toggled(void);
	virtual void on_axeSpinButton_changed(void);
	virtual void on_pseudoAxeSpinButton_value_changed(void);
	virtual void on_cell_TreeView_axes_read_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_axes_write_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_axes_min_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_axes_max_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_pseudoAxes_write_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_pseudoAxes_is_initialized_toggled(Glib::ustring const &);
	virtual void on_cell_TreeView_pseudoAxes_parameters_value_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_name_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_a_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_b_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_c_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_alpha_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_beta_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_crystals_gamma_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_h_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_k_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_l_edited(Glib::ustring const &, Glib::ustring const &);
	virtual void on_cell_TreeView_reflections_flag_toggled(Glib::ustring const &);
	virtual void on_toolbutton_add_reflection_clicked(void);
	virtual void on_toolbutton_goto_reflection_clicked(void);
	virtual void on_toolbutton_del_reflection_clicked(void);
	virtual void on_toolbutton_computeUB_clicked(void);
	virtual void on_toolbutton_add_crystal_clicked(void);
	virtual void on_toolbutton_copy_crystal_clicked(void);
	virtual void on_toolbutton_del_crystal_clicked(void);
	virtual void on_toolbutton_affiner_clicked(void);
	virtual bool on_treeViewReflections_key_press_event(GdkEventKey *);
	virtual void on_treeViewCrystals_cursor_changed(void);
	virtual void on_treeView_pseudoAxes_cursor_changed(void);
	virtual bool on_treeViewCrystals_key_press_event(GdkEventKey *);
	virtual void on_pseudoAxesFrame_changed(void);

protected:
	//Non-Signal handlers
	void set_up_TreeView_axes(void);
	void set_up_TreeView_pseudoAxes(void);
	void set_up_TreeView_pseudoAxes_parameters(void);
	void updateSource(void);
	void updateAxes(void);
	void updatePseudoAxes(void);
	void update_pseudoAxes_parameters(void);
	void updateLattice(void);
	void updateLatticeParameters(void);
	void updateReciprocalLattice(void);
	void updateFitness(void);
	void updateHKL(void);
	void updateTreeViewCrystals(void);
	void updateUB(void);
	void updateReflections(const HklSample *sample, Glib::RefPtr<Gtk::ListStore> &);
	void updateStatusBar(const HklError *error);
	void updateAffinement(void);
	void updateCrystalModel(HklSample *sample);
	void updatePseudoAxesFrames(void);

private:
	//variables
	Glib::RefPtr<Gtk::Builder> m_refGlade;
	// pointers on usefull widgets.
	Gtk::Label * m_label_UB11;
	Gtk::Label * m_label_UB12;
	Gtk::Label * m_label_UB13;
	Gtk::Label * m_label_UB21;
	Gtk::Label * m_label_UB22;
	Gtk::Label * m_label_UB23;
	Gtk::Label * m_label_UB31;
	Gtk::Label * m_label_UB32;
	Gtk::Label * m_label_UB33;
	Gtk::Label * m_label_fitness;
	Gtk::Label * m_label_nb_iterations;
	Gtk::SpinButton * m_spinbutton_a;
	Gtk::SpinButton * m_spinbutton_b;
	Gtk::SpinButton * m_spinbutton_c;
	Gtk::SpinButton * m_spinbutton_alpha;
	Gtk::SpinButton * m_spinbutton_beta;
	Gtk::SpinButton * m_spinbutton_gamma;
	Gtk::SpinButton * m_spinbutton_a_min;
	Gtk::SpinButton * m_spinbutton_b_min;
	Gtk::SpinButton * m_spinbutton_c_min;
	Gtk::SpinButton * m_spinbutton_alpha_min;
	Gtk::SpinButton * m_spinbutton_beta_min;
	Gtk::SpinButton * m_spinbutton_gamma_min;
	Gtk::SpinButton * m_spinbutton_a_max;
	Gtk::SpinButton * m_spinbutton_b_max;
	Gtk::SpinButton * m_spinbutton_c_max;
	Gtk::SpinButton * m_spinbutton_alpha_max;
	Gtk::SpinButton * m_spinbutton_beta_max;
	Gtk::SpinButton * m_spinbutton_gamma_max;
	Gtk::SpinButton * m_spinbutton_h;
	Gtk::SpinButton * m_spinbutton_k;
	Gtk::SpinButton * m_spinbutton_l;
	Gtk::SpinButton * m_spinbutton_lambda;
	Gtk::SpinButton * m_spinbutton_a_star;
	Gtk::SpinButton * m_spinbutton_b_star;
	Gtk::SpinButton * m_spinbutton_c_star;
	Gtk::SpinButton * m_spinbutton_alpha_star;
	Gtk::SpinButton * m_spinbutton_beta_star;
	Gtk::SpinButton * m_spinbutton_gamma_star;
	Gtk::SpinButton * m_spinbutton_max_iteration;
	Gtk::CheckButton * m_checkbutton_a;
	Gtk::CheckButton * m_checkbutton_b;
	Gtk::CheckButton * m_checkbutton_c;
	Gtk::CheckButton * m_checkbutton_alpha;
	Gtk::CheckButton * m_checkbutton_beta;
	Gtk::CheckButton * m_checkbutton_gamma;
	Gtk::CheckButton * m_checkbutton_U;
	Gtk::Button * m_button_goto_hkl;
	Gtk::TreeView * m_treeViewReflections;
	Gtk::TreeView * m_treeViewCrystals;
	Gtk::TreeView * m_TreeView_axes;
	Gtk::TreeView * m_TreeView_pseudoAxes;
	Gtk::TreeView * m_TreeView_pseudoAxes_parameters;
	Gtk::ToolButton * m_toolbutton_add_reflection;
	Gtk::ToolButton * m_toolbutton_goto_reflection;
	Gtk::ToolButton * m_toolbutton_del_reflection;
	Gtk::ToolButton * m_toolbutton_computeUB;
	Gtk::ToolButton * m_toolbutton_add_crystal;
	Gtk::ToolButton * m_toolbutton_copy_crystal;
	Gtk::ToolButton * m_toolbutton_del_crystal;
	Gtk::ToolButton * m_toolbutton_affiner;
	Gtk::Statusbar * m_statusBar;

	HklGeometry *_geometry;
	HklDetector *_detector;
	HklSampleList *_samples;
	HklPseudoAxisEngineList *_engines;
	HklPseudoAxisEngine *_hkl;

	AxeSpinButtonList m_axeSpinButtonList;
	unsigned int m_nb_axes;
	unsigned int m_nb_sampleAxes;
	unsigned int m_nb_detectorAxes;
	std::vector<std::string> m_sampleAxesNames;
	std::vector<std::string> m_detectorAxesNames;

	unsigned int m_nb_pseudoAxes;
	std::vector<std::string> m_pseudoAxesNames;

	Gtk::ComboBoxText m_comboboxentrytext_modes;
	Gtk::ComboBoxText m_comboboxentrytext_affinement;

	ReflectionModelColumns m_reflectionModelColumns;
	std::map<Glib::ustring, Glib::RefPtr<Gtk::ListStore> > m_mapReflectionModel;

	CrystalModelColumns m_crystalModelColumns;
	Glib::RefPtr<Gtk::ListStore> m_crystalModel;

	AxeModelColumns m_axeModelColumns;
	Glib::RefPtr<Gtk::ListStore> m_axeModel;

	PseudoAxeModelColumns m_pseudoAxeModelColumns;
	Glib::RefPtr<Gtk::ListStore> m_pseudoAxeModel;

	ParameterModelColumns m_parameterModelColumns;
	std::map<HklPseudoAxis *, Glib::RefPtr<Gtk::ListStore> > m_mapPseudoAxeParameterModel;

	Gtk::MessageDialog * m_message;

	std::vector<PseudoAxesFrame *> _pseudoAxesFrames;
};

#endif // GTKMM_HKL_WINDOW_H
