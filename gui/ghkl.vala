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
using Gtk;

enum ReflectionCol
{
	INDEX = 0,
	H,
	K,
	L,
	FLAG,
	N_COLUMNS
}

enum AxisCol
{
	AXIS = 0,
	NAME,
	READ,
	WRITE,
	MIN,
	MAX,
	N_COLUMNS
}

enum PseudoAxisCol
{
	PSEUDOAXIS = 0,
	NAME,
	READ,
	WRITE,
	MIN,
	MAX,
	INITIALIZED,
	N_COLUMNS
}

enum ParameterCol
{
	PARAMETER = 0,
	NAME,
	VALUE,
	COLUMNS
}

enum SampleCol
{
	SAMPLE = 0,
	REFLECTIONS,
	NAME,
	A,
	B,
	C,
	ALPHA,
	BETA,
	GAMMA,
	N_COLUMNS
}

enum SolutionCol
{
	INDEX = 0,
	N_COLUMNS,
	/* SOLUTION_COL_AXIS1..N */
}

enum DiffractometerCol
{
	NAME = 0,
}

public class Hkl.Gui.Window : 
{
	/***********/
	/* members */
	/***********/

	Gtk.Window window;
	Gtk.Label _label_UB11;
	Gtk.Label _label_UB12;
	Gtk.Label _label_UB13;
	Gtk.Label _label_UB21;
	Gtk.Label _label_UB22;
	Gtk.Label _label_UB23;
	Gtk.Label _label_UB31;
	Gtk.Label _label_UB32;
	Gtk.Label _label_UB33;
	Gtk.Button _button2;
	Gtk.SpinButton _spinbutton_a;
	Gtk.SpinButton _spinbutton_b;
	Gtk.SpinButton _spinbutton_c;
	Gtk.SpinButton _spinbutton_alpha;
	Gtk.SpinButton _spinbutton_beta;
	Gtk.SpinButton _spinbutton_gamma;
	Gtk.SpinButton _spinbutton_a_min;
	Gtk.SpinButton _spinbutton_b_min;
	Gtk.SpinButton _spinbutton_c_min;
	Gtk.SpinButton _spinbutton_alpha_min;
	Gtk.SpinButton _spinbutton_beta_min;
	Gtk.SpinButton _spinbutton_gamma_min;
	Gtk.SpinButton _spinbutton_a_max;
	Gtk.SpinButton _spinbutton_b_max;
	Gtk.SpinButton _spinbutton_c_max;
	Gtk.SpinButton _spinbutton_alpha_max;
	Gtk.SpinButton _spinbutton_beta_max;
	Gtk.SpinButton _spinbutton_gamma_max;
	Gtk.SpinButton _spinbutton_lambda;
	Gtk.SpinButton _spinbutton_a_star;
	Gtk.SpinButton _spinbutton_b_star;
	Gtk.SpinButton _spinbutton_c_star;
	Gtk.SpinButton _spinbutton_alpha_star;
	Gtk.SpinButton _spinbutton_beta_star;
	Gtk.SpinButton _spinbutton_gamma_star;
	Gtk.SpinButton _spinbutton_ux;
	Gtk.SpinButton _spinbutton_uy;
	Gtk.SpinButton _spinbutton_uz;
	Gtk.SpinButton _spinbutton_U11;
	Gtk.SpinButton _spinbutton_U12;
	Gtk.SpinButton _spinbutton_U13;
	Gtk.SpinButton _spinbutton_U21;
	Gtk.SpinButton _spinbutton_U22;
	Gtk.SpinButton _spinbutton_U23;
	Gtk.SpinButton _spinbutton_U31;
	Gtk.SpinButton _spinbutton_U32;
	Gtk.SpinButton _spinbutton_U33;
	Gtk.CheckButton _checkbutton_a;
	Gtk.CheckButton _checkbutton_b;
	Gtk.CheckButton _checkbutton_c;
	Gtk.CheckButton _checkbutton_alpha;
	Gtk.CheckButton _checkbutton_beta;
	Gtk.CheckButton _checkbutton_gamma;
	Gtk.CheckButton _checkbutton_Ux;
	Gtk.CheckButton _checkbutton_Uy;
	Gtk.CheckButton _checkbutton_Uz;
	Gtk.TreeView _treeview_reflections;
	Gtk.TreeView _treeview_crystals;
	Gtk.TreeView _treeview_axes;
	Gtk.TreeView _treeview_pseudo_axes;
	Gtk.TreeView _treeview_pseudo_axes_parameters;
	Gtk.TreeView _treeview1; // attached to the _solutionModel
	Gtk.ToolButton _toolbutton_add_reflection;
	Gtk.ToolButton _toolbutton_goto_reflection;
	Gtk.ToolButton _toolbutton_del_reflection;
	Gtk.ToolButton _toolbutton_setUB;
	Gtk.ToolButton _toolbutton_computeUB;
	Gtk.ToolButton _toolbutton_add_crystal;
	Gtk.ToolButton _toolbutton_copy_crystal;
	Gtk.ToolButton _toolbutton_del_crystal;
	Gtk.ToolButton _toolbutton_affiner;
	Gtk.Statusbar _statusBar;
	Gtk.ImageMenuItem _menuitem5; // menu preferences

	// dialog1 preferences
	Gtk.Dialog _dialog1;
	Gtk.Button _button1; // close
	Gtk.ComboBox _combobox1; // select diffractometer type

	Hkl.Geometry geometry;
	Hkl.Detector detector;
	Hkl.SampleList samples;
	Hkl.Lattice reciprocal;
	Hkl.PseudoAxisEngineList engines;

	uint _nb_axes;
	uint _nb_sampleAxes;
	uint _nb_detectorAxes;
	string[] _sampleAxesNames;
	string[] _detectorAxesNames;

	uint _nb_pseudoAxes;
	string[] _pseudoAxesNames;

	Gtk.ListStore store_diffractometer;
	Gtk.ListStore store_axis;
	Gtk.ListStore store_pseudo_axis;
	Gtk.ListStore store_solutions;
	Gtk.ListStore store_samples;
	G.HashTable hash_store_pseudo_axis_parameter; /* use to store the pseudo_axis_parameters liststore */

	Gtk.MessageDialog _message;

	Hkl.Gui.PseudoAxesFrame[] pseudoAxesFrames;


	void get_widgets_and_objects_from_ui()
	{
		Gtk.Builder builder = new Gtk.Builder;

		this.builder = builder;
		try{
			builder.add_from_file("ghkl.ui");
		}catch(GLib.Error e){
			return;
		}

		Gtk.CellRenderer renderer;

		// objects
		this.store_diffractometer = builder.get_object("liststore1") as Gtk.ListStore;

		// window1
		this._label_UB11 = builder.get_object("label_UB11") as Gtk.Label;
		this._label_UB12 = builder.get_object("label_UB12") as Gtk.Label;
		this._label_UB13 = builder.get_object("label_UB13") as Gtk.Label;
		this._label_UB21 = builder.get_object("label_UB21") as Gtk.Label;
		this._label_UB22 = builder.get_object("label_UB22") as Gtk.Label;
		this._label_UB23 = builder.get_object("label_UB23") as Gtk.Label;
		this._label_UB31 = builder.get_object("label_UB31") as Gtk.Label;
		this._label_UB32 = builder.get_object("label_UB32") as Gtk.Label;
		this._label_UB33 = builder.get_object("label_UB33") as Gtk.Label;
		this._button2 = builder.get_object("button2") as Gtk.Button;
		this._spinbutton_a_star = builder.get_object("spinbutton_a_star") as Gtk.SpinButton;
		this._spinbutton_b_star = builder.get_object("spinbutton_b_star") as Gtk.SpinButton;
		this._spinbutton_c_star = builder.get_object("spinbutton_c_star") as Gtk.SpinButton;
		this._spinbutton_alpha_star = builder.get_object("spinbutton_alpha_star") as Gtk.SpinButton;
		this._spinbutton_beta_star = builder.get_object("spinbutton_beta_star") as Gtk.SpinButton;
		this._spinbutton_gamma_star = builder.get_object("spinbutton_gamma_star") as Gtk.SpinButton;
		this._spinbutton_a = builder.get_object("spinbutton_a") as Gtk.SpinButton;
		this._spinbutton_b = builder.get_object("spinbutton_b") as Gtk.SpinButton;
		this._spinbutton_c = builder.get_object("spinbutton_c") as Gtk.SpinButton;
		this._spinbutton_alpha = builder.get_object("spinbutton_alpha") as Gtk.SpinButton;
		this._spinbutton_beta = builder.get_object("spinbutton_beta") as Gtk.SpinButton;
		this._spinbutton_gamma = builder.get_object("spinbutton_gamma") as Gtk.SpinButton;
		this._spinbutton_a_min = builder.get_object("spinbutton_a_min") as Gtk.SpinButton;
		this._spinbutton_b_min = builder.get_object("spinbutton_b_min") as Gtk.SpinButton;
		this._spinbutton_c_min = builder.get_object("spinbutton_c_min") as Gtk.SpinButton;
		this._spinbutton_alpha_min = builder.get_object("spinbutton_alpha_min") as Gtk.SpinButton;
		this._spinbutton_beta_min = builder.get_object("spinbutton_beta_min") as Gtk.SpinButton;
		this._spinbutton_gamma_min = builder.get_object("spinbutton_gamma_min") as Gtk.SpinButton;
		this._spinbutton_a_max = builder.get_object("spinbutton_a_max") as Gtk.SpinButton;
		this._spinbutton_b_max = builder.get_object("spinbutton_b_max") as Gtk.SpinButton;
		this._spinbutton_c_max = builder.get_object("spinbutton_c_max") as Gtk.SpinButton;
		this._spinbutton_alpha_max = builder.get_object("spinbutton_alpha_max") as Gtk.SpinButton;
		this._spinbutton_beta_max = builder.get_object("spinbutton_beta_max") as Gtk.SpinButton;
		this._spinbutton_gamma_max = builder.get_object("spinbutton_gamma_max") as Gtk.SpinButton;
		this._spinbutton_lambda = builder.get_object("spinbutton_lambda") as Gtk.SpinButton;
		this._spinbutton_ux = builder.get_object("spinbutton_ux") as Gtk.SpinButton;
		this._spinbutton_uy = builder.get_object("spinbutton_uy") as Gtk.SpinButton;
		this._spinbutton_uz = builder.get_object("spinbutton_uz") as Gtk.SpinButton;
		this._spinbutton_U11 = builder.get_object("spinbutton_U11") as Gtk.SpinButton;
		this._spinbutton_U12 = builder.get_object("spinbutton_U12") as Gtk.SpinButton;
		this._spinbutton_U13 = builder.get_object("spinbutton_U13") as Gtk.SpinButton;
		this._spinbutton_U21 = builder.get_object("spinbutton_U21") as Gtk.SpinButton;
		this._spinbutton_U22 = builder.get_object("spinbutton_U22") as Gtk.SpinButton;
		this._spinbutton_U23 = builder.get_object("spinbutton_U23") as Gtk.SpinButton;
		this._spinbutton_U31 = builder.get_object("spinbutton_U31") as Gtk.SpinButton;
		this._spinbutton_U32 = builder.get_object("spinbutton_U32") as Gtk.SpinButton;
		this._spinbutton_U33 = builder.get_object("spinbutton_U33") as Gtk.SpinButton;
		this._checkbutton_a = builder.get_object("checkbutton_a") as Gtk.CheckButton;
		this._checkbutton_b = builder.get_object("checkbutton_b") as Gtk.CheckButton;
		this._checkbutton_c = builder.get_object("checkbutton_c") as Gtk.CheckButton;
		this._checkbutton_alpha = builder.get_object("checkbutton_alpha") as Gtk.CheckButton;
		this._checkbutton_beta = builder.get_object("checkbutton_beta") as Gtk.CheckButton;
		this._checkbutton_gamma = builder.get_object("checkbutton_gamma") as Gtk.CheckButton;
		this._checkbutton_Ux = builder.get_object("checkbutton_Ux") as Gtk.CheckButton;
		this._checkbutton_Uy = builder.get_object("checkbutton_Uy") as Gtk.CheckButton;
		this._checkbutton_Uz = builder.get_object("checkbutton_Uz") as Gtk.CheckButton;
		this._treeview_reflections = builder.get_object("treeview_reflections") as Gtk.TreeView;
		this._treeview_crystals = builder.get_object("treeview_crystals") as Gtk.TreeView;
		this._treeview_axes = builder.get_object("treeview_axes") as Gtk.TreeView;
		this._treeview_pseudo_axes = builder.get_object("treeview_pseudoAxes") as Gtk.TreeView;
		this._treeview_pseudo_axes_parameters = builder.get_object("treeview_pseudoAxes_parameters") as Gtk.TreeView;
		this._treeview1 = builder.get_object("treeview1") as Gtk.TreeView;
		this._toolbutton_add_reflection = builder.get_object("toolbutton_add_reflection") as Gtk.ToolButton;
		this._toolbutton_goto_reflection = builder.get_object("toolbutton_goto_reflection") as Gtk.ToolButton;
		this._toolbutton_del_reflection = builder.get_object("toolbutton_del_reflection") as Gtk.ToolButton;
		this._toolbutton_setUB = builder.get_object("toolbutton_setUB") as Gtk.ToolButton;
		this._toolbutton_computeUB = builder.get_object("toolbutton_computeUB") as Gtk.ToolButton;
		this._toolbutton_add_crystal = builder.get_object("toolbutton_add_crystal") as Gtk.ToolButton;
		this._toolbutton_copy_crystal = builder.get_object("toolbutton_copy_crystal") as Gtk.ToolButton;
		this._toolbutton_del_crystal = builder.get_object("toolbutton_del_crystal") as Gtk.ToolButton;
		this._toolbutton_affiner = builder.get_object("toolbutton_affiner") as Gtk.ToolButton;
		this._statusBar = builder.get_object("statusbar") as Gtk.Statusbar;
		this._menuitem5 = builder.get_object("menuitem5") as Gtk.ImageMenuItem;

		// dialog1
		this._dialog1 = builder.get_object("dialog1") as Gtk.Dialog;
		this._button1 = builder.get_object("button1") as Gtk.Button;
		this._combobox1 = builder.get_object("combobox1") as Gtk.ComboBox;
	}

	void set_up_diffractometer_model()
	{
		foreach(unowned Hkl.GeometryConfig config name in hkl_geometry_factory_configs)
			Gtk.TreeIter iter;
		this.store_diffractometer.append(out iter);
		this.store_diffractometer.set(iter,
									  DiffractometerCol.NAME, config.name);
	}

	void connect_all_signals()
	{
		g_signal_connect(G_OBJECT(this._spinbutton_a), "value-changed", 
						 G_CALLBACK(on_spinbutton_a_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_b), "value-changed", 
						 G_CALLBACK(on_spinbutton_b_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_c), "value-changed", 
						 G_CALLBACK(on_spinbutton_c_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_alpha), "value-changed", 
						 G_CALLBACK(on_spinbutton_alpha_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_beta), "value-changed", 
						 G_CALLBACK(on_spinbutton_beta_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_gamma), "value-changed", 
						 G_CALLBACK(on_spinbutton_gamma_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_a_min), "value-changed", 
						 G_CALLBACK(on_spinbutton_a_min_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_b_min), "value-changed", 
						 G_CALLBACK(on_spinbutton_b_min_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_c_min), "value-changed", 
						 G_CALLBACK(on_spinbutton_c_min_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_alpha_min), "value-changed", 
						 G_CALLBACK(on_spinbutton_alpha_min_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_beta_min), "value-changed", 
						 G_CALLBACK(on_spinbutton_beta_min_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_gamma_min), "value-changed", 
						 G_CALLBACK(on_spinbutton_gamma_min_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_a_max), "value-changed", 
						 G_CALLBACK(on_spinbutton_a_max_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_b_max), "value-changed", 
						 G_CALLBACK(on_spinbutton_b_max_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_c_max), "value-changed", 
						 G_CALLBACK(on_spinbutton_c_max_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_alpha_max), "value-changed", 
						 G_CALLBACK(on_spinbutton_alpha_max_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_beta_max), "value-changed", 
						 G_CALLBACK(on_spinbutton_beta_max_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_gamma_max), "value-changed", 
						 G_CALLBACK(on_spinbutton_gamma_max_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_lambda), "value-changed", 
						 G_CALLBACK(on_spinbutton_lambda_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_ux), "value-changed", 
						 G_CALLBACK(on_spinbutton_uxuyuz_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_uy), "value-changed", 
						 G_CALLBACK(on_spinbutton_uxuyuz_value_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._spinbutton_uz), "value-changed", 
						 G_CALLBACK(on_spinbutton_uxuyuz_value_changed), 
						 self);

		g_signal_connect(G_OBJECT(this._button2), "clicked", 
						 G_CALLBACK(on_button2_clicked), 
						 self);

		g_signal_connect(G_OBJECT(this._checkbutton_a), "toggled", 
						 G_CALLBACK(on_checkbutton_a_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_b), "toggled", 
						 G_CALLBACK(on_checkbutton_b_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_c), "toggled", 
						 G_CALLBACK(on_checkbutton_c_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_alpha), "toggled", 
						 G_CALLBACK(on_checkbutton_alpha_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_beta), "toggled", 
						 G_CALLBACK(on_checkbutton_beta_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_gamma), "toggled", 
						 G_CALLBACK(on_checkbutton_gamma_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_Ux), "toggled", 
						 G_CALLBACK(on_checkbutton_Ux_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_Uy), "toggled", 
						 G_CALLBACK(on_checkbutton_Uy_toggled), 
						 self);
		g_signal_connect(G_OBJECT(this._checkbutton_Uz), "toggled", 
						 G_CALLBACK(on_checkbutton_Uz_toggled), 
						 self);

		g_signal_connect(G_OBJECT(this._treeview_reflections), "key-press-event", 
						 G_CALLBACK(on_tree_view_reflections_key_press_event), 
						 self);
		g_signal_connect(G_OBJECT(this._treeview_pseudo_axes), "cursor-changed", 
						 G_CALLBACK(on_tree_view_pseudo_axes_cursor_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._treeview_crystals), "cursor-changed", 
						 G_CALLBACK(on_tree_view_crystals_cursor_changed), 
						 self);
		g_signal_connect(G_OBJECT(this._treeview_crystals), "key-press-event", 
						 G_CALLBACK(on_tree_view_crystals_key_press_event), 
						 self);

		g_signal_connect(G_OBJECT(this._toolbutton_add_reflection), "clicked", 
						 G_CALLBACK(on_toolbutton_add_reflection_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_goto_reflection), "clicked", 
						 G_CALLBACK(on_toolbutton_goto_reflection_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_del_reflection), "clicked", 
						 G_CALLBACK(on_toolbutton_del_reflection_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_setUB), "clicked", 
						 G_CALLBACK(on_toolbutton_setUB_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_computeUB), "clicked", 
						 G_CALLBACK(on_toolbutton_computeUB_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_add_crystal), "clicked", 
						 G_CALLBACK(on_toolbutton_add_crystal_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_copy_crystal), "clicked", 
						 G_CALLBACK(on_toolbutton_copy_crystal_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_del_crystal), "clicked", 
						 G_CALLBACK(on_toolbutton_del_crystal_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._toolbutton_affiner), "clicked", 
						 G_CALLBACK(on_toolbutton_affiner_clicked), 
						 self);

		g_signal_connect(G_OBJECT(this._menuitem5), "activate", 
						 G_CALLBACK(on_menuitem5_activate), 
						 self);

		// dialog1
		g_signal_connect(G_OBJECT(this._button1), "clicked", 
						 G_CALLBACK(on_button1_clicked), 
						 self);
		g_signal_connect(G_OBJECT(this._combobox1), "changed", 
						 G_CALLBACK(on_combobox1_changed), 
						 self);
	}

	/**********/
	/* PUBLIC */
	/**********/

	public Window()
	{
		this.geometry = null;
		this.engines = null;

		this.detector = new Hkl.Detector.Factory(HKL_DETECTOR_TYPE_0D);
		this.detector.idx = 1;
	
		/* add a default crystal */
		this.samples = new Hkl.SampleList();
		this.samples.append(new Hkl.Sample("test", Hkl.SampleType.MONOCRYSTAL));
		this.samples.select_current("test");

		this.store_samples = new Gtk.ListStore();

		/* create the reciprocal lattice */
		this.reciprocal = new Hkl.Lattice.default();

		this.pseudoAxesFrames = new Hkl.Gui.PseudoAxesFrame[];

		this.get_widgets_and_objects_from_ui();

		this.set_up_diffractometer_model();
		this.set_up_tree_view_reflections();
		this.set_up_tree_view_crystals();

		/* update the widgets */
		this.update_tree_view_crystals();
		this.update_source();
		this.update_lattice();
		this.update_lattice_parameters();
		this.update_reciprocal_lattice();
		this.update_UxUyUz();
		this.update_UB();

		this.connect_all_signals();
	}

	void set_up_pseudo_axes_frames()
	{
		size_t i;
		Gtk.VBox vbox2 = this.builder.get_object("vbox2") as Gtk.VBox;

		// first clear the previous frames
		this.pseudoAxesFrames = new Hkl.Gui.PseudoAxesFrame[];

		foreach(Hkl.PseudoAxesEngine engine in this.engines.engines){
			Hkl.Gui.PseudoAxesFrame pseudo;

			pseudo = new Hkl.Gui.PseudoAxisFrame(engine);
			this.pseudoaxesFrames += pseudo;

			vbox2.add(pseudo.frame);
			pseudo.changed.connect(on_pseudo_axes_frame_changed);
		}
		vbox2.show_all();
	}

	void set_up_tree_view_axes()
	{
		size_t i;
		int index;
		Gtk.CellRenderer * renderer;
		GList *columns, *col;
		Gtk.TreeViewColumn *column;

		g_return_if_fail(self);

		/* Create the Model and fill it */
		/* FIXME remove in the destructor and when changing the diffractometer */
		this.store_axis = new Gtk.ListStore(AxisCol.N_COLUMNS,
											typeof(gpointer),
											typeof(string), typeof(double),
											typeof(string), typeof(double),
											typeof(double));

		foreach(unowned Hkl.Axis axis in this.geometry.axes){
			Gtk.TreeIter iter;

			this.store_axis.append(&iter);
			this.store_axis.set(iter,
								AxisCol.AXIS, axis,
								AxisCol.NAME, axis.name);
		}

		/* first remove all the columns */
		G.List columns this._treeview_axes.get_columns();
		foreach(weak Gtk.TreeColumns col in columns)
			this._treeview_axes.remove_column(col.data);

		view.set_model (this.axis_store);

		/* add the columns */
		/* name */
		this._treeview_axes.insert_column_with_attributes (-1, 
														   "name", new CellRendererText (),
														   "text", Axiscol.NAME);

 		/* read */
        var cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "read", cell,
														   "text", Axiscol.READ);
		cell.edited.connect(on_cell_tree_view_axes_read_edited);

		/* write */
        cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "write", cell,
														   "text", Axiscol.WRITE);
		cell.edited.connect(on_cell_tree_view_axes_write_edited);

		/* min */
        cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "min", cell,
														   "text", Axiscol.MIN);
		cell.edited.connect(on_cell_tree_view_axes_min_edited);

		/* max */
        cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "max", cell,
														   "text", Axiscol.MAX);
		cell.edited.connect(on_cell_tree_view_axes_max_edited);

		this.update_axes();
	}

	void hkl_gui_set_up_tree_view_pseudo_axes(HklGuiWindow *self)
	{
		LOG;

		size_t i;
		size_t j;
		size_t k;
		int index;
		Gtk.CellRenderer * renderer;
		GList *columns, *col;
		Gtk.TreeViewColumn *column;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		columns = col = gtk_tree_view_get_columns(this._treeview_pseudo_axes);
		while(col){
			gtk_tree_view_remove_column(this._treeview_pseudo_axes, col->data);
			g_list_next(col);
		}
		g_list_free(columns);

		/* add the columns */
		/* name */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("name",
														   renderer,
														   "text", PSEUDOAXIS_COL_NAME,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes, column);

		/* read */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("read",
														   renderer,
														   "text", PSEUDOAXIS_COL_READ,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes, column);

		/* write */
		renderer = gtk_cell_renderer_text_new ();
		g_signal_connect(G_OBJECT(renderer), "edited", 
						 G_CALLBACK(on_cell_tree_view_pseudo_axes_write_edited), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("write",
														   renderer,
														   "text", PSEUDOAXIS_COL_WRITE,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes, column);

		/* min */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("min",
														   renderer,
														   "text", PSEUDOAXIS_COL_MIN,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes, column);

		/* max */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("max",
														   renderer,
														   "text", PSEUDOAXIS_COL_MAX,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes, column);


		/* initialized */
		renderer = gtk_cell_renderer_toggle_new ();
		g_signal_connect(G_OBJECT(renderer), "toggled", 
						 G_CALLBACK(on_cell_tree_view_pseudo_axes_is_initialized_toggled), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("write",
														   renderer,
														   "active", PSEUDOAXIS_COL_INITIALIZED,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes, column);

		/* Create the Model and fill it */
		/* FIXME remove in the destructor and when changing the diffractometer */
		this.store_pseudo_axis = gtk_list_store_new(PSEUDOAXIS_N_COLUMNS,
													G_TYPE_POINTER, /* pseudoaxis */
													G_TYPE_STRING, /* name */
													G_TYPE_DOUBLE, /* read */
													G_TYPE_DOUBLE, /* write */
													G_TYPE_DOUBLE, /* min */
													G_TYPE_DOUBLE, /* max */
													G_TYPE_BOOLEAN); /* initialized */
		/* FIXME remove in the destructor */
		this.hash_store_pseudo_axis_parameter = g_hash_table_new(NULL, NULL);
		for(i=0; i<HKL_LIST_LEN(this.engines->engines); ++i){
			HklPseudoAxisEngine *engine = this.engines->engines[i];

			for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j){
				HklPseudoAxis *pseudo_axis;
				Gtk.TreeIter iter;

				pseudo_axis = engine->pseudoAxes[j];
				gtk_list_store_append(this.store_pseudo_axis, &iter);
				gtk_list_store_set(this.store_pseudo_axis, &iter,
								   PSEUDOAXIS_COL_PSEUDOAXIS, pseudo_axis,
								   PSEUDOAXIS_COL_NAME, ((HklParameter *)pseudo_axis)->name,
								   -1);

				if(HKL_LIST_LEN(engine->mode->parameters)){
					Gtk.ListStore *model;

					model = gtk_list_store_new(PARAMETER_N_COLUMNS,
											   G_TYPE_POINTER, /* pseudoaxis */
											   G_TYPE_STRING, /* name */
											   G_TYPE_DOUBLE); /* value */
					for(k=0; k<HKL_LIST_LEN(engine->mode->parameters); ++k){
						HklParameter *parameter;

						parameter = &engine->mode->parameters[k];
						gtk_list_store_append(model, &iter);
						gtk_list_store_set(model, &iter,
										   PARAMETER_COL_PARAMETER, parameter,
										   PARAMETER_COL_NAME, parameter->name,
										   PARAMETER_COL_VALUE, hkl_parameter_get_value_unit(parameter),
										   -1);
					}
					g_hash_table_insert(this.hash_store_pseudo_axis_parameter,
										pseudo_axis, model);
				}
			}
		}
		/* Set the model for the TreeView */
		gtk_tree_view_set_model(this._treeview_pseudo_axes,
								GTK_TREE_MODEL(this.store_pseudo_axis));
		hkl_gui_update_pseudo_axes(self);
	}

	void hkl_gui_set_up_tree_view_pseudo_axes_parameters(HklGuiWindow *self)
	{
		LOG;

		Gtk.CellRenderer * renderer;
		GList *columns, *col;
		Gtk.TreeViewColumn *column;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		columns = col = gtk_tree_view_get_columns(this._treeview_pseudo_axes_parameters);
		while(col){
			gtk_tree_view_remove_column(this._treeview_pseudo_axes_parameters, col->data);
			g_list_next(col);
		}
		g_list_free(columns);

		/* add the columns */
		/* name */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("name",
														   renderer,
														   "text", PARAMETER_COL_NAME,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes_parameters, column);

		/* value */
		renderer = gtk_cell_renderer_text_new ();
		g_signal_connect(G_OBJECT(renderer), "edited", 
						 G_CALLBACK(on_cell_tree_view_pseudo_axes_parameters_value_edited), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("read",
														   renderer,
														   "text", PARAMETER_COL_VALUE,
														   NULL);
		gtk_tree_view_append_column(this._treeview_pseudo_axes_parameters, column);
	}

	void hkl_gui_set_up_tree_view_treeview1(HklGuiWindow *self)
	{
		LOG;

		size_t i;
		GList *columns, *col;
		Gtk.TreeViewColumn *column;
		Gtk.CellRenderer * renderer;
		GType *types;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		columns = col = gtk_tree_view_get_columns(this._treeview1);
		while(col){
			gtk_tree_view_remove_column(this._treeview1, col->data);
			g_list_next(col);
		}
		g_list_free(columns);

		/* add the columns index + axes */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("index",
														   renderer,
														   "text", SOLUTION_COL_INDEX,
														   NULL);
		gtk_tree_view_append_column(this._treeview1, column);
		for(i=0; i<HKL_LIST_LEN(this.geometry->axes); ++i){
			renderer = gtk_cell_renderer_text_new ();
			column = gtk_tree_view_column_new_with_attributes (((HklParameter *)&this.geometry->axes[i])->name,
															   renderer,
															   "text", SOLUTION_N_COLUMNS + i,
															   NULL);
			gtk_tree_view_append_column(this._treeview1, column);
		}

		/* Create the Model and fill it */
		/* FIXME remove in the destructor and when changing the diffractometer */
		types = g_new(GType, SOLUTION_N_COLUMNS + HKL_LIST_LEN(this.geometry->axes));
		types[0] = G_TYPE_INT;
		for(i=0; i<HKL_LIST_LEN(this.geometry->axes); ++i)
			types[SOLUTION_N_COLUMNS + i] = G_TYPE_DOUBLE;
		this.store_solutions = gtk_list_store_newv(SOLUTION_N_COLUMNS, types);
		g_free(types);

		gtk_tree_view_set_model(this._treeview1, GTK_TREE_MODEL(this.store_solutions));

		g_signal_connect(G_OBJECT(this._treeview1), "cursor-changed", 
						 G_CALLBACK(on_tree_view1_cursor_changed), 
						 self);

		hkl_gui_update_solutions(self);
	}

	void hkl_gui_set_up_tree_view_reflections(HklGuiWindow *self)
	{
		LOG;

		GList *columns, *col;
		Gtk.TreeViewColumn *column;
		Gtk.CellRenderer * renderer;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		columns = col = gtk_tree_view_get_columns(this._treeview_reflections);
		while(col){
			gtk_tree_view_remove_column(this._treeview_reflections, col->data);
			g_list_next(col);
		}
		g_list_free(columns);

		/* add the columns */
		/* index */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("index",
														   renderer,
														   "text", REFLECTION_COL_INDEX,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* h */
		renderer = gtk_cell_renderer_text_new ();
		g_signal_connect(G_OBJECT(renderer), "edited", 
						 G_CALLBACK(on_cell_tree_view_reflections_h_edited), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("h",
														   renderer,
														   "text", REFLECTION_COL_H,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* k */
		renderer = gtk_cell_renderer_text_new ();
		g_signal_connect(G_OBJECT(renderer), "edited", 
						 G_CALLBACK(on_cell_tree_view_reflections_k_edited), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("k",
														   renderer,
														   "text", REFLECTION_COL_K,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* l */
		renderer = gtk_cell_renderer_text_new ();
		g_signal_connect(G_OBJECT(renderer), "edited", 
						 G_CALLBACK(on_cell_tree_view_reflections_h_edited), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("l",
														   renderer,
														   "text", REFLECTION_COL_L,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* flag */
		renderer = gtk_cell_renderer_toggle_new ();
		g_signal_connect(G_OBJECT(renderer), "toggled", 
						 G_CALLBACK(on_cell_tree_view_reflections_flag_toggled), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("flag",
														   renderer,
														   "text", REFLECTION_COL_FLAG,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);


		gtk_tree_selection_set_mode(gtk_tree_view_get_selection(this._treeview_reflections),
									GTK_SELECTION_MULTIPLE);
	}

	void hkl_gui_set_up_tree_view_crystals(HklGuiWindow *self)
	{
		LOG;

		GList *columns, *col;
		Gtk.TreeViewColumn *column;
		Gtk.CellRenderer * renderer;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		columns = col = gtk_tree_view_get_columns(this._treeview_crystals);
		while(col){
			gtk_tree_view_remove_column(this._treeview_crystals, col->data);
			g_list_next(col);
		}
		g_list_free(columns);

		/* add the columns */
		/* name */
		renderer = gtk_cell_renderer_text_new ();
		g_signal_connect(G_OBJECT(renderer), "edited", 
						 G_CALLBACK(on_cell_tree_view_crystals_name_edited), 
						 self);
		column = gtk_tree_view_column_new_with_attributes ("name",
														   renderer,
														   "text", SAMPLE_COL_NAME,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		/* a */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("a",
														   renderer,
														   "text", SAMPLE_COL_A,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		/* b */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("b",
														   renderer,
														   "text", SAMPLE_COL_B,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		/* c */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("c",
														   renderer,
														   "text", SAMPLE_COL_C,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		/* alpha */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("alpha",
														   renderer,
														   "text", SAMPLE_COL_ALPHA,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		/* beta */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("beta",
														   renderer,
														   "text", SAMPLE_COL_BETA,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		/* gamma */
		renderer = gtk_cell_renderer_text_new ();
		column = gtk_tree_view_column_new_with_attributes ("gamma",
														   renderer,
														   "text", SAMPLE_COL_GAMMA,
														   NULL);
		gtk_tree_view_append_column(this._treeview_crystals, column);

		gtk_tree_selection_set_mode(gtk_tree_view_get_selection(this._treeview_crystals),
									GTK_SELECTION_MULTIPLE);
	}

	void hkl_gui_update_source(HklGuiWindow *self)
	{
		LOG;

		g_return_if_fail(self);

		if(this.geometry){
			double lambda = hkl_source_get_wavelength(&this.geometry->source);
			gtk_spin_button_set_value(this._spinbutton_lambda, lambda);
		}
	}

	void hkl_gui_update_axes(HklGuiWindow *self)
	{
		HklAxis *axis;
		double min;
		double max;
		gboolean valid;
		Gtk.TreeIter iter;

		LOG;

		// update the model
		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_axis), &iter);
		while(valid){
			gtk_tree_model_get(GTK_TREE_MODEL(this.store_axis), &iter,
							   AXIS_COL_AXIS, &axis,
							   -1);
			hkl_parameter_get_range_unit((HklParameter *)axis, &min, &max);
			gtk_list_store_set(this.store_axis, &iter,
							   AXIS_COL_READ, hkl_axis_get_value_unit(axis),
							   AXIS_COL_WRITE, hkl_axis_get_value_unit(axis),
							   AXIS_COL_MIN, min,
							   AXIS_COL_MAX, max,
							   -1);
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_axis), &iter);
		}
	}

	void hkl_gui_update_pseudo_axes(HklGuiWindow *self)
	{
		double min;
		double max;
		HklParameter *parameter;
		gboolean valid;
		Gtk.TreeIter iter;

		LOG;

		// first compute all the pseudoAxes values
		hkl_pseudo_axis_engine_list_get(this.engines);

		// update the model
		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_pseudo_axis), &iter);
		while(valid){
			gtk_tree_model_get(GTK_TREE_MODEL(this.store_pseudo_axis), &iter,
							   PSEUDOAXIS_COL_PSEUDOAXIS, &parameter,
							   -1);
			hkl_parameter_get_range_unit((HklParameter *)parameter, &min, &max);
			gtk_list_store_set(this.store_pseudo_axis, &iter,
							   PSEUDOAXIS_COL_READ, hkl_parameter_get_value_unit(parameter),
							   PSEUDOAXIS_COL_WRITE, hkl_parameter_get_value_unit(parameter),
							   PSEUDOAXIS_COL_MIN, min,
							   PSEUDOAXIS_COL_MAX, max,
							   PSEUDOAXIS_COL_INITIALIZED, TRUE,
							   -1);
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_pseudo_axis), &iter);
		}
	}

	void hkl_gui_update_pseudo_axes_parameters(HklGuiWindow *self)
	{
		GHashTableIter iter;
		gpointer key, value;

		LOG;

		g_hash_table_iter_init (&iter, this.hash_store_pseudo_axis_parameter);
		while (g_hash_table_iter_next (&iter, &key, &value)) /* key = pseudo_axis, value = model */
		{
			Gtk.ListStore *model;
			Gtk.TreeIter iter2;
			gboolean valid;

			model = value;
			valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(model), &iter2);
			while(valid){
				HklParameter *parameter;

				gtk_tree_model_get(GTK_TREE_MODEL(model), &iter2,
								   PARAMETER_COL_PARAMETER, &parameter,
								   -1);
				gtk_list_store_set(model, &iter2,
								   PARAMETER_COL_NAME, parameter->name,
								   PARAMETER_COL_VALUE, hkl_parameter_get_value_unit(parameter),
								   -1);
				valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(model), &iter2);
			}
		}
	}

	void hkl_gui_update_lattice(HklGuiWindow *self)
	{
		LOG;

		HklSample *sample = this.samples->current;
		if(sample){
			gtk_spin_button_set_value(this._spinbutton_a, 
									  hkl_parameter_get_value_unit(sample->lattice->a));
			gtk_spin_button_set_value(this._spinbutton_b,
									  hkl_parameter_get_value_unit(sample->lattice->b));
			gtk_spin_button_set_value(this._spinbutton_c,
									  hkl_parameter_get_value_unit(sample->lattice->c));
			gtk_spin_button_set_value(this._spinbutton_alpha,
									  hkl_parameter_get_value_unit(sample->lattice->alpha));
			gtk_spin_button_set_value(this._spinbutton_beta,
									  hkl_parameter_get_value_unit(sample->lattice->beta));
			gtk_spin_button_set_value(this._spinbutton_gamma,
									  hkl_parameter_get_value_unit(sample->lattice->gamma));
		}
	}

	void hkl_gui_update_lattice_parameters(HklGuiWindow *self)
	{
		LOG;

		HklSample *sample = this.samples->current;
		if(sample){
			double min;
			double max;
			gboolean to_fit;
			HklParameter *parameter;


			parameter = sample->lattice->a;
			hkl_parameter_get_range_unit(parameter, &min, &max);
			gtk_spin_button_set_value(this._spinbutton_a_min, min);
			gtk_spin_button_set_value(this._spinbutton_a_max, max);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_a),
										 parameter->fit);

			parameter = sample->lattice->b;
			hkl_parameter_get_range_unit(parameter, &min, &max);
			gtk_spin_button_set_value(this._spinbutton_b_min, min);
			gtk_spin_button_set_value(this._spinbutton_b_max, max);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_b),
										 parameter->fit);

			parameter = sample->lattice->c;
			hkl_parameter_get_range_unit(parameter, &min, &max);
			gtk_spin_button_set_value(this._spinbutton_c_min, min);
			gtk_spin_button_set_value(this._spinbutton_c_max, max);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_c),
										 parameter->fit);

			parameter = sample->lattice->alpha;
			hkl_parameter_get_range_unit(parameter, &min, &max);
			gtk_spin_button_set_value(this._spinbutton_alpha_min, min);
			gtk_spin_button_set_value(this._spinbutton_alpha_max, max);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_alpha),
										 parameter->fit);

			parameter = sample->lattice->beta;
			hkl_parameter_get_range_unit(parameter, &min, &max);
			gtk_spin_button_set_value(this._spinbutton_beta_min, min);
			gtk_spin_button_set_value(this._spinbutton_beta_max, max);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_beta),
										 parameter->fit);

			parameter = sample->lattice->gamma;
			hkl_parameter_get_range_unit(parameter, &min, &max);
			gtk_spin_button_set_value(this._spinbutton_gamma_min, min);
			gtk_spin_button_set_value(this._spinbutton_gamma_max, max);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_gamma),
										 parameter->fit);
		}
	}

	void hkl_gui_update_reciprocal_lattice(HklGuiWindow *self)
	{
		LOG;

		HklSample *sample = this.samples->current;
		if(sample){
			hkl_lattice_reciprocal(sample->lattice, this.reciprocal);

			gtk_spin_button_set_value(this._spinbutton_a_star,
									  hkl_parameter_get_value_unit(this.reciprocal->a));
			gtk_spin_button_set_value(this._spinbutton_b_star,
									  hkl_parameter_get_value_unit(this.reciprocal->b));
			gtk_spin_button_set_value(this._spinbutton_c_star,
									  hkl_parameter_get_value_unit(this.reciprocal->c));
			gtk_spin_button_set_value(this._spinbutton_alpha_star,
									  hkl_parameter_get_value_unit(this.reciprocal->alpha));
			gtk_spin_button_set_value(this._spinbutton_beta_star,
									  hkl_parameter_get_value_unit(this.reciprocal->beta));
			gtk_spin_button_set_value(this._spinbutton_gamma_star,
									  hkl_parameter_get_value_unit(this.reciprocal->gamma));
		}
	}

	void hkl_gui_update_UB(HklGuiWindow *self)
	{
		LOG;

		HklSample *sample = this.samples->current;
		if(sample){
			static const char *format = "%f";
			char tmp[100];
			HklMatrix UB;

			hkl_sample_get_UB(sample, &UB);
			sprintf(tmp, format, UB.data[0][0]);
			gtk_label_set_text(this._label_UB11, tmp);
			sprintf(tmp, format, UB.data[0][1]);
			gtk_label_set_text(this._label_UB12, tmp);
			sprintf(tmp, format, UB.data[0][2]);
			gtk_label_set_text(this._label_UB13, tmp);
			sprintf(tmp, format, UB.data[1][0]);
			gtk_label_set_text(this._label_UB21, tmp);
			sprintf(tmp, format, UB.data[1][1]);
			gtk_label_set_text(this._label_UB22, tmp);
			sprintf(tmp, format, UB.data[1][2]);
			gtk_label_set_text(this._label_UB23, tmp);
			sprintf(tmp, format, UB.data[2][0]);
			gtk_label_set_text(this._label_UB31, tmp);
			sprintf(tmp, format, UB.data[2][1]);
			gtk_label_set_text(this._label_UB32, tmp);
			sprintf(tmp, format, UB.data[2][2]);
			gtk_label_set_text(this._label_UB33, tmp);
		}
	}

	void hkl_gui_update_UxUyUz(HklGuiWindow *self)
	{
		LOG;

		HklSample *sample = this.samples->current;
		if(sample){
			gtk_spin_button_set_value(this._spinbutton_ux, hkl_parameter_get_value_unit(sample->ux));
			gtk_spin_button_set_value(this._spinbutton_uy, hkl_parameter_get_value_unit(sample->uy));
			gtk_spin_button_set_value(this._spinbutton_uz, hkl_parameter_get_value_unit(sample->uz));
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_Ux), sample->ux->fit);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_Uy), sample->uy->fit);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_Uz), sample->uz->fit);
		}
	}

	static void _hkl_gui_update_reflections(const HklSample *sample, Gtk.ListStore *model)
	{
		LOG;

		size_t i;

		gtk_list_store_clear(model);
		for(i=0; i<HKL_LIST_LEN(sample->reflections); ++i){
			Gtk.TreeIter iter;
			HklSampleReflection *reflection = sample->reflections[i];

			gtk_list_store_append(model, &iter);
			gtk_list_store_set(model, &iter,
							   REFLECTION_COL_INDEX, i,
							   REFLECTION_COL_H, reflection->hkl.data[0],
							   REFLECTION_COL_K, reflection->hkl.data[1],
							   REFLECTION_COL_L, reflection->hkl.data[2],
							   REFLECTION_COL_FLAG, reflection->flag,
							   -1);
		}
	}

	void hkl_gui_update_tree_view_crystals(HklGuiWindow *self)
	{
		LOG;

		size_t i;
		HklSample *sample;
		Gtk.ListStore *reflections;
		Gtk.ListStore *reflections_current;
		Gtk.TreeIter iter;
		Gtk.TreeIter iter_current;
		gboolean valid;
		const char *current_crystal_name;
		gboolean is_current_crystal_set = FALSE;

		/* clear all data in the sample model */
		if (this.store_samples){
			/* first the reflections models */
			valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_samples), &iter);
			while(valid){
				gtk_tree_model_get(GTK_TREE_MODEL(this.store_samples), &iter,
								   SAMPLE_COL_REFLECTIONS, &reflections,
								   -1);
				g_object_unref(reflections);
				valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_samples), &iter);
			}
			g_object_unref(this.store_samples);
		}

		this.store_samples = gtk_list_store_new(SAMPLE_N_COLUMNS,
												G_TYPE_POINTER, G_TYPE_POINTER,
												G_TYPE_STRING, G_TYPE_DOUBLE,
												G_TYPE_DOUBLE, G_TYPE_DOUBLE,
												G_TYPE_DOUBLE, G_TYPE_DOUBLE,
												G_TYPE_DOUBLE);

		if(this.samples->current){
			is_current_crystal_set = TRUE;
			current_crystal_name = this.samples->current->name;
		}

		/* Fill the models from the crystalList */
		for(i=0; i<HKL_LIST_LEN(this.samples->samples); ++i){
			HklLattice *lattice;

			sample = this.samples->samples[i];
			lattice = sample->lattice;

			/* create the attached reflections model */
			reflections = gtk_list_store_new(REFLECTION_N_COLUMNS,
											 G_TYPE_INT,
											 G_TYPE_DOUBLE,
											 G_TYPE_DOUBLE,
											 G_TYPE_DOUBLE,
											 G_TYPE_BOOLEAN);
			_hkl_gui_update_reflections(sample, reflections);

			gtk_list_store_append(this.store_samples, &iter);
			gtk_list_store_set(this.store_samples, &iter,
							   SAMPLE_COL_SAMPLE, sample,
							   SAMPLE_COL_REFLECTIONS, reflections,
							   SAMPLE_COL_NAME,sample->name,
							   SAMPLE_COL_A, hkl_parameter_get_value_unit(lattice->a),
							   SAMPLE_COL_B, hkl_parameter_get_value_unit(lattice->b),
							   SAMPLE_COL_C, hkl_parameter_get_value_unit(lattice->c),
							   SAMPLE_COL_ALPHA, hkl_parameter_get_value_unit(lattice->alpha),
							   SAMPLE_COL_BETA, hkl_parameter_get_value_unit(lattice->beta),
							   SAMPLE_COL_GAMMA, hkl_parameter_get_value_unit(lattice->gamma),
							   -1);

			/* save a few parameters if it is the current sample */
			if (is_current_crystal_set && current_crystal_name == sample->name){
				iter_current = iter;
				reflections_current = reflections;
			}
		}

		/* Set the model for the TreeView */
		gtk_tree_view_set_model(this._treeview_crystals,
								GTK_TREE_MODEL(this.store_samples));
		if (is_current_crystal_set){
			Gtk.TreePath *path;

			path = gtk_tree_model_get_path(GTK_TREE_MODEL(this.store_samples), &iter_current);
			gtk_tree_view_set_cursor(this._treeview_crystals, path, NULL, FALSE);
			gtk_tree_view_set_model(this._treeview_reflections, GTK_TREE_MODEL(reflections_current));
			gtk_tree_path_free(path);
		}
	}

	void hkl_gui_update_reflections(HklGuiWindow *self, const HklSample *sample)
	{
		gboolean valid;
		Gtk.TreeIter iter;

		LOG;

		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_samples), &iter);
		while(valid){
			const HklSample *sample_iter;
			Gtk.ListStore *model;

			gtk_tree_model_get(GTK_TREE_MODEL(this.store_samples), &iter,
							   SAMPLE_COL_SAMPLE, &sample_iter,
							   SAMPLE_COL_REFLECTIONS, &model,
							   -1);
			if (sample == sample_iter){
				_hkl_gui_update_reflections(sample, model);
				break;
			}
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_samples), &iter);
		}
	}

	void hkl_gui_update_status_bar(HklGuiWindow *self, const HklError *error)
	{
		LOG;

		gtk_statusbar_push(this._statusBar, 0, error->message);
	}

	void hkl_gui_update_crystal_model(HklGuiWindow *self, HklSample * sample)
	{
		Gtk.TreeIter iter;
		gboolean valid;

		LOG;

		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_samples), &iter);
		while(valid){
			const char *name;

			gtk_tree_model_get(GTK_TREE_MODEL(this.store_samples), &iter,
							   SAMPLE_COL_NAME, &name,
							   -1);
			if (!strcmp(name, sample->name)){
				HklLattice *lattice;
				
				lattice = sample->lattice;
				gtk_list_store_set(this.store_samples, &iter,
								   SAMPLE_COL_A, hkl_parameter_get_value_unit(lattice->a),
								   SAMPLE_COL_B, hkl_parameter_get_value_unit(lattice->b),
								   SAMPLE_COL_C, hkl_parameter_get_value_unit(lattice->c),
								   SAMPLE_COL_ALPHA, hkl_parameter_get_value_unit(lattice->alpha),
								   SAMPLE_COL_BETA, hkl_parameter_get_value_unit(lattice->beta),
								   SAMPLE_COL_GAMMA, hkl_parameter_get_value_unit(lattice->gamma),
								   -1);
				break;
			}
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_samples), &iter);
		}
	}

	void hkl_gui_update_pseudo_axes_frames(HklGuiWindow *self)
	{
		LOG;

		size_t i;

		for(i=0; i<this.pseudoAxesFrames_len; ++i)
			hkl_gui_pseudo_axes_frame_update(this.pseudoAxesFrames[i]);
	}

	void hkl_gui_update_solutions(HklGuiWindow *self)
	{
		LOG;

		size_t i;

		gtk_list_store_clear(this.store_solutions);
		for(i=0; i<hkl_geometry_list_len(this.engines->geometries); ++i){
			size_t j;
			HklGeometry *geometry;
			Gtk.TreeIter iter;

			geometry = this.engines->geometries->items[i]->geometry;

			gtk_list_store_append(this.store_solutions, &iter);
			gtk_list_store_set(this.store_solutions, &iter,
							   SOLUTION_COL_INDEX, i,
							   -1);
			for(j=0; j<HKL_LIST_LEN(geometry->axes); ++j)
				gtk_list_store_set(this.store_solutions, &iter,
								   SOLUTION_N_COLUMNS + j, hkl_parameter_get_value_unit((HklParameter *)&geometry->axes[j]),
								   -1);
		}
	}
