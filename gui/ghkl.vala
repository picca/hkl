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

public class Hkl.Gui.Window : GLib.Object
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
	GLib.HashTable hash_store_pseudo_axis_parameter; /* use to store the pseudo_axis_parameters liststore */

	Gtk.MessageDialog _message;

	Hkl.Gui.PseudoAxesFrame[] pseudoAxesFrames;


	void get_widgets_and_objects_from_ui()
	{
		Gtk.Builder builder = new Gtk.Builder();

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
		foreach(unowned Hkl.GeometryConfig config in hkl_geometry_factory_configs){
			Gtk.TreeIter iter;
			this.store_diffractometer.append(out iter);
			this.store_diffractometer.set(iter,
										  DiffractometerCol.NAME, config.name);
		}
	}

	void connect_all_signals()
	{
		this._spinbutton_a.value_changed.connect(on_spinbutton_a_value_changed);
		this._spinbutton_b.value_changed.connect(on_spinbutton_b_value_changed);
		this._spinbutton_c.value_changed.connect(on_spinbutton_c_value_changed);
		this._spinbutton_alpha.value_changed.connect(on_spinbutton_alpha_value_changed);
		this._spinbutton_beta.value_changed.connect(on_spinbutton_beta_value_changed);
		this._spinbutton_gamma.value_changed.connect(on_spinbutton_gamma_value_changed);

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

		foreach(Hkl.PseudoAxisEngine engine in this.engines.engines){
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
		Gtk.CellRenderer renderer;
		Gtk.TreeViewColumn column;

		/* Create the Model and fill it */
		/* FIXME remove in the destructor and when changing the diffractometer */
		this.store_axis = new Gtk.ListStore(AxisCol.N_COLUMNS,
											typeof(void *),
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
		GLib.List columns = this._treeview_axes.get_columns();
		foreach(weak Gtk.TreeViewColumn col in columns)
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

	void set_up_tree_view_pseudo_axes()
	{
		size_t i;
		size_t j;
		size_t k;
		int index;
		Gtk.CellRenderer renderer;
		Gtk.TreeViewColumn column;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		GLib.List columns = col = gtk_tree_view_get_columns(this._treeview_pseudo_axes);
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
			Hkl.PseudoAxisEngine engine = this.engines.engines[i];

			for(j=0; j<HKL_LIST_LEN(engine->pseudoAxes); ++j){
				Hkl.PseudoAxis pseudo_axis;
				Gtk.TreeIter iter;

				pseudo_axis = engine->pseudoAxes[j];
				gtk_list_store_append(this.store_pseudo_axis, &iter);
				gtk_list_store_set(this.store_pseudo_axis, &iter,
								   PSEUDOAXIS_COL_PSEUDOAXIS, pseudo_axis,
								   PSEUDOAXIS_COL_NAME, pseudo_axis.parent.name,
								   -1);

				if(HKL_LIST_LEN(engine->mode->parameters)){
					Gtk.ListStore *model;

					model = gtk_list_store_new(PARAMETER_N_COLUMNS,
											   G_TYPE_POINTER, /* pseudoaxis */
											   G_TYPE_STRING, /* name */
											   G_TYPE_DOUBLE); /* value */
					for(k=0; k<HKL_LIST_LEN(engine->mode->parameters); ++k){
						Hkl.Parameter parameter;

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

	void set_up_tree_view_pseudo_axes_parameters()
	{
		Gtk.CellRenderer renderer;
		Gtk.TreeViewColumn column;

		g_return_if_fail(self);

		/* first remove all columns of the tree view */
		GLib.List columns = col = gtk_tree_view_get_columns(this._treeview_pseudo_axes_parameters);
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

	void set_up_tree_view_treeview1()
	{
		size_t i;
		Gtk.TreeViewColumn column;
		Gtk.CellRenderer renderer;
	
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
			column = gtk_tree_view_column_new_with_attributes (this.geometry.axes[i].parent.name,
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

		update_solutions(self);
	}

	void set_up_tree_view_reflections()
	{
		Gtk.TreeViewColumn column;
		Gtk.CellRenderer renderer;

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

	void set_up_tree_view_crystals()
	{
		Gtk.TreeViewColumn column;
		Gtk.CellRenderer renderer;

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

	void update_source()
	{
		if(this.geometry){
			double lambda = hkl_source_get_wavelength(&this.geometry->source);
			gtk_spin_button_set_value(this._spinbutton_lambda, lambda);
		}
	}

	void update_axes()
	{
		Hkl.Axis axis;
		double min;
		double max;
		bool valid;
		Gtk.TreeIter iter;

		// update the model
		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_axis), &iter);
		while(valid){
			gtk_tree_model_get(GTK_TREE_MODEL(this.store_axis), &iter,
							   AXIS_COL_AXIS, &axis,
							   -1);
			axis.parent.get_range_unit(out min, out max);
			gtk_list_store_set(this.store_axis, &iter,
							   AXIS_COL_READ, hkl_axis_get_value_unit(axis),
							   AXIS_COL_WRITE, hkl_axis_get_value_unit(axis),
							   AXIS_COL_MIN, min,
							   AXIS_COL_MAX, max,
							   -1);
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_axis), &iter);
		}
	}

	void update_pseudo_axes()
	{
		double min;
		double max;
		Hkl.Parameter parameter;
		bool valid;
		Gtk.TreeIter iter;

		// first compute all the pseudoAxes values
		hkl_pseudo_axis_engine_list_get(this.engines);

		// update the model
		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_pseudo_axis), &iter);
		while(valid){
			gtk_tree_model_get(GTK_TREE_MODEL(this.store_pseudo_axis), &iter,
							   PSEUDOAXIS_COL_PSEUDOAXIS, &parameter,
							   -1);
			parameter.get_range_unit(out min, out max);
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

	void update_pseudo_axes_parameters()
	{
		GLib.HashTableIter iter;
		void *key;
		void *value;

		g_hash_table_iter_init (&iter, this.hash_store_pseudo_axis_parameter);
		while (g_hash_table_iter_next (&iter, &key, &value)) /* key = pseudo_axis, value = model */
		{
			Gtk.ListStore *model;
			Gtk.TreeIter iter2;
			bool valid;

			model = value;
			valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(model), &iter2);
			while(valid){
				Hkl.Parameter *parameter;

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

	void update_lattice()
	{
		Hkl.Sample *sample = this.samples->current;
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

	void update_lattice_parameters()
	{
		Hkl.Sample *sample = this.samples->current;
		if(sample){
			double min;
			double max;
			bool to_fit;
			Hkl.Parameter *parameter;


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

	void update_reciprocal_lattice()
	{
		Hkl.Sample *sample = this.samples->current;
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

	void update_UB()
	{
		Hkl.Sample *sample = this.samples->current;
		if(sample){
			string format = "%f";
			char tmp[100];
			Hkl.Matrix UB;

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

	void update_UxUyUz()
	{
		Hkl.Sample *sample = this.samples->current;
		if(sample){
			gtk_spin_button_set_value(this._spinbutton_ux, hkl_parameter_get_value_unit(sample->ux));
			gtk_spin_button_set_value(this._spinbutton_uy, hkl_parameter_get_value_unit(sample->uy));
			gtk_spin_button_set_value(this._spinbutton_uz, hkl_parameter_get_value_unit(sample->uz));
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_Ux), sample->ux->fit);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_Uy), sample->uy->fit);
			gtk_toggle_button_set_active(GTK_TOGGLE_BUTTON(this._checkbutton_Uz), sample->uz->fit);
		}
	}

	static void _update_reflections(Hkl.Sample sample, Gtk.ListStore model)
	{
		size_t i;

		gtk_list_store_clear(model);
		for(i=0; i<HKL_LIST_LEN(sample->reflections); ++i){
			Gtk.TreeIter iter;
			Hkl.SampleReflection *reflection = sample->reflections[i];

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

	void update_tree_view_crystals()
	{
		size_t i;
		Hkl.Sample *sample;
		Gtk.ListStore *reflections;
		Gtk.ListStore *reflections_current;
		Gtk.TreeIter iter;
		Gtk.TreeIter iter_current;
		bool valid;
		string current_crystal_name;
		bool is_current_crystal_set = FALSE;

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
			Hkl.Lattice *lattice;

			sample = this.samples->samples[i];
			lattice = sample->lattice;

			/* create the attached reflections model */
			reflections = gtk_list_store_new(REFLECTION_N_COLUMNS,
											 G_TYPE_INT,
											 G_TYPE_DOUBLE,
											 G_TYPE_DOUBLE,
											 G_TYPE_DOUBLE,
											 G_TYPE_BOOLEAN);
			_update_reflections(sample, reflections);

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

	void update_reflections(Hkl.Sample sample)
	{
		bool valid;
		Gtk.TreeIter iter;

		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_samples), &iter);
		while(valid){
			Hkl.Sample 
			sample_iter;
			Gtk.ListStore *model;

			gtk_tree_model_get(GTK_TREE_MODEL(this.store_samples), &iter,
							   SAMPLE_COL_SAMPLE, &sample_iter,
							   SAMPLE_COL_REFLECTIONS, &model,
							   -1);
			if (sample == sample_iter){
				_update_reflections(sample, model);
				break;
			}
			valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(this.store_samples), &iter);
		}
	}

	void update_status_bar(Hkl.Error error)
	{
		gtk_statusbar_push(this._statusBar, 0, error->message);
	}

	void update_crystal_model(Hkl.Sample sample)
	{
		Gtk.TreeIter iter;
		bool valid;

		valid = gtk_tree_model_get_iter_first(GTK_TREE_MODEL(this.store_samples), &iter);
		while(valid){
			string name;

			gtk_tree_model_get(GTK_TREE_MODEL(this.store_samples), &iter,
							   SAMPLE_COL_NAME, &name,
							   -1);
			if (!strcmp(name, sample->name)){
				Hkl.Lattice lattice;
				
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

	void update_pseudo_axes_frames()
	{
		size_t i;

		for(i=0; i<this.pseudoAxesFrames_len; ++i)
			pseudo_axes_frame_update(this.pseudoAxesFrames[i]);
	}

	void update_solutions()
	{
		size_t i;

		gtk_list_store_clear(this.store_solutions);
		for(i=0; i<hkl_geometry_list_len(this.engines->geometries); ++i){
			size_t j;
			Hkl.Geometry geometry;
			Gtk.TreeIter iter;

			geometry = this.engines->geometries->items[i]->geometry;

			gtk_list_store_append(this.store_solutions, &iter);
			gtk_list_store_set(this.store_solutions, &iter,
							   SOLUTION_COL_INDEX, i,
							   -1);
			for(j=0; j<HKL_LIST_LEN(geometry->axes); ++j)
				gtk_list_store_set(this.store_solutions, &iter,
								   SOLUTION_N_COLUMNS + j, hkl_parameter_get_value_unit(geometry.axes[j].parent),
								   -1);
		}
	}

	/*****************/
	/*** Callbacks ***/
	/*****************/

	void on_tree_view_pseudo_axes_cursor_changed(Gtk.TreeView *tree_view,
												 void *user_data)
	{
		Gtk.TreePath *path;
		Gtk.TreeViewColumn *focus_column;
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.PseudoAxis pseudoAxis;

		gtk_tree_view_get_cursor(tree_view, &path, &focus_column);
		model = gtk_tree_view_get_model(tree_view);
		gtk_tree_model_get_iter(model, &iter, path);
		gtk_tree_model_get(model, &iter, PSEUDOAXIS_COL_PSEUDOAXIS, &pseudoAxis, -1);
		this._treeview_pseudo_axes_parameters.set_model(model);
	}

	void on_tree_view_crystals_cursor_changed(Gtk.TreeView *tree_view,
											  void *user_data)
	{
		Gtk.TreePath *path;
		Gtk.TreeViewColumn *focus_column;
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		string name;

		g_return_if_fail(user_data);

		hkl = user_data;
		gtk_tree_view_get_cursor(tree_view, &path, &focus_column);
		model = gtk_tree_view_get_model(tree_view);
		gtk_tree_model_get_iter(model, &iter, path);
		gtk_tree_model_get(model, &iter, SAMPLE_COL_NAME, &name, -1);

		hkl_sample_list_select_current(hkl->samples, name);
		hkl_pseudo_axis_engine_list_init(hkl->engines, hkl->geometry,
										 hkl->detector, hkl->samples->current);
		gtk_tree_view_set_model(hkl->_treeview_reflections, model);
		hkl_gui_update_lattice(hkl);
		hkl_gui_update_lattice_parameters(hkl);
		hkl_gui_update_reciprocal_lattice(hkl);
		hkl_gui_update_UxUyUz(hkl);
		hkl_gui_update_UB(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}

	void on_spinbutton_a_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_b_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_c_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_alpha_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_beta_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_gamma_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_a_min_value_changed(Gtk.SpinButton *spinbutton,
										   Gtk.ScrollType  arg1,
										   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_b_min_value_changed(Gtk.SpinButton *spinbutton,
										   Gtk.ScrollType  arg1,
										   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_c_min_value_changed(Gtk.SpinButton *spinbutton,
										   Gtk.ScrollType  arg1,
										   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_alpha_min_value_changed(Gtk.SpinButton *spinbutton,
											   Gtk.ScrollType  arg1,
											   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_beta_min_value_changed(Gtk.SpinButton *spinbutton,
											  Gtk.ScrollType  arg1,
											  void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_gamma_min_value_changed(Gtk.SpinButton *spinbutton,
											   Gtk.ScrollType  arg1,
											   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_a_max_value_changed(Gtk.SpinButton *spinbutton,
										   Gtk.ScrollType  arg1,
										   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_b_max_value_changed(Gtk.SpinButton *spinbutton,
										   Gtk.ScrollType  arg1,
										   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_c_max_value_changed(Gtk.SpinButton *spinbutton,
										   Gtk.ScrollType  arg1,
										   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_alpha_max_value_changed(Gtk.SpinButton *spinbutton,
											   Gtk.ScrollType  arg1,
											   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_beta_max_value_changed(Gtk.SpinButton *spinbutton,
											  Gtk.ScrollType  arg1,
											  void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_gamma_max_value_changed(Gtk.SpinButton *spinbutton,
											   Gtk.ScrollType  arg1,
											   void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_lambda_value_changed(Gtk.SpinButton *spinbutton,
											Gtk.ScrollType  arg1,
											void *       user_data)
	{
		if(hkl->geometry){
			hkl->geometry->source.wave_length = gtk_spin_button_get_value(spinbutton);
			hkl_gui_update_pseudo_axes(hkl);
			hkl_gui_update_pseudo_axes_frames(hkl);
		}
	}

	void on_spinbutton_uxuyuz_value_changed(Gtk.SpinButton *spinbutton,
											Gtk.ScrollType  arg1,
											void *       user_data)
	{
		// TODO change the cell background color if not synchro
	}

	void on_button2_clicked(Gtk.Button *button,
							void *   user_data)
	{
		Hkl.Sample sample;
		sample = hkl->samples->current;
		if(sample){
			hkl_sample_set_lattice(sample,
								   gtk_spin_button_get_value(hkl->_spinbutton_a),
								   gtk_spin_button_get_value(hkl->_spinbutton_b),
								   gtk_spin_button_get_value(hkl->_spinbutton_c),
								   gtk_spin_button_get_value(hkl->_spinbutton_alpha) * HKL_DEGTORAD,
								   gtk_spin_button_get_value(hkl->_spinbutton_beta) * HKL_DEGTORAD,
								   gtk_spin_button_get_value(hkl->_spinbutton_gamma) * HKL_DEGTORAD);

			hkl_sample_set_U_from_euler(sample,
										gtk_spin_button_get_value(hkl->_spinbutton_ux) * HKL_DEGTORAD,
										gtk_spin_button_get_value(hkl->_spinbutton_uy) * HKL_DEGTORAD,
										gtk_spin_button_get_value(hkl->_spinbutton_uz) * HKL_DEGTORAD);

			// set min/max
			hkl_parameter_set_range_unit(sample->lattice->a,
										 gtk_spin_button_get_value(hkl->_spinbutton_a_min),
										 gtk_spin_button_get_value(hkl->_spinbutton_a_max));
			hkl_parameter_set_range_unit(sample->lattice->b,
										 gtk_spin_button_get_value(hkl->_spinbutton_b_min),
										 gtk_spin_button_get_value(hkl->_spinbutton_b_max));
			hkl_parameter_set_range_unit(sample->lattice->c,
										 gtk_spin_button_get_value(hkl->_spinbutton_c_min),
										 gtk_spin_button_get_value(hkl->_spinbutton_c_max));
			hkl_parameter_set_range_unit(sample->lattice->alpha,
										 gtk_spin_button_get_value(hkl->_spinbutton_alpha_min),
										 gtk_spin_button_get_value(hkl->_spinbutton_alpha_max));
			hkl_parameter_set_range_unit(sample->lattice->beta,
										 gtk_spin_button_get_value(hkl->_spinbutton_beta_min),
										 gtk_spin_button_get_value(hkl->_spinbutton_beta_max));
			hkl_parameter_set_range_unit(sample->lattice->gamma,
										 gtk_spin_button_get_value(hkl->_spinbutton_gamma_min),
										 gtk_spin_button_get_value(hkl->_spinbutton_gamma_max));

			hkl_gui_update_crystal_model(hkl, sample);
			hkl_gui_update_reciprocal_lattice(hkl);
			hkl_gui_update_UB(hkl);
			hkl_gui_update_pseudo_axes(hkl);
			hkl_gui_update_pseudo_axes_frames(hkl);
		}
	}

	void on_checkbutton_a_toggled(Gtk.ToggleButton *togglebutton,
								  void *         user_data)
	{
		Hkl.Sample sample;

		sample = hkl->samples->current;
		if(sample)
			sample->lattice->a->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_b_toggled(Gtk.ToggleButton *togglebutton,
								  void *         user_data)
	{
		Hkl.Sample *sample;

		g_return_if_fail(user_data);

		hkl = user_data;
		sample = hkl->samples->current;
		if(sample)
			sample->lattice->b->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_c_toggled(Gtk.ToggleButton *togglebutton,
								  void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->lattice->c->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_alpha_toggled(Gtk.ToggleButton *togglebutton,
									  void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->lattice->alpha->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_beta_toggled(Gtk.ToggleButton *togglebutton,
									 void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->lattice->beta->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_gamma_toggled(Gtk.ToggleButton *togglebutton,
									  void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->lattice->gamma->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_Ux_toggled(Gtk.ToggleButton *togglebutton,
								   void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->ux->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_Uy_toggled(Gtk.ToggleButton *togglebutton,
								   void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->uy->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_checkbutton_Uz_toggled(Gtk.ToggleButton *togglebutton,
								   void *         user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			sample->uz->fit = gtk_toggle_button_get_active(togglebutton);
	}

	void on_cell_tree_view_axes_read_edited(Gtk.CellRendererText *renderer,
											string path,
											string new_text,
											void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis *axis;

		model = gtk_tree_view_get_model(hkl->_treeview_axes);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

		sscanf(new_text, "%lf", &value);
		axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
		hkl_axis_set_value_unit(axis, value);
		hkl_geometry_update(hkl->geometry);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
						   AXIS_COL_READ, value,
						   -1);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}

	void on_cell_tree_view_axes_write_edited(Gtk.CellRendererText *renderer,
											 string path,
											 string new_text,
											 void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis *axis;

		model = gtk_tree_view_get_model(hkl->_treeview_axes);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

		sscanf(new_text, "%lf", &value);
		axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
		hkl_axis_set_value_unit(axis, value);
		hkl_geometry_update(hkl->geometry);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
						   AXIS_COL_WRITE, value,
						   -1);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}

	void on_cell_tree_view_axes_min_edited(Gtk.CellRendererText *renderer,
										   string path,
										   string new_text,
										   void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis *axis;
		double shit;
		double max;

		model = gtk_tree_view_get_model(hkl->_treeview_axes);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

		sscanf(new_text, "%lf", &value);
		axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
		hkl_parameter_get_range_unit(axis.parent, &shit, &max);
		hkl_parameter_set_range_unit(axis.parent, value, max);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
						   AXIS_COL_MIN, value,
						   -1);
		hkl_gui_update_pseudo_axes(hkl);
	}

	void on_cell_tree_view_axes_max_edited(Gtk.CellRendererText *renderer,
										   string path,
										   string new_text,
										   void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis *axis;
		double shit;
		double min;

		model = gtk_tree_view_get_model(hkl->_treeview_axes);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, AXIS_COL_NAME, &name, -1);

		sscanf(new_text, "%lf", &value);

		axis = hkl_geometry_get_axis_by_name(hkl->geometry, name);
		hkl_parameter_get_range_unit(axis.parent, &min, &shit);
		hkl_parameter_set_range_unit(axis.parent, min, value);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
						   AXIS_COL_MAX, value,
						   -1);
		hkl_gui_update_pseudo_axes(hkl);
	}

// PseudoAxes
	void on_cell_tree_view_pseudo_axes_write_edited(Gtk.CellRendererText *renderer,
													string path,
													string new_text,
													void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.PseudoAxis *pseudoAxis;
		Hkl.Error *error;
		int res;

		model = gtk_tree_view_get_model(hkl->_treeview_pseudo_axes);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, PSEUDOAXIS_COL_PSEUDOAXIS, &pseudoAxis, -1);
		gtk_tree_model_get(model, &iter, PSEUDOAXIS_COL_NAME, &name, -1);

		sscanf(new_text, "%lf", &value);

		hkl_parameter_set_value_unit(pseudoAxis.parent, value);
		error = NULL;
		if(hkl_pseudo_axis_engine_set(pseudoAxis->engine, &error) == HKL_SUCCESS){
			hkl_geometry_init_geometry(hkl->geometry,
									   hkl->engines->geometries->items[0]->geometry);
			hkl_pseudo_axis_engine_list_get(hkl->engines);
			gtk_list_store_set(GTK_LIST_STORE(model), &iter,
							   PSEUDOAXIS_COL_WRITE, value,
							   -1);
			hkl_gui_update_axes(hkl);
			hkl_gui_update_pseudo_axes(hkl);
			hkl_gui_update_pseudo_axes_frames(hkl);
			hkl_gui_update_solutions(hkl);
		}
	}

	void on_cell_tree_view_pseudo_axes_is_initialized_toggled(Gtk.CellRendererToggle *cell_renderer,
															  string path,
															  void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.PseudoAxis *pseudoAxis;
		int old_flag;

		model = gtk_tree_view_get_model(hkl->_treeview_pseudo_axes);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter,
						   PSEUDOAXIS_COL_PSEUDOAXIS, &pseudoAxis,
						   PSEUDOAXIS_COL_INITIALIZED, &old_flag,
						   -1);

		if (!old_flag){
			int res;

			res = hkl_pseudo_axis_engine_initialize(pseudoAxis->engine, NULL);
			if(res == HKL_SUCCESS)
				hkl_gui_update_pseudo_axes(hkl);
		}
	}

/* PseudoAxes Parameters */
	void on_cell_tree_view_pseudo_axes_parameters_value_edited(Gtk.CellRendererText *renderer,
															   string path,
															   string new_text,
															   void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		double value;
		Hkl.Parameter *parameter;

		model = gtk_tree_view_get_model(hkl->_treeview_pseudo_axes_parameters);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, PARAMETER_COL_PARAMETER, &parameter, -1);

		sscanf(new_text, "%lf", &value);

		hkl_parameter_set_value_unit(parameter, value);

		gtk_list_store_set(GTK_LIST_STORE(model), &iter,
						   PARAMETER_COL_VALUE, value,
						   -1);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_parameters(hkl);
	}

	void on_cell_tree_view_crystals_name_edited(Gtk.CellRendererText *renderer,
												string path,
												string new_text,
												void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;
		string name;

		model = gtk_tree_view_get_model(hkl->_treeview_crystals);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, SAMPLE_COL_NAME, &name, -1);

		sample = hkl_sample_list_get_by_name(hkl->samples, name);
		if(sample){
			hkl_sample_set_name(sample, new_text);
			hkl_gui_update_tree_view_crystals(hkl);
		}
	}

	void on_cell_tree_view_reflections_h_edited(Gtk.CellRendererText *renderer,
												string path,
												string new_text,
												void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;

		model = gtk_tree_view_get_model(hkl->_treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);

		sample = hkl->samples->current;
		if(sample){
			int index;
			double h;
			double k;
			double l;
			Hkl.SampleReflection *reflection;

			gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
			reflection = sample->reflections[index];

			sscanf(new_text, "%lf", &h);
			k = reflection->hkl.data[1];
			l = reflection->hkl.data[2];

			hkl_sample_reflection_set_hkl(reflection, h, k, l);

			gtk_list_store_set(GTK_LIST_STORE(model), &iter,
							   REFLECTION_COL_H, h,
							   REFLECTION_COL_FLAG, reflection->flag,
							   -1);
			hkl_gui_update_crystal_model(hkl, sample);
		}
	}

	void on_cell_tree_view_reflections_k_edited(Gtk.CellRendererText *renderer,
												string path,
												string new_text,
												void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;

		model = gtk_tree_view_get_model(hkl->_treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);

		sample = hkl->samples->current;
		if(sample){
			int index;
			double h;
			double k;
			double l;
			Hkl.SampleReflection *reflection;

			gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
			reflection = sample->reflections[index];

			h = reflection->hkl.data[0];
			sscanf(new_text, "%lf", &k);
			l = reflection->hkl.data[2];

			hkl_sample_reflection_set_hkl(reflection, h, k, l);

			gtk_list_store_set(GTK_LIST_STORE(model), &iter,
							   REFLECTION_COL_K, k,
							   REFLECTION_COL_FLAG, reflection->flag,
							   -1);
			hkl_gui_update_crystal_model(hkl, sample);
		}
	}

	void on_cell_tree_view_reflections_l_edited(Gtk.CellRendererText *renderer,
												string path,
												string new_text,
												void * user_data)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;

		model = gtk_tree_view_get_model(hkl->_treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);

		sample = hkl->samples->current;
		if(sample){
			int index;
			double h;
			double k;
			double l;
			Hkl.SampleReflection *reflection;

			gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
			reflection = sample->reflections[index];

			h = reflection->hkl.data[0];
			k = reflection->hkl.data[1];
			sscanf(new_text, "%lf", &l);

			hkl_sample_reflection_set_hkl(reflection, h, k, l);

			gtk_list_store_set(GTK_LIST_STORE(model), &iter,
							   REFLECTION_COL_L, l,
							   REFLECTION_COL_FLAG, reflection->flag,
							   -1);
			hkl_gui_update_crystal_model(hkl, sample);
		}
	}

	void on_cell_tree_view_reflections_flag_toggled(Gtk.CellRendererToggle *cell_renderer,
													string path,
													void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample){
			int index;
			int flag;
			Hkl.SampleReflection *reflection;
			Gtk.TreeModel *model;
			Gtk.TreeIter iter;

			model = gtk_tree_view_get_model(hkl->_treeview_reflections);
			gtk_tree_model_get_iter_from_string(model, &iter, path);
			gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
			reflection = sample->reflections[index];
			flag = !reflection->flag;
			hkl_sample_reflection_set_flag(reflection, flag);
			gtk_list_store_set(GTK_LIST_STORE(model), &iter, REFLECTION_COL_FLAG, flag, -1);
		}
	}

	void on_toolbutton_add_reflection_clicked(Gtk.ToolButton *toolbutton,
											  void * user_data)
	{
		Hkl.Sample *sample;

		sample=hkl->samples->current;
		if(sample){
			double h;
			double k;
			double l;

			hkl_sample_add_reflection(sample, hkl->geometry, hkl->detector, h, k, l);
			hkl_gui_update_reflections(hkl, sample);
		}
	}

	void on_toolbutton_goto_reflection_clicked(Gtk.ToolButton *toolbutton,
											   void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample){
			Gtk.TreeSelection *selection;
			uint nb_rows;

			selection = gtk_tree_view_get_selection(hkl->_treeview_reflections);
			nb_rows = gtk_tree_selection_count_selected_rows(selection);
			if(nb_rows == 1){
				GLib.List *list;
				uint index;
				Gtk.TreeIter iter;
				Gtk.TreeModel *model;

				/* first need to find the right reflections_store and put it in model */
				list = gtk_tree_selection_get_selected_rows(selection, &model);
				gtk_tree_model_get_iter(model, &iter, list->data);
				gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);

				hkl_geometry_init_geometry(hkl->geometry,
										   sample->reflections[index]->geometry);

				hkl_gui_update_source(hkl);
				hkl_gui_update_axes(hkl);
				hkl_gui_update_pseudo_axes(hkl);

				g_list_foreach (list, gtk_tree_path_free, NULL);
				g_list_free (list);
			}else{
				if (nb_rows)
					gtk_statusbar_push(hkl->_statusBar, 0, "Please select only one reflection.");
				else
					gtk_statusbar_push(hkl->_statusBar, 0, "Please select at least one reflection.");
			}
		}
	}

	void on_toolbutton_del_reflection_clicked(Gtk.ToolButton *toolbutton,
											  void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample){
			Gtk.TreeSelection *selection;
			uint nb_rows;

			selection = gtk_tree_view_get_selection(hkl->_treeview_reflections);
			nb_rows = gtk_tree_selection_count_selected_rows(selection);
			if (nb_rows){
				GLib.List *list;
				Gtk.TreeModel *model;
				Gtk.TreeIter iter;
				uint index;
				uint[] indexes;
				int i;
				Gtk.Widget *dialog;
				int respons;

				list = gtk_tree_selection_get_selected_rows(selection, &model);
				// fill indexes with the reflections index
				indexes = new uint[nb_rows];
				i = 0;
				while(list){
					gtk_tree_model_get_iter(model, &iter, list->data);
					gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &indexes[i++], -1);
					g_list_next(list);
				}

				/* display a dialog message to confirm the deletion */
				dialog = gtk_message_dialog_new(NULL,
												GTK_DIALOG_DESTROY_WITH_PARENT,
												GTK_MESSAGE_WARNING,
												GTK_BUTTONS_YES_NO,
												"Are you sure that you want to delete reflections");
				//"Are you sure that you want to delete reflections");
				respons = gtk_dialog_run (GTK_DIALOG (dialog));
				switch (respons){
				case GTK_RESPONSE_YES:
					for(i=0;i<nb_rows;++i){
						// compute the correct index of the reflection
						uint index = indexes[i] - i;
						hkl_sample_del_reflection(sample, index);
					}
					hkl_gui_update_reflections(hkl, sample);
					break;
				}
				gtk_widget_destroy(dialog);

				free(indexes);
			}else
				gtk_statusbar_push(hkl->_statusBar, 0, "Please select at least one reflection.");
		}
	}
	void on_toolbutton_setUB_clicked(Gtk.ToolButton *toolbutton,
									 void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample){
			Hkl.Matrix UB;

			UB.data[0][0] = gtk_spin_button_get_value(hkl->_spinbutton_U11);
			UB.data[0][1] = gtk_spin_button_get_value(hkl->_spinbutton_U12);
			UB.data[0][2] = gtk_spin_button_get_value(hkl->_spinbutton_U13);
			UB.data[1][0] = gtk_spin_button_get_value(hkl->_spinbutton_U21);
			UB.data[1][1] = gtk_spin_button_get_value(hkl->_spinbutton_U22);
			UB.data[1][2] = gtk_spin_button_get_value(hkl->_spinbutton_U23);
			UB.data[2][0] = gtk_spin_button_get_value(hkl->_spinbutton_U31);
			UB.data[2][1] = gtk_spin_button_get_value(hkl->_spinbutton_U32);
			UB.data[2][2] = gtk_spin_button_get_value(hkl->_spinbutton_U33);

			hkl_sample_set_UB(sample, &UB);
			hkl_sample_fprintf(stdout, sample);

			hkl_gui_update_lattice(hkl);
			hkl_gui_update_lattice_parameters(hkl);
			hkl_gui_update_reciprocal_lattice(hkl);
			hkl_gui_update_crystal_model(hkl, sample);
			hkl_gui_update_UB(hkl);
			hkl_gui_update_UxUyUz(hkl);
			hkl_gui_update_pseudo_axes(hkl);
			hkl_gui_update_pseudo_axes_frames(hkl);
		}
	}

	void on_toolbutton_computeUB_clicked(Gtk.ToolButton *toolbutton,
										 void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample){
			hkl_sample_compute_UB_busing_levy(sample, 0, 1);
			hkl_gui_update_UB(hkl);
			hkl_gui_update_UxUyUz(hkl);
			hkl_gui_update_pseudo_axes(hkl);
			hkl_gui_update_pseudo_axes_frames(hkl);
		}
	}

	void on_toolbutton_add_crystal_clicked(Gtk.ToolButton *toolbutton,
										   void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl_sample_new("new_sample", HKL_SAMPLE_TYPE_MONOCRYSTAL);
		if(sample){
			Gtk.TreePath *path;
			Gtk.TreeViewColumn *column;

			hkl_sample_list_append(hkl->samples, sample);
			hkl_sample_list_select_current(hkl->samples, "new_sample");
			hkl_gui_update_tree_view_crystals(hkl);

			// activate for edition the name of the new crystal
			gtk_tree_view_get_cursor(hkl->_treeview_crystals, &path, &column);
			column = gtk_tree_view_get_column(hkl->_treeview_crystals, 0);
			gtk_tree_view_set_cursor(hkl->_treeview_crystals, path, column, TRUE);
		}
	}

	void on_toolbutton_copy_crystal_clicked(Gtk.ToolButton *toolbutton,
											void * user_data)
	{
		Hkl.Sample *sample;
		Hkl.Sample *old_sample;
		Gtk.TreePath *path;
		Gtk.TreeViewColumn *column;

		old_sample = hkl->samples->current;
		if(!old_sample){
			gtk_statusbar_push(hkl->_statusBar, 0, "Please select a crystal to copy.");
			return;
		}

		sample = hkl_sample_new_copy(hkl->samples->current);
		hkl_sample_set_name(sample, "copy");
		hkl_sample_list_append(hkl->samples, sample);
		hkl_sample_list_select_current(hkl->samples, "copy");
		hkl_gui_update_tree_view_crystals(hkl);

		// activate for edition the name of the new crystal
		gtk_tree_view_get_cursor(hkl->_treeview_crystals, &path, &column);
		column = gtk_tree_view_get_column(hkl->_treeview_crystals, 0);
		gtk_tree_view_set_cursor(hkl->_treeview_crystals, path, column, TRUE);
	}

	void on_toolbutton_del_crystal_clicked(Gtk.ToolButton *toolbutton,
										   void * user_data)
	{
		Hkl.Sample *sample;
		if(hkl->samples->current){
			hkl_sample_list_del(hkl->samples, hkl->samples->current);
			hkl_gui_update_tree_view_crystals(hkl);
		}
	}

	void on_toolbutton_affiner_clicked(Gtk.ToolButton *toolbutton,
									   void * user_data)
	{
		Hkl.Sample *sample;

		sample = hkl->samples->current;
		if(sample)
			hkl_sample_affine(sample);

		hkl_gui_update_crystal_model(hkl, sample);
		hkl_gui_update_lattice(hkl);
		hkl_gui_update_reciprocal_lattice(hkl);
		hkl_gui_update_UB(hkl);
		hkl_gui_update_UxUyUz(hkl);
	}

	bool on_tree_view_reflections_key_press_event(Gtk.Widget *widget,
												  Gdk.EventKey *event,
												  void * user_data)
	{
		switch (event->keyval)
		{
		case GDK_Insert:
		case GDK_KP_Insert:
			on_toolbutton_add_reflection_clicked(hkl->_toolbutton_add_crystal, user_data);
			break;
		case GDK_Delete:
		case GDK_KP_Delete:
			on_toolbutton_del_reflection_clicked(hkl->_toolbutton_del_reflection, user_data);
			break;
		}
		return TRUE;
	}

	bool on_tree_view_crystals_key_press_event(Gtk.Widget *widget,
											   Gdk.EventKey *event,
											   void * user_data)
	{
		switch (event->keyval)
		{
		case GDK_Insert:
		case GDK_KP_Insert:
			on_toolbutton_add_crystal_clicked(hkl->_toolbutton_add_crystal, user_data);
			break;
		case GDK_Delete:
		case GDK_KP_Delete:
			on_toolbutton_del_crystal_clicked(hkl->_toolbutton_del_reflection, user_data);
			break;
		}
		return TRUE;
	}

	void on_tree_view1_cursor_changed(Gtk.TreeView *tree_view,
									  void * user_data)
	{
		Gtk.TreePath *path;
		Gtk.TreeViewColumn *focus_column;
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		size_t index;

		gtk_tree_view_get_cursor(tree_view, &path, &focus_column);
		model = gtk_tree_view_get_model(tree_view);
		gtk_tree_model_get_iter(model, &iter, path);
		gtk_tree_model_get(model, &iter, SOLUTION_COL_INDEX, &index, -1);


		hkl_geometry_init_geometry(hkl->geometry,
								   hkl->engines->geometries->items[index]->geometry);
		hkl_pseudo_axis_engine_list_get(hkl->engines);

		hkl_gui_update_axes(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}

	void on_pseudo_axes_frame_changed(void * user_data)
	{
		hkl_gui_update_axes(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
		hkl_gui_update_solutions(hkl);
	}

	void on_menuitem5_activate(Gtk.MenuItem *menuitem,
							   void * user_data)
	{
		this._dialog1.show();
	}

	void on_button1_clicked(Gtk.Button *button,
							void * user_data)
	{
		this._dialog1.hide();
	}

	void on_combobox1_changed(Gtk.ComboBox *widget,
							  void * user_data)
	{
		size_t idx;
		Hkl.GeometryConfig config;

		idx = gtk_combo_box_get_active(widget);

		config = &hkl_geometry_factory_configs[idx];
		if(hkl->geometry)
			hkl_geometry_free(hkl->geometry);
		hkl->geometry = hkl_geometry_factory_new(config, 50 * HKL_DEGTORAD);

		if(hkl->engines)
			hkl_pseudo_axis_engine_list_free(hkl->engines);

		hkl->engines = hkl_pseudo_axis_engine_list_factory(config);
		hkl_pseudo_axis_engine_list_init(hkl->engines, hkl->geometry,
										 hkl->detector, hkl->samples->current);

		hkl_gui_set_up_pseudo_axes_frames(hkl);
		hkl_gui_set_up_tree_view_axes(hkl);
		hkl_gui_set_up_tree_view_pseudo_axes_parameters(hkl);
		hkl_gui_set_up_tree_view_pseudo_axes(hkl);

		/* FIXME create the right solution Model Column */
		/* hkl->_solutionModelColumns = 0; */
		hkl_gui_set_up_tree_view_treeview1(hkl);
	}
}