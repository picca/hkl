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
	N_COLUMNS
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
	Gtk.Builder builder;

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
			this.builder = builder;
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

		this._spinbutton_a_min.value_changed.connect(on_spinbutton_a_min_value_changed);
		this._spinbutton_b_min.value_changed.connect(on_spinbutton_b_min_value_changed);
		this._spinbutton_c_min.value_changed.connect(on_spinbutton_c_min_value_changed);
		this._spinbutton_alpha_min.value_changed.connect(on_spinbutton_alpha_min_value_changed);
		this._spinbutton_beta_min.value_changed.connect(on_spinbutton_beta_min_value_changed);
		this._spinbutton_gamma_min.value_changed.connect(on_spinbutton_gamma_min_value_changed);

		this._spinbutton_a_max.value_changed.connect(on_spinbutton_a_max_value_changed);
		this._spinbutton_b_max.value_changed.connect(on_spinbutton_b_max_value_changed);
		this._spinbutton_c_max.value_changed.connect(on_spinbutton_c_max_value_changed);
		this._spinbutton_alpha_max.value_changed.connect(on_spinbutton_alpha_max_value_changed);
		this._spinbutton_beta_max.value_changed.connect(on_spinbutton_beta_max_value_changed);
		this._spinbutton_gamma_max.value_changed.connect(on_spinbutton_gamma_max_value_changed);

		this._spinbutton_lambda.value_changed.connect(on_spinbutton_lambda_value_changed);
		this._spinbutton_ux.value_changed.connect(on_spinbutton_uxuyuz_value_changed);
		this._spinbutton_uy.value_changed.connect(on_spinbutton_uxuyuz_value_changed);
		this._spinbutton_uz.value_changed.connect(on_spinbutton_uxuyuz_value_changed);

		this._button2.clicked.connect(on_button2_clicked);

		this._checkbutton_a.toggled.connect(on_checkbutton_a_toggled);
		this._checkbutton_b.toggled.connect(on_checkbutton_b_toggled);
		this._checkbutton_c.toggled.connect(on_checkbutton_c_toggled);
		this._checkbutton_alpha.toggled.connect(on_checkbutton_alpha_toggled);
		this._checkbutton_beta.toggled.connect(on_checkbutton_beta_toggled);
		this._checkbutton_gamma.toggled.connect(on_checkbutton_gamma_toggled);
		this._checkbutton_Ux.toggled.connect(on_checkbutton_Ux_toggled);
		this._checkbutton_Uy.toggled.connect(on_checkbutton_Uy_toggled);
		this._checkbutton_Uz.toggled.connect(on_checkbutton_Uz_toggled);

		this._treeview_reflections.key_press_event.connect(on_tree_view_reflections_key_press_event);
		
		this._treeview_pseudo_axes.cursor_changed.connect(on_tree_view_pseudo_axes_cursor_changed);
		this._treeview_crystals.cursor_changed.connect(on_tree_view_crystals_cursor_changed);
		this._treeview_crystals.key_press_event.connect( on_tree_view_crystals_key_press_event);


		this._toolbutton_add_reflection.clicked.connect(on_toolbutton_add_reflection_clicked);
		this._toolbutton_goto_reflection.clicked.connect(on_toolbutton_goto_reflection_clicked);
		this._toolbutton_del_reflection.clicked.connect(on_toolbutton_del_reflection_clicked);
		this._toolbutton_setUB.clicked.connect(on_toolbutton_setUB_clicked);
		this._toolbutton_computeUB.clicked.connect(on_toolbutton_computeUB_clicked);
		this._toolbutton_add_crystal.clicked.connect(on_toolbutton_add_crystal_clicked);
		this._toolbutton_copy_crystal.clicked.connect(on_toolbutton_copy_crystal_clicked);
		this._toolbutton_del_crystal.clicked.connect(on_toolbutton_del_crystal_clicked);
		this._toolbutton_affiner.clicked.connect(on_toolbutton_affiner_clicked);

		this._menuitem5.activate.connect(on_menuitem5_activate);

		// dialog1
		this._button1.clicked.connect(on_button1_clicked);
		this._combobox1.changed.connect(on_combobox1_changed);
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
		// this.pseudoAxesFrames = new Hkl.Gui.PseudoAxesFrame[];

		foreach(Hkl.PseudoAxisEngine engine in this.engines.engines){
			Hkl.Gui.PseudoAxesFrame pseudo;

			pseudo = new Hkl.Gui.PseudoAxesFrame(engine);
			this.pseudoAxesFrames += pseudo;

			vbox2.add(pseudo.frame1);
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

			this.store_axis.append(out iter);
			this.store_axis.set(iter,
								AxisCol.AXIS, axis,
								AxisCol.NAME, axis.parent.name);
		}

		/* first remove all the columns */
		var columns = this._treeview_axes.get_columns();
		foreach(weak Gtk.TreeViewColumn col in columns){
			this._treeview_axes.remove_column(col);
		}

		this._treeview_axes.set_model(this.store_axis);

		/* add the columns */
		/* name */
		this._treeview_axes.insert_column_with_attributes (-1, 
														   "name", new CellRendererText (),
														   "text", AxisCol.NAME);

 		/* read */
        var cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "read", cell,
														   "text", AxisCol.READ);
		cell.edited.connect(on_cell_tree_view_axes_read_edited);

		/* write */
        cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "write", cell,
														   "text", AxisCol.WRITE);
		cell.edited.connect(on_cell_tree_view_axes_write_edited);

		/* min */
        cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "min", cell,
														   "text", AxisCol.MIN);
		cell.edited.connect(on_cell_tree_view_axes_min_edited);

		/* max */
        cell = new CellRendererText ();
 		this._treeview_axes.insert_column_with_attributes (-1, 
														   "max", cell,
														   "text", AxisCol.MAX);
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
		renderer.edited.connect(on_cell_tree_view_pseudo_axes_write_edited);
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
		renderer.toggled.connect(on_cell_tree_view_pseudo_axes_is_initialized_toggled);
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
		renderer.edited.connect(on_cell_tree_view_pseudo_axes_parameters_value_edited);
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

		this._treeview1.cursor_changed.connect(on_tree_view1_cursor_changed);
		this.update_solutions();
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
		renderer.edited.connect(on_cell_tree_view_reflections_h_edited);
		column = gtk_tree_view_column_new_with_attributes ("h",
														   renderer,
														   "text", REFLECTION_COL_H,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* k */
		renderer = gtk_cell_renderer_text_new ();
		renderer.edited.connect(on_cell_tree_view_reflections_k_edited);
		column = gtk_tree_view_column_new_with_attributes ("k",
														   renderer,
														   "text", REFLECTION_COL_K,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* l */
		renderer = gtk_cell_renderer_text_new ();
		renderer.edited.connect(on_cell_tree_view_reflections_h_edited);
		column = gtk_tree_view_column_new_with_attributes ("l",
														   renderer,
														   "text", REFLECTION_COL_L,
														   NULL);
		gtk_tree_view_append_column(this._treeview_reflections, column);

		/* flag */
		renderer = gtk_cell_renderer_toggle_new ();
		renderer.toggled.connect(on_cell_tree_view_reflections_flag_toggled);
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
		renderer.edited.connect(on_cell_tree_view_crystals_name_edited);
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
		if(this.geometry != null){
			double lambda = this.geometry.source.get_wavelength();
			this._spinbutton_lambda.set_value(lambda);
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
		valid = this.store_axis.get_iter_first(out iter);
		while(valid){
			this.store_axis.get(iter,
								AxisCol.AXIS, ref axis);
			axis.parent.get_range_unit(out min, out max);
			this.store_axis.set(iter,
								AxisCol.READ, axis.parent.get_value_unit(),
								AxisCol.WRITE, axis.parent.get_value_unit(),
								AxisCol.MIN, min,
								AxisCol.MAX, max);
			valid = this.store_axis.iter_next(ref iter);
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
		this.engines.get();

		// update the model
		valid = this.store_pseudo_axis.get_iter_first(out iter);
		while(valid){
			this.store_pseudo_axis.get(iter,
									   PseudoAxisCol.PSEUDOAXIS, parameter);
			parameter.get_range_unit(out min, out max);
			this.store_pseudo_axis.set(iter,
									   PseudoAxisCol.READ, parameter.get_value_unit(),
									   PseudoAxisCol.WRITE, parameter.get_value_unit(),
									   PseudoAxisCol.MIN, min,
									   PseudoAxisCol.MAX, max,
									   PseudoAxisCol.INITIALIZED, true);
			valid = this.store_pseudo_axis.iter_next(ref iter);
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
		Hkl.Sample *sample = this.samples.current;
		if(sample != null){
			this._spinbutton_a.set_value(sample->lattice.a.get_value_unit()); 
			this._spinbutton_b.set_value(sample->lattice.b.get_value_unit()); 
			this._spinbutton_c.set_value(sample->lattice.c.get_value_unit()); 
			this._spinbutton_alpha.set_value(sample->lattice.alpha.get_value_unit()); 
			this._spinbutton_beta.set_value(sample->lattice.beta.get_value_unit()); 
			this._spinbutton_gamma.set_value(sample->lattice.gamma.get_value_unit()); 
		}
	}

	void update_lattice_parameters()
	{
		Hkl.Sample? sample = this.samples.current;
		if(sample != null){
			double min;
			double max;
			bool to_fit;
			Hkl.Parameter parameter;


			parameter = sample.lattice.a;
			parameter.get_range_unit(out min, out max);
			this._spinbutton_a_min.set_value(min);
			this._spinbutton_a_max.set_value(max);
			this._checkbutton_a.set_active(parameter.fit);

			parameter = sample.lattice.b;
			parameter.get_range_unit(out min, out max);
			this._spinbutton_b_min.set_value(min);
			this._spinbutton_b_max.set_value(max);
			this._checkbutton_b.set_active(parameter.fit);

			parameter = sample.lattice.c;
			parameter.get_range_unit(out min, out max);
			this._spinbutton_c_min.set_value(min);
			this._spinbutton_c_max.set_value(max);
			this._checkbutton_c.set_active(parameter.fit);

			parameter = sample.lattice.alpha;
			parameter.get_range_unit(out min, out max);
			this._spinbutton_alpha_min.set_value(min);
			this._spinbutton_alpha_max.set_value(max);
			this._checkbutton_alpha.set_active(parameter.fit);

			parameter = sample.lattice.beta;
			parameter.get_range_unit(out min, out max);
			this._spinbutton_beta_min.set_value(min);
			this._spinbutton_beta_max.set_value(max);
			this._checkbutton_beta.set_active(parameter.fit);

			parameter = sample.lattice.gamma;
			parameter.get_range_unit(out min, out max);
			this._spinbutton_gamma_min.set_value(min);
			this._spinbutton_gamma_max.set_value(max);
			this._checkbutton_gamma.set_active(parameter.fit);
		}
	}

	void update_reciprocal_lattice()
	{
		Hkl.Sample *sample = this.samples.current;
		if(sample != null){
			sample->lattice.reciprocal(out this.reciprocal);

			this._spinbutton_a_star.set_value(this.reciprocal.a.get_value_unit());
			this._spinbutton_b_star.set_value(this.reciprocal.b.get_value_unit());
			this._spinbutton_c_star.set_value(this.reciprocal.c.get_value_unit());
			this._spinbutton_alpha_star.set_value(this.reciprocal.alpha.get_value_unit());
			this._spinbutton_beta_star.set_value(this.reciprocal.beta.get_value_unit());
			this._spinbutton_gamma_star.set_value(this.reciprocal.gamma.get_value_unit());
		}
	}

	void update_UB()
	{
		Hkl.Sample *sample = this.samples.current;
		if(sample != null){
			Hkl.Matrix UB;

			sample->get_UB(out UB);

			this._label_UB11.set_text(UB.data[0,0].to_string());
			this._label_UB12.set_text(UB.data[0,1].to_string());
			this._label_UB13.set_text(UB.data[0,2].to_string());
			this._label_UB21.set_text(UB.data[1,0].to_string());
			this._label_UB22.set_text(UB.data[1,1].to_string());
			this._label_UB23.set_text(UB.data[1,2].to_string());
			this._label_UB31.set_text(UB.data[2,0].to_string());
			this._label_UB32.set_text(UB.data[2,1].to_string());
			this._label_UB33.set_text(UB.data[2,2].to_string());
		}
	}

	void update_UxUyUz()
	{
		Hkl.Sample? sample = this.samples.current;
		if(sample != null){
			this._spinbutton_ux.set_value(sample.ux.get_value_unit());
			this._spinbutton_uy.set_value(sample.uy.get_value_unit());
			this._spinbutton_uz.set_value(sample.uz.get_value_unit());
			this._checkbutton_Ux.set_active(sample.ux.fit);
			this._checkbutton_Uy.set_active(sample.uy.fit);
			this._checkbutton_Uz.set_active(sample.uz.fit);
		}
	}

	static void _update_reflections(Hkl.Sample sample, Gtk.ListStore model)
	{
		size_t i;

		model.clear();
		i=0;
		foreach(weak Hkl.SampleReflection reflection in sample.reflections){
			Gtk.TreeIter iter;

			model.append(out iter);
			model.set(iter,
					  ReflectionCol.INDEX, i++,
					  ReflectionCol.H, reflection.hkl.data[0],
					  ReflectionCol.K, reflection.hkl.data[1],
					  ReflectionCol.L, reflection.hkl.data[2],
					  ReflectionCol.FLAG, reflection.flag);
		}
	}

	void update_tree_view_crystals()
	{
		size_t i;
		Hkl.Sample sample;
		Gtk.ListStore reflections;
		Gtk.ListStore reflections_current;
		Gtk.TreeIter iter;
		Gtk.TreeIter iter_current;
		bool valid;
		string current_crystal_name;
		bool is_current_crystal_set = false;

		this.store_samples = new Gtk.ListStore(SampleCol.N_COLUMNS,
											   typeof(void *), typeof(void *),
											   typeof(string), typeof(double),
											   typeof(double), typeof(double),
											   typeof(double), typeof(double),
											   typeof(double));

		if (this.samples.current != null){
			is_current_crystal_set = true;
			current_crystal_name = this.samples.current.name;
		}

		/* Fill the models from the crystalList */
		foreach(Hkl.Sample sample in this.samples.samples){
			weak Hkl.Lattice lattice = sample.lattice;

			/* create the attached reflections model */
			reflections = new Gtk.ListStore(ReflectionCol.N_COLUMNS,
											typeof(int),
											typeof(double),
											typeof(double),
											typeof(double),
											typeof(bool));
			this._update_reflections(sample, reflections);

			this.store_samples.append(out iter);
			this.store_samples.set(iter,
								   SampleCol.SAMPLE, sample,
								   SampleCol.REFLECTIONS, reflections,
								   SampleCol.NAME, sample.name,
								   SampleCol.A, lattice.a.get_value_unit(),
								   SampleCol.B, lattice.b.get_value_unit(),
								   SampleCol.C, lattice.c.get_value_unit(),
								   SampleCol.ALPHA, lattice.alpha.get_value_unit(),
								   SampleCol.BETA, lattice.beta.get_value_unit(),
								   SampleCol.GAMMA, lattice.gamma.get_value_unit());

			/* save a few parameters if it is the current sample */
			if (is_current_crystal_set && current_crystal_name == sample.name){
				iter_current = iter;
				reflections_current = reflections;
			}
		}

		/* Set the model for the TreeView */
		this._treeview_crystals.set_model(this.store_samples);
		if (is_current_crystal_set){
			Gtk.TreePath path;

			path = this.store_samples.get_path(iter_current);
			this._treeview_crystals.set_cursor(path, null, false);
			this._treeview_reflections.set_model(reflections_current);
		}
	}

	void update_reflections(Hkl.Sample sample)
	{
		bool valid;
		Gtk.TreeIter iter;

		valid = this.store_samples.get_iter_first(out iter);
		while(valid){
			Hkl.Sample sample_iter;
			Gtk.ListStore model;

			this.store_samples.get(iter,
								   SampleCol.SAMPLE, sample_iter,
								   SampleCol.REFLECTIONS, model);
			if (sample == sample_iter){
				this._update_reflections(sample, model);
				break;
			}
			valid = this.store_samples.iter_next(ref iter);
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

		valid = this.store_samples.get_iter_first(out iter);
		while(valid){
			string name;

			this.store_samples.get(iter,
								   SampleCol.NAME, name);
			if (name == sample.name){
				unowned Hkl.Lattice lattice = sample.lattice;
				this.store_samples.set(iter,
									   SampleCol.A, lattice.a.get_value_unit(),
									   SampleCol.B, lattice.b.get_value_unit(),
									   SampleCol.C, lattice.c.get_value_unit(),
									   SampleCol.ALPHA, lattice.alpha.get_value_unit(),
									   SampleCol.BETA, lattice.beta.get_value_unit(),
									   SampleCol.GAMMA, lattice.gamma.get_value_unit());
				break;
			}
			valid = this.store_samples.iter_next(ref iter);
		}
	}

	void update_pseudo_axes_frames()
	{
		size_t i;

		foreach(Hkl.Gui.PseudoAxesFrame frame in this.pseudoAxesFrames){
			frame.update();
		}
	}

	void update_solutions()
	{
		size_t i;

		this.store_solutions.clear();
		foreach(Hkl.GeometryListItem item in this.engines.geometries.items){
			size_t j=0;
			Gtk.TreeIter iter;

			this.store_solutions.append(out iter);
			this.store_solutions.set(iter, SolutionCol.INDEX, i);
			foreach(Hkl.Axis axis in item.geometry.axes){
				this.store_solutions.set(iter, SolutionCol.N_COLUMNS + j++, axis.parent.get_value_unit());
			}
		}
	}

	/*****************/
	/*** Callbacks ***/
	/*****************/

	void on_tree_view_pseudo_axes_cursor_changed()
	{
		Gtk.TreePath path;
		Gtk.TreeViewColumn focus_column;
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		Hkl.PseudoAxis pseudoAxis;

		this._treeview_pseudo_axes.get_cursor(out path, out focus_column);
		model = this._treeview_pseudo_axes.get_model();
		model.get_iter(out iter, path);
		model.get(iter, PseudoAxisCol.PSEUDOAXIS, pseudoAxis);
		this._treeview_pseudo_axes_parameters.set_model(model);
	}

	void on_tree_view_crystals_cursor_changed()
	{
		Gtk.TreePath path;
		Gtk.TreeViewColumn focus_column;
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		string name;

		this._treeview_crystals.get_cursor(out path, out focus_column);
		model = this._treeview_crystals.get_model();
		model.get_iter(out iter, path);
		model.get(iter, SampleCol.NAME, name);

		this.samples.select_current(name);
		this.engines.init(this.geometry, this.detector, this.samples.current);
		this._treeview_reflections.set_model(model);
		this.update_lattice();
		this.update_lattice_parameters();
		this.update_reciprocal_lattice();
		this.update_UxUyUz();
		this.update_UB();
		this.update_pseudo_axes();
		this.update_pseudo_axes_frames();
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

	void on_spinbutton_a_min_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_b_min_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_c_min_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_alpha_min_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_beta_min_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_gamma_min_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_a_max_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_b_max_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_c_max_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_alpha_max_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_beta_max_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_gamma_max_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_spinbutton_lambda_value_changed()
	{
		this.geometry.source.wave_length = this._spinbutton_lambda.get_value();
		this.update_pseudo_axes();
		this.update_pseudo_axes_frames();
	}

	void on_spinbutton_uxuyuz_value_changed()
	{
		// TODO change the cell background color if not synchro
	}

	void on_button2_clicked()
	{
		Hkl.Sample *sample;
		sample = this.samples.current;
		if(sample != null){
			sample->set_lattice(this._spinbutton_a.get_value(),
								this._spinbutton_b.get_value(),
								this._spinbutton_c.get_value(),
								this._spinbutton_alpha.get_value() * Hkl.DEGTORAD,
								this._spinbutton_beta.get_value() * Hkl.DEGTORAD,
								this._spinbutton_gamma.get_value() * Hkl.DEGTORAD);

			sample->set_U_from_euler(this._spinbutton_ux.get_value() * Hkl.DEGTORAD,
									 this._spinbutton_uy.get_value() * Hkl.DEGTORAD,
									 this._spinbutton_uz.get_value() * Hkl.DEGTORAD);

			// set min/max
			sample->lattice.a.set_range_unit(this._spinbutton_a_min.get_value(),
											 this._spinbutton_a_max.get_value());
			sample->lattice.b.set_range_unit(this._spinbutton_b_min.get_value(),
											 this._spinbutton_b_max.get_value());
			sample->lattice.c.set_range_unit(this._spinbutton_c_min.get_value(),
											 this._spinbutton_c_max.get_value());
			sample->lattice.alpha.set_range_unit(this._spinbutton_alpha_min.get_value(),
												 this._spinbutton_alpha_max.get_value());
			sample->lattice.beta.set_range_unit(this._spinbutton_beta_min.get_value(),
												this._spinbutton_beta_max.get_value());
			sample->lattice.gamma.set_range_unit(this._spinbutton_gamma_min.get_value(),
												 this._spinbutton_gamma_max.get_value());

			this.update_crystal_model(sample);
			this.update_reciprocal_lattice();
			this.update_UB();
			this.update_pseudo_axes();
			this.update_pseudo_axes_frames();
		}
	}

	void on_checkbutton_a_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.lattice.a.fit = this._checkbutton_a.get_active();
	}

	void on_checkbutton_b_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.lattice.b.fit = this._checkbutton_b.get_active();
	}

	void on_checkbutton_c_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.lattice.c.fit = this._checkbutton_c.get_active();
	}

	void on_checkbutton_alpha_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.lattice.alpha.fit = this._checkbutton_alpha.get_active();
	}

	void on_checkbutton_beta_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.lattice.beta.fit = this._checkbutton_beta.get_active();
	}

	void on_checkbutton_gamma_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.lattice.gamma.fit = this._checkbutton_gamma.get_active();
	}

	void on_checkbutton_Ux_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.ux.fit = this._checkbutton_Ux.get_active();
	}

	void on_checkbutton_Uy_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.uy.fit = this._checkbutton_Uy.get_active();
	}

	void on_checkbutton_Uz_toggled()
	{
		unowned Hkl.Sample? sample = this.samples.current;
		if(sample != null)
			sample.uz.fit = this._checkbutton_Uz.get_active();
	}

	void on_cell_tree_view_axes_read_edited(string path,
											string new_text)
	{
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis axis;

		model = this._treeview_axes.get_model();
		model.get_iter_from_string(out iter, path);
		model.get(iter, AxisCol.NAME, out name);

		axis = this.geometry.get_axis_by_name(name);
		value = new_text.to_double();
		axis.set_value_unit(value);
		this.geometry.update();

		model.set(iter, AxisCol.READ, value);
		this.update_pseudo_axes();
		this.update_pseudo_axes_frames();
	}

	void on_cell_tree_view_axes_write_edited(string path,
											 string new_text)
	{
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis axis;

		model = this._treeview_axes.get_model();
		model.get_iter_from_string(out iter, path);
		model.get(iter, AxisCol.NAME, out name);

		axis = this.geometry.get_axis_by_name(name);
		value = new_text.to_double();
		axis.parent.set_value_unit(value);
		this.geometry.update();

		model.set(iter, AxisCol.WRITE, value);

		this.update_pseudo_axes();
		this.update_pseudo_axes_frames();
	}

	void on_cell_tree_view_axes_min_edited(string path,
										   string new_text)
	{
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis axis;
		double shit;
		double max;

		model = this._treeview_axes.get_model();
		model.get_iter_from_string(out iter, path);
		model.get(iter, AxisCol.NAME, out name);

		value = new_text.to_double();
		axis = this.geometry.get_axis_by_name(name);
		axis.parent.get_range_unit(axis.parent, out shit, out max);
		axis.parent.set_range_unit(axis.parent, value, max);

		model.set(iter, AxisCol.MIN, value);
		this.update_pseudo_axes();
	}

	void on_cell_tree_view_axes_max_edited(string path,
										   string new_text)
	{
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.Axis axis;
		double shit;
		double min;

		model = this._treeview_axes.get_model();
		model.get_iter_from_string(out iter, path);
		model.get(iter, AxisCol.NAME, out name);

		value = new_text.to_double();
		axis = this.geometry.get_axis_by_name(name);
		axis.parent.get_range_unit(out min, out shit);
		axis.parent.set_range_unit(min, value);

		model.set(iter, Axiscol.MAX, value);
		this.update_pseudo_axes();
	}

// PseudoAxes
	void on_cell_tree_view_pseudo_axes_write_edited(string path,
													string new_text)
	{
		Gtk.TreeModel model;
		Gtk.TreeIter iter;
		string name;
		double value;
		Hkl.PseudoAxis pseudoAxis;
		Hkl.Error error;
		int res;

		model = this._treeview_pseudo_axes.get_model();
		model.get_iter_from_string(out iter, path);
		model.get(iter,
				  PseudoAxisCol.PSEUDOAXIS, out pseudoAxis,
				  PseudoAxisCol.NAME, out name);

		value = new_text.to_double();

		pseudoAxis.parent.set_value_unit(value);
		if(pseudoAxis.engine.set(ref error)){
			hkl_geometry_init_geometry(this.geometry,
									   this.engines->geometries->items[0]->geometry);
			hkl_pseudo_axis_engine_list_get(this.engines);
			gtk_list_store_set(GTK_LIST_STORE(model), &iter,
							   PSEUDOAXIS_COL_WRITE, value,
							   -1);
			hkl_gui_update_axes(hkl);
			hkl_gui_update_pseudo_axes(hkl);
			hkl_gui_update_pseudo_axes_frames(hkl);
			hkl_gui_update_solutions(hkl);
		}
	}

	void on_cell_tree_view_pseudo_axes_is_initialized_toggled(string path)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.PseudoAxis *pseudoAxis;
		int old_flag;

		model = gtk_tree_view_get_model(this._treeview_pseudo_axes);
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
	void on_cell_tree_view_pseudo_axes_parameters_value_edited(string path,
															   string new_text)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		double value;
		Hkl.Parameter *parameter;

		model = gtk_tree_view_get_model(this._treeview_pseudo_axes_parameters);
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

	void on_cell_tree_view_crystals_name_edited(string path,
												string new_text)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;
		string name;

		model = gtk_tree_view_get_model(this._treeview_crystals);
		gtk_tree_model_get_iter_from_string(model, &iter, path);
		gtk_tree_model_get(model, &iter, SAMPLE_COL_NAME, &name, -1);

		sample = hkl_sample_list_get_by_name(this.samples, name);
		if(sample){
			hkl_sample_set_name(sample, new_text);
			hkl_gui_update_tree_view_crystals(hkl);
		}
	}

	void on_cell_tree_view_reflections_h_edited(string path,
												string new_text)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;

		model = gtk_tree_view_get_model(this._treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);

		sample = this.samples->current;
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

	void on_cell_tree_view_reflections_k_edited(string path,
												string new_text)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;

		model = gtk_tree_view_get_model(this._treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);

		sample = this.samples->current;
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

	void on_cell_tree_view_reflections_l_edited(string path,
												string new_text)
	{
		Gtk.TreeModel *model;
		Gtk.TreeIter iter;
		Hkl.Sample *sample;

		model = gtk_tree_view_get_model(this._treeview_reflections);
		gtk_tree_model_get_iter_from_string(model, &iter, path);

		sample = this.samples->current;
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

	void on_cell_tree_view_reflections_flag_toggled(string path)
	{
		Hkl.Sample *sample;

		sample = this.samples->current;
		if(sample){
			int index;
			int flag;
			Hkl.SampleReflection *reflection;
			Gtk.TreeModel *model;
			Gtk.TreeIter iter;

			model = gtk_tree_view_get_model(this._treeview_reflections);
			gtk_tree_model_get_iter_from_string(model, &iter, path);
			gtk_tree_model_get(model, &iter, REFLECTION_COL_INDEX, &index, -1);
			reflection = sample->reflections[index];
			flag = !reflection->flag;
			hkl_sample_reflection_set_flag(reflection, flag);
			gtk_list_store_set(GTK_LIST_STORE(model), &iter, REFLECTION_COL_FLAG, flag, -1);
		}
	}

	void on_toolbutton_add_reflection_clicked()
	{
		Hkl.Sample? sample;

		sample = this.samples.current;
		if(sample != null){
			double h;
			double k;
			double l;

			sample.add_reflection(this.geometry, this.detector, h, k, l);
			this.update_reflections(sample);
		}
	}

	void on_toolbutton_goto_reflection_clicked()
	{
		Hkl.Sample? sample;

		sample = this.samples.current;
		if(sample != null){
			Gtk.TreeSelection selection;
			uint nb_rows;

			selection = this._treeview_reflections.get_selection();
			nb_rows = selection.count_selected_rows();
			if(nb_rows == 1){
				uint index;
				Gtk.TreeIter iter;
				Gtk.TreeModel model;

				/* first need to find the right reflections_store and put it in model */
				var list = selection.get_selected_rows(out model);
				model.get_iter(out iter, list.data);
				model.get(iter, ReflectionCol.INDEX, ref index);

				this.geometry.init_geometry(sample.reflections[index].geometry);

				this.update_source();
				this.update_axes();
				this.update_pseudo_axes();
			}else{
				if (nb_rows > 0)
					this._statusBar.push(0, "Please select only one reflection.");
				else
					this._statusBar.push(0, "Please select at least one reflection.");
			}
		}
	}

	void on_toolbutton_del_reflection_clicked()
	{
		Hkl.Sample sample;

		sample = this.samples.current;
		if(sample != null){
			Gtk.TreeSelection selection;
			uint nb_rows;

			selection = this._treeview_reflections.get_selection();
			nb_rows = selection.count_selected_rows();
			if (nb_rows > 0){
				Gtk.TreeModel model;
				Gtk.TreeIter iter;

				var list = selection.get_selected_rows(out model);
				// fill indexes with the reflections index
				var indexes = new uint[nb_rows];
				int i = 0;
				foreach(weak Gtk.TreePath path in list){
					model.get_iter(out iter, path);
					model.get(iter, ReflectionCol.INDEX, out indexes[i++]);
				}

				/* display a dialog message to confirm the deletion */
				var dialog = new Gtk.MessageDialog(null,
												   Gtk.DialogFlags.DESTROY_WITH_PARENT,
												   Gtk.MessageType.WARNING,
												   Gtk.ButtonsType.YES_NO,
												   "Are you sure that you want to delete reflections");
				//"Are you sure that you want to delete reflections");
				int respons = dialog.run();
				switch (respons){
				case Gtk.ResponseType.YES:
					for(i=0;i<nb_rows;++i){
						// compute the correct index of the reflection
						uint index = indexes[i] - i;
						sample.del_reflection(index);
					}
					this.update_reflections(sample);
					break;
				}
			}else
				this._statusBar.push(0, "Please select at least one reflection.");
		}
	}

	void on_toolbutton_setUB_clicked()
	{
		Hkl.Sample? sample;

		sample = this.samples.current;
		if(sample != null){
			Hkl.Matrix UB;

			UB.data[0,0] = this._spinbutton_U11.get_value();
			UB.data[0,1] = this._spinbutton_U12.get_value();
			UB.data[0,2] = this._spinbutton_U13.get_value();
			UB.data[1,0] = this._spinbutton_U21.get_value();
			UB.data[1,1] = this._spinbutton_U22.get_value();
			UB.data[1,2] = this._spinbutton_U23.get_value();
			UB.data[2,0] = this._spinbutton_U31.get_value();
			UB.data[2,1] = this._spinbutton_U32.get_value();
			UB.data[2,2] = this._spinbutton_U33.get_value();

			sample.set_UB(UB);
			sample.fprintf(GLib.stdout);

			this.update_lattice();
			this.update_lattice_parameters();
			this.update_reciprocal_lattice();
			this.update_crystal_model(sample);
			this.update_UB();
			this.update_UxUyUz();
			this.update_pseudo_axes();
			this.update_pseudo_axes_frames();
		}
	}

	void on_toolbutton_computeUB_clicked()
	{
		Hkl.Sample? sample;

		sample = this.samples.current;
		if(sample != null){
			sample.compute_UB_busing_levy(0, 1);
			this.update_UB();
			this.update_UxUyUz();
			this.update_pseudo_axes();
			this.update_pseudo_axes_frames();
		}
	}

	void on_toolbutton_add_crystal_clicked()
	{
		Hkl.Sample sample;

		sample = new Hkl.Sample("new_sample", Hkl.SampleType.MONOCRYSTAL);
		if(sample != null){
			Gtk.TreePath path;
			Gtk.TreeViewColumn column;

			this.samples.append(sample);
			this.samples.select_current("new_sample");
			this.update_tree_view_crystals();

			// activate for edition the name of the new crystal
			this._treeview_crystals.get_cursor(out path, out column);
			column = this._treeview_crystals.get_column(0);
			this._treeview_crystals.set_cursor(path, column, true);
		}
	}

	void on_toolbutton_copy_crystal_clicked()
	{
		Hkl.Sample sample;
		Hkl.Sample old_sample;
		Gtk.TreePath path;
		Gtk.TreeViewColumn column;

		old_sample = this.samples.current;
		if(old_sample == null){
			this._statusBar.push(0, "Please select a crystal to copy.");
			return;
		}

		sample = new Hkl.Sample.Copy(this.samples.current);
		sample.set_name("copy");
		this.samples.append(sample);
		this.samples.select_current("copy");
		this.update_tree_view_crystals();

		// activate for edition the name of the new crystal
		this._treeview_crystals.get_cursor(out path, out column);
		column = this._treeview_crystals.get_column(0);
		this._treeview_crystals.set_cursor(path, column, true);
	}

	void on_toolbutton_del_crystal_clicked()
	{
		Hkl.Sample? sample;
		if(this.samples.current != null){
			this.samples.del(this.samples.current);
			this.update_tree_view_crystals();
		}
	}

	void on_toolbutton_affiner_clicked()
	{
		Hkl.Sample? sample;

		sample = this.samples.current;
		if(sample != null)
			sample.affine();

		this.update_crystal_model(sample);
		this.update_lattice();
		this.update_reciprocal_lattice();
		this.update_UB();
		this.update_UxUyUz();
	}

	bool on_tree_view_reflections_key_press_event(Gdk.EventKey event)
	{
/*
		switch (event->keyval)
		{
		case Gdk.Insert:
		case Gdk.KP_Insert:
			this._toolbutton_add_crystal.clicked();
			break;
		case Gdk.Delete:
		case Gdk.KP_Delete:
			this._toolbutton_del_reflection.clicked();
			break;
		}
		return TRUE;
*/
	}

	bool on_tree_view_crystals_key_press_event(Gdk.EventKey event)
	{
/*
		switch (event.keyval)
		{
		case Gdk.Insert:
		case Gdk.KP_Insert:
			this._toolbutton_add_crystal.clicked();
			break;
		case Gdk.Delete:
		case Gdk.KP_Delete:
			this._toolbutton_del_reflection.clicked();
			break;
		}
*/
		return true;
	}

	void on_tree_view1_cursor_changed()
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


		hkl_geometry_init_geometry(this.geometry,
								   this.engines->geometries->items[index]->geometry);
		hkl_pseudo_axis_engine_list_get(this.engines);

		hkl_gui_update_axes(hkl);
		hkl_gui_update_pseudo_axes(hkl);
		hkl_gui_update_pseudo_axes_frames(hkl);
	}

	void on_pseudo_axes_frame_changed()
	{
		this.update_axes();
		this.update_pseudo_axes();
		this.update_pseudo_axes_frames();
		this.update_solutions();
	}

	void on_menuitem5_activate()
	{
		this._dialog1.show();
	}

	void on_button1_clicked()
	{
		this._dialog1.hide();
	}

	void on_combobox1_changed()
	{
		size_t idx;
		Hkl.GeometryConfig config;

		idx = this._combobox1.get_active();

		config = hkl_geometry_factory_configs[idx];
		this.geometry = hkl_geometry_factory_new(config, 50 * Hkl.DEGTORAD);

		this.engines = hkl_pseudo_axis_engine_list_factory(config);
		this.engines.init(this.geometry, this.detector, this.samples.current);

		this.set_up_pseudo_axes_frames();
		this.set_up_tree_view_axes();
		this.set_up_tree_view_pseudo_axes_parameters();
		this.set_up_tree_view_pseudo_axes();

		/* FIXME create the right solution Model Column */
		/* this._solutionModelColumns = 0; */
		this.set_up_tree_view_treeview1();
	}
}