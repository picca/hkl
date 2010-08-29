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

enum ModeCol
{
	NAME = 0,
	NUM_COLS
}

enum PseudoCol
{
	NAME = 0,
	VALUE,
	PSEUDO,
	NUM_COLS
}

public class Hkl.Gui.PseudoAxesFrame : GLib.Object
{
	public weak Hkl.PseudoAxisEngine engine;
	public Gtk.Frame frame1;
	public Gtk.Label label2;
	public Gtk.ComboBox combobox1;
	public Gtk.Expander expander1;
	public Gtk.TreeView treeview1;
	public Gtk.Button button1;
	public Gtk.Button button2;
	public Gtk.ListStore store_mode;
	public Gtk.ListStore store_pseudo;
	public Gtk.ListStore store_mode_parameter;

	public signal void changed();

	public PseudoAxesFrame(Hkl.PseudoAxisEngine engine)
	{
		Gtk.Builder builder = new Gtk.Builder();

		this.engine = engine;

		try{
			builder.add_from_file("pseudo.ui");
		}catch(GLib.Error e)
		{
			return;
		}

		// extract widgets from the builder
		this.frame1 = builder.get_object("frame1") as Gtk.Frame;
		this.label2 = builder.get_object("label2") as Gtk.Label;
		this.combobox1 = builder.get_object("combobox1") as Gtk.ComboBox;
		this.expander1 = builder.get_object("expander1") as Gtk.Expander;
		this.treeview1 = builder.get_object("treeview1") as Gtk.TreeView;
		this.button1 = builder.get_object("button1") as Gtk.Button;
		this.button2 = builder.get_object("button2") as Gtk.Button;
		this.store_mode = builder.get_object("liststore1") as Gtk.ListStore;
		this.store_pseudo = builder.get_object("liststore2")as Gtk.ListStore;
		this.store_mode_parameter = builder.get_object("liststore3") as Gtk.ListStore;

		// title
		this.label2.set_label(this.engine.name);

		// update all the liststore
		this.update_pseudo_axis();
		this.update_mode();
		this.update_mode_parameters();

		builder.connect_signals(this);

		// extra connect signals
		Gtk.TreeViewColumn col = this.treeview1.get_column(1);
		GLib.List<Gtk.CellRenderer> cells = col.get_cells();
		foreach(var item in cells){
			var cell = item as Gtk.CellRendererText;
			cell.edited.connect(this.on_cell_tree_view_pseudo_axis_value_edited);
		}
	}

	public void update()
	{
		this.update_pseudo_axis();

		Gtk.TreeViewColumn col = this.treeview1.get_column(1);
		var cell = col.get_cells().data as Gtk.CellRenderer;
		cell.set("background", null);
	}

	/****************/
	/* Non-Callback */
	/****************/

	void update_pseudo_axis()
	{
		Gtk.TreeIter iter;

		this.store_pseudo.clear();
		foreach(weak Hkl.PseudoAxis pseudoAxis in this.engine.pseudoAxes){
			this.store_pseudo.append(out iter);

			this.store_pseudo.set(iter,
								  PseudoCol.NAME, pseudoAxis.parent.name,
								  PseudoCol.VALUE, pseudoAxis.parent.get_value_unit(),
								  PseudoCol.PSEUDO, pseudoAxis);
		}
	}

	void update_mode()
	{
		size_t i;
		Gtk.TreeIter iter;

		this.store_mode.clear();
		for(i=0; i<this.engine.modes.length; ++i){
			this.store_mode.append(out iter);
			this.store_mode.set(iter,
								ModeCol.NAME, this.engine.modes[i].name);
		}
	}

	void update_mode_parameters()
	{
		if(this.engine.mode != null){
			size_t len = this.engine.mode.parameters.length;
			if(len > 0){
				Gtk.TreeIter iter;

				this.store_mode_parameter.clear();
				foreach(unowned Hkl.Parameter parameter in this.engine.mode.parameters){
					this.store_mode_parameter.append(out iter);
					this.store_mode_parameter.set(iter,
												  PseudoCol.NAME, parameter.name,
												  PseudoCol.VALUE, parameter.get_value_unit());
				}
				this.expander1.set_expanded(true);
				this.expander1.show();
			}else
				this.expander1.hide();
		}else
			this.expander1.hide();
	}

	/************/
	/* Callback */
	/************/

	void on_combobox1_changed(Gtk.ComboBox combobox)
	{
		size_t idx;

		idx = combobox.get_active();
		if(idx < this.engine.modes.length){
			this.engine.select_mode(idx);
			this.update_mode_parameters();
		}
	}

	void on_button1_clicked(Gtk.Button button)
	{
		if(!this.engine.set(null))
			this.engine.engines.geometry.init_geometry(this.engine.engines.geometries.items[0].geometry);

		this.changed();
	}

	void on_button2_clicked(Gtk.Button button)
	{
		if(!this.engine.initialize(null)){
			this.update_mode_parameters(); //some initialize function modify the parameters
			//hkl_pseudo_axis_engine_fprintf(stdout, this.engine);
		}
	}

	void on_cell_tree_view_pseudo_axis_value_edited(Gtk.CellRendererText renderer,
													string path, string new_text)
	{
		Gtk.TreeIter iter;
		Gtk.ListStore model;


		model = this.store_pseudo;
		if (model.get_iter_from_string(out iter, path)){
			double value;
			weak Hkl.PseudoAxis? pseudo;

			value = new_text.to_double();
			model.get(iter, PseudoCol.PSEUDO, out pseudo);
			if(pseudo != null){
				renderer.set("background", "red", null);
				pseudo.parent.set_value_unit(value);
				model.set(iter, PseudoCol.VALUE, value);
			}
		}
	}
}