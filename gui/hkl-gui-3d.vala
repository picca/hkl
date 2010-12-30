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

using Posix;
using GLib;
using Gtk;
using Hkl;
using Hkl3D;

enum Hkl.Gui.ObjectCol
{
	NAME = 0,
	HIDE,
	CONFIG,
	OBJECT,
	N_COLUMNS
}

public class Hkl.Gui.3DFrame : GLib.Object
{
	Hkl3D.Anticollision hkl3d;
	// Hkl3dGui::Scene *_Scene;

	/* widgets */
	Gtk.Frame _frame1;
	Gtk.VBox _vbox1;
	Gtk.TreeView _treeview1;
	Gtk.ToolButton _toolbutton1;
	Gtk.ToolButton _toolbutton2;
	Gtk.FileChooserDialog _filechooserdialog1;
	Gtk.Button _button1;
	Gtk.Button _button2;

	/* objects */
	Gtk.TreeStore store_objects; /* use to fill the hkl3d objects properties */


	public 3DFrame(string filename, Hkl.Geometry geometry)
	{
		Gtk.Builder builder = new Gtk.Builder();

		try{
			builder.add_from_file("3d.ui");
		}catch(GLib.Error e){
			/*
			try{
				//builder.add_from_file(PKGDATA + "3d.ui");
			}catch(GLib.Error e){
				Posix.exit(0);
			}
			*/
		}

		// widgets
		this._frame1 = builder.get_object("frame1") as Gtk.Frame;
		this._vbox1 = builder.get_object("vbox1") as Gtk.VBox;
		this._treeview1 = builder.get_object("treeview1") as Gtk.TreeView;
		this._toolbutton1 = builder.get_object("toolbutton1") as Gtk.ToolButton;
		this._toolbutton2 = builder.get_object("toolbutton2") as Gtk.ToolButton;
		this._filechooserdialog1 = builder.get_object("filechooserdialog1") as Gtk.FileChooserDialog;
		this._button1 = builder.get_object("button1") as Gtk.Button;
		this._button2 = builder.get_object("button2") as Gtk.Button;

		// objects
		this.store_objects = builder.get_object("treestore1") as Gtk.TreeStore;

		if(filename != null && geometry != null){
			//Gtk.CellRendererToggle renderer;

			//_hkl3d = hkl3d_new(filename, geometry);
			//_Scene = new Hkl3dGui::Scene(_hkl3d, false, false, false);

			this.update_hkl3d_objects_TreeStore();
			// this._vbox1->pack_start(*_Scene);
			this._vbox1.show_all();

			// connect signals
			//renderer = this._treeview1.get_column(1); // 1 is the index of the value column
			//renderer.toggled.connect(on_cell_treeview1_toggled);

			this._treeview1.cursor_changed.connect(on_treeview1_cursor_changed);
			this._toolbutton1.clicked.connect(on_toolbutton1_clicked);
			this._toolbutton2.clicked.connect(on_toolbutton2_clicked);
			this._button1.clicked.connect(on_button1_clicked);
			this._button2.clicked.connect(on_button2_clicked);
		}
	}

	public void is_colliding()
	{
		//hkl3d_is_colliding(_hkl3d);
	}

	public void invalidate()
	{
		//_Scene.invalidate();
	}

	void update_hkl3d_objects_TreeStore()
	{
		this.store_objects.clear();
		foreach(Hkl3D.Config config in this.hkl3d.configs.configs){
			Gtk.TreeIter iter;

			this.store_objects.append(out iter, null);
			this.store_objects.set(iter,
								   ObjectCol.NAME, config.filename,
								   ObjectCol.CONFIG, config,
								   ObjectCol.OBJECT, null);
			
			foreach(Hkl3D.Object object in config.objects){
				Gtk.TreeIter iter2;

				this.store_objects.append(out iter2, iter);
				this.store_objects.set(iter2,
									   ObjectCol.NAME, object.axis_name,
									   ObjectCol.HIDE, object.hide,
									   ObjectCol.CONFIG, config,
									   ObjectCol.OBJECT, object);
			}
		}
	}

	/************/
	/* Callback */
	/************/

	// void on_cell_treeview1_toggled(string path)
	// {
	// 	bool hide;
	// 	Hkl3D.Config? config;
	// 	Hkl3D.Object? object;
	// 	Gtk.TreeIter iter;

	// 	this.store_objects.get_iter_from_string(out iter, path);

	// 	this.store_objects.get(iter,
	// 						   ObjectCol.HIDE, out hide,
	// 						   ObjectCol.OBJECT, out object);

	// 	if(object != null){
	// 		this.hkl3d.hide_object(object, hide);
	// 		this.store_objects.set(iter,
	// 							   ObjectCol.HIDE, hide);
	// 		this.is_colliding();
	// 		this.invalidate();
	// 	}else{
	// 		this.store_objects.get(iter,
	// 							   ObjectCol.CONFIG, out config);
	// 		if(config != null){
	// 			Gtk.TreeIter iter2;

	// 			this.store_objects.set(iter,
	// 								   ObjectCol.HIDE, hide);
	// 			/* set all the children rows */
	// 			/* check thaht it works */
	// 			this.store_objects.iter_children(out iter2, iter);
	// 			while(this.store_objects.iter_next(ref iter2)){
	// 				this.store_objects.get(iter2,
	// 									   ObjectCol.OBJECT, out object);
	// 				this.store_objects.set(iter2,
	// 									   ObjectCol.HIDE, hide);
	// 				this.hkl3d.hide_object(object, hide);
	// 			}
	// 			//_hkl3d->save_config(_hkl3d->filename);
	// 			this.is_colliding();
	// 			this.invalidate();
	// 		}
	// 	}
	// }

	void on_treeview1_cursor_changed()
	{
		Hkl3D.Object? object;
		Gtk.TreePath path;
		Gtk.TreeViewColumn focus_column;
		Gtk.TreeIter iter;

		this._treeview1.get_cursor(out path, out focus_column);
		this.store_objects.get_iter(out iter, path);

		/* need to unselect of objects of all 3d models */
		foreach(Hkl3D.Config config in this.hkl3d.configs.configs){
			foreach(Hkl3D.Object object in config.objects){
				object.selected = false;
			}
		}

		/* now select the right object */
		this.store_objects.get(iter,
							   ObjectCol.OBJECT, out object);
		if(object != null)
			object.selected = true;

		this.invalidate();
	}

	void on_toolbutton1_clicked()
	{
		this._filechooserdialog1.show();
	}

	/* remove a object from the model */
	void on_toolbutton2_clicked()
	{
		Hkl3D.Object? object;
		Gtk.TreePath path;
		Gtk.TreeViewColumn focus_column;
		Gtk.TreeIter iter;

		this._treeview1.get_cursor(out path, out focus_column);
		this.store_objects.get_iter(out iter, path);
		this.store_objects.get(iter,
							   ObjectCol.OBJECT, out object);
		if(object != null){
			this.hkl3d.remove_object(object);
			this.update_hkl3d_objects_TreeStore();
			this.invalidate();
		}
	}

	void on_button1_clicked()
	{
		string directory;

		var filenames = this._filechooserdialog1.get_filenames();
		directory = _filechooserdialog1.get_current_folder();
		foreach(var filename in filenames){
			this.hkl3d.add_model_from_file(filename, directory);
			this.hkl3d.connect_all_axes();
		}

		this.update_hkl3d_objects_TreeStore();
		//_hkl3d->save_config(_hkl3d->filename);
		this._filechooserdialog1.hide();
	}

	void on_button2_clicked()
	{
		this._filechooserdialog1.hide();
	}
}
