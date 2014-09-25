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
 * Copyright (C) 2003-2014 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *	    Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
 */
#include "hkl-gui.h"
#include "hkl-gui-3d.h"

typedef enum  {
	HKL_GUI_3D_COL_NAME = 0,
	HKL_GUI_3D_COL_HIDE,
	HKL_GUI_3D_COL_MODEL,
	HKL_GUI_3D_COL_OBJECT,
	HKL_GUI_3D_COL_NUM_COLS
} HklGui3DCol;

enum {
	PROP_0,

	PROP_FILENAME,
	PROP_GEOMETRY,

	N_PROPERTIES
};

/* Keep a pointer to the properties definition */
static GParamSpec *obj_properties[N_PROPERTIES] = { NULL, };

enum {
	CHANGED,

	N_SIGNALS
};

static guint signals[N_SIGNALS] = { 0 };


struct _HklGui3D {
	GObject parent_instance;

	/*< private >*/
	HklGui3DPrivate * priv;
};

struct _HklGui3DClass {
	GObjectClass parent_class;

};

struct _HklGui3DPrivate {
	/* Properties */
	char *filename;
	HklGeometry *geometry;
	/* Properties */

	GtkBuilder *builder;

	GtkFrame *_frame1;
	GtkVBox *_vbox1;
	GtkTreeView *_treeview1;
	GtkToolButton *_toolbutton1;
	GtkToolButton *_toolbutton2;
	GtkFileChooserDialog *_filechooserdialog1;
	GtkButton *_button1;
	GtkButton *_button2;
	GtkTreeStore *_treestore1;

	Hkl3D *hkl3d;
	HklGui3DScene *scene;
};

G_DEFINE_TYPE (HklGui3D, hkl_gui_3d, G_TYPE_OBJECT);

#define HKL_GUI_3D_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HKL_GUI_TYPE_3D, HklGui3DPrivate))


static void hkl_gui_3d_update_hkl3d_objects_TreeStore(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);
	size_t i;
	size_t j;

	gtk_tree_store_clear(priv->_treestore1);

	for(i=0; i<priv->hkl3d->config->len; ++i){
		GtkTreeIter iter = {0};

		gtk_tree_store_append(priv->_treestore1, &iter, NULL);
		gtk_tree_store_set(priv->_treestore1, &iter,
				   HKL_GUI_3D_COL_NAME, priv->hkl3d->config->models[i]->filename,
				   HKL_GUI_3D_COL_MODEL, priv->hkl3d->config->models[i],
				   HKL_GUI_3D_COL_OBJECT, NULL,
				   -1);
		
		for(j=0; j<priv->hkl3d->config->models[i]->len; ++j){
			GtkTreeIter citer = {0};

			gtk_tree_store_append(priv->_treestore1, &citer, &iter);
			gtk_tree_store_set(priv->_treestore1, &citer,
					   HKL_GUI_3D_COL_NAME, priv->hkl3d->config->models[i]->objects[j]->axis_name,
					   HKL_GUI_3D_COL_HIDE, priv->hkl3d->config->models[i]->objects[j]->hide,
					   HKL_GUI_3D_COL_MODEL, priv->hkl3d->config->models[i],
					   HKL_GUI_3D_COL_OBJECT, priv->hkl3d->config->models[i]->objects[j],
					   -1);
		}
	}
}

/* properties */

static const char *
hkl_gui_3d_get_filename(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	return priv->filename;
}

static HklGeometry *
hkl_gui_3d_get_geometry(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	return priv->geometry;
}

void _filename_and_geometry(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);
	if(priv->filename && priv->geometry){
		if (priv->hkl3d)
			hkl3d_free(priv->hkl3d);
		priv->hkl3d = hkl3d_new(priv->filename, priv->geometry);
		if(priv->hkl3d){
			priv->scene = hkl_gui_3d_scene_new(priv->hkl3d, FALSE, FALSE, FALSE, FALSE);

			hkl_gui_3d_update_hkl3d_objects_TreeStore(self);
			gtk_box_pack_start(GTK_BOX(priv->_vbox1),
					   GTK_WIDGET(priv->scene),
					   TRUE, TRUE, 0);
			gtk_widget_show_all(GTK_WIDGET(priv->_vbox1));
		}
	}
}

static void
hkl_gui_3d_set_filename(HklGui3D *self, const char *filename)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	if(priv->filename)
		g_free(priv->filename);
	priv->filename = g_strdup(filename);

	_filename_and_geometry(self);
}

static void
hkl_gui_3d_set_geometry(HklGui3D *self, HklGeometry *geometry)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	priv->geometry = geometry;
	_filename_and_geometry(self);
}

static void
set_property (GObject *object, guint prop_id,
	      const GValue *value, GParamSpec *pspec)
{
	HklGui3D *self = HKL_GUI_3D (object);
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	switch (prop_id) {
	case PROP_FILENAME:
		hkl_gui_3d_set_filename(self, g_value_get_string (value));
		break;
	case PROP_GEOMETRY:
		hkl_gui_3d_set_geometry(self, g_value_get_pointer (value));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
get_property (GObject *object, guint prop_id,
	      GValue *value, GParamSpec *pspec)
{
	HklGui3D *self = HKL_GUI_3D (object);
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	switch (prop_id)
	{
	case PROP_FILENAME:
		g_value_set_string (value, hkl_gui_3d_get_filename (self));
		break;
	case PROP_GEOMETRY:
		g_value_set_pointer (value, hkl_gui_3d_get_geometry (self));
		break;
	default:
		G_OBJECT_WARN_INVALID_PROPERTY_ID (object, prop_id, pspec);
		break;
	}
}

static void
finalize (GObject* object)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(object);

	g_object_unref(priv->builder);

	hkl3d_free(priv->hkl3d);
	hkl_gui_3d_scene_free(priv->scene);

	g_free(priv->filename);

	G_OBJECT_CLASS (hkl_gui_3d_parent_class)->finalize (object);
}

HklGui3D*
hkl_gui_3d_new (const char *filename, HklGeometry *geometry)
{
	return g_object_new (HKL_GUI_TYPE_3D,
			     "filename", filename,
			     "geometry", geometry,
			     NULL);
}

GtkFrame *hkl_gui_3d_frame_get(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	return priv->_frame1;
}

void hkl_gui_3d_is_colliding(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	if(priv->hkl3d)
		hkl3d_is_colliding(priv->hkl3d);
}

void hkl_gui_3d_invalidate(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	if(priv->scene)
		hkl_gui_3d_scene_invalidate(priv->scene);
}

/************/
/* Callback */
/************/

void hkl_gui_3d_cellrenderertext2_toggled_cb(GtkCellRendererToggle* renderer,
					     const gchar* path, gpointer user_data)
{
	HklGui3D *self = HKL_GUI_3D(user_data);
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	guint hide;
	Hkl3DObject *object;
	GtkTreeIter iter = {0};


	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->_treestore1),
					     &iter, path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_treestore1),
			    &iter,
			    HKL_GUI_3D_COL_OBJECT, &object,
			    -1);

	hide = !gtk_cell_renderer_toggle_get_active(renderer);

	if(object){
		hkl3d_hide_object(priv->hkl3d, object, hide);
		gtk_tree_store_set (priv->_treestore1,
				    &iter,
				    HKL_GUI_3D_COL_HIDE, hide,
				    -1);
		hkl_gui_3d_is_colliding(self);
		hkl_gui_3d_invalidate(self);
	}else{
		Hkl3DModel *model;

		gtk_tree_model_get (GTK_TREE_MODEL(priv->_treestore1),
				    &iter,
				    HKL_GUI_3D_COL_MODEL, &model,
				    -1);
		if(model){
			GtkTreeIter children = {0};
			gboolean valid;
			size_t i = 0;

			gtk_tree_store_set (priv->_treestore1,
					    &iter,
					    HKL_GUI_3D_COL_HIDE, hide,
					    -1);

			/* set all the children rows */
			valid = gtk_tree_model_iter_children(GTK_TREE_MODEL(priv->_treestore1),
							     &children, &iter);
			while(valid){
				hkl3d_hide_object(priv->hkl3d, model->objects[i++], hide);
				gtk_tree_store_set (priv->_treestore1,
						    &children,
						    HKL_GUI_3D_COL_HIDE, hide,
						    -1);
				
				valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(priv->_treestore1), &children);
			}
			hkl_gui_3d_is_colliding(self);
			hkl_gui_3d_invalidate(self);
		}
	}
}

void hkl_gui_3d_treeview1_cursor_changed_cb(GtkTreeView *tree_view,
					    gpointer user_data)
{
	int i;
	int j;
	HklGui3D *self = user_data;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	GtkTreeIter iter = {0};
	Hkl3DObject *object;
	GtkTreePath *path;
	GtkTreeViewColumn * column;

	gtk_tree_view_get_cursor(priv->_treeview1, &path, &column);
	gtk_tree_model_get_iter(GTK_TREE_MODEL(priv->_treestore1), &iter, path);
	gtk_tree_path_free(path);

	/* need to unselect of objects of all 3d models */
	for(i=0; i<priv->hkl3d->config->len; ++i)
		for(j=0; j<priv->hkl3d->config->models[i]->len; ++j)
			priv->hkl3d->config->models[i]->objects[j]->selected = FALSE;

	/* now select the right object */
	gtk_tree_model_get (GTK_TREE_MODEL(priv->_treestore1),
			    &iter,
			    HKL_GUI_3D_COL_OBJECT, &object,
			    -1);
	if(object)
		object->selected = TRUE;

	hkl_gui_3d_invalidate(self);
}

void hkl_gui_3d_toolbutton1_clicked_cb(GtkToolButton *toolbutton,
				       gpointer user_data)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	gtk_widget_show(GTK_WIDGET(priv->_filechooserdialog1));
}

/* remove an object from the model */
void hkl_gui_3d_toolbutton2_clicked_cb(GtkToolButton *toolbutton,
				       gpointer user_data)
{
	HklGui3D *self = user_data;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	GtkTreeIter iter = {0};
	GtkTreePath *path;
	GtkTreeViewColumn * column;
	Hkl3DObject *object;
	int i;
	int j;

	gtk_tree_view_get_cursor(priv->_treeview1, &path, &column);
	gtk_tree_model_get_iter(GTK_TREE_MODEL(priv->_treestore1), &iter, path);
	gtk_tree_path_free(path);

	gtk_tree_model_get (GTK_TREE_MODEL(priv->_treestore1),
			    &iter,
			    HKL_GUI_3D_COL_OBJECT, &object,
			    -1);
	if(object){
		hkl3d_remove_object(priv->hkl3d, object);
		hkl_gui_3d_update_hkl3d_objects_TreeStore(self);
		hkl_gui_3d_invalidate(self);
	}
}

void hkl_gui_3d_button1_clicked_cb(GtkButton *button,
				   gpointer user_data)
{
	HklGui3D *self = user_data;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	size_t i;
	GSList *filenames;
	GSList *filename;

	filenames = gtk_file_chooser_get_files(GTK_FILE_CHOOSER(priv->_filechooserdialog1));
	filename = filenames;
	while(filename){
		GFile *file = filename->data;
		GFile *directory = g_file_get_parent(file);
		char *basename = g_file_get_basename(file);
		char *path = g_file_get_path(directory);

		hkl3d_add_model_from_file(priv->hkl3d, basename, path);
		hkl3d_connect_all_axes(priv->hkl3d);

		g_free(path);
		g_free(basename);
		g_object_unref(G_OBJECT(directory));
		g_object_unref(G_OBJECT(file));
		filename = g_slist_next(filename);
	};

	hkl_gui_3d_update_hkl3d_objects_TreeStore(self);
	gtk_widget_hide(GTK_WIDGET(priv->_filechooserdialog1));
	g_slist_free(filenames);
}

void hkl_gui_3d_button2_clicked_cb(GtkButton *button,
				   gpointer user_data)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	gtk_widget_hide(GTK_WIDGET(priv->_filechooserdialog1));
}


static void
hkl_gui_3d_class_init (HklGui3DClass *class)
{
	GObjectClass *gobject_class = G_OBJECT_CLASS (class);

	g_type_class_add_private (class, sizeof (HklGui3DPrivate));

	/* virtual method */
	gobject_class->finalize = finalize;
	gobject_class->set_property = set_property;
	gobject_class->get_property = get_property;

	/* properties */
	obj_properties[PROP_FILENAME] =
		g_param_spec_string ("filename",
				     "Filename",
				     "The confuration filename",
				     NULL,
				     G_PARAM_CONSTRUCT_ONLY |
				     G_PARAM_READWRITE |
				     G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_GEOMETRY] =
		g_param_spec_pointer ("geometry",
				      "Geometry",
				      "The Hkl Geometry used underneath",
				      G_PARAM_CONSTRUCT_ONLY |
				      G_PARAM_READWRITE |
				      G_PARAM_STATIC_STRINGS);

	g_object_class_install_properties (gobject_class,
					   N_PROPERTIES,
					   obj_properties);

}

static void hkl_gui_3d_init (HklGui3D * self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);
	GtkBuilder *builder;

	/* properties */
	priv->filename = NULL;
	priv->geometry = NULL;

	priv->builder = builder = gtk_builder_new ();

	gtk_builder_add_from_file (builder, "3d.ui", NULL);

	// widgets
	get_object(builder, GTK_FRAME, priv, frame1);
	get_object(builder, GTK_VBOX, priv, vbox1);
	get_object(builder, GTK_TREE_VIEW, priv, treeview1);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton1);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton2);
	get_object(builder, GTK_FILE_CHOOSER_DIALOG, priv, filechooserdialog1);
	get_object(builder, GTK_BUTTON, priv, button1);
	get_object(builder, GTK_BUTTON, priv, button2);
	get_object(builder, GTK_TREE_STORE, priv, treestore1);

	gtk_builder_connect_signals (builder, self);
}
