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
 * Copyright (C) 2003-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *	    Oussama Sboui <oussama.sboui@synchrotron-soleil.fr>
 */
#include <gtk/gtkgl.h>
#include <GL/gl.h>
#include <g3d/quat.h>

#include "hkl3d.h"
#include "hkl-gui.h"
#include "hkl-gui-macros.h"
#include "hkl-gui-3d.h"
#include "hkl-gui-3d-gl.h"

/*
 * updates glarea widget (redraw)
 */
static void glarea_update(GtkWidget *glarea)
{
	gtk_widget_queue_draw_area(glarea,
				   0, 0,
				   glarea->allocation.width,
				   glarea->allocation.height);
}


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

	GtkFrame *frame1;
	GtkVBox *vbox1;
	GtkTreeView *treeview1;
	GtkToolButton *toolbutton1;
	GtkToolButton *toolbutton2;
	GtkToolButton *toolbutton3;
	GtkFileChooserDialog *filechooserdialog1;
	GtkButton *button1;
	GtkButton *button2;
	GtkTreeStore *treestore1;
	GtkDrawingArea *drawingarea1;

	Hkl3D *hkl3d;

	/* opengl connected to the drawingarea1 */
	G3DGLRenderOptions renderoptions;
	struct {
		gint32 beginx;
		gint32 beginy;
	} mouse;
	gboolean aabb;
};

G_DEFINE_TYPE (HklGui3D, hkl_gui_3d, G_TYPE_OBJECT);

#define HKL_GUI_3D_GET_PRIVATE(o) (G_TYPE_INSTANCE_GET_PRIVATE ((o), HKL_GUI_TYPE_3D, HklGui3DPrivate))


static void hkl_gui_3d_update_hkl3d_objects_TreeStore(HklGui3D *self)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);
	size_t i;
	size_t j;

	gtk_tree_store_clear(priv->treestore1);

	for(i=0; i<priv->hkl3d->config->len; ++i){
		GtkTreeIter iter = {0};

		gtk_tree_store_append(priv->treestore1, &iter, NULL);
		gtk_tree_store_set(priv->treestore1, &iter,
				   HKL_GUI_3D_COL_NAME, priv->hkl3d->config->models[i]->filename,
				   HKL_GUI_3D_COL_MODEL, priv->hkl3d->config->models[i],
				   HKL_GUI_3D_COL_OBJECT, NULL,
				   -1);

		for(j=0; j<priv->hkl3d->config->models[i]->len; ++j){
			GtkTreeIter citer = {0};

			gtk_tree_store_append(priv->treestore1, &citer, &iter);
			gtk_tree_store_set(priv->treestore1, &citer,
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
			/* priv->scene = hkl_gui_3d_scene_new(priv->hkl3d, FALSE, FALSE, FALSE, FALSE); */

			hkl_gui_3d_update_hkl3d_objects_TreeStore(self);
			/* gtk_box_pack_start(GTK_BOX(priv->vbox1), */
			/* 		   GTK_WIDGET(priv->scene), */
			/* 		   TRUE, TRUE, 0); */
			/* gtk_widget_show_all(GTK_WIDGET(priv->vbox1)); */
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


	g_free(priv->filename);

	g_object_unref(priv->builder);

	hkl3d_free(priv->hkl3d);

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

	return priv->frame1;
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

	priv->renderoptions.updated = TRUE;
	glarea_update(GTK_WIDGET(priv->drawingarea1));
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


	gtk_tree_model_get_iter_from_string (GTK_TREE_MODEL(priv->treestore1),
					     &iter, path);
	gtk_tree_model_get (GTK_TREE_MODEL(priv->treestore1),
			    &iter,
			    HKL_GUI_3D_COL_OBJECT, &object,
			    -1);

	hide = !gtk_cell_renderer_toggle_get_active(renderer);

	if(object){
		hkl3d_hide_object(priv->hkl3d, object, hide);
		gtk_tree_store_set (priv->treestore1,
				    &iter,
				    HKL_GUI_3D_COL_HIDE, hide,
				    -1);
		hkl_gui_3d_is_colliding(self);
		hkl_gui_3d_invalidate(self);
	}else{
		Hkl3DModel *model;

		gtk_tree_model_get (GTK_TREE_MODEL(priv->treestore1),
				    &iter,
				    HKL_GUI_3D_COL_MODEL, &model,
				    -1);
		if(model){
			GtkTreeIter children = {0};
			gboolean valid;
			size_t i = 0;

			gtk_tree_store_set (priv->treestore1,
					    &iter,
					    HKL_GUI_3D_COL_HIDE, hide,
					    -1);

			/* set all the children rows */
			valid = gtk_tree_model_iter_children(GTK_TREE_MODEL(priv->treestore1),
							     &children, &iter);
			while(valid){
				hkl3d_hide_object(priv->hkl3d, model->objects[i++], hide);
				gtk_tree_store_set (priv->treestore1,
						    &children,
						    HKL_GUI_3D_COL_HIDE, hide,
						    -1);

				valid = gtk_tree_model_iter_next(GTK_TREE_MODEL(priv->treestore1), &children);
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

	gtk_tree_view_get_cursor(priv->treeview1, &path, &column);
	gtk_tree_model_get_iter(GTK_TREE_MODEL(priv->treestore1), &iter, path);
	gtk_tree_path_free(path);

	/* need to unselect of objects of all 3d models */
	for(i=0; i<priv->hkl3d->config->len; ++i)
		for(j=0; j<priv->hkl3d->config->models[i]->len; ++j)
			priv->hkl3d->config->models[i]->objects[j]->selected = FALSE;

	/* now select the right object */
	gtk_tree_model_get (GTK_TREE_MODEL(priv->treestore1),
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

	gtk_widget_show(GTK_WIDGET(priv->filechooserdialog1));
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

	gtk_tree_view_get_cursor(priv->treeview1, &path, &column);
	gtk_tree_model_get_iter(GTK_TREE_MODEL(priv->treestore1), &iter, path);
	gtk_tree_path_free(path);

	gtk_tree_model_get (GTK_TREE_MODEL(priv->treestore1),
			    &iter,
			    HKL_GUI_3D_COL_OBJECT, &object,
			    -1);
	if(object){
		hkl3d_remove_object(priv->hkl3d, object);
		hkl_gui_3d_update_hkl3d_objects_TreeStore(self);
		hkl_gui_3d_invalidate(self);
	}
}

static void
reset_3d(G3DGLRenderOptions *renderoptions)
{
	/* renderoptions */
	renderoptions->updated = TRUE;
	renderoptions->initialized = FALSE;
        renderoptions->zoom = 7;
        renderoptions->bgcolor[0] = 0.9;
        renderoptions->bgcolor[1] = 0.8;
        renderoptions->bgcolor[2] = 0.6;
        renderoptions->bgcolor[3] = 1.0;
	renderoptions->glflags =
//		G3D_FLAG_GL_ISOMETRIC |
		G3D_FLAG_GL_SPECULAR |
		G3D_FLAG_GL_SHININESS |
		G3D_FLAG_GL_TEXTURES |
		G3D_FLAG_GL_COLORS|
		G3D_FLAG_GL_COORD_AXES;

        g3d_quat_trackball(renderoptions->quat, 0.0, 0.0, 0.0, 0.0, 0.8);

	/* rotate a little bit */
	gfloat q1[4], q2[4];
	gfloat a1[3] = { 0.0, 1.0, 0.0 }, a2[3] = {1.0, 0.0, 1.0};

	g3d_quat_rotate(q1, a1, - 45.0 * G_PI / 180.0);
	g3d_quat_rotate(q2, a2, - 45.0 * G_PI / 180.0);
	g3d_quat_add(renderoptions->quat, q1, q2);
}

/* re-initialize the 3d view */
void hkl_gui_3d_toolbutton3_clicked_cb(GtkToolButton *toolbutton,
				       gpointer user_data)
{
	HklGui3D *self = user_data;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	reset_3d(&priv->renderoptions);
	hkl_gui_3d_invalidate(self);
}

void hkl_gui_3d_toolbutton4_toggled_cb(GtkToggleToolButton *toggle_tool_button,
				       gpointer user_data)
{
	HklGui3D *self = user_data;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	priv->aabb = gtk_toggle_tool_button_get_active(toggle_tool_button);
	hkl_gui_3d_invalidate(self);
}

void hkl_gui_3d_button1_clicked_cb(GtkButton *button,
				   gpointer user_data)
{
	HklGui3D *self = user_data;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	size_t i;
	GSList *filenames;
	GSList *filename;

	filenames = gtk_file_chooser_get_files(GTK_FILE_CHOOSER(priv->filechooserdialog1));
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
	gtk_widget_hide(GTK_WIDGET(priv->filechooserdialog1));
	g_slist_free(filenames);
}

void hkl_gui_3d_button2_clicked_cb(GtkButton *button,
				   gpointer user_data)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	gtk_widget_hide(GTK_WIDGET(priv->filechooserdialog1));
}

/***************/
/* OpenGL part */
/***************/

enum DisplayList {
	MODEL = 1,
	BULLET,
	COLLISION,
	AABBBOX,
	HIGHLIGHT
};

static void hkl_gui_3d_draw_g3dmodel(HklGui3D *self)
{
	int i;
	int j;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	/* set the alpha canal to 0.5 if there is a collision */
	for(i=0; i<priv->hkl3d->config->len; i++)
		for(j=0; j<priv->hkl3d->config->models[i]->len; j++){
			GSList *faces;
			G3DFace *face;
			G3DMaterial *material;
			double alpha;

			if(priv->hkl3d->config->models[i]->objects[j]->is_colliding)
				alpha = 0.5;
			else
				alpha = 1;

			faces = priv->hkl3d->config->models[i]->objects[j]->g3d->faces;
			while(faces){
				face = (G3DFace *)(faces->data);
				face->material->a = alpha;
				faces = g_slist_next(faces);
			}
		}

	/* draw the G3DObjects */
	gl_draw(&priv->renderoptions, priv->hkl3d->model);
}

static void draw_g3dObject(G3DObject *object)
{
	GSList *faces;
	G3DFace *face;
	float *vertex;

	faces = object->faces;
	vertex = object->vertex_data;

	glPushMatrix();

	/* apply the transformation of the object */
	if(object->transformation)
		glMultMatrixf(object->transformation->matrix);

	/* draw all faces with the current stencil */
	while(faces){
		G3DFace * face;

		face = (G3DFace*)faces->data;
		glBegin(GL_TRIANGLES);
		glVertex3d(vertex[3*(face->vertex_indices[0])],
			   vertex[3*(face->vertex_indices[0])+1],
			   vertex[3*(face->vertex_indices[0])+2]);
		glVertex3d(vertex[3*(face->vertex_indices[1])],
			   vertex[3*(face->vertex_indices[1])+1],
			   vertex[3*(face->vertex_indices[1])+2]);
		glVertex3d(vertex[3*(face->vertex_indices[2])],
			   vertex[3*(face->vertex_indices[2])+1],
			   vertex[3*(face->vertex_indices[2])+2]);
		glEnd();
		faces = g_slist_next(faces);
	}

	glPopMatrix();
}

void hkl_gui_3d_draw_selected(HklGui3D *self)
{
	int i;
	int j;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	/* glDisable(GL_LIGHTING); */

	for(i=0; i<priv->hkl3d->config->len; i++)
		for(j=0; j<priv->hkl3d->config->models[i]->len; j++){
			if(priv->hkl3d->config->models[i]->objects[j]->selected
			   && !priv->hkl3d->config->models[i]->objects[j]->hide){
				// Push the GL attribute bits so that we don't wreck any settings
				glPushAttrib( GL_ALL_ATTRIB_BITS );

				// Enable polygon offsets, and offset filled polygons forward by 2.5
				glEnable( GL_POLYGON_OFFSET_FILL );
				glPolygonOffset( -2.5, -2.5);

				// Set the render mode to be line rendering with a thick line width
				glPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
				glLineWidth( 3.f );
				// Set the colour to be pink
				glColor3f( 1.f, .0f, 1.f );
				// Render the object
				draw_g3dObject(priv->hkl3d->config->models[i]->objects[j]->g3d);
				// Set the polygon mode to be filled triangles
				glLineWidth( 1.f );
				glPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
				// Set the colour to the background
				glCullFace(GL_FRONT);
				glColor3f( 0.0f, 0.0f, 0.0f );
				// Render the object
				draw_g3dObject(priv->hkl3d->config->models[i]->objects[j]->g3d);

				// Pop the state changes off the attribute
				// to set things back how they were
				glPopAttrib();
			}
		}
	/* glEnable(GL_LIGHTING); */
}

static void draw_sphere(float radius, int lats, int longs)
{
	int i, j;
	for(i=0;i<=lats;i++){
		float lat0 = M_PI * (-0.5 + (float) (i - 1) / lats);
		float z0  = radius * sin(lat0);
		float zr0 =  radius * cos(lat0);

		float lat1 = M_PI * (-0.5 + (float) i / lats);
		float z1 = radius * sin(lat1);
		float zr1 = radius * cos(lat1);

		glBegin(GL_QUAD_STRIP);
		for(j=0;j<=longs;j++) {
			float lng = 2 * M_PI * (float) (j - 1) / longs;
			float x = cos(lng);
			float y = sin(lng);

			glNormal3f(x * zr1, y * zr1, z1);
			glVertex3f(x * zr1, y * zr1, z1);
			glNormal3f(x * zr0, y * zr0, z0);
			glVertex3f(x * zr0, y * zr0, z0);
		}
		glEnd();
	}
}

void hkl_gui_3d_draw_collisions(HklGui3D *self)
{
	int i;
	int numManifolds;
	gboolean isColliding;
	float m[16];
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	/* glDisable(GL_LIGHTING); */
	///one way to draw all the contact points is iterating over contact manifolds / points:
	numManifolds = hkl3d_get_nb_manifolds(priv->hkl3d);
	for (i=0; i<numManifolds; i++){
		int numContacts;
		int j;

		// now draw the manifolds / points
		numContacts = hkl3d_get_nb_contacts(priv->hkl3d, i);
		for (j=0; j<numContacts; j++){
			double xa, ya, za;
			double xb, yb, zb;

			hkl3d_get_collision_coordinates(priv->hkl3d, i, j,
							&xa, &ya, &za, &xb, &yb, &zb);

			glDisable(GL_DEPTH_TEST);
			glBegin(GL_LINES);
			glColor4f(0, 0, 0, 1);
			glVertex3d(xa, ya, za);
			glVertex3d(xb, yb, zb);
			glEnd();
			glColor4f(1, 0, 0, 1);
			glPushMatrix();
			glTranslatef (xb, yb, zb);
			glScaled(0.05,0.05,0.05);

			draw_sphere(1, 10, 10);

			glPopMatrix();
			glColor4f(1, 1, 0, 1);
			glPushMatrix();
			glTranslatef (xa, ya, za);
			glScaled(0.05,0.05,0.05);

			draw_sphere(1, 10, 10);

			glPopMatrix();
			glEnable(GL_DEPTH_TEST);
		}
	}
	glFlush();
}

static void draw_line(const float from[3], const float to[3],
		      const float fromColor[3], const float toColor[3])
{
	glBegin(GL_LINES);
	glColor3f(fromColor[0], fromColor[1], fromColor[2]);
	glVertex3d(from[0], from[1], from[2]);
	glColor3f(toColor[0], toColor[1], toColor[2]);
	glVertex3d(to[0], to[1], to[2]);
	glEnd();
}

static void draw_aabb(const float from[3], const float to[3], const float color[3])
{
	float halfExtents[3] = {
		(to[0] - from[0]) * .5,
		(to[1] - from[1]) * .5,
		(to[2] - from[2]) * .5
	};
	float center[3] = {
		(to[0] + from[0]) * .5,
		(to[1] + from[1]) * .5,
		(to[2] + from[2]) * .5
	};
	int i, j;

	float edgecoord[3] = {1., 1., 1.};

	for (i=0;i<4;i++){
		for (j=0;j<3;j++){
			float pa[3] = {
				edgecoord[0] * halfExtents[0] + center[0],
				edgecoord[1] * halfExtents[1] + center[1],
				edgecoord[2] * halfExtents[2] + center[2]
			};

			int othercoord = j % 3;
			edgecoord[othercoord] *= -1.f;

			float pb[3] = {
				edgecoord[0] * halfExtents[0] + center[0],
				edgecoord[1] * halfExtents[1] + center[1],
				edgecoord[2] * halfExtents[2] + center[2]
			};
			draw_line(pa, pb, color, color);
		}
		edgecoord[0] = -1;
		edgecoord[1] = -1;
		edgecoord[2] = -1;
		if (i < 3)
			edgecoord[i] *= -1;
	}
}

static void
hkl_gui_3d_draw_aabb_object(const Hkl3DObject *self)
{
	GLfloat from[3];
	GLfloat to[3];
	GLfloat color[3] = {1, 0, 0};

	if(self->hide)
		return;

	hkl3d_object_aabb_get(self, from, to);
	draw_aabb(from, to, color);
}

void
hkl_gui_3d_draw_aabb(const HklGui3D *self)
{
	int i;
	int j;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self);

	for(i=0; i<priv->hkl3d->config->len; i++)
		for(j=0; j<priv->hkl3d->config->models[i]->len; j++)
			hkl_gui_3d_draw_aabb_object(priv->hkl3d->config->models[i]->objects[j]);
	glFlush();
}


/* void hkl_gui_3d_draw_bullet_object(const Hkl3DObject *self) */
/* { */
/* 	int i; */
/* 	int j; */
/* 	btScalar m[16]; */
/* 	btVector3 worldBoundsMin; */
/* 	btVector3 worldBoundsMax; */
/* 	btVector3 aabbMin,aabbMax; */

/* 	/\* get the bounding box from bullet *\/ */
/* 	hkl3d_get_bounding_boxes(_hkl3d, &worldBoundsMin, &worldBoundsMax); */

/* 	object = _hkl3d->config->models[i]->objects[j]; */
/* 	if(!object->hide){ */
/* 		btCollisionObject *btObject; */

/* 		btObject = object->btObject; */
/* 		btObject->getWorldTransform().getOpenGLMatrix( m ); */
/* 		m_shapeDrawer.drawOpenGL(m, */
/* 					 btObject->getCollisionShape(), */
/* 					 *object->color, */
/* 					 0, /\* debug mode *\/ */
/* 					 worldBoundsMin, */
/* 					 worldBoundsMax); */
/* 	} */
/* 	glFlush(); */
/* } */

/* void */
/* hkl_gui_3d_draw_bullet(const HklGui3D *self) */
/* { */
/* 	int i; */
/* 	int j; */
/* 	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(self); */

/* 	for(i=0; i<priv->hkl3d->config->len; i++) */
/* 		for(j=0; j<priv->hkl3d->config->models[i]->len; j++) */
/* 			hkl_gui_3d_draw_bullet_object(priv->hkl3d->config->models[i]->objects[j]); */
/* 	glFlush(); */
/* } */

gboolean
hkl_gui_3d_drawingarea1_expose_cb(GtkWidget *drawing_area, GdkEventExpose *event, gpointer user_data)
{
	GtkAllocation alloc;
	HklGui3D *self = HKL_GUI_3D(user_data);
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	GdkGLContext *gl_context = gtk_widget_get_gl_context(drawing_area);
	GdkGLDrawable *gl_drawable = gtk_widget_get_gl_drawable(drawing_area);

	/* Delimits the begining of the OpenGL execution. */
	if (!gdk_gl_drawable_gl_begin(gl_drawable, gl_context))
		g_assert_not_reached();

	gtk_widget_get_allocation(drawing_area, &alloc);
	glViewport(0,0, alloc.width, alloc.height);
	priv->renderoptions.aspect = (gfloat)alloc.width / (gfloat)alloc.height;

	hkl_gui_3d_draw_g3dmodel(self);
	hkl_gui_3d_draw_selected(self);
	hkl_gui_3d_draw_collisions(self);
	if(priv->aabb)
		hkl_gui_3d_draw_aabb(self);
	/* hkl_gui_3d_draw_bullet(self); */

	/* swap buffer if we're using double-buffering */
	if (gdk_gl_drawable_is_double_buffered(gl_drawable))
		gdk_gl_drawable_swap_buffers(gl_drawable);
	else {
		glFlush();
	}

	/* Delimits the end of the OpenGL execution. */
	gdk_gl_drawable_gl_end(gl_drawable);

	return FALSE;
}

gboolean
hkl_gui_3d_drawingarea1_configure_event_cb(GtkWidget *drawing_area,
					   GdkEventConfigure *event,
					   gpointer user_data)
{
	GtkAllocation alloc;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	GdkGLContext *gl_context = gtk_widget_get_gl_context(drawing_area);
	GdkGLDrawable *gl_drawable = gtk_widget_get_gl_drawable(drawing_area);

	/* Delimits the begining of the OpenGL execution. */
	if (!gdk_gl_drawable_gl_begin(gl_drawable, gl_context))
		g_assert_not_reached();

	gtk_widget_get_allocation(drawing_area, &alloc);
	glViewport(0,0, alloc.width, alloc.height);
	priv->renderoptions.aspect = (gfloat)alloc.width / (gfloat)alloc.height;

	/* Delimits the end of the OpenGL execution. */
	gdk_gl_drawable_gl_end(gl_drawable);

	return FALSE;
}

gboolean
hkl_gui_3d_idle_cb(gpointer user_data)
{
	/* update control data/params in this function if needed */
	GtkWidget *drawing_area = GTK_WIDGET(user_data);

	glarea_update(drawing_area);

	return FALSE;
}

gboolean
hkl_gui_3d_drawingarea1_button_press_event_cb(GtkWidget *drawing_area,
					      GdkEventButton* event,
					      gpointer user_data)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	/* left mouse buttom: rotate object */
	if(event->button == 1)
	{
		priv->mouse.beginx = event->x;
		priv->mouse.beginy = event->y;
		return TRUE;
	}

	// don't block
	return FALSE;
}

gboolean
hkl_gui_3d_drawingarea1_scroll_event_cb(GtkWidget *drawing_area,
					GdkEventScroll *event,
					gpointer user_data)
{
	GtkAllocation alloc;
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);

	gtk_widget_get_allocation(drawing_area, &alloc);

#define ZOOM_BY 10
	if(event->direction == GDK_SCROLL_DOWN)
		priv->renderoptions.zoom += ZOOM_BY;
	else
		priv->renderoptions.zoom -= ZOOM_BY;
#undef ZOOM_BY

	if(priv->renderoptions.zoom < 1)
		priv->renderoptions.zoom = 1;
	if(priv->renderoptions.zoom > 120)
		priv->renderoptions.zoom = 120;

	glarea_update(drawing_area);

	return FALSE;
}

gboolean
hkl_gui_3d_drawingarea1_motion_notify_event_cb(GtkWidget *drawing_area,
					       GdkEventMotion* event,
					       gpointer user_data)
{
	HklGui3DPrivate *priv = HKL_GUI_3D_GET_PRIVATE(user_data);
	GtkAllocation alloc;
	gint x, y;
	GdkModifierType state;
	G3DFloat rx, ry, rz;

	gtk_widget_get_allocation(drawing_area, &alloc);

	if(event->is_hint)
		gdk_window_get_pointer(event->window, &x, &y, &state);
	else
	{
		x = event->x;
		y = event->y;
		state = event->state;
	}

	/* left button pressed */
	if(state & GDK_BUTTON1_MASK)
	{
		if(state & GDK_SHIFT_MASK)
		{
			/* shift pressed, translate view */
			priv->renderoptions.offx += (float)(x - priv->mouse.beginx) / alloc.width / priv->renderoptions.zoom;
			priv->renderoptions.offy -= (float)(y - priv->mouse.beginy) / alloc.height / priv->renderoptions.zoom;
		}
		else
		{
			/* rotate view */
			gfloat spin_quat[4];
			g3d_quat_trackball(spin_quat,
				(2.0 * priv->mouse.beginx - alloc.width) / alloc.width,
				(alloc.height - 2.0 * priv->mouse.beginy) / alloc.height,
				(2.0 * x - alloc.width) / alloc.width,
				(alloc.height - 2.0 * y) / alloc.height,
				0.8 /* trackball radius */);
			g3d_quat_add(priv->renderoptions.quat,
				spin_quat, priv->renderoptions.quat);
			/* normalize quat some times */
			priv->renderoptions.norm_count ++;
			if(priv->renderoptions.norm_count > 97) {
				priv->renderoptions.norm_count = 0;
				g3d_quat_normalize(priv->renderoptions.quat);
			}

			/* g3d_quat_to_rotation_xyz(priv->renderoptions.quat, */
			/* 	&rx, &ry, &rz); */
			/* text = g_strdup_printf("%-.2f°, %-.2f°, %-.2f°", */
			/* 	rx * 180.0 / G_PI, ry * 180.0 / G_PI, rz * 180.0 / G_PI); */
			/* gui_glade_status(priv, text); */
			/* g_free(text); */
		}

		glarea_update(GTK_WIDGET(priv->drawingarea1));
	}

	/* middle mouse button */
	if(state & GDK_BUTTON2_MASK)
	{
		priv->renderoptions.zoom +=
			((y - priv->mouse.beginy) / (gfloat)alloc.height) * 40;
		if(priv->renderoptions.zoom < 1)
			priv->renderoptions.zoom = 1;
		if(priv->renderoptions.zoom > 120)
			priv->renderoptions.zoom = 120;

		glarea_update(GTK_WIDGET(priv->drawingarea1));
	}
	priv->mouse.beginx = x;
	priv->mouse.beginy = y;

	return FALSE;
}

/*********************************************/
/* HklGui3D and HklGui3DClass initialization */
/*********************************************/

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
	G3DGLRenderOptions renderoptions = {0};

	/* properties */
	priv->filename = NULL;
	priv->geometry = NULL;

	priv->builder = builder = gtk_builder_new ();

	get_ui(builder, "3d.ui");

	// widgets
	get_object(builder, GTK_FRAME, priv, frame1);
	get_object(builder, GTK_VBOX, priv, vbox1);
	get_object(builder, GTK_TREE_VIEW, priv, treeview1);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton1);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton2);
	get_object(builder, GTK_TOOL_BUTTON, priv, toolbutton3);
	get_object(builder, GTK_FILE_CHOOSER_DIALOG, priv, filechooserdialog1);
	get_object(builder, GTK_BUTTON, priv, button1);
	get_object(builder, GTK_BUTTON, priv, button2);
	get_object(builder, GTK_TREE_STORE, priv, treestore1);
	get_object(builder, GTK_DRAWING_AREA, priv, drawingarea1);

	gtk_builder_connect_signals (builder, self);

	/* OPENGL */

	/* renderoptions */
	reset_3d(&priv->renderoptions);
	priv->aabb = FALSE;

	/* attache GL capability to drawingarea1 */
	GdkGLConfig *gl_config = gdk_gl_config_new_by_mode(GDK_GL_MODE_RGBA |
							   GDK_GL_MODE_DEPTH |
							   GDK_GL_MODE_DOUBLE);
	if (!gl_config)
		g_assert_not_reached();
	if (!gtk_widget_set_gl_capability(GTK_WIDGET(priv->drawingarea1), gl_config, NULL, TRUE,
					  GDK_GL_RGBA_TYPE))
		g_assert_not_reached();

	gtk_widget_set_can_focus(GTK_WIDGET(priv->drawingarea1), TRUE);
	gtk_widget_add_events(GTK_WIDGET(priv->drawingarea1),
			      GDK_BUTTON1_MOTION_MASK |
			      GDK_BUTTON2_MOTION_MASK |
			      GDK_BUTTON_PRESS_MASK |
			      GDK_VISIBILITY_NOTIFY_MASK);

	/* connect the GL callbacks */
	const gdouble TIMEOUT_PERIOD = 1000 / 60;
	g_timeout_add(TIMEOUT_PERIOD, hkl_gui_3d_idle_cb, priv->drawingarea1);
	g_signal_connect(priv->drawingarea1, "expose-event",
			 G_CALLBACK(hkl_gui_3d_drawingarea1_expose_cb), self);
	g_signal_connect(priv->drawingarea1, "button-press-event",
			 G_CALLBACK(hkl_gui_3d_drawingarea1_button_press_event_cb), self);
	g_signal_connect(priv->drawingarea1, "motion-notify-event",
			 G_CALLBACK(hkl_gui_3d_drawingarea1_motion_notify_event_cb), self);
}
