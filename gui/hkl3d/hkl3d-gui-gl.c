/* $Id: gl.c 127 2009-09-05 19:45:12Z mmmaddd $ */

/*
	G3DViewer - 3D object viewer

	Copyright (C) 2005, 2006  Markus Dahms <mad@automagically.de>

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <glib.h>

#include <GL/gl.h>
#include <GL/glu.h>

#include <g3d/types.h>
#include <g3d/quat.h>
#include <g3d/matrix.h>
#include <g3d/vector.h>
#include <g3d/face.h>

#include "hkl3d-gui-gl.h"

struct _G3DGLRenderState {
	gint32 gl_dlist, gl_dlist_shadow;
	G3DMaterial *prev_material;
	guint32 prev_texid;
};

#if DEBUG > 0
#define TIMING
#endif

#ifdef TIMING
static GTimer *timer = NULL;
#endif

static void gl_init(void)
{
	GLenum error;

#if DEBUG > 1
	g_printerr("init OpenGL\n");
#endif


	GLfloat light0_pos[4] = { -50.0, 50.0, 0.0, 0.0 };
	GLfloat light0_col[4] = { 0.6, 0.6, 0.6, 1.0 };
	GLfloat light1_pos[4] = {  50.0, 50.0, 0.0, 0.0 };
	GLfloat light1_col[4] = { 0.4, 0.4, 0.4, 1.0 };
	GLfloat ambient_lc[4] = { 0.35, 0.35, 0.35, 1.0 };

	/* transparency and blending */
#if 0
	glAlphaFunc(GL_GREATER, 0.1);
#endif
	glEnable(GL_ALPHA_TEST);

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	glEnable(GL_BLEND);

	glDepthFunc(GL_LEQUAL);
	glDepthMask(GL_TRUE);
	glEnable(GL_DEPTH_TEST);


#if 0
	glEnable(GL_LINE_SMOOTH);
	glEnable(GL_POLYGON_SMOOTH);
#endif

#if 0
	glDisable(GL_DITHER);
#endif
	glShadeModel(GL_SMOOTH);

	glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);
	glHint(GL_LINE_SMOOTH_HINT, GL_NICEST);
	glHint(GL_POLYGON_SMOOTH_HINT, GL_NICEST);

	glLightModelfv(GL_LIGHT_MODEL_AMBIENT, ambient_lc);
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, 0);
#ifdef GL_LIGHT_MODEL_COLOR_CONTROL
	glLightModeli(GL_LIGHT_MODEL_COLOR_CONTROL, GL_SEPARATE_SPECULAR_COLOR);
#endif
	glLightModeli(GL_LIGHT_MODEL_LOCAL_VIEWER, 1);

	glLightfv(GL_LIGHT0, GL_POSITION, light0_pos);
	glLightfv(GL_LIGHT0, GL_DIFFUSE,  light0_col);
	glLightfv(GL_LIGHT1, GL_POSITION, light1_pos);
	glLightfv(GL_LIGHT1, GL_DIFFUSE,  light1_col);
	glLightfv(GL_LIGHT1, GL_SPECULAR,  light1_col);
	glEnable(GL_LIGHT0);
	glEnable(GL_LIGHT1);
	glEnable(GL_LIGHTING);


	/* colors and materials */
	glColorMaterial(GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE);
	glEnable(GL_COLOR_MATERIAL);


	/* texture stuff */
	glEnable(GL_TEXTURE_2D);


#ifdef TIMING
	timer = g_timer_new();
#endif
}

void gl_set_twoside(gboolean twoside)
{
	glLightModeli(GL_LIGHT_MODEL_TWO_SIDE, twoside ? 1 : 0);
	glColorMaterial(
		twoside ? GL_FRONT_AND_BACK : GL_FRONT,
		GL_AMBIENT_AND_DIFFUSE);
}

void gl_set_textures(gboolean textures)
{
	if(textures)
		glEnable(GL_TEXTURE_2D);
	else
	{
		glBindTexture(GL_TEXTURE_2D, 0);
		glDisable(GL_TEXTURE_2D);
	}
}

/* GHFunc */
void gl_load_texture(gpointer key, gpointer value, gpointer data)
{
	G3DImage *image = (G3DImage *)value;
	gint32 env;
	GLenum error;


#if 0
	/* predefined - update object->_tex_images else... */
	glGenTextures(1, &(image->tex_id));
#endif

#if DEBUG > 0
	g_print("gl: loading texture '%s' (%dx%dx%d) - id %d\n",
		image->name ? image->name : "(null)",
		image->width, image->height, image->depth,
		image->tex_id);
#endif

	glBindTexture(GL_TEXTURE_2D, image->tex_id);
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER,
		GL_LINEAR_MIPMAP_NEAREST);


	switch(image->tex_env)
	{
		case G3D_TEXENV_BLEND: env = GL_BLEND; break;
		case G3D_TEXENV_MODULATE: env = GL_MODULATE; break;
		case G3D_TEXENV_DECAL: env = GL_DECAL; break;
		case G3D_TEXENV_REPLACE: env = GL_REPLACE; break;
		default: env = GL_MODULATE; break;
	}
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, env);
	gluBuild2DMipmaps(
		GL_TEXTURE_2D,
		GL_RGBA,
		image->width,
		image->height,
		GL_RGBA,
		GL_UNSIGNED_BYTE,
		image->pixeldata);
}

static inline void gl_update_material(G3DGLRenderOptions *options,
	G3DMaterial *material)
{
	GLenum facetype;
	GLfloat normspec[4] = { 0.0, 0.0, 0.0, 1.0 };

	g_return_if_fail(material != NULL);

	if(options->glflags & G3D_FLAG_GL_ALLTWOSIDE)
		facetype = GL_FRONT_AND_BACK;
	else
		facetype = GL_FRONT;

	if(options->glflags & G3D_FLAG_GL_COLORS)
		glColor4f(
			material->r,
			material->g,
			material->b,
			material->a);
	else
		glColor4f(0.7, 0.7, 0.7, 1.0);

	return;

	if(options->glflags & G3D_FLAG_GL_SPECULAR)
		glMaterialfv(facetype, GL_SPECULAR, material->specular);
	else
		glMaterialfv(facetype, GL_SPECULAR, normspec);

	if(options->glflags & G3D_FLAG_GL_SHININESS)
		glMaterialf(facetype, GL_SHININESS, material->shininess * 10);
	else
		glMaterialf(facetype, GL_SHININESS, 0.0);
}

static inline void gl_draw_face(G3DGLRenderOptions *options,
	G3DObject *object, gint32 i, gfloat min_a, gfloat max_a,
	gboolean *dont_render, gboolean *init, gboolean is_shadow)
{
	gint32 j;

	if(*init)
	{
		options->state->prev_material = NULL;
		options->state->prev_texid = 0;
		*init = FALSE;
	}

	/* material check */
	if(!is_shadow && (options->state->prev_material != object->_materials[i]))
	{
		if((object->_materials[i]->a < min_a) ||
			(object->_materials[i]->a >= max_a))
		{
			*dont_render = TRUE;
			return;
		}

		*dont_render = FALSE;

		glEnd();
		gl_update_material(options, object->_materials[i]);
		glBegin(GL_TRIANGLES);
		options->state->prev_material = object->_materials[i];

		options->state->prev_texid = 0;
	}

	if(*dont_render) return;

	/* texture stuff */
	if(!is_shadow && (options->glflags & G3D_FLAG_GL_TEXTURES) &&
		(object->_flags[i] & G3D_FLAG_FAC_TEXMAP))
	{
		/* if texture has changed update to new texture */
		if(object->_tex_images[i] != options->state->prev_texid) {
			options->state->prev_texid = object->_tex_images[i];
			glEnd();
			glBindTexture(GL_TEXTURE_2D, options->state->prev_texid);
			glBegin(GL_TRIANGLES);
#if DEBUG > 5
			g_print("gl: binding to texture id %d\n", prev_texid);
#endif
		}
	}

	/* draw triangles */
	for(j = 0; j < 3; j ++)
	{
		if(!is_shadow && (options->glflags & G3D_FLAG_GL_TEXTURES) &&
			(object->_flags[i] & G3D_FLAG_FAC_TEXMAP))
		{
			glTexCoord2f(
				object->_tex_coords[(i * 3 + j) * 2 + 0],
				object->_tex_coords[(i * 3 + j) * 2 + 1]);
#if DEBUG > 5
			g_print("gl: setting texture coords: %f, %f\n",
				object->_tex_coords[(i * 3 + j) * 2 + 0],
				object->_tex_coords[(i * 3 + j) * 2 + 1]);
#endif
		}

		glNormal3f(
			object->_normals[(i*3+j)*3+0],
			object->_normals[(i*3+j)*3+1],
			object->_normals[(i*3+j)*3+2]);
		glVertex3f(
			object->vertex_data[object->_indices[i*3+j]*3+0],
			object->vertex_data[object->_indices[i*3+j]*3+1],
			object->vertex_data[object->_indices[i*3+j]*3+2]);

	} /* 1 .. 3 */
}

static inline void gl_may_end(gint32 ftype)
{
	if(ftype != -1)
		glEnd();
}

static inline void gl_may_begin(gint32 ftype)
{
	if(ftype != -1)
		glBegin(ftype);
}

static inline void gl_draw_face_list(G3DGLRenderOptions *options,
	G3DObject *object, gfloat min_a, gfloat max_a,
	gboolean *init, gboolean is_shadow)
{
	GSList *fitem;
	G3DFace *face;
	G3DVector nx, ny, nz;
	gint32 prev_ftype = -1;
	gint32 index, j, ftype;

	if(*init) {
		options->state->prev_material = NULL;
		options->state->prev_texid = 0;
		*init = FALSE;
	}

	for(fitem = object->faces; fitem != NULL; fitem = fitem->next) {
		face = fitem->data;
		if(!is_shadow && (options->state->prev_material != face->material)) {
			if((face->material->a < min_a) || (face->material->a >= max_a)) {
				return;
			}

			gl_may_end(prev_ftype);
			gl_update_material(options, face->material);
			gl_may_begin(prev_ftype);

			options->state->prev_material = face->material;

			options->state->prev_texid = 0;
		}

		/* texture stuff */
		if(!is_shadow && (options->glflags & G3D_FLAG_GL_TEXTURES) &&
			(face->flags & G3D_FLAG_FAC_TEXMAP)) {
			/* if texture has changed update to new texture */
			if(face->tex_image) {
				if(face->tex_image->tex_id != options->state->prev_texid) {
					options->state->prev_texid = face->tex_image->tex_id;

					gl_may_end(prev_ftype);
					glBindTexture(GL_TEXTURE_2D, options->state->prev_texid);
					gl_may_begin(prev_ftype);
#if DEBUG > 5
					g_print("gl: binding to texture id %d\n", prev_texid);
#endif
				}
			}
		} /* texture stuff */

		switch(face->vertex_count) {
			case 3: ftype = GL_TRIANGLES; break;
			case 4: ftype = GL_QUADS; break;
			case 2: ftype = GL_LINES; break;
			default: ftype = GL_POLYGON;
#if DEBUG > 0
				g_debug("face vertex count: %d", face->vertex_count);
#endif
				break;
		}
		if(ftype != prev_ftype) {
			gl_may_end(prev_ftype);
			glBegin(ftype);
			prev_ftype = ftype;
		}

		if(!(face->flags & G3D_FLAG_FAC_NORMALS)) {
			face->normals = g_new0(G3DVector, face->vertex_count * 3);

			g3d_face_get_normal(face, object, &nx, &ny, &nz);
			g3d_vector_unify(&nx, &ny, &nz);

			for(j = 0; j < face->vertex_count; j ++) {
				face->normals[j * 3 + 0] = nx;
				face->normals[j * 3 + 1] = ny;
				face->normals[j * 3 + 2] = nz;
			}
			face->flags |= G3D_FLAG_FAC_NORMALS;
		}

		for(j = 0; j < face->vertex_count; j ++) {
			index = face->vertex_indices[j];

			if(!is_shadow && (options->glflags & G3D_FLAG_GL_TEXTURES) &&
				(face->flags & G3D_FLAG_FAC_TEXMAP))
			{
				glTexCoord2f(
					face->tex_vertex_data[j * 2 + 0],
					face->tex_vertex_data[j * 2 + 1]);
			}

			glNormal3f(
				face->normals[j * 3 + 0],
				face->normals[j * 3 + 1],
				face->normals[j * 3 + 2]);

			glVertex3f(
				object->vertex_data[index * 3 + 0],
				object->vertex_data[index * 3 + 1],
				object->vertex_data[index * 3 + 2]);
		}

	} /* face loop */

	gl_may_end(prev_ftype);
}


static inline void gl_draw_objects(G3DGLRenderOptions *options,
	GSList *objects, gfloat min_a, gfloat max_a, gboolean is_shadow)
{
	GSList *olist;
	int i;
	G3DObject *object;
	gboolean dont_render;
	gboolean init = TRUE;

	olist = objects;
	while(olist != NULL)
	{
		object = (G3DObject *)olist->data;
		olist = olist->next;

		dont_render = FALSE;

		/* don't render invisible objects */
		if(object->hide) continue;

		g_return_if_fail(object != NULL);
#if DEBUG > 3
		g_printerr("name: %s {", object->name);
#endif

#if DEBUG > 2
		g_printerr("new object\n");
#endif

		glPushMatrix();

		if(object->transformation)
		{
			glMultMatrixf(object->transformation->matrix);
		}

#define EXPERIMENTAL
#ifdef EXPERIMENTAL
		gl_draw_face_list(options, object, min_a, max_a, &init, is_shadow);
#else
		glBegin(GL_TRIANGLES);

		for(i = 0; i < object->_num_faces; i ++)
		{
			gl_draw_face(options, object, i, min_a, max_a,
				&dont_render, &init, is_shadow);
		} /* all faces */

		glEnd();
#endif

		if(!is_shadow && (options->glflags & G3D_FLAG_GL_POINTS)) {
			glColor4f(0.2, 0.2, 0.2, 1.0);
			glBegin(GL_POINTS);
			for(i = 0; i < object->vertex_count; i ++) {
				glVertex3f(
					object->vertex_data[i * 3 + 0],
					object->vertex_data[i * 3 + 1],
					object->vertex_data[i * 3 + 2]);
			}
			glEnd();
		}

		/* handle sub-objects */
		gl_draw_objects(options, object->objects, min_a, max_a, is_shadow);

		glPopMatrix();

	} /* while olist != NULL */
}

static inline void matrix_g3d_to_gl(G3DMatrix *g3dm, GLfloat glm[4][4])
{
	guint32 i, j;

	for(i = 0; i < 4; i ++)
		for(j = 0; j < 4; j ++)
			glm[i][j] = g3dm[i * 4 + j];
}

static inline void gl_setup_view(G3DGLRenderOptions *options)
{
	GLfloat m[4][4];
	G3DMatrix *g3dm;
	G3DFloat w, h;

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	if(options->glflags & G3D_FLAG_GL_ISOMETRIC) {
		w = 0.5 * options->zoom;
		h = w / options->aspect;
		glOrtho(-w / 2.0, w / 2.0, -h / 2.0, h / 2.0, 1, 100);
	} else {
		gluPerspective(options->zoom, options->aspect, 1, 100);
	}
	/* translation of view */
	glTranslatef(options->offx, options->offy, 0.0);

	glMatrixMode(GL_MODELVIEW);

	glClearColor(
		options->bgcolor[0],
		options->bgcolor[1],
		options->bgcolor[2],
		options->bgcolor[3]);
	glClearDepth(1.0);
	glClearIndex(0.3);
	glClear(
		GL_COLOR_BUFFER_BIT |
		GL_DEPTH_BUFFER_BIT |
		GL_ACCUM_BUFFER_BIT |
		GL_STENCIL_BUFFER_BIT);

	glLoadIdentity();
	glTranslatef(0, 0, -30);
	g3dm = g3d_matrix_new();
	g3d_quat_to_matrix(options->quat, g3dm);
	matrix_g3d_to_gl(g3dm, m);

	g3d_matrix_free(g3dm);
	glMultMatrixf(&m[0][0]);
}

static void gl_setup_shadow_matrix(G3DGLRenderOptions *options,
	G3DVector *l, G3DVector *p, G3DVector *n)
{
	G3DDouble c, d;
	G3DMatrix *m = options->shadow_matrix;

	d = n[0] * l[0] + n[1] * l[1] + n[2] * l[2];
	c = p[0] * n[0] + p[1] * n[1] + p[2] * n[2] - d;

	m[0 * 4 + 0] = l[0] * n[0] + c;
	m[1 * 4 + 0] = l[0] * n[1];
	m[2 * 4 + 0] = l[0] * n[2];
	m[3 * 4 + 0] = - l[0] * c - l[0] * d;

	m[0 * 4 + 1] = l[1] * n[0];
	m[1 * 4 + 1] = l[1] * n[1] + c;
	m[2 * 4 + 1] = l[1] * n[2];
	m[3 * 4 + 1] = - l[1] * c - l[1] * d;

	m[0 * 4 + 2] = l[2] * n[0];
	m[1 * 4 + 2] = l[2] * n[1];
	m[2 * 4 + 2] = l[2] * n[2] + c;
	m[3 * 4 + 2] = - l[2] * c - l[2] * d;

	m[0 * 4 + 3] = n[0];
	m[1 * 4 + 3] = n[1];
	m[2 * 4 + 3] = n[2];
	m[3 * 4 + 3] = -d;
}

void gl_draw_coord_system(G3DGLRenderOptions *options)
{
	if(options->glflags & G3D_FLAG_GL_COORD_AXES) {
		/* x: red */
		glColor3f(1.0, 0.0, 0.0);
		glBegin(GL_LINES);
		glVertex3f(0.0, 0.0, 0.0);
		glVertex3f(10.0, 0.0, 0.0);
		glEnd();
		/* y: green */
		glColor3f(0.0, 1.0, 0.0);
		glBegin(GL_LINES);
		glVertex3f(0.0, 0.0, 0.0);
		glVertex3f(0.0, 10.0, 0.0);
		glEnd();
		/* z: blue */
		glColor3f(0.0, 0.0, 1.0);
		glBegin(GL_LINES);
		glVertex3f(0.0, 0.0, 0.0);
		glVertex3f(0.0, 0.0, 10.0);
		glEnd();
	}
}

static G3DFloat gl_min_y(GSList *objects)
{
	G3DFloat min_y = 10.0, tmp_y;
	GSList *oitem;
	G3DObject *object;
	gint32 i;

	for(oitem = objects; oitem != NULL; oitem = oitem->next) {
		object = oitem->data;
		for(i = 0; i < object->vertex_count; i ++)
			if(object->vertex_data[i * 3 + 1] < min_y)
				min_y = object->vertex_data[i * 3 + 1];
		tmp_y = gl_min_y(object->objects);
		if(tmp_y < min_y)
			min_y = tmp_y;
	}
	return min_y;
}

static void gl_draw_plane(G3DGLRenderOptions *options)
{
	glBegin(GL_QUADS);
	glNormal3f(0.0, -1.0, 0.0);
#define PLANE_MAX 12
	glVertex3f(-PLANE_MAX, options->min_y - 0.001,  PLANE_MAX);
	glVertex3f( PLANE_MAX, options->min_y - 0.001,  PLANE_MAX);
	glVertex3f( PLANE_MAX, options->min_y - 0.001, -PLANE_MAX);
	glVertex3f(-PLANE_MAX, options->min_y - 0.001, -PLANE_MAX);
#undef PLANE_MAX
	glEnd();
}

static void gl_setup_floor_stencil(G3DGLRenderOptions *options)
{
	glClear(GL_STENCIL_BUFFER_BIT);
	glDepthMask(GL_FALSE);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

	glEnable(GL_STENCIL_TEST);
	glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
	glStencilFunc(GL_ALWAYS, 1, 0xffffffff);

	gl_draw_plane(options);

	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	glDepthMask(GL_TRUE);

	glStencilFunc(GL_EQUAL, 1, 0xffffffff);
	glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
}

static void gl_setup_shadow_stencil(G3DGLRenderOptions *options)
{
	glClear(GL_STENCIL_BUFFER_BIT);
	glDepthMask(GL_FALSE);
	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);

	glEnable(GL_STENCIL_TEST);
	glStencilOp(GL_REPLACE, GL_REPLACE, GL_REPLACE);
	glStencilFunc(GL_ALWAYS, 1, 0xffffffff);

	glCallList(options->state->gl_dlist_shadow);

	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	glDepthMask(GL_TRUE);

	glStencilFunc(GL_EQUAL, 1, 0xffffffff);
	glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP);
}

void gl_draw(G3DGLRenderOptions *options, G3DModel *model)
{
	GLenum error;
	gfloat f;
#ifdef TIMING
	gboolean ignore_timing = FALSE;
	gulong msec, add;
	gdouble sec;
#endif
	G3DVector light[3] = { 100.0, 500.0, 20.0 };
	G3DVector plane[3] = { 0.0, -20.0, 0.0 };
	G3DVector normal[3] = { 0.0, -1.0, 0.0 };


	if(!options->initialized)
	{
		gl_init();
		options->initialized = TRUE;
#ifdef TIMING
		ignore_timing = TRUE;
#endif
	}

	/* prepare viewport */
	//gl_setup_view(options);

	/* reset texture */
	glBindTexture (GL_TEXTURE_2D, 0);

	if(model == NULL)
		return;

#ifdef TIMING
	g_timer_start(timer);
#endif

	if(options->updated) {
		options->updated = FALSE;
#ifdef TIMING
		ignore_timing = TRUE;
#endif
#if DEBUG > 2
		g_printerr("[gl] creating new display list\n");
#endif
		options->min_y = gl_min_y(model->objects);

		/* update render state */
		if(options->state) {
			glDeleteLists(options->state->gl_dlist, 1);
			glDeleteLists(options->state->gl_dlist_shadow, 1);
			g_free(options->state);
		}
		options->state = g_new0(G3DGLRenderState, 1);

		/* create and execute display list */
		options->state->gl_dlist = glGenLists(1);
		options->state->gl_dlist_shadow = glGenLists(1);

		glNewList(options->state->gl_dlist, GL_COMPILE);
		/* draw all objects */
		for(f = 1.0; f >= 0.0; f -= 0.2)
			gl_draw_objects(options, model->objects, f, f + 0.2, FALSE);
		glEndList();

		if(options->glflags & G3D_FLAG_GL_SHADOW) {
			glNewList(options->state->gl_dlist_shadow, GL_COMPILE);
			gl_draw_objects(options, model->objects, 0.0, 1.0, TRUE);
			glEndList();
		}

	}

	g_return_if_fail(options->state != NULL);

	gl_draw_coord_system(options);

	if(options->glflags & G3D_FLAG_GL_SHADOW) {
		plane[1] = options->min_y;

		/* reflection */
		glPushMatrix();
		gl_setup_floor_stencil(options);
		glTranslatef(0.0, (options->min_y * 2), 0.0);
		glScalef(1.0, -1.0, 1.0);
		glCallList(options->state->gl_dlist);
		glPopMatrix();

		/* plane */
		glDisable(GL_LIGHTING);
		glBindTexture (GL_TEXTURE_2D, 0);
		glColor4f(0.5, 0.5, 0.5, 0.7);
		gl_draw_plane(options);
		glEnable(GL_LIGHTING);
		/* shadow */
		glPushMatrix();
		gl_setup_shadow_matrix(options, light, plane, normal);
		glBindTexture (GL_TEXTURE_2D, 0);
		glDisable(GL_LIGHTING);
		glDisable(GL_DEPTH_TEST);
		glMultMatrixf(options->shadow_matrix);
		gl_setup_shadow_stencil(options);
		glPopMatrix();
		glPushMatrix();
		glTranslatef(0.0, 0.001, 0.0);
		glColor4f(0.3, 0.3, 0.3, 0.7);
		gl_draw_plane(options);
		glEnable(GL_DEPTH_TEST);
		glEnable(GL_LIGHTING);
		glPopMatrix();

		glDisable(GL_STENCIL_TEST);
	}

	/* execute display list */
	glCallList(options->state->gl_dlist);


/* get time to draw one frame to compare algorithms */
#ifdef TIMING
	g_timer_stop(timer);

	if(!ignore_timing) {
		if(options->avg_msec == 0) {
			sec = g_timer_elapsed(timer, &msec);
			options->avg_msec = (gulong)sec * 1000000 + msec;
		} else {
			sec = g_timer_elapsed(timer, &msec);
			add = (gulong)sec * 1000000 + msec;
			options->avg_msec = (options->avg_msec + add) / 2;
		}
	}
#endif

#if DEBUG > 3
	g_printerr("gl.c: drawn...\n");
#endif
}
