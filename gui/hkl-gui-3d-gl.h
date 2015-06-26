/* $Id: gl.h 108 2009-01-08 16:38:36Z mmmaddd $ */

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

#ifndef _GL_H
#define _GL_H

#include <g3d/types.h>

#define G3D_FLAG_GL_SPECULAR        (1L << 0)
#define G3D_FLAG_GL_SHININESS       (1L << 1)
#define G3D_FLAG_GL_ALLTWOSIDE      (1L << 2)
#define G3D_FLAG_GL_TEXTURES        (1L << 3)
#define G3D_FLAG_GL_COLORS          (1L << 4)
#define G3D_FLAG_GL_POINTS          (1L << 5)
#define G3D_FLAG_GL_COORD_AXES      (1L << 6)
#define G3D_FLAG_GL_SHADOW          (1L << 7)
#define G3D_FLAG_GL_ISOMETRIC       (1L << 8)

typedef struct _G3DGLRenderState G3DGLRenderState;

typedef struct {
	/* to be set by caller */
	gfloat zoom;
	gfloat aspect;
	gfloat bgcolor[4];
	gfloat quat[4];
	G3DMatrix shadow_matrix[16];
	G3DFloat min_y;
	guint32 norm_count;
	gfloat offx;
	gfloat offy;
	gint32 glflags;
	gboolean updated;
	gboolean initialized;
	/* can be read by caller */
	guint32 avg_msec;
	/* to be used by caller only */
	G3DGLRenderState *state;
} G3DGLRenderOptions;

void gl_set_twoside(gboolean twoside);
void gl_set_textures(gboolean textures);
void gl_load_texture(gpointer key, gpointer value, gpointer data);
void gl_draw(G3DGLRenderOptions *options, G3DModel *model);
#endif
