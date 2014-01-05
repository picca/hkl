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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#ifndef __HKL_GUI_ENGINE_H__
#define __HKL_GUI_ENGINE_H__

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include <hkl.h>

G_BEGIN_DECLS

#define HKL_GUI_TYPE_ENGINE            (hkl_gui_engine_get_type ())
#define HKL_GUI_ENGINE(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), HKL_GUI_TYPE_ENGINE, HklGuiEngine))
#define HKL_GUI_ENGINE_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), HKL_GUI_TYPE_ENGINE, HklGuiEngineClass))
#define HKL_GUI_IS_ENGINE(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HKL_GUI_TYPE_ENGINE))
#define HKL_GUI_IS_ENGINE_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), HKL_GUI_TYPE_ENGINE))
#define HKL_GUI_ENGINE_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), HKL_GUI_TYPE_ENGINE, HklGuiEngineClass))

typedef struct _HklGuiEngine HklGuiEngine;
typedef struct _HklGuiEngineClass HklGuiEngineClass;
typedef struct _HklGuiEnginePrivate HklGuiEnginePrivate;

typedef enum  {
	MODE_COL_NAME = 0,
	MODE_COL_NUM_COLS
} ModeCol;

typedef enum  {
	PSEUDO_COL_NAME = 0,
	PSEUDO_COL_VALUE,
	PSEUDO_COL_PSEUDO,
	PSEUDO_COL_NUM_COLS
} PseudoCol;

struct _HklGuiEngine {
	GObject parent_instance;

	/*< private >*/
	HklGuiEnginePrivate * priv;
};

struct _HklGuiEngineClass {
	GObjectClass parent_class;
};

GType hkl_gui_engine_get_type (void) G_GNUC_CONST;

HklGuiEngine* hkl_gui_engine_new (HklEngine* engine);

void hkl_gui_engine_set_engine (HklGuiEngine *gui_engine,
				HklEngine *engine);

HklEngine* hkl_gui_engine_get_engine (HklGuiEngine *gui_engine);

GtkFrame *hkl_gui_engine_get_frame(HklGuiEngine *self);

void hkl_gui_engine_update (HklGuiEngine* self);

G_END_DECLS

#endif /* __HKL_GUI_ENGINE_H__ */


