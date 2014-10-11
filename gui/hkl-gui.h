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
 */

#include <glib.h>
#include <glib-object.h>
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include "hkl.h"
#include "hkl-gui-pseudoaxes.h"

#define HKL_GUI_TYPE_WINDOW (hkl_gui_window_get_type ())
#define HKL_GUI_WINDOW(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), HKL_GUI_TYPE_WINDOW, HklGuiWindow))
#define HKL_GUI_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_CAST ((klass), HKL_GUI_TYPE_WINDOW, HklGuiWindowClass))
#define HKL_GUI_IS_WINDOW(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HKL_GUI_TYPE_WINDOW))
#define HKL_GUI_IS_WINDOW_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), HKL_GUI_TYPE_WINDOW))
#define HKL_GUI_WINDOW_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), HKL_GUI_TYPE_WINDOW, HklGuiWindowClass))

typedef struct _HklGuiWindow HklGuiWindow;
typedef struct _HklGuiWindowClass HklGuiWindowClass;
typedef struct _HklGuiWindowPrivate HklGuiWindowPrivate;

struct _HklGuiWindow {
	GObject parent_instance;
	HklGuiWindowPrivate * priv;
};

struct _HklGuiWindowClass {
	GObjectClass parent_class;
};

GType hkl_gui_window_get_type (void) G_GNUC_CONST;

HklGuiWindow* hkl_gui_window_new (void);




