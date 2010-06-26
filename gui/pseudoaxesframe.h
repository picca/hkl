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
#ifndef __PSEUDO_AXES_FRAME_H__
#define __PSEUDO_AXES_FRAME_H__

#include <gtk/gtk.h>
#include "hkl.h"

typedef struct _HklGuiPseudoAxesFrame HklGuiPseudoAxesFrame;

struct _HklGuiPseudoAxesFrame
{
	HklPseudoAxisEngine *engine;

	GtkBuilder *builder;
	GtkFrame *frame1;
	GtkLabel *label2;
	GtkComboBox *combobox1;
	GtkExpander *expander1;
	GtkTreeView *treeview1;
	GtkButton *button1;
	GtkButton *button2;

	// objects
	GtkListStore *store_mode;
	GtkListStore *store_pseudo;
	GtkListStore *store_mode_parameter;
};

HklGuiPseudoAxesFrame *hkl_gui_pseudo_axes_frame_new(HklPseudoAxisEngine *engine);

void hkl_gui_pseudo_axes_frame_free(HklGuiPseudoAxesFrame *self);

void hkl_gui_pseudo_axes_frame_update(HklGuiPseudoAxesFrame *self);

#endif // __PSEUDO_AXES_FRAME_H__
