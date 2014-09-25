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
#ifndef __HKL_GUI_3D_H__
#define __HKL_GUI_3D_H__

#include <gtk/gtk.h>

#include "hkl3d.h"

G_BEGIN_DECLS

/* HklGui3DScene forward declaration */

typedef struct _HklGui3DScene HklGui3DScene;

HklGui3DScene *hkl_gui_3d_scene_new(Hkl3D *hkl3d,
				    gboolean enableBulletDraw, gboolean enableWireframe,
				    gboolean enableAAbbBoxDraw, gboolean enableOrthoView);
void hkl_gui_3d_scene_free(HklGui3DScene *self);
void hkl_gui_3d_scene_invalidate(HklGui3DScene *self);
void hkl_gui_3d_scene_gl_init(int *argc, char *** argv);

/* HklGui3D */

#define HKL_GUI_TYPE_3D            (hkl_gui_3d_get_type ())
#define HKL_GUI_3D(obj)            (G_TYPE_CHECK_INSTANCE_CAST ((obj), HKL_GUI_TYPE_3D, HklGui3D))
#define HKL_GUI_3D_CLASS(klass)    (G_TYPE_CHECK_CLASS_CAST ((klass), HKL_GUI_TYPE_3D, HklGui3DClass))
#define HKL_GUI_IS_3D(obj)         (G_TYPE_CHECK_INSTANCE_TYPE ((obj), HKL_GUI_TYPE_3D))
#define HKL_GUI_IS_3D_CLASS(klass) (G_TYPE_CHECK_CLASS_TYPE ((klass), HKL_GUI_TYPE_3D))
#define HKL_GUI_3D_GET_CLASS(obj)  (G_TYPE_INSTANCE_GET_CLASS ((obj), HKL_GUI_TYPE_3D, HklGuiEngineClass))

typedef struct _HklGui3D HklGui3D;
typedef struct _HklGui3DClass HklGui3DClass;
typedef struct _HklGui3DPrivate HklGui3DPrivate;

GType hkl_gui_3d_get_type (void) G_GNUC_CONST;

HklGui3D *hkl_gui_3d_new (const char *filename, HklGeometry *geometry);

void hkl_gui_3d_is_colliding(HklGui3D *self);

void hkl_gui_3d_invalidate(HklGui3D *self);

GtkFrame *hkl_gui_3d_frame_get(HklGui3D *self);

/* class Hkl3DFrame */
/* { */
/* public: */
/* 	Hkl3DFrame(const char *filename, HklGeometry *geometry); */
/* 	virtual ~Hkl3DFrame(void); */

/* 	Gtk::Frame &frame(void) {return *_frame1;} */
/* 	void is_colliding(void); */
/* 	void invalidate(void); */

/* // callback */
/* protected: */
/* 	void on_cell_treeview1_toggled(Glib::ustring const & path); */
/* 	void on_treeview1_cursor_changed(void); */
/* 	void on_toolbutton1_clicked(void); */
/* 	void on_toolbutton2_clicked(void); */
/* 	void on_button1_clicked(void); */
/* 	void on_button2_clicked(void); */

/* // non callback */
/* protected: */
/* 	void update_hkl3d_objects_TreeStore(void); */

/* // members */
/* protected: */
/* 	Hkl3D *_hkl3d; */
/* 	Hkl3dGui::Scene *_Scene; */

/* 	/\* widgets *\/ */
/* 	Gtk::Frame *_frame1; */
/* 	Gtk::VBox *_vbox1; */
/* 	Gtk::TreeView *_treeview1; */
/* 	Gtk::ToolButton *_toolbutton1; */
/* 	Gtk::ToolButton *_toolbutton2; */
/* 	Gtk::FileChooserDialog *_filechooserdialog1; */
/* 	Gtk::Button *_button1; */
/* 	Gtk::Button *_button2; */

/* 	/\* objects *\/ */
/* 	Glib::RefPtr<Gtk::TreeStore> _treestore1; /\* use to fill the hkl3d objects properties *\/ */
/* 	Hkl3DObjectsModelColumns _hkl3d_objects_columns; */
/* 	Glib::RefPtr<Gtk::Builder> _refGlade; */
/* }; */

#endif // __HKL_GUI_3D_H__
