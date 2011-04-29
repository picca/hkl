/* This file is part of the hkl3d library.
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
 * Copyright (C) 2010      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 *          Oussama SBOUI <oussama.sboui@synchrotron-soleil.fr>
 */

namespace Hkl3D {

	[CCode (cheader_filename="hkl3d.h", destroy_function="", ref_function="", unref_function="")]
	public class Object
	{
		public string axis_name;
		public bool hide;
		public bool selected;
		public bool is_colliding;
		public G3D.Object g3dObject;
	}

	[CCode (cheader_filename="hkl3d.h", destroy_function="", ref_function="", unref_function="")]
	public class Config
	{
		public unowned string filename;
		[CCode (array_length_cname="len")]
		public Hkl3D.Object[] objects;
	}

	[Compact]
	[CCode (cheader_filename="hkl3d.h", destroy_function="")]
	public class Configs
	{
		[CCode (array_length_cname="len")]
		public Config[] configs;
	}

	[CCode (cname="Hkl3D", cprefix="hkl3d_", cheader_filename="hkl3d.h", destroy_function=false, ref_function="", unref_function="")]
	public class Anticollision
	{
		public Configs configs;
		public string filename;

		[CCode (cname="hkl3d_new")]
		public Anticollision(string filename, Hkl.Geometry geometry);

		public Config? add_model_from_file(string filename, string directory);
		public void connect_all_axes();
		public void hide_object(Object object, bool hide);
		public void remove_object(Object object);
		public int is_colliding();
		public void save_config(string filename);
		public int get_nb_manifolds();
		public int get_nb_contacts(int i);
		public void get_collision_coordinates(int manifold, int contact,
											  out double xa, out double ya, out double za,
											  out double xb, out double yb, out double zb);
	}
}
// 		char const *filename; /* config filename */
// 		HklGeometry *geometry; /* do not own this object */
// 		G3DModel *model;
// 		struct Hkl3DStats stats;
// 		struct Hkl3DConfigs *configs;
// 		struct Hkl3DGeometry *movingObjects;

// 		size_t _len;
// 		G3DContext *_context;
// 		struct btCollisionConfiguration *_btCollisionConfiguration;
// 		struct btBroadphaseInterface *_btBroadphase;
// 		struct btCollisionWorld *_btWorld;
// 		struct btCollisionDispatcher *_btDispatcher;
// #ifdef USE_PARALLEL_DISPATCHER
// 		struct btThreadSupportInterface *_btThreadSupportInterface;
// #endif
// 	};

// 	extern void hkl3d_free(struct Hkl3D *self);

// 	extern int hkl3d_is_colliding(struct Hkl3D *self);
// 	extern void hkl3d_load_config(struct Hkl3D *self, const char *filename);
// 	extern void hkl3d_save_config(struct Hkl3D *self, const char *filename);
// 	extern struct Hkl3DConfig *hkl3d_add_model_from_file(struct Hkl3D *self,
// 							     const char *filename, const char *directory);

// 	extern void hkl3d_connect_all_axes(struct Hkl3D *self);
// 	extern void hkl3d_hide_object(struct Hkl3D *self, struct Hkl3DObject *object, int hide);
// 	extern void hkl3d_remove_object(struct Hkl3D *self, struct Hkl3DObject *object);

// 	extern void hkl3d_get_bounding_boxes(struct Hkl3D *self,
// 					     struct btVector3 *min, struct btVector3 *max);

// 	extern void hkl3d_connect_object_to_axis(struct Hkl3D *self,
// 						 struct Hkl3DObject *object, const char *name);

// 	extern void hkl3d_fprintf(FILE *f, const struct Hkl3D *self);