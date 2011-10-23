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
 * Copyright (C) 2011      Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl/hkl-types.h>

GType hkl_vector_get_type (void) {
        static volatile gsize hkl_vector_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_vector_type_id__volatile)) {
                GType hkl_vector_type_id;
                hkl_vector_type_id = g_boxed_type_register_static ("HklVector",
								   (GBoxedCopyFunc) hkl_vector_dup,
								   (GBoxedFreeFunc) hkl_vector_free);
                g_once_init_leave (&hkl_vector_type_id__volatile, hkl_vector_type_id);
        }
        return hkl_vector_type_id__volatile;
}

GType hkl_matrix_get_type (void) {
        static volatile gsize hkl_matrix_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_matrix_type_id__volatile)) {
                GType hkl_matrix_type_id;
                hkl_matrix_type_id = g_boxed_type_register_static ("HklMatrix",
								   (GBoxedCopyFunc) hkl_matrix_dup,
								   (GBoxedFreeFunc) hkl_matrix_free);
                g_once_init_leave (&hkl_matrix_type_id__volatile, hkl_matrix_type_id);
        }
        return hkl_matrix_type_id__volatile;
}

GType hkl_quaternion_get_type (void) {
        static volatile gsize hkl_quaternion_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_quaternion_type_id__volatile)) {
                GType hkl_quaternion_type_id;
                hkl_quaternion_type_id = g_boxed_type_register_static ("HklQuaternion",
								       (GBoxedCopyFunc) hkl_quaternion_dup,
								       (GBoxedFreeFunc) hkl_quaternion_free);
                g_once_init_leave (&hkl_quaternion_type_id__volatile, hkl_quaternion_type_id);
        }
        return hkl_quaternion_type_id__volatile;
}

GType hkl_axis_get_type (void) {
        static volatile gsize hkl_axis_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_axis_type_id__volatile)) {
                GType hkl_axis_type_id;
                hkl_axis_type_id = g_boxed_type_register_static ("HklAxis",
								 (GBoxedCopyFunc) hkl_axis_new_copy,
								 (GBoxedFreeFunc) hkl_axis_free);
                g_once_init_leave (&hkl_axis_type_id__volatile, hkl_axis_type_id);
        }
        return hkl_axis_type_id__volatile;
}
