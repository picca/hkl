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
 * Copyright (C) 2011-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hkl/hkl-types.h>
#include <hkl/hkl-pseudoaxis-private.h>

GType hkl_error_get_type (void) {
        static volatile gsize hkl_error_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_error_type_id__volatile)) {
                GType hkl_error_type_id;
                hkl_error_type_id = g_boxed_type_register_static ("HklError",
								  (GBoxedCopyFunc) hkl_error_new_copy,
								  (GBoxedFreeFunc) hkl_error_free);
                g_once_init_leave (&hkl_error_type_id__volatile, hkl_error_type_id);
        }
        return hkl_error_type_id__volatile;
}

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

GType hkl_unit_get_type (void) {
        static volatile gsize hkl_unit_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_unit_type_id__volatile)) {
                GType hkl_unit_type_id;
                hkl_unit_type_id = g_boxed_type_register_static ("HklUnit",
								 (GBoxedCopyFunc) hkl_unit_dup,
								 (GBoxedFreeFunc) hkl_unit_free);
                g_once_init_leave (&hkl_unit_type_id__volatile, hkl_unit_type_id);
        }
        return hkl_unit_type_id__volatile;
}

GType hkl_interval_get_type (void) {
        static volatile gsize hkl_interval_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_interval_type_id__volatile)) {
                GType hkl_interval_type_id;
                hkl_interval_type_id = g_boxed_type_register_static ("HklInterval",
								     (GBoxedCopyFunc) hkl_interval_dup,
								     (GBoxedFreeFunc) hkl_interval_free);
                g_once_init_leave (&hkl_interval_type_id__volatile, hkl_interval_type_id);
        }
        return hkl_interval_type_id__volatile;
}

GType hkl_parameter_get_type (void) {
        static volatile gsize hkl_parameter_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_parameter_type_id__volatile)) {
                GType hkl_parameter_type_id;
                hkl_parameter_type_id = g_boxed_type_register_static ("HklParameter",
								      (GBoxedCopyFunc) hkl_parameter_new_copy,
								      (GBoxedFreeFunc) hkl_parameter_free);
                g_once_init_leave (&hkl_parameter_type_id__volatile, hkl_parameter_type_id);
        }
        return hkl_parameter_type_id__volatile;
}

static HklEngine* hkl_parameter_list_fake_ref(HklEngine *src)
{
	return src;
}

static void hkl_parameter_list_fake_unref(HklEngine *src)
{
	return;
}

GType hkl_parameter_list_get_type (void) {
        static volatile gsize hkl_parameter_list_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_parameter_list_type_id__volatile)) {
                GType hkl_parameter_list_type_id;
                hkl_parameter_list_type_id = g_boxed_type_register_static ("HklParameterList",
									   (GBoxedCopyFunc) hkl_parameter_list_fake_ref,
									   (GBoxedFreeFunc) hkl_parameter_list_fake_unref);
                g_once_init_leave (&hkl_parameter_list_type_id__volatile, hkl_parameter_list_type_id);
        }
        return hkl_parameter_list_type_id__volatile;
}

GType hkl_axis_get_type (void) {
        static volatile gsize hkl_axis_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_axis_type_id__volatile)) {
                GType hkl_axis_type_id;
                hkl_axis_type_id = g_boxed_type_register_static ("HklAxis",
								 (GBoxedCopyFunc) hkl_parameter_new_copy,
								 (GBoxedFreeFunc) hkl_parameter_free);
                g_once_init_leave (&hkl_axis_type_id__volatile, hkl_axis_type_id);
        }
        return hkl_axis_type_id__volatile;
}

GType hkl_source_get_type (void) {
        static volatile gsize hkl_source_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_source_type_id__volatile)) {
                GType hkl_source_type_id;
                hkl_source_type_id = g_boxed_type_register_static ("HklSource",
								   (GBoxedCopyFunc) hkl_source_dup,
								   (GBoxedFreeFunc) hkl_source_free);
                g_once_init_leave (&hkl_source_type_id__volatile, hkl_source_type_id);
        }
        return hkl_source_type_id__volatile;
}

GType hkl_geometry_get_type (void) {
        static volatile gsize hkl_geometry_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_geometry_type_id__volatile)) {
                GType hkl_geometry_type_id;
                hkl_geometry_type_id = g_boxed_type_register_static ("HklGeometry",
								     (GBoxedCopyFunc) hkl_geometry_new_copy,
								     (GBoxedFreeFunc) hkl_geometry_free);
                g_once_init_leave (&hkl_geometry_type_id__volatile, hkl_geometry_type_id);
        }
        return hkl_geometry_type_id__volatile;
}

GType hkl_geometry_list_item_get_type (void) {
        static volatile gsize hkl_geometry_list_item_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_geometry_list_item_type_id__volatile)) {
                GType hkl_geometry_list_item_type_id;
                hkl_geometry_list_item_type_id = g_boxed_type_register_static ("HklGeometryListItem",
									       (GBoxedCopyFunc) hkl_geometry_list_item_new_copy,
									       (GBoxedFreeFunc) hkl_geometry_list_item_free);
                g_once_init_leave (&hkl_geometry_list_item_type_id__volatile, hkl_geometry_list_item_type_id);
        }
        return hkl_geometry_list_item_type_id__volatile;
}

GType hkl_geometry_list_get_type (void) {
        static volatile gsize hkl_geometry_list_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_geometry_list_type_id__volatile)) {
                GType hkl_geometry_list_type_id;
                hkl_geometry_list_type_id = g_boxed_type_register_static ("HklGeometryList",
									  (GBoxedCopyFunc) hkl_geometry_list_new_copy,
									  (GBoxedFreeFunc) hkl_geometry_list_free);
                g_once_init_leave (&hkl_geometry_list_type_id__volatile, hkl_geometry_list_type_id);
        }
        return hkl_geometry_list_type_id__volatile;
}

GType hkl_detector_get_type (void) {
        static volatile gsize hkl_detector_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_detector_type_id__volatile)) {
                GType hkl_detector_type_id;
                hkl_detector_type_id = g_boxed_type_register_static ("HklDetector",
								     (GBoxedCopyFunc) hkl_detector_new_copy,
								     (GBoxedFreeFunc) hkl_detector_free);
                g_once_init_leave (&hkl_detector_type_id__volatile, hkl_detector_type_id);
        }
        return hkl_detector_type_id__volatile;
}

GType hkl_lattice_get_type (void) {
        static volatile gsize hkl_lattice_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_lattice_type_id__volatile)) {
                GType hkl_lattice_type_id;
                hkl_lattice_type_id = g_boxed_type_register_static ("HklLattice",
								    (GBoxedCopyFunc) hkl_lattice_new_copy,
								    (GBoxedFreeFunc) hkl_lattice_free);
                g_once_init_leave (&hkl_lattice_type_id__volatile, hkl_lattice_type_id);
        }
        return hkl_lattice_type_id__volatile;
}

GType hkl_sample_reflection_get_type (void) {
        static volatile gsize hkl_sample_reflection_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_sample_reflection_type_id__volatile)) {
                GType hkl_sample_reflection_type_id;
                hkl_sample_reflection_type_id = g_boxed_type_register_static ("HklSampleReflection",
									      (GBoxedCopyFunc) hkl_sample_reflection_new_copy,
									      (GBoxedFreeFunc) hkl_sample_reflection_free);
                g_once_init_leave (&hkl_sample_reflection_type_id__volatile, hkl_sample_reflection_type_id);
        }
        return hkl_sample_reflection_type_id__volatile;
}

GType hkl_sample_get_type (void) {
        static volatile gsize hkl_sample_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_sample_type_id__volatile)) {
                GType hkl_sample_type_id;
                hkl_sample_type_id = g_boxed_type_register_static ("HklSample",
								   (GBoxedCopyFunc) hkl_sample_new_copy,
								   (GBoxedFreeFunc) hkl_sample_free);
                g_once_init_leave (&hkl_sample_type_id__volatile, hkl_sample_type_id);
        }
        return hkl_sample_type_id__volatile;
}

GType hkl_sample_list_get_type (void) {
        static volatile gsize hkl_sample_list_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_sample_list_type_id__volatile)) {
                GType hkl_sample_list_type_id;
                hkl_sample_list_type_id = g_boxed_type_register_static ("HklSampleList",
									(GBoxedCopyFunc) hkl_sample_list_new_copy,
									(GBoxedFreeFunc) hkl_sample_list_free);
                g_once_init_leave (&hkl_sample_list_type_id__volatile, hkl_sample_list_type_id);
        }
        return hkl_sample_list_type_id__volatile;
}

GType hkl_pseudo_axis_get_type (void) {
        static volatile gsize hkl_pseudo_axis_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_pseudo_axis_type_id__volatile)) {
                GType hkl_pseudo_axis_type_id;
                hkl_pseudo_axis_type_id = g_boxed_type_register_static ("HklPseudoAxis",
									(GBoxedCopyFunc) hkl_parameter_new_copy,
									(GBoxedFreeFunc) hkl_parameter_free);
                g_once_init_leave (&hkl_pseudo_axis_type_id__volatile, hkl_pseudo_axis_type_id);
        }
        return hkl_pseudo_axis_type_id__volatile;
}

static HklEngine* hkl_engine_fake_ref(HklEngine *src)
{
	return src;
}

static void hkl_engine_fake_unref(HklEngine *src)
{
	return;
}

GType hkl_engine_get_type (void) {
        static volatile gsize hkl_engine_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_engine_type_id__volatile)) {
                GType hkl_engine_type_id;
                hkl_engine_type_id = g_boxed_type_register_static (
			"HklEngine",
			(GBoxedCopyFunc) hkl_engine_fake_ref,
			(GBoxedFreeFunc) hkl_engine_fake_unref);
                g_once_init_leave (&hkl_engine_type_id__volatile, hkl_engine_type_id);
        }
        return hkl_engine_type_id__volatile;
}

GType hkl_engine_list_get_type (void) {
        static volatile gsize hkl_engine_list_type_id__volatile = 0;
        if (g_once_init_enter (&hkl_engine_list_type_id__volatile)) {
                GType hkl_engine_list_type_id;
                hkl_engine_list_type_id = g_boxed_type_register_static (
			"HklEngineList",
			(GBoxedCopyFunc) hkl_engine_list_new_copy,
			(GBoxedFreeFunc) hkl_engine_list_free);
                g_once_init_leave (&hkl_engine_list_type_id__volatile, hkl_engine_list_type_id);
        }
        return hkl_engine_list_type_id__volatile;
}
