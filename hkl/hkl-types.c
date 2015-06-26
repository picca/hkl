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
 * Copyright (C) 2011-2015 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <glib-object.h>
#include "hkl-types.h"
#include "glib/gthread.h"               // for g_once_init_enter, etc
#include "glibconfig.h"                 // for gsize
#include "hkl-detector-private.h"       // for hkl_detector_new_copy
#include "hkl-geometry-private.h"       // for hkl_geometry_list_free, etc
#include "hkl-matrix-private.h"         // for hkl_matrix_dup
#include "hkl-pseudoaxis-private.h"     // for hkl_engine_list_new_copy
#include "hkl-sample-private.h"         // for hkl_sample_reflection_free, etc
#include "hkl-unit-private.h"           // for hkl_unit_dup, hkl_unit_free
#include "hkl-vector-private.h"         // for hkl_vector_dup, etc

#define HKL_TYPE(type, camelcase_type, copy, free)			\
	GType hkl_## type ## _get_type (void) {				\
		static volatile gsize hkl_type_id__volatile = 0;	\
		if (g_once_init_enter (&hkl_type_id__volatile)) {	\
			GType hkl_type_id;				\
			hkl_type_id = g_boxed_type_register_static (	\
				#camelcase_type,			\
				(GBoxedCopyFunc) copy,			\
				(GBoxedFreeFunc) free);			\
			g_once_init_leave (&hkl_type_id__volatile, hkl_type_id); \
		}							\
		return hkl_type_id__volatile;				\
	}

static void * hkl_fake_ref(void *src) { return src; }
static void hkl_fake_unref(void *src) { return; }

HKL_TYPE(detector, HklDetector, hkl_detector_new_copy, hkl_detector_free);
HKL_TYPE(engine, HklEngine, hkl_fake_ref, hkl_fake_unref);
HKL_TYPE(engine_list, HklEngineList, hkl_engine_list_new_copy, hkl_engine_list_free);
HKL_TYPE(factory, HklFactory, hkl_fake_ref, hkl_fake_unref);
HKL_TYPE(geometry, HklGeometry, hkl_geometry_new_copy, hkl_geometry_free);
HKL_TYPE(geometry_list, HklGeometryList, hkl_geometry_list_new_copy, hkl_geometry_list_free);
HKL_TYPE(geometry_list_item, HklGeometryListItem, hkl_geometry_list_item_new_copy, hkl_geometry_list_item_free);
HKL_TYPE(lattice, HklLattice, hkl_lattice_new_copy, hkl_lattice_free);
HKL_TYPE(matrix, HklMatrix, hkl_matrix_dup, hkl_matrix_free);
HKL_TYPE(parameter, HklParameter, hkl_parameter_new_copy, hkl_parameter_free);
HKL_TYPE(quaternion, HklQuaternion, hkl_quaternion_dup, hkl_quaternion_free);
HKL_TYPE(sample, HklSample, hkl_sample_new_copy, hkl_sample_free);
HKL_TYPE(sample_reflection, HklSampleReflection, hkl_fake_ref, hkl_fake_unref);
HKL_TYPE(unit, HklUnit, hkl_unit_dup, hkl_unit_free);
HKL_TYPE(vector, HklVector, hkl_vector_dup, hkl_vector_free);
