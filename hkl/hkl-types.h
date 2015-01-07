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
#ifndef __HKL_GLIB_H__
#define __HKL_GLIB_H__

#include <glib-object.h>               // for GType
#include "hkl.h"                        // for HKLAPI

G_BEGIN_DECLS

#define TYPE_HKL_VECTOR (hkl_vector_get_type ())
HKLAPI GType hkl_vector_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_MATRIX (hkl_matrix_get_type ())
HKLAPI GType hkl_matrix_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_UNIT (hkl_unit_get_type ())
HKLAPI GType hkl_unit_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_PARAMETER (hkl_parameter_get_type ())
HKLAPI GType hkl_parameter_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_GEOMETRY (hkl_geometry_get_type ())
HKLAPI GType hkl_geometry_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_GEOMETRY_LIST_ITEM (hkl_geometry_list_item_get_type ())
HKLAPI GType hkl_geometry_list_item_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_GEOMETRY_LIST (hkl_geometry_list_get_type ())
HKLAPI GType hkl_geometry_list_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_DETECTOR (hkl_detector_get_type ())
HKLAPI GType hkl_detector_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_LATTICE (hkl_lattice_get_type ())
HKLAPI GType hkl_lattice_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_SAMPLE_REFLECTION (hkl_sample_reflection_get_type ())
HKLAPI GType hkl_sample_reflection_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_SAMPLE (hkl_sample_get_type ())
HKLAPI GType hkl_sample_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_ENGINE (hkl_engine_get_type ())
HKLAPI GType hkl_engine_get_type (void) G_GNUC_CONST;

#define TYPE_HKL_ENGINE_LIST (hkl_engine_list_get_type ())
HKLAPI GType hkl_engine_list_get_type (void) G_GNUC_CONST;

G_END_DECLS

#endif
