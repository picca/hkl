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
#ifndef __HKL_H__
#define __HKL_H__

#include <math.h>                       // for M_PI
#include <stddef.h>                     // for size_t
#include <stdio.h>                      // for FILE
#include <glib.h>                       // for g_set_error, GError etc
#include <hkl/ccan/darray/darray.h>     // for darray

/* deprecated */
#if    __GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 1)
# define HKL_DEPRECATED __attribute__((__deprecated__))
#elif defined(_MSC_VER) && (_MSC_VER >= 1300)
# define HKL_DEPRECATED __declspec(deprecated)
#else
# define HKL_DEPRECATED
#endif

#if    __GNUC__ > 4 || (__GNUC__ == 4 && __GNUC_MINOR__ >= 5)
# define HKL_DEPRECATED_FOR(f) __attribute__((__deprecated__("Use '" #f "' instead")))
#elif defined(_MSC_FULL_VER) && (_MSC_FULL_VER > 140050320)
# define HKL_DEPRECATED_FOR(f) __declspec(deprecated("is deprecated. Use '" #f "' instead"))
#else
# define HKL_DEPRECATED_FOR(f) HKL_DEPRECATED
#endif

#define HKL_TINY 1e-7
#define HKL_EPSILON 1e-6
#define HKL_DEGTORAD (M_PI/180.)
#define HKL_RADTODEG (180./M_PI)

/* tau = 2pi or 1 */
#define HKL_TAU (2. * M_PI)
/* #define HKL_TAU 1 */

#ifdef HKLAPI
# undef HKLEAPI
#endif

#ifdef __GNUC__
# if __GNUC__ >= 4
#  define HKLAPI __attribute__ ((visibility("default")))
# else
#  define HKLAPI
# endif
#else
/**
 * @def HKLAPI
 * @brief Used to export functions(by changing visibility).
 */
# define HKLAPI
#endif

#if (__GNUC__ > 3 || (__GNUC__ == 3 && __GNUC_MINOR__ >= 3))
# define HKL_ARG_NONNULL(...) __attribute__ ((__nonnull__(__VA_ARGS__)))
#else
# define HKL_ARG_NONNULL(...)
#endif

#define _darray(type) type *item; size_t size; size_t alloc

G_BEGIN_DECLS

/**********/
/* Matrix */
/**********/

typedef struct _HklMatrix HklMatrix;

HKLAPI HklMatrix *hkl_matrix_new(void);

HKLAPI HklMatrix *hkl_matrix_new_full(double m11, double m12, double m13,
				      double m21, double m22, double m23,
				      double m31, double m32, double m33);

HKLAPI HklMatrix *hkl_matrix_new_euler(double euler_x, double euler_y, double euler_z);

HKLAPI double hkl_matrix_get(const HklMatrix *self, unsigned int i, unsigned int j) HKL_ARG_NONNULL(1);

HKLAPI void hkl_matrix_free(HklMatrix *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_matrix_init(HklMatrix *self,
			    double m11, double m12, double m13,
			    double m21, double m22, double m23,
			    double m31, double m32, double m33) HKL_ARG_NONNULL(1);

HKLAPI int hkl_matrix_cmp(const HklMatrix *self, const HklMatrix *m) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_matrix_times_matrix(HklMatrix *self, const HklMatrix *m) HKL_ARG_NONNULL(1, 2);

/********/
/* Unit */
/********/

typedef enum _HklUnitEnum
{
	HKL_UNIT_DEFAULT,
	HKL_UNIT_USER
} HklUnitEnum;

/*************/
/* Parameter */
/*************/

typedef struct _HklParameter HklParameter;

/* HklParameter */

HKLAPI HklParameter *hkl_parameter_new_copy(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_free(HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_parameter_name_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_parameter_default_unit_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_parameter_user_unit_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI double hkl_parameter_value_get(const HklParameter *self,
				      HklUnitEnum unit_type) HKL_ARG_NONNULL(1);

HKLAPI int hkl_parameter_value_set(HklParameter *self, double value,
				   HklUnitEnum unit_type, GError **error) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_min_max_get(const HklParameter *self, double *min, double *max,
				      HklUnitEnum unit_type) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI int hkl_parameter_min_max_set(HklParameter *self, double min, double max,
				     HklUnitEnum unit_type, GError **error) HKL_ARG_NONNULL(1);

HKLAPI int hkl_parameter_fit_get(const HklParameter *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_fit_set(HklParameter *self, int fit) HKL_ARG_NONNULL(1);

HKLAPI void hkl_parameter_randomize(HklParameter *self) HKL_ARG_NONNULL(1);

/************/
/* Detector */
/************/

typedef struct _HklDetector HklDetector;
typedef enum _HklDetectorType
{
	HKL_DETECTOR_TYPE_0D
} HklDetectorType;

HKLAPI HklDetector *hkl_detector_factory_new(HklDetectorType type);

HKLAPI HklDetector *hkl_detector_new_copy(const HklDetector *src) HKL_ARG_NONNULL(1);

HKLAPI void hkl_detector_free(HklDetector *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_detector_idx_set(HklDetector *self, int idx) HKL_ARG_NONNULL(1);

HKLAPI void hkl_detector_fprintf(FILE *f, const HklDetector *self) HKL_ARG_NONNULL(1, 2);

/************/
/* Geometry */
/************/

typedef struct _HklGeometry HklGeometry;
typedef struct _HklGeometryList HklGeometryList;
typedef struct _HklGeometryListItem HklGeometryListItem;

/* HklGeometry */

HKLAPI void hkl_geometry_free(HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_geometry_set(HklGeometry *self, const HklGeometry *src) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_geometry_axis_get(const HklGeometry *self, const char *name,
						 GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_geometry_axis_set(HklGeometry *self, const char *name,
				 const HklParameter *axis,
				 GError **error) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI void hkl_geometry_axes_values_get(const HklGeometry *self,
					 double values[], size_t n_values,
					 HklUnitEnum unit_type) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_geometry_axes_values_set(HklGeometry *self,
					double values[], size_t n_values,
					HklUnitEnum unit_type,
					GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const char *hkl_geometry_name_get(const HklGeometry *self) HKL_ARG_NONNULL(1);

HKLAPI double hkl_geometry_wavelength_get(const HklGeometry *self,
					  HklUnitEnum unit_type) HKL_ARG_NONNULL(1);

HKLAPI int hkl_geometry_wavelength_set(HklGeometry *self, double wavelength,
				       HklUnitEnum unit_type, GError **error) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_randomize(HklGeometry *self) HKL_ARG_NONNULL(1);

/* TODO after bissecting it seems that this method is slow (to replace) */
HKLAPI int hkl_geometry_set_values_v(HklGeometry *self, HklUnitEnum unit_type,
				     GError **error, ...) HKL_ARG_NONNULL(1);

HKLAPI void hkl_geometry_fprintf(FILE *file, const HklGeometry *self) HKL_ARG_NONNULL(1, 2);

/* HklGeometryList */

#define HKL_GEOMETRY_LIST_FOREACH(item, list) for((item)=hkl_geometry_list_items_first_get((list)); \
						  (item);		\
						  (item)=hkl_geometry_list_items_next_get((list), (item)))

HKLAPI void hkl_geometry_list_free(HklGeometryList *self) HKL_ARG_NONNULL(1);

HKLAPI size_t hkl_geometry_list_n_items_get(const HklGeometryList *self) HKL_ARG_NONNULL(1);

HKLAPI const HklGeometryListItem *hkl_geometry_list_items_first_get(const HklGeometryList *self) HKL_ARG_NONNULL(1);

HKLAPI const HklGeometryListItem *hkl_geometry_list_items_next_get(const HklGeometryList *self,
								   const HklGeometryListItem *item) HKL_ARG_NONNULL(1, 2);

/* HklGeometryListItem */

HKLAPI const HklGeometry *hkl_geometry_list_item_geometry_get(const HklGeometryListItem *self) HKL_ARG_NONNULL(1);

/**********/
/* Sample */
/**********/

typedef struct _HklLattice HklLattice;
typedef struct _HklSample HklSample;
typedef struct _HklSampleReflection HklSampleReflection;

/* Lattice */

HKLAPI HklLattice *hkl_lattice_new(double a, double b, double c,
				   double alpha, double beta, double gamma,
				   GError **error);

HKLAPI HklLattice *hkl_lattice_new_copy(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI HklLattice *hkl_lattice_new_default(void);

HKLAPI void hkl_lattice_free(HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI const HklParameter *hkl_lattice_a_get(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_a_set(HklLattice *self, const HklParameter *parameter,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_b_get(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_b_set(HklLattice *self, const HklParameter *parameter,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_c_get(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_c_set(HklLattice *self, const HklParameter *parameter,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_alpha_get(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_alpha_set(HklLattice *self, const HklParameter *parameter,
				 GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_beta_get(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_beta_set(HklLattice *self, const HklParameter *parameter,
				GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_lattice_gamma_get(const HklLattice *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_lattice_gamma_set(HklLattice *self, const HklParameter *parameter,
				 GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_lattice_set(HklLattice *self,
			   double a, double b, double c,
			   double alpha, double beta, double gamma,
			   GError **error) HKL_ARG_NONNULL(1);

HKLAPI void hkl_lattice_get(const HklLattice *self,
			    double *a, double *b, double *c,
			    double *alpha, double *beta, double *gamma) HKL_ARG_NONNULL(1, 2, 3, 4, 5, 6, 7);

HKLAPI int hkl_lattice_get_B(const HklLattice *self, HklMatrix *B) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_lattice_get_1_B(const HklLattice *self, HklMatrix *B) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_lattice_reciprocal(const HklLattice *self, HklLattice *reciprocal) HKL_ARG_NONNULL(1, 2);

/* HklSample */

HKLAPI HklSample *hkl_sample_new(const char *name) HKL_ARG_NONNULL(1);

HKLAPI HklSample *hkl_sample_new_copy(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_free(HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_sample_name_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_name_set(HklSample *self, const char *name) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklLattice *hkl_sample_lattice_get(HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_lattice_set(HklSample *self, const HklLattice *lattice) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_sample_ux_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_sample_ux_set(HklSample *self, const HklParameter *ux,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_sample_uy_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_sample_uy_set(HklSample *self, const HklParameter *uy,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_sample_uz_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_sample_uz_set(HklSample *self, const HklParameter *uz,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklMatrix *hkl_sample_U_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_U_set(HklSample *self, const HklMatrix *U) HKL_ARG_NONNULL(1);

HKLAPI const HklMatrix *hkl_sample_UB_get(const HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_sample_UB_set(HklSample *self, const HklMatrix *UB,
			     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI HklSampleReflection *hkl_sample_first_reflection_get(HklSample *self) HKL_ARG_NONNULL(1);

HKLAPI HklSampleReflection *hkl_sample_next_reflection_get(HklSample *self,
							   HklSampleReflection *reflection) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_sample_del_reflection(HklSample *self,
				      HklSampleReflection *reflection) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_sample_add_reflection(HklSample *self,
				      HklSampleReflection *reflection) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_sample_compute_UB_busing_levy(HklSample *self,
					     const HklSampleReflection *r1,
					     const HklSampleReflection *r2,
					     GError **error) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI double hkl_sample_get_reflection_mesured_angle(const HklSample *self,
						      const HklSampleReflection *r1,
						      const HklSampleReflection *r2) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI double hkl_sample_get_reflection_theoretical_angle(const HklSample *self,
							  const HklSampleReflection *r1,
							  const HklSampleReflection *r2) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI int hkl_sample_affine(HklSample *self, GError **error) HKL_ARG_NONNULL(1);

/* hklSampleReflection */

HKLAPI HklSampleReflection *hkl_sample_reflection_new(const HklGeometry *geometry,
						      const HklDetector *detector,
						      double h, double k, double l,
						      GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_sample_reflection_hkl_get(const HklSampleReflection *self,
					  double *h, double *k, double *l) HKL_ARG_NONNULL(1, 2, 3, 4);

HKLAPI int hkl_sample_reflection_hkl_set(HklSampleReflection *self,
					 double h, double k, double l,
					 GError **error) HKL_ARG_NONNULL(1);

HKLAPI int hkl_sample_reflection_flag_get(const HklSampleReflection *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_reflection_flag_set(HklSampleReflection *self, int flag) HKL_ARG_NONNULL(1);

HKLAPI const HklGeometry *hkl_sample_reflection_geometry_get(HklSampleReflection *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_sample_reflection_geometry_set(HklSampleReflection *self,
					       const HklGeometry *geometry) HKL_ARG_NONNULL(1, 2);

/**************/
/* PseudoAxis */
/**************/

typedef struct _HklEngine HklEngine;
typedef struct _HklEngineList HklEngineList;

typedef darray(const char *) darray_string;
typedef darray(HklEngine *) darray_engine;

/* HklEngine */

HKLAPI const char *hkl_engine_name_get(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI unsigned int hkl_engine_len(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI const darray_string *hkl_engine_pseudo_axes_names_get(HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_engine_pseudo_axes_values_get(HklEngine *self,
					     double values[], size_t n_values,
					     HklUnitEnum unit_type, GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI HklGeometryList *hkl_engine_pseudo_axes_values_set(HklEngine *self,
							  double values[], size_t n_values,
							  HklUnitEnum unit_type, GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI const HklParameter *hkl_engine_pseudo_axis_get(const HklEngine *self,
						      const char *name,
						      GError **error) HKL_ARG_NONNULL(1, 2);

typedef enum _HklEngineCapabilities
{
	HKL_ENGINE_CAPABILITIES_READABLE = 1u << 0,
	HKL_ENGINE_CAPABILITIES_WRITABLE = 1u << 1,
	HKL_ENGINE_CAPABILITIES_INITIALIZABLE = 1u << 2,
} HklEngineCapabilities;

HKLAPI unsigned int hkl_engine_capabilities_get(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_engine_initialized_get(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_engine_initialized_set(HklEngine *self, int initialized,
				      GError **error) HKL_ARG_NONNULL(1);

HKLAPI void hkl_engine_fprintf(FILE *f, const HklEngine *self) HKL_ARG_NONNULL(1, 2);

/* mode */

HKLAPI const darray_string *hkl_engine_modes_names_get(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_engine_current_mode_get(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_engine_current_mode_set(HklEngine *self, const char *mode, GError **error) HKL_ARG_NONNULL(1, 2);

typedef enum _HklEngineAxesNamesGet
{
	HKL_ENGINE_AXES_NAMES_GET_READ,
	HKL_ENGINE_AXES_NAMES_GET_WRITE,
} HklEngineAxesNamesGet;

HKLAPI const darray_string *hkl_engine_axes_names_get(const HklEngine *self,
						      HklEngineAxesNamesGet mode) HKL_ARG_NONNULL(1);

HKLAPI const darray_string *hkl_engine_parameters_names_get(const HklEngine *self) HKL_ARG_NONNULL(1);

HKLAPI const HklParameter *hkl_engine_parameter_get(const HklEngine *self, const char *name,
						    GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_engine_parameter_set(HklEngine *self,
				    const char *name, const HklParameter *parameter,
				    GError **error) HKL_ARG_NONNULL(1, 2, 3);

HKLAPI void hkl_engine_parameters_values_get(const HklEngine *self,
					     double values[], size_t n_values,
					     HklUnitEnum unit_type) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_engine_parameters_values_set(HklEngine *self,
					    double values[], size_t n_values,
					    HklUnitEnum unit_type, GError **error) HKL_ARG_NONNULL(1, 2);

/* HklEngineList */

HKLAPI void hkl_engine_list_free(HklEngineList *self) HKL_ARG_NONNULL(1);

HKLAPI darray_engine *hkl_engine_list_engines_get(HklEngineList *self) HKL_ARG_NONNULL(1);

HKLAPI HklGeometry *hkl_engine_list_geometry_get(HklEngineList *self) HKL_ARG_NONNULL(1);

HKLAPI int hkl_engine_list_geometry_set(HklEngineList *self, const HklGeometry *geometry) HKL_ARG_NONNULL(1, 2);

HKLAPI int hkl_engine_list_select_solution(HklEngineList *self,
					   const HklGeometryListItem *item) HKL_ARG_NONNULL(1);

HKLAPI HklEngine *hkl_engine_list_engine_get_by_name(HklEngineList *self,
						     const char *name,
						     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI HklParameter *hkl_engine_list_pseudo_axis_get_by_name(const HklEngineList *self,
							     const char *name,
							     GError **error) HKL_ARG_NONNULL(1, 2);

HKLAPI void hkl_engine_list_init(HklEngineList *self,
				 HklGeometry *geometry,
				 HklDetector *detector,
				 HklSample *sample) HKL_ARG_NONNULL(1, 2, 3, 4);

HKLAPI int hkl_engine_list_get(HklEngineList *self) HKL_ARG_NONNULL(1);

HKLAPI void hkl_engine_list_fprintf(FILE *f,
				    const HklEngineList *self) HKL_ARG_NONNULL(1, 2);

/***********/
/* Factory */
/***********/

typedef struct _HklFactory HklFactory;

HKLAPI HklFactory **hkl_factory_get_all(unsigned int *n) HKL_ARG_NONNULL(1);

HKLAPI HklFactory *hkl_factory_get_by_name(const char *name,
					   GError **error) HKL_ARG_NONNULL(1);

HKLAPI const char *hkl_factory_name(const HklFactory *self) HKL_ARG_NONNULL(1);

HKLAPI const char **hkl_factory_axes_names_get(const HklFactory *self,
					       size_t *length) HKL_ARG_NONNULL(1, 2);

HKLAPI HklGeometry *hkl_factory_create_new_geometry(const HklFactory *self) HKL_ARG_NONNULL(1);

HKLAPI HklEngineList *hkl_factory_create_new_engine_list(const HklFactory *self) HKL_ARG_NONNULL(1);

G_END_DECLS

#endif
