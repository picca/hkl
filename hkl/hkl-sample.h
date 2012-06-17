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
#ifndef __HKL_SAMPLE_H__
#define __HKL_SAMPLE_H__

#include <hkl/hkl-lattice.h>
#include <hkl/hkl-geometry.h>
#include <hkl/hkl-detector.h>

HKL_BEGIN_DECLS

typedef struct _HklSample HklSample;
typedef struct _HklSampleReflection HklSampleReflection;
typedef struct _HklSampleList HklSampleList;

typedef enum _HklSampleType 
{
	HKL_SAMPLE_TYPE_MONOCRYSTAL
} HklSampleType;

struct _HklSample {
	char *name;
	HklSampleType type;
	HklLattice *lattice;
	HklMatrix U;
	HklMatrix UB;
	HklParameter *ux;
	HklParameter *uy;
	HklParameter *uz;
	HklSampleReflection **reflections;
	size_t reflections_len;
};

struct _HklSampleReflection {
	HklGeometry *geometry;
	HklDetector detector;
	HklVector hkl;
	HklVector _hkl;
	int flag;
};

struct _HklSampleList {
	HklSample **samples;
	size_t len;
	size_t alloc;
	HklSample *current;
};

/*************/
/* HklSample */
/*************/

extern HklSample *hkl_sample_new(const char *name, HklSampleType type);

extern HklSample *hkl_sample_new_copy(const HklSample *self);

extern void hkl_sample_free(HklSample *self);

extern void hkl_sample_set_name(HklSample *self, const char *name);

extern int hkl_sample_set_lattice(HklSample *self,
				  double a, double b, double c,
				  double alpha, double beta, double gamma);

extern int hkl_sample_set_U_from_euler(HklSample *self,
				       double x, double y, double z);

extern void hkl_sample_get_UB(HklSample *self, HklMatrix *UB);

extern double hkl_sample_set_UB(HklSample *self, const HklMatrix *UB);

extern HklSampleReflection *hkl_sample_add_reflection(HklSample *self,
						      HklGeometry *geometry,
						      const HklDetector *detector,
						      double h, double k, double l);

extern HklSampleReflection *hkl_sample_get_ith_reflection(const HklSample *self,
							  size_t idx);

extern int hkl_sample_del_reflection(HklSample *self, size_t idx);

extern int hkl_sample_compute_UB_busing_levy(HklSample *self,
					     size_t idx1, size_t idx2);

extern double hkl_sample_affine(HklSample *self);

extern double hkl_sample_get_reflection_mesured_angle(const HklSample *self,
						      size_t idx1, size_t idx2);

extern double hkl_sample_get_reflection_theoretical_angle(const HklSample *self,
							  size_t idx1, size_t idx2);

extern void hkl_sample_fprintf(FILE *f, const HklSample *self);


/***********************/
/* hklSampleReflection */
/***********************/

extern HklSampleReflection *hkl_sample_reflection_new(HklGeometry *geometry,
						      const HklDetector *detector,
						      double h, double k, double l);

extern HklSampleReflection *hkl_sample_reflection_new_copy(const HklSampleReflection *self);

extern void hkl_sample_reflection_free(HklSampleReflection *self);

extern void hkl_sample_reflection_set_hkl(HklSampleReflection *self, double h, double k, double l);

extern void hkl_sample_reflection_set_flag(HklSampleReflection *self, int flag);

extern void hkl_sample_reflection_set_geometry(HklSampleReflection *self, HklGeometry *geometry);

/*****************/
/* HklSampleList */
/*****************/

extern HklSampleList *hkl_sample_list_new(void);

extern HklSampleList *hkl_sample_list_new_copy(const HklSampleList *self);

extern void hkl_sample_list_free(HklSampleList *self);

extern HklSample *hkl_sample_list_append(HklSampleList *self,
					 HklSample *sample);

extern void hkl_sample_list_clear(HklSampleList *self);

extern void hkl_sample_list_del(HklSampleList *self, HklSample *sample);

extern size_t hkl_sample_list_len(const HklSampleList *self);

extern HklSample *hkl_sample_list_get_ith(HklSampleList *self, size_t idx);

extern HklSample *hkl_sample_list_get_by_name(HklSampleList *self,
					      const char *name);

extern int hkl_sample_list_get_idx_from_name(HklSampleList *self,
					     const char *name);

extern int hkl_sample_list_select_current(HklSampleList *self, char const *name);

extern void hkl_sample_list_fprintf(FILE *f, const HklSampleList *self);

HKL_END_DECLS

#endif

