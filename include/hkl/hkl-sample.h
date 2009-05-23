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
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
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
#include <hkl/hkl-list.h>

HKL_BEGIN_DECLS

typedef struct _HklSample HklSample;
typedef struct _HklSampleReflection HklSampleReflection;
typedef struct _HklSampleList HklSampleList;

enum _HklSampleType {
	HKL_SAMPLE_MONOCRYSTAL
};

typedef enum _HklSampleType HklSampleType;

struct _HklSample {
	char *name;
	HklSampleType type;
	HklLattice *lattice;
	HklMatrix U;
	HklMatrix UB;
	HKL_LIST(HklSampleReflection *, reflections);
};

struct _HklSampleReflection {
	HklGeometry *geometry;
	HklDetector detector;
	HklVector hkl;
	HklVector _hkl;
	int flag;
};

struct _HklSampleList {
	HKL_LIST(HklSample *, samples);
	HklSample *current;
};

/*************/
/* HklSample */
/*************/

extern HklSample *hkl_sample_new(char const *name, HklSampleType type);
extern HklSample *hkl_sample_new_copy(HklSample const *self);

extern void hkl_sample_free(HklSample *self);

extern void hkl_sample_set_name(HklSample *self, char const *name);

extern int hkl_sample_set_lattice(HklSample *self,
				  double a, double b, double c,
				  double alpha, double beta, double gamma);

extern int hkl_sample_set_U_from_euler(HklSample *self,
				       double x, double y, double z);

extern void hkl_sample_get_UB(HklSample *self, HklMatrix *matrix);

extern HklSampleReflection *hkl_sample_add_reflection(HklSample *self,
						      HklGeometry *geometry,
						      HklDetector const *detector,
						      double h, double k, double l);

extern HklSampleReflection *hkl_sample_get_ith_reflection(HklSample const *self,
							  size_t idx);

extern int hkl_sample_del_reflection(HklSample *self, size_t idx);

extern int hkl_sample_compute_UB_busing_levy(HklSample *self,
					     size_t idx1, size_t idx2);

extern double hkl_sample_affine(HklSample *sample);

extern double hkl_sample_get_reflection_mesured_angle(HklSample const *self,
						      size_t idx1, size_t idx2);

extern double hkl_sample_get_reflection_theoretical_angle(HklSample const *self,
							  size_t idx1, size_t idx2);

extern void hkl_sample_fprintf(FILE *f, HklSample const *self);


/***********************/
/* hklSampleReflection */
/***********************/

void hkl_sample_reflection_set_hkl(HklSampleReflection *self, double h, double k, double l);

void hkl_sample_reflection_set_flag(HklSampleReflection *self, int flag);

/*****************/
/* HklSampleList */
/*****************/

extern HklSampleList *hkl_sample_list_new(void);

extern void hkl_sample_list_free(HklSampleList *self);

extern HklSample *hkl_sample_list_append(HklSampleList *self,
					 HklSample *sample);

extern void hkl_sample_list_clear(HklSampleList *self);

extern void hkl_sample_list_del(HklSampleList *self, HklSample *sample);

extern size_t hkl_sample_list_len(HklSampleList const *self);

extern HklSample *hkl_sample_list_get_ith(HklSampleList *self, size_t idx);

extern HklSample *hkl_sample_list_get_by_name(HklSampleList *self,
					      char const *name);

extern size_t hkl_sample_list_get_idx_from_name(HklSampleList *self,
						char const *name);

extern int hkl_sample_list_select_current(HklSampleList *self, char const *name);

extern void hkl_sample_list_fprintf(FILE *f, HklSampleList const *self);

HKL_END_DECLS

#endif
