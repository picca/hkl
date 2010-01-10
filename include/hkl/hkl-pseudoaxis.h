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
#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
#include <gsl/gsl_multiroots.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngineMode HklPseudoAxisEngineMode;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;
typedef struct _HklPseudoAxisEngineList HklPseudoAxisEngineList;

typedef int (* HklPseudoAxisEngineModeFunc) (HklPseudoAxisEngineMode *self,
					     HklPseudoAxisEngine *engine,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample);

typedef int (* HklFunction) (const gsl_vector *x, void *params, gsl_vector *f);

struct _HklPseudoAxis
{
	HklParameter parent;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngineMode
{
	char const *name;
	HklPseudoAxisEngineModeFunc initialize;
	HklPseudoAxisEngineModeFunc get;
	HklPseudoAxisEngineModeFunc set;
	HKL_LIST(HklFunction, functions);
	HKL_LIST(HklParameter, parameters);
	HKL_LIST(const char*, axes_names);
	HklGeometry *geometry_init;
	HklDetector *detector_init;
	HklSample *sample_init;
};

struct _HklPseudoAxisEngine
{
	char const *name;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HKL_LIST(HklPseudoAxisEngineMode *, modes);
	HKL_LIST(HklAxis *, axes);
	HKL_LIST(HklPseudoAxis *, pseudoAxes);
	HklPseudoAxisEngineMode *mode;
	HklPseudoAxisEngineList *engines;
};

struct _HklPseudoAxisEngineList
{
	HKL_LIST(HklPseudoAxisEngine *, engines);
	HklGeometryList *geometries;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
};

/*****************/
/* HklPseudoAxis */
/*****************/

extern HklPseudoAxis *hkl_pseudo_axis_new(HklParameter const *parameter,
					  HklPseudoAxisEngine *engine);

extern void hkl_pseudo_axis_init(HklPseudoAxis *self,
				 HklParameter const *parameter,
				 HklPseudoAxisEngine *engine);

extern void hkl_pseudo_axis_free(HklPseudoAxis *self);

extern void hkl_pseudo_axis_fprintf(FILE *f, HklPseudoAxis *self);

/*****************************/
/* HklPseudoAxisEngineMode */
/*****************************/

extern HklPseudoAxisEngineMode *hkl_pseudo_axis_engine_mode_new(
	char const *name,
	HklPseudoAxisEngineModeFunc initialize,
	HklPseudoAxisEngineModeFunc get,
	HklPseudoAxisEngineModeFunc set,
	size_t n, ...);

extern int hkl_pseudo_axis_engine_mode_init(
	HklPseudoAxisEngineMode *self,
	char const *name,
	HklPseudoAxisEngineModeFunc initialize,
	HklPseudoAxisEngineModeFunc get,
	HklPseudoAxisEngineModeFunc set,
	size_t n_func, HklFunction functions[],
	size_t n_p, HklParameter parameters[],
	size_t n_axes, char const *axes_names[]);

extern void hkl_pseudo_axis_engine_mode_free(HklPseudoAxisEngineMode *self);

/***********************/
/* HklPseudoAxisEngine */
/***********************/

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_new(char const *name,
						       size_t n, ...);

extern void hkl_pseudo_axis_engine_free(HklPseudoAxisEngine *self);

extern void hkl_pseudo_axis_engine_add_mode(HklPseudoAxisEngine *self,
					    HklPseudoAxisEngineMode *mode);

extern void hkl_pseudo_axis_engine_add_geometry(HklPseudoAxisEngine *self,
						double const x[]);

extern void hkl_pseudo_axis_engine_select_mode(HklPseudoAxisEngine *self,
					       size_t idx);

extern int hkl_pseudo_axis_engine_initialize(HklPseudoAxisEngine *self);

extern int hkl_pseudo_axis_engine_set(HklPseudoAxisEngine *self);

extern int hkl_pseudo_axis_engine_get(HklPseudoAxisEngine *self);

extern void hkl_pseudo_axis_engine_fprintf(FILE *f, HklPseudoAxisEngine const *self);

/***************************/
/* HklPseudoAxisEngineList */
/***************************/

extern HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_new(void);

extern void hkl_pseudo_axis_engine_list_free(HklPseudoAxisEngineList *self);

extern int hkl_pseudo_axis_engine_list_add(HklPseudoAxisEngineList *self,
					   HklPseudoAxisEngine *engine);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_list_get_by_name(HklPseudoAxisEngineList *self,
								    char const *name);

extern HklPseudoAxis *hkl_pseudo_axis_engine_list_get_pseudo_axis_by_name(HklPseudoAxisEngineList *self,
									  char const *name);

extern void hkl_pseudo_axis_engine_list_clear(HklPseudoAxisEngineList *self);

extern void hkl_pseudo_axis_engine_list_init(HklPseudoAxisEngineList *self,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample);

extern int hkl_pseudo_axis_engine_list_getter(HklPseudoAxisEngineList *self);

extern void hkl_pseudo_axis_engine_list_fprintf(FILE *f,
						HklPseudoAxisEngineList const *self);
HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
