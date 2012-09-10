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
 * Copyright (C) 2003-2012 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <ccan/darray/darray.h>
#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>
#include <hkl/hkl-error.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklModeOperations HklModeOperations;
typedef struct _HklModeInfo HklModeInfo;
typedef struct _HklMode HklMode;
typedef struct _HklEngineInfo HklEngineInfo;
typedef struct _HklEngine HklEngine;
typedef struct _HklEngineList HklEngineList;
typedef struct _HklEngineOperations HklEngineOperations;

struct _HklModeInfo {
	const char *name;
	const char **axes;
	uint n_axes;
	const HklParameter *parameters;
	uint n_parameters;
};

struct _HklMode
{
	const HklModeInfo *info;
	const HklModeOperations *op;
	HklGeometry *geometry_init;
	HklDetector *detector_init;
	HklSample *sample_init;
	HklParameterList parameters;
};

struct _HklEngineInfo {
	const char *name;
	const HklPseudoAxis **pseudo_axes;
	uint n_pseudo_axes;
};

typedef darray(HklMode *) HklDArrayMode;
typedef darray(HklParameter *) HklDArrayParameter;

struct _HklEngine
{
	const HklEngineInfo *info;
	const HklEngineOperations *ops;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HklMode *mode; /* not owned */
	HklEngineList *engines; /* not owned */
	HklParameterList pseudo_axes;
	HklDArrayMode modes;
	HklDArrayParameter axes;
};

/**
 * HklEngineList:
 **/
struct _HklEngineList
{
	HklGeometryList *geometries;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HklEngine **item;
	size_t size;
	size_t alloc;
};

/***************************/
/* HklMode */
/***************************/

extern void hkl_mode_fprintf(FILE *f, const HklMode *self);

/***********************/
/* HklEngine */
/***********************/

extern void hkl_engine_select_mode(HklEngine *self,
					       HklMode *mode);

extern void hkl_engine_select_mode_by_name(HklEngine *self,
						       const char *name);

extern int hkl_engine_initialize(HklEngine *self, HklError **error);

extern int hkl_engine_set(HklEngine *self, HklError **error);

extern int hkl_engine_get(HklEngine *self, HklError **error);

extern void hkl_engine_fprintf(FILE *f, const HklEngine *self);

/***************************/
/* HklEngineList */
/***************************/

extern void hkl_engine_list_free(HklEngineList *self);

extern HklEngine *hkl_engine_list_get_by_name(HklEngineList *self,
								    const char *name);

extern HklParameter *hkl_engine_list_get_pseudo_axis_by_name(
	const HklEngineList *self, const char *name);

extern void hkl_engine_list_init(HklEngineList *self,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample);

extern int hkl_engine_list_get(HklEngineList *self);

extern void hkl_engine_list_fprintf(FILE *f,
						const HklEngineList *self);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
