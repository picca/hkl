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

#include <ccan/list/list.h>
#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>
#include <hkl/hkl-error.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngineModeOperations HklPseudoAxisEngineModeOperations;
typedef struct _HklPseudoAxisEngineModeInfo HklPseudoAxisEngineModeInfo;
typedef struct _HklPseudoAxisEngineMode HklPseudoAxisEngineMode;
typedef struct _HklPseudoAxisEngineInfo HklPseudoAxisEngineInfo;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;
typedef struct _HklPseudoAxisEngineList HklPseudoAxisEngineList;

struct _HklPseudoAxis
{
	HklParameter parent;
	HklPseudoAxisEngine *engine;
	struct list_node list; /* PseudoAxisEngine */
};

struct _HklPseudoAxisEngineModeInfo {
	const char *name;
	const char **axes;
	uint n_axes;
	const HklParameter *parameters;
	uint n_parameters;
};

struct _HklPseudoAxisEngineMode
{
	const HklPseudoAxisEngineModeInfo *info;
	const HklPseudoAxisEngineModeOperations *op;
	HklParameter *parameters;
	size_t parameters_len;
	HklGeometry *geometry_init;
	HklDetector *detector_init;
	HklSample *sample_init;
	struct list_node list;
};

struct _HklPseudoAxisEngineInfo {
	const char *name;
	const HklPseudoAxis **pseudo_axes;
	uint n_pseudo_axes;
};

struct _HklPseudoAxisEngine
{
	const HklPseudoAxisEngineInfo *info;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	struct list_head modes; /* owned */
	struct list_head pseudo_axes; /* owned */
	struct list_head axes; /* item not owned */
	HklPseudoAxisEngineMode *mode; /* not owned */
	HklPseudoAxisEngineList *engines; /* not owned */
	struct list_node list; /* PseudoAxisEngineList */
};

/**
 * HklPseudoAxisEngineList:
 **/
struct _HklPseudoAxisEngineList
{
	struct list_head engines;
	HklGeometryList *geometries;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
};

/*****************/
/* HklPseudoAxis */
/*****************/

extern void hkl_pseudo_axis_fprintf(FILE *f, HklPseudoAxis *self);

/***************************/
/* HklPseudoAxisEngineMode */
/***************************/

extern void hkl_pseudo_axis_engine_mode_fprintf(FILE *f, const HklPseudoAxisEngineMode *self);

/***********************/
/* HklPseudoAxisEngine */
/***********************/

extern void hkl_pseudo_axis_engine_select_mode(HklPseudoAxisEngine *self,
					       HklPseudoAxisEngineMode *mode);

extern void hkl_pseudo_axis_engine_select_mode_by_name(HklPseudoAxisEngine *self,
						       const char *name);

extern int hkl_pseudo_axis_engine_initialize(HklPseudoAxisEngine *self, HklError **error);

extern int hkl_pseudo_axis_engine_set(HklPseudoAxisEngine *self, HklError **error);

extern int hkl_pseudo_axis_engine_get(HklPseudoAxisEngine *self, HklError **error);

extern void hkl_pseudo_axis_engine_set_values(HklPseudoAxisEngine *self,
					      double values[], uint len);

extern void hkl_pseudo_axis_engine_get_values(HklPseudoAxisEngine *self,
					      double values[], uint *len);

extern void hkl_pseudo_axis_engine_fprintf(FILE *f, const HklPseudoAxisEngine *self);

/***************************/
/* HklPseudoAxisEngineList */
/***************************/

extern void hkl_pseudo_axis_engine_list_free(HklPseudoAxisEngineList *self);

extern HklPseudoAxisEngine *hkl_pseudo_axis_engine_list_get_by_name(HklPseudoAxisEngineList *self,
								    const char *name);

extern HklPseudoAxis *hkl_pseudo_axis_engine_list_get_pseudo_axis_by_name(HklPseudoAxisEngineList *self,
									  const char *name);

extern void hkl_pseudo_axis_engine_list_init(HklPseudoAxisEngineList *self,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample);

extern int hkl_pseudo_axis_engine_list_get(HklPseudoAxisEngineList *self);

extern void hkl_pseudo_axis_engine_list_fprintf(FILE *f,
						const HklPseudoAxisEngineList *self);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
