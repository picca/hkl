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
 * Copyright (C) 2003-2013 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_PSEUDOAXIS_AUTO_H__
#define __HKL_PSEUDOAXIS_AUTO_H__

#include <gsl/gsl_vector_double.h>      // for gsl_vector
#include <stddef.h>                     // for NULL
#include <sys/types.h>                  // for uint
#include "hkl-detector-private.h"       // for hkl_detector_new_copy
#include "hkl-error-private.h"          // for hkl_error_set
#include "hkl-geometry-private.h"       // for hkl_geometry_new_copy
#include "hkl-macros-private.h"         // for hkl_assert, etc
#include "hkl-pseudoaxis-private.h"     // for HklModeOperations, etc
#include "hkl.h"                        // for HklMode, hkl_detector_free, etc
#include "hkl/ccan/container_of/container_of.h"  // for container_of

HKL_BEGIN_DECLS

typedef struct _HklFunction HklFunction;
typedef struct _HklModeAutoInfo HklModeAutoInfo;
typedef struct _HklModeAutoWithInit HklModeAutoWithInit;

struct _HklFunction
{
	const uint size;
	int (* function) (const gsl_vector *x, void *params, gsl_vector *f);
};

struct _HklModeAutoInfo {
	const HklModeInfo mode;
	const HklFunction **functions;
	const uint n_functions;
};

#define HKL_MODE_OPERATIONS_AUTO_DEFAULTS	\
	HKL_MODE_OPERATIONS_DEFAULTS,		\
		.set = hkl_mode_auto_set_real

#define CHECK_NAN(x, len) do{				\
		for(uint i=0; i<len; ++i)		\
			if(gsl_isnan(x[i]))		\
				return GSL_ENOMEM;	\
	}while(0)

#define INFO_AUTO(name, axes, fn) .mode={INFO(name, axes),}, .functions=fn, .n_functions=ARRAY_SIZE(fn)
#define INFO_AUTO_WITH_PARAMS(name, axes, fn, parameters) .mode={INFO_WITH_PARAMS(name, axes, parameters)}, .functions=fn, .n_functions=ARRAY_SIZE(fn)

/***************/
/* HklModeAuto */
/***************/

extern HklMode *hkl_mode_auto_new(
	const HklModeAutoInfo *info,
	const HklModeOperations *ops);

void hkl_mode_auto_init(HklMode *self,
			const HklModeAutoInfo *info,
			const HklModeOperations *ops);

extern int hkl_mode_auto_set_real(HklMode *self,
				  HklEngine *engine,
				  HklGeometry *geometry,
				  HklDetector *detector,
				  HklSample *sample,
				  HklError **error);

/***********************/
/* HklModeAutoWithInit */
/***********************/

struct _HklModeAutoWithInit {
	HklMode mode;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
};

#define HKL_MODE_OPERATIONS_AUTO_WITH_INIT_DEFAULTS		\
	HKL_MODE_OPERATIONS_AUTO_DEFAULTS,			\
		.free = hkl_mode_auto_with_init_free_real,	\
		.init = hkl_mode_auto_with_init_init_real

static void hkl_mode_auto_with_init_free_real(HklMode *mode)
{
	HklModeAutoWithInit *self = container_of(mode, HklModeAutoWithInit, mode);

	if(self->geometry)
		hkl_geometry_free(self->geometry);
	if(self->detector)
		hkl_detector_free(self->detector);
	if(self->sample)
		hkl_sample_free(self->sample);

	hkl_mode_free_real(mode);
}

static int hkl_mode_auto_with_init_init_real(HklMode *mode,
					     HklEngine *engine,
					     HklGeometry *geometry,
					     HklDetector *detector,
					     HklSample *sample,
					     HklError **error)
{
	HklModeAutoWithInit *self = container_of(mode, HklModeAutoWithInit, mode);

	hkl_return_val_if_fail(error == NULL || *error == NULL, HKL_FALSE);

	if (!hkl_mode_init_real(mode, engine, geometry, detector, sample, error)){
		hkl_error_set(error, "internal error");
	}
	hkl_assert(error == NULL || *error == NULL);

	if(geometry){
		if(self->geometry)
			hkl_geometry_free(self->geometry);
		self->geometry = hkl_geometry_new_copy(geometry);
	}
	if(detector){
		if(self->detector)
			hkl_detector_free(self->detector);
		self->detector = hkl_detector_new_copy(detector);
	}
	if(sample){
		if(self->sample)
			hkl_sample_free(self->sample);
		self->sample = hkl_sample_new_copy(sample);
	}

	return HKL_TRUE;
}

extern HklMode *hkl_mode_auto_with_init_new(const HklModeAutoInfo *info,
					    const HklModeOperations *ops);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_AUTO_H__ */
