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
#ifndef __HKL_PSEUDOAXIS_AUTO_H__
#define __HKL_PSEUDOAXIS_AUTO_H__

#include <gsl/gsl_vector_double.h>      // for gsl_vector
#include <stddef.h>                     // for NULL
#include <sys/types.h>                  // for uint
#include "hkl-detector-private.h"       // for hkl_detector_new_copy
#include "hkl-geometry-private.h"       // for hkl_geometry_new_copy
#include "hkl-macros-private.h"         // for hkl_assert, etc
#include "hkl-pseudoaxis-private.h"     // for HklModeOperations, etc
#include "hkl.h"                        // for HklMode, hkl_detector_free, etc
#include "hkl/ccan/container_of/container_of.h"  // for container_of
#include "hkl/ccan/array_size/array_size.h"  // ARRAY_SIZE

G_BEGIN_DECLS

typedef struct _HklFunction HklFunction;
typedef struct _HklModeAutoInfo HklModeAutoInfo;
typedef struct _HklModeAutoWithInit HklModeAutoWithInit;

/***************/
/* HklModeAuto */
/***************/

struct _HklFunction
{
	const uint size;
	int (* function) (const gsl_vector *x, void *params, gsl_vector *f);
};

typedef darray(const HklFunction*) darray_function;

struct _HklModeAutoInfo {
	const HklModeInfo info;
	darray_function functions;
};

#define HKL_MODE_OPERATIONS_AUTO_DEFAULTS	\
	HKL_MODE_OPERATIONS_DEFAULTS,		\
		.set = hkl_mode_auto_set_real

#define CHECK_NAN(x, len) do{				\
		for(uint i=0; i<len; ++i)		\
			if(gsl_isnan(x[i]))		\
				return GSL_EINVAL;	\
	}while(0)

#define HKL_MODE_AUTO_INFO(_name, _axes_r, _axes_w, _fn) .info={HKL_MODE_INFO(_name, _axes_r, _axes_w),}, .functions=DARRAY(_fn)

#define HKL_MODE_AUTO_INFO_WITH_PARAMS(_name, _axes_r, _axes_w, _fn, _parameters) .info={HKL_MODE_INFO_WITH_PARAMS(_name, _axes_r, _axes_w, _parameters)}, .functions=DARRAY(_fn)

extern HklMode *hkl_mode_auto_new(const HklModeAutoInfo *auto_info,
				  const HklModeOperations *ops,
				  int initialized);

extern void hkl_mode_auto_init(HklMode *self,
			       const HklModeAutoInfo *auto_info,
			       const HklModeOperations *ops,
			       int initialized);

extern int hkl_mode_auto_set_real(HklMode *self,
				  HklEngine *engine,
				  HklGeometry *geometry,
				  HklDetector *detector,
				  HklSample *sample,
				  GError **error);

/***********************/
/* HklModeAutoWithInit */
/***********************/

struct _HklModeAutoWithInit {
	HklMode mode;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
};

#define HKL_MODE_AUTO_WITH_INIT_ERROR hkl_mode_auto_with_init_error_quark ()

static GQuark hkl_mode_auto_with_init_error_quark (void)
{
	return g_quark_from_static_string ("hkl-mode-auto-with-init-error-quark");
}

typedef enum {
	HKL_MODE_AUTO_WITH_INIT_ERROR_INIT /* can not set the pseudo axis engine */
} HklModeError;

#define HKL_MODE_OPERATIONS_AUTO_WITH_INIT_DEFAULTS			\
	HKL_MODE_OPERATIONS_AUTO_DEFAULTS,				\
		.free = hkl_mode_auto_with_init_free_real,		\
		.initialized_set = hkl_mode_auto_with_init_initialized_set_real

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


static int hkl_mode_auto_with_init_initialized_set_real(HklMode *mode,
							HklEngine *engine,
							HklGeometry *geometry,
							HklDetector *detector,
							HklSample *sample,
							int initialized,
							GError **error)
{
	HklModeAutoWithInit *self = container_of(mode, HklModeAutoWithInit, mode);

	hkl_error(error == NULL || *error == NULL);

	if(initialized){
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
	}

	mode->initialized = initialized;

	return TRUE;
}

extern HklMode *hkl_mode_auto_with_init_new(const HklModeAutoInfo *info,
					    const HklModeOperations *ops,
					    int initialized);

G_END_DECLS

#endif /* __HKL_PSEUDOAXIS_AUTO_H__ */
