#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
//#include <gsl/gsl_vector.h>
#include <gsl/gsl_multiroots.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef int (* HklPseudoAxisEngineFunction) (const gsl_vector *x, void *params, gsl_vector *f);
typedef struct _HklPseudoAxisEngineFunc HklPseudoAxisEngineFunc;
typedef struct _HklPseudoAxisEngineConfig HklPseudoAxisEngineConfig;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;

struct _HklPseudoAxis
{
	char const * name;
	HklAxisConfig config;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngineFunc
{
	HklPseudoAxisEngineFunction *f;
	size_t f_len;
	HklParameter *parameters;
	size_t parameters_len;
};

struct _HklPseudoAxisEngineConfig
{
	char const *name;
	char const **pseudo_names;
	size_t pseudo_names_len;
	char const **axes_names;
	size_t axes_names_len;
	HklPseudoAxisEngineFunc *functions;
	size_t functions_len;
};

struct _HklPseudoAxisEngine
{
	HklPseudoAxisEngineConfig config;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HklPseudoAxisEngineFunc function;
	HklAxis **axes;
	size_t axes_len;
	HklPseudoAxis *pseudoAxes;
	size_t pseudoAxes_len;
	HklGeometry **geometries;
	size_t geometries_len;
	size_t geometries_alloc;
};

extern HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(HklPseudoAxisEngineConfig *config);

extern void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *self);

extern int RUBh_minus_Q(double const *x, void *params, double *f);

extern void hkl_pseudoAxisEngine_set(HklPseudoAxisEngine *self,
		size_t idx_f, HklGeometry *geom, HklDetector *det,
		HklSample *sample);

extern int hkl_pseudoAxisEngine_to_geometry(HklPseudoAxisEngine *self);

extern int hkl_pseudoAxisEngine_to_pseudoAxes(HklPseudoAxisEngine *self);

extern void hkl_pseudoAxisEngine_fprintf(HklPseudoAxisEngine *self, FILE *f);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
