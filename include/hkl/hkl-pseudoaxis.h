#ifndef __HKL_PSEUDOAXIS_H__
#define __HKL_PSEUDOAXIS_H__

#include <stdarg.h>
#include <gsl/gsl_multiroots.h>

#include <hkl/hkl-detector.h>
#include <hkl/hkl-sample.h>

HKL_BEGIN_DECLS

typedef struct _HklPseudoAxis HklPseudoAxis;
typedef struct _HklPseudoAxisEngineGetSet HklPseudoAxisEngineGetSet;
typedef struct _HklPseudoAxisEngine HklPseudoAxisEngine;

typedef int (* HklPseudoAxisEngineGetterFunc) (HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector const *detector,
		HklSample const *sample);

typedef int (* HklPseudoAxisEngineSetterFunc) (HklPseudoAxisEngine *self,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample);

struct _HklPseudoAxis
{
	char const * name;
	HklAxisConfig config;
	HklPseudoAxisEngine *engine;
};

struct _HklPseudoAxisEngineGetSet
{
	char const *name;
	HklPseudoAxisEngineGetterFunc get;
	HklPseudoAxisEngineSetterFunc set;
	HklParameter *parameters;
	size_t parameters_len;
	char const **axes_names;
	size_t axes_names_len;
};

struct _HklPseudoAxisEngine
{
	char const *name;
	HklGeometry *geometry;
	HklDetector *detector;
	HklSample *sample;
	HklPseudoAxisEngineGetSet **getsets;
	size_t getsets_len;
	HklPseudoAxisEngineGetSet *getset;
	HklAxis **axes;
	size_t axes_len;
	HklPseudoAxis *pseudoAxes;
	size_t pseudoAxes_len;
	HklGeometry **geometries;
	size_t geometries_len;
	size_t geometries_alloc;
};

/*****************************/
/* HklPseudoAxisEngineGetSet */
/*****************************/

extern HklPseudoAxisEngineGetSet *hkl_pseudo_axis_engine_get_set_new(
		char const *name,
		HklPseudoAxisEngineGetterFunc get,
		HklPseudoAxisEngineSetterFunc set,
		size_t n, ...);

extern void hkl_pseudo_axis_engine_get_set_free(HklPseudoAxisEngineGetSet *self);

/***********************/
/* HklPseudoAxisEngine */
/***********************/

extern HklPseudoAxisEngine *hkl_pseudoAxisEngine_new(char const *name,
		size_t n, ...);

extern void hkl_pseudoAxisEngine_free(HklPseudoAxisEngine *self);

extern void hkl_pseudoAxisEngine_add_get_set(HklPseudoAxisEngine *self,
		HklPseudoAxisEngineGetSet *getset);

extern void hkl_pseudoAxisEngine_add_geometry(HklPseudoAxisEngine *self,
		double const x[]);

extern void hkl_pseudoAxisEngine_select_get_set(HklPseudoAxisEngine *self,
		size_t idx);

extern void hkl_pseudoAxeEngine_prepare_internal(HklPseudoAxisEngine *engine,
		HklGeometry *geometry, HklDetector *detector,
		HklSample *sample);

extern int hkl_pseudoAxisEngine_setter(HklPseudoAxisEngine *self,
		HklGeometry *geom, HklDetector *det, HklSample *sample);

extern void hkl_pseudoAxisEngine_getter(HklPseudoAxisEngine *self,
		HklGeometry *geom, HklDetector *det, HklSample *sample);

extern void hkl_pseudoAxisEngine_fprintf(HklPseudoAxisEngine *self, FILE *f);

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_H__ */
