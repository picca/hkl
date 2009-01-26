#ifndef __HKL_PSEUDOAXIS_FACTORY_H__
#define __HKL_PSEUDOAXIS_FACTORY_H__

#include <hkl/hkl-geometry-factory.h>
#include <hkl/hkl-pseudoaxis-common-eulerians.h>
#include <hkl/hkl-pseudoaxis-e4cv.h>
#include <hkl/hkl-pseudoaxis-k4cv.h>
#include <hkl/hkl-pseudoaxis-e6c.h>
#include <hkl/hkl-pseudoaxis-k6c.h>

HKL_BEGIN_DECLS

static HklPseudoAxisEngineList *hkl_pseudo_axis_engine_list_factory(HklGeometryType type)
{
	HklPseudoAxisEngineList *self = NULL;

	self = hkl_pseudo_axis_engine_list_new();

	switch(type){
	case HKL_GEOMETRY_TWOC_VERTICAL:
		break;
	case HKL_GEOMETRY_EULERIAN4C_VERTICAL:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e4cv_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e4cv_psi_new());
		break;
	case HKL_GEOMETRY_KAPPA4C_VERTICAL:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k4cv_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_eulerians_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k4cv_psi_new());
		break;
	case HKL_GEOMETRY_EULERIAN6C:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e6c_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_e6c_psi_new());
		break;
	case HKL_GEOMETRY_KAPPA6C:
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k6c_hkl_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_eulerians_new());
		hkl_pseudo_axis_engine_list_add(self, hkl_pseudo_axis_engine_k6c_psi_new());
		break;
	}
	return self;
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_FACTORY_H__ */
