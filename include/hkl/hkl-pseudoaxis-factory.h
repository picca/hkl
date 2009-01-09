#ifndef __HKL_PSEUDOAXIS_TWOC_VERTICAL_H__
#define __HKL_PSEUDOAXIS_TWOC_VERTICAL_H__

#include <stdarg.h>
#include <hkl/hkl-pseudoaxis.h>

HKL_BEGIN_DECLS

static HklPseudoAxisEngine *hkl_pseudo_axis_engine_new_hkl(void)
{
	HklPseudoAxisEngine *engine = NULL;
	engine = malloc(sizeof(*engine));
	if (!engine)
		die("Can not allocate memory for an HklPseudoAxisEngine");

	engine->is_initialized = 0;
	engine->is_readable = 0;
	engine->is_writable = 0;
	engine->g_init = NULL;
	engine->init = NULL;
	engine->update = &hkl_pseudo_axis_engine_hkl_update;
	engine->set = NULL;

	hkl_pseudo_axis_engine_add_pseudoAxes(engine, "h", "k", "l");

	return engine;
}

static HklPseudoAxisEngine TwoCircleVertical = 
{
	0,
	0,
	0,
	NULL,
	&TwoC_Vertical_hkl_init;
	&TwoC_Vertical_hkl_update;
	&TwoC_Vertical_hkl_set;
}

HKL_END_DECLS

#endif /* __HKL_PSEUDOAXIS_TWOC_VERTICAL_H__ */
