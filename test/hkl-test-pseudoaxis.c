#include <math.h>

#include <hkl/hkl-pseudoaxis.h>

#include "hkl-test.h"

#ifdef HKL_TEST_SUITE_NAME
# undef HKL_TEST_SUITE_NAME
#endif
#define HKL_TEST_SUITE_NAME pseudoaxis

HKL_TEST_SUITE_FUNC(new)
{
	HklPseudoAxisEngine *engine = NULL;

	engine = hkl_pseudoAxisEngine_new("hkl", 3, "h", "k", "l");

	hkl_pseudoAxisEngine_free(engine);

	return HKL_TEST_PASS;
}

HKL_TEST_SUITE_BEGIN

HKL_TEST( new );

HKL_TEST_SUITE_END
