#ifndef _PARAMETER_H_
#define _PARAMETER_H_

#include "interval.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	struct hkl_parameter {
		const char *name;
		struct hkl_interval range;
		double value;
		int not_to_fit;
	};

	extern int hkl_parameter_init(struct hkl_parameter *parameter, char const *name, double min, double value, double max, int not_to_fit);

	extern void hkl_parameter_randomize(struct hkl_parameter *parameter);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _PARAMETER_H_ */
