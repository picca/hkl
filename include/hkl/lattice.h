#ifndef _LATTICE_H_
#define _LATTICE_H_

#include "parameter.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	/* forward declaration begin */
	struct hkl_smatrix;
	/* forward declaration end */

	struct hkl_lattice
	{
		struct hkl_parameter a;
		struct hkl_parameter b;
		struct hkl_parameter c;
		struct hkl_parameter alpha;
		struct hkl_parameter beta;
		struct hkl_parameter gamma;
	};

	extern int hkl_lattice_init(struct hkl_lattice *lattice, double a, double b, double c, double alpha, double beta, double gamma);

	extern int hkl_lattice_get_B(struct hkl_lattice const *lattice, struct hkl_smatrix *B);

	extern int hkl_lattice_reciprocal(struct hkl_lattice const *lattice, struct hkl_lattice *reciprocal);

	extern void hkl_lattice_randomize(struct hkl_lattice *lattice);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _LATTICE_H_ */
