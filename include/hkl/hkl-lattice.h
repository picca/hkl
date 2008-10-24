#ifndef __HKL_LATTICE_H__
#define __HKL_LATTICE_H__

#include <hkl/hkl-parameter.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

typedef struct _HklLattice HklLattice;

struct _HklLattice
{
	HklParameter *a;
	HklParameter *b;
	HklParameter *c;
	HklParameter *alpha;
	HklParameter *beta;
	HklParameter *gamma;
};

extern HklLattice *hkl_lattice_new(double a, double b, double c,
				   double alpha, double beta, double gamma);
extern HklLattice *hkl_lattice_new_copy(HklLattice const *self);
extern HklLattice *hkl_lattice_new_default(void);

extern void hkl_lattice_free(HklLattice *self);

extern int hkl_lattice_set(HklLattice *self,
			   double a, double b, double c,
			   double alpha, double beta, double gamma);

extern int hkl_lattice_get_B(HklLattice const *self, HklMatrix *B);

extern int hkl_lattice_reciprocal(HklLattice const *self, HklLattice *r);

extern void hkl_lattice_randomize(HklLattice *self);

HKL_END_DECLS

#endif /* __HKL_LATTICE_H__ */
