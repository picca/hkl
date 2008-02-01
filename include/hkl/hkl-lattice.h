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
extern HklLattice *hkl_lattice_new_copy(HklLattice const *l);
extern HklLattice *hkl_lattice_new_default(void);

extern void hkl_lattice_free(HklLattice *l);

extern int hkl_lattice_set(HklLattice *l, double a, double b, double c,
		double alpha, double beta, double gamma);

extern int hkl_lattice_get_B(HklLattice const *l, HklMatrix *B);

extern int hkl_lattice_reciprocal(HklLattice const *l, HklLattice *r);

extern void hkl_lattice_randomize(HklLattice *l);

HKL_END_DECLS

#endif /* __HKL_LATTICE_H__ */
