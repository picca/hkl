#ifndef __HKL_LATTICE_H__
#define __HKL_LATTICE_H__

#include <hkl/hkl-parameter.h>
#include <hkl/hkl-matrix.h>

HKL_BEGIN_DECLS

typedef struct _HklLattice HklLattice;

struct _HklLattice
{
	HklParameter a;
	HklParameter b;
	HklParameter c;
	HklParameter alpha;
	HklParameter beta;
	HklParameter gamma;
};

extern int hkl_lattice_init(HklLattice *lattice, double a, double b, double c, double alpha, double beta, double gamma);

extern int hkl_lattice_get_B(HklLattice const *lattice, HklMatrix *B);

extern int hkl_lattice_reciprocal(HklLattice const *lattice, HklLattice *reciprocal);

extern void hkl_lattice_randomize(HklLattice *lattice);

HKL_END_DECLS

#endif /* __HKL_LATTICE_H__ */
