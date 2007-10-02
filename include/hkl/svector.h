#ifndef _SVECTOR_H
#define _SVECTOR_H

#include <stdlib.h>
#include <stdio.h>

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

#define X 0
#define Y 1
#define Z 2

    /* forward declaration */
    struct hkl_smatrix;
    struct hkl_quaternion;

    struct hkl_svector
      {
        double data[3];
      };

    extern void hkl_svector_fprintf(FILE * file, struct hkl_svector const * v);

    extern void hkl_svector_set(struct hkl_svector * v, double const x, double const y, double const z);

    extern int hkl_svector_cmp(struct hkl_svector const * v, struct hkl_svector const * v1);

    extern int hkl_svector_is_opposite(struct hkl_svector const * v, struct hkl_svector const * v1);

    extern void hkl_svector_minus_svector(struct hkl_svector * v, struct hkl_svector const * v1);

    extern void hkl_svector_div_double(struct hkl_svector * v, double const d);

    extern void hkl_svector_times_double(struct hkl_svector * v, double const d);

    extern void hkl_svector_times_svector(struct hkl_svector * v, struct hkl_svector const * v1);

    extern void hkl_svector_times_smatrix(struct hkl_svector * v, struct hkl_smatrix const * m);

    extern double hkl_svector_sum(struct hkl_svector const * v);

    extern double hkl_svector_scalar_product(struct hkl_svector const * v, struct hkl_svector const * v1);

    extern void hkl_svector_vectorial_product(struct hkl_svector * v, struct hkl_svector const * v1);

    extern double hkl_svector_angle(struct hkl_svector const * v, struct hkl_svector const * v1);

    extern double hkl_svector_norm2(struct hkl_svector const * v);

    extern int hkl_svector_normalize(struct hkl_svector * v);

    extern int hkl_svector_is_colinear(struct hkl_svector const * v, struct hkl_svector const * v1);

    extern void hkl_svector_randomize(struct hkl_svector * v);
    extern void hkl_svector_randomize_svector(struct hkl_svector * v, struct hkl_svector const * v1);
    extern void hkl_svector_randomize_svector_svector(struct hkl_svector * v, struct hkl_svector const * v1, struct hkl_svector const * v2);

    extern void hkl_svector_rotated_around_vector(struct hkl_svector * v, struct hkl_svector const * axe, double angle);

    extern int hkl_svector_is_null(struct hkl_svector const * v);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif
