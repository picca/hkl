#ifndef _SMATRIX_H
#define _SMATRIX_H

#include <stdlib.h>
#include <stdio.h>

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

    struct hkl_svector;

    struct hkl_smatrix
      {
        double data[3][3];
      };

    extern void hkl_smatrix_fprintf(FILE * file, struct hkl_smatrix const * m);

    extern void hkl_smatrix_from_two_svector(struct hkl_smatrix * m, struct hkl_svector const * v1, struct hkl_svector const * v2);

    extern void hkl_smatrix_from_euler(struct hkl_smatrix * m, double euler_x, double euler_y, double euler_z);

    extern void hkl_smatrix_to_euler(struct hkl_smatrix const * m, double * euler_x, double * euler_y, double * euler_z);

    extern int hkl_smatrix_cmp(struct hkl_smatrix const * m, struct hkl_smatrix const * m1);

    extern void hkl_smatrix_times_smatrix(struct hkl_smatrix * m, struct hkl_smatrix const * m1);

    extern void hkl_smatrix_times_svector(struct hkl_smatrix const * m, struct hkl_svector * v);

    extern void hkl_smatrix_transpose(struct hkl_smatrix * m);

    extern double hkl_smatrix_det(struct hkl_smatrix const * m);

    extern int hkl_smatrix_solve(struct hkl_smatrix const * m, struct hkl_svector * x, struct hkl_svector const * b);

    extern int hkl_smatrix_is_null(struct hkl_smatrix const * m);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif
