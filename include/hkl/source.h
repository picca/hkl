#ifndef _HKL_SOURCE_H_
#define _HKL_SOURCE_H_

#include "svector.h"

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

#define HKL_SOURCE_DEFAULT_WAVE_LENGTH (1.54)

    struct hkl_source
      {
        double wave_length;
        struct hkl_svector direction;
      };

    extern int hkl_source_cmp(struct hkl_source const * s1, struct hkl_source const * s2);

    extern void hkl_source_get_ki(struct hkl_source const * source, struct hkl_svector * ki);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif /* _HKL_SOURCE_H_ */
