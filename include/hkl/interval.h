#ifndef _HKL_INTERVAL_H
#define _HKL_INTERVAL_H

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
  {
#endif

    struct hkl_interval
      {
        double min;
        double max;
      };

    int hkl_interval_cmp(struct hkl_interval const * interval, struct hkl_interval const * interval1);

    void hkl_interval_plus_interval(struct hkl_interval * interval, struct hkl_interval const * interval1);

    void hkl_interval_plus_double(struct hkl_interval * interval, double const d);

    void hkl_interval_minus_interval(struct hkl_interval * interval, struct hkl_interval const * interval1);

    void hkl_interval_minus_double(struct hkl_interval * interval, double const d);

    void hkl_interval_times_interval(struct hkl_interval * interval, struct hkl_interval const * interval1);

    void hkl_interval_times_double(struct hkl_interval * interval, double const d);

    void hkl_interval_divides_double(struct hkl_interval * interval, double const d);

    int hkl_interval_contain_zero(struct hkl_interval const * interval);

    void hkl_interval_cos(struct hkl_interval * interval);

    void hkl_interval_acos(struct hkl_interval * interval);

    void hkl_interval_sin(struct hkl_interval * interval);

    void hkl_interval_asin(struct hkl_interval * interval);

    void hkl_interval_tan(struct hkl_interval * interval);

    void hkl_interval_atan(struct hkl_interval * interval);

#ifdef __cplusplus
  }
#endif  /* C++ */

#endif
