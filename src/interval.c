#define _GNU_SOURCE
#include <math.h>

#include "config.h"
#include "interval.h"

/** compare two intervals */
int hkl_interval_cmp(struct hkl_interval const * interval, struct hkl_interval const * interval1)
{
  return interval->min == interval1->min && interval->max == interval1->max;
}

/** add two intervals */
void hkl_interval_plus_interval(struct hkl_interval * interval, struct hkl_interval const * interval1)
{
  interval->min += interval1->min;
  interval->max += interval1->max;
}

/** add to an interval a double */
void hkl_interval_plus_double(struct hkl_interval * interval, double const d)
{
  interval->min += d;
  interval->max += d;
}


void hkl_interval_minus_interval(struct hkl_interval * interval, struct hkl_interval const * interval1)
{
  interval->min -= interval1->max;
  interval->max -= interval1->min;
}


void hkl_interval_minus_double(struct hkl_interval * interval, double const d)
{
  interval->min -= d;
  interval->max -= d;
}

void hkl_interval_times_interval(struct hkl_interval * interval, struct hkl_interval const * interval1)
{
  double m1 = interval->min * interval1->min;
  double m2 = interval->min * interval1->max;
  double m3 = interval->max * interval1->min;
  double m4 = interval->max * interval1->max;

  double min = m1;
  if (m2 < min)
    min = m2;
  if (m3 < min)
    min = m3;
  if (m4 < min)
    min = m4;

  double max = m1;
  if (m2 > max)
    max = m2;
  if (m3 > max)
    max = m3;
  if (m4 > max)
    max = m4;

  interval->min = min;
  interval->max = max;
}

void hkl_interval_times_double(struct hkl_interval * interval, double const d)
{
  double min;
  double max;
  if (d < 0)
    {
      min = interval->max * d;
      max = interval->min * d;
    }
  else
    {
      min = interval->min * d;
      max = interval->max * d;
    }
  interval->min = min;
  interval->max = max;
}

void hkl_interval_divides_double(struct hkl_interval * interval, double const d)
{
  double min = interval->min / d;
  double max = interval->max / d;
  if (min > max)
    {
      double tmp = min;
      min = max;
      max = tmp;
    }
  interval->min = min;
  interval->max = max;
}

int hkl_interval_contain_zero(struct hkl_interval const * interval)
{
  if (interval->min <= 0 && interval->max >= 0)
    return HKL_TRUE;
  else
    return HKL_FALSE;
}

void hkl_interval_cos(struct hkl_interval * interval)
{
  double min;
  double max;
  double cmin;
  double cmax;

  cmin = cos(interval->min);
  cmax = cos(interval->max);

  if (interval->max - interval->min >= 2 * M_PI)
    {
      min = -1;
      max = 1;
    }
  else
    {
      int quad_min = (int)floor(interval->min / M_PI_2) % 4;
      if (quad_min < 0)
        quad_min += 4;

      int quad_max = (int)floor(interval->max / M_PI_2) % 4;
      if (quad_max < 0)
        quad_max += 4;

      switch (quad_max)
        {
        case 0:
          switch (quad_min)
            {
            case 0:
              min = cmax;
              max = cmin;
              break;
            case 1:
              min = -1;
              max = 1;
              break;
            case 2:
              min = cmin;
              max = 1;
              break;
            case 3:
              if (cmin < cmax)
                {
                  min = cmin;
                  max = 1;
                }
              else
                {
                  min = cmax;
                  max = 1;
                }
              break;
            }
          break;
        case 1:
          switch (quad_min)
            {
            case 0:
              min = cmax;
              max = cmin;
              break;
            case 1:
              min = -1;
              max = 1;
              break;
            case 2:
              if (cmin < cmax)
                {
                  min = cmin;
                  max = 1;
                }
              else
                {
                  min = cmax;
                  max = 1;
                }
              break;
            case 3:
              min = cmax;
              max = 1;
              break;
            }
          break;
        case 2:
          switch (quad_min)
            {
            case 0:
              min = -1;
              max = cmin;
              break;
            case 1:
              if (cmin < cmax)
                {
                  min = -1;
                  max = cmax;
                }
              else
                {
                  min = -1;
                  max = cmin;
                }
              break;
            case 2:
              if (cmin < cmax)
                {
                  min = cmin;
                  max = cmax;
                }
              else
                {
                  min = -1;
                  max = 1;
                }
              break;
            case 3:
              min = -1;
              max = 1;
              break;
            }
          break;
        case 3:
          switch (quad_min)
            {
            case 0:
              if (cmin < cmax)
                {
                  min = -1;
                  max = cmax;
                }
              else
                {
                  min = -1;
                  max = cmin;
                }
              break;
            case 1:
              min = -1;
              max = cmax;
              break;
            case 2:
              min = cmin;
              max = cmax;
              break;
            case 3:
              if (cmin < cmax)
                {
                  min = cmin;
                  max = cmax;
                }
              else
                {
                  min = -1;
                  max = 1;
                }
              break;
            }
          break;
        }
    }
  interval->min = min;
  interval->max = max;
}

void hkl_interval_acos(struct hkl_interval * interval)
{
  double tmp;

  tmp = interval->min;
  interval->min = acos(interval->max);
  interval->max = acos(tmp);
}


void hkl_interval_sin(struct hkl_interval * interval)
{
  double min;
  double max;
  double smin;
  double smax;
  
  smin = sin(interval->min);
  smax = sin(interval->max);

  /* if there is at least one period in b, then a = [-1, 1] */
  if ( interval->max - interval->min >= 2 * M_PI)
    {
      min = -1;
      max = 1;
    }
  else
    {
      int quad_min = (int)floor(interval->min / M_PI_2) % 4;
      if (quad_min < 0)
        quad_min += 4;

      int quad_max = (int)floor(interval->max / M_PI_2) % 4;
      if (quad_max < 0)
        quad_max += 4;

      switch (quad_max)
        {
        case 0:
          switch (quad_min)
            {
            case 0:
              if (smin < smax)
                {
                  min = smin;
                  max = smax;
                }
              else
                {
                  min = -1;
                  max = 1;
                }
              break;
            case 3:
              min = smin;
              max = smax;
              break;
            case 1:
              if (smin > smax)
                {
                  min = -1;
                  max = smin;
                }
              else
                {
                  min = -1;
                  max = smax;
                }
              break;
            case 2:
              min = -1;
              max = smax;
              break;
            }
          break;
        case 1:
          switch (quad_min)
            {
            case 0:
              if (smin < smax)
                {
                  min = smin;
                  max = 1;
                }
              else
                {
                  min = smax;
                  max = 1;
                }
              break;
            case 1:
              if (smin < smax)
                {
                  min = -1;
                  max = 1;
                }
              else
                {
                  min = smax;
                  max = smin;
                }
              break;
            case 2:
              min = -1;
              max = 1;
              break;
            case 3:
              min = smin;
              max = 1;
              break;
            }
          break;
        case 2:
          switch (quad_min)
            {
            case 0:
              min = smax;
              max = 1;
              break;
            case 1:
            case 2:
              if (smin < smax)
                {
                  min = -1;
                  max = 1;
                }
              else
                {
                  min = smax;
                  max = smin;
                }
              break;
            case 3:
              if (smin < smax)
                {
                  min = smin;
                  max = 1;
                }
              else
                {
                  min = smax;
                  max = 1;
                }
              break;
            }
          break;
        case 3:
          switch (quad_min)
            {
            case 0:
              min = -1;
              max = 1;
              break;
            case 1:
              min = -1;
              max = smin;
              break;
            case 2:
              if (smin < smax)
                {
                  min = -1;
                  max = smax;
                }
              else
                {
                  min = -1;
                  max = smin;
                }
              break;
            case 3:
              if (smin < smax)
                {
                  min = smin;
                  max = smax;
                }
              else
                {
                  min = -1;
                  max = 1;
                }
              break;
            }
          break;
        }
    }
  interval->min = min;
  interval->max = max;
}

void hkl_interval_asin(struct hkl_interval * interval)
{
  interval->min = asin(interval->min);
  interval->max = asin(interval->max);
}

void hkl_interval_tan(struct hkl_interval * interval)
{
  int quadrant_down = (int)floor(interval->min / M_PI_2);
  int quadrant_up = (int)floor(interval->max / M_PI_2);

  /* if there is at least one period in b or if b contains a Pi/2 + k*Pi, */
  /* then a = ]-oo, +oo[ */
  if ( ((quadrant_up - quadrant_down) >= 2)
       || (!(quadrant_down % 2) && (quadrant_up % 2)) )
    {
      interval->min = -INFINITY;
      interval->max = INFINITY;
    }
  else
    {
      interval->min = tan(interval->min);
      interval->max = tan(interval->max);
    }
}

void hkl_interval_atan(struct hkl_interval * interval)
{
  interval->min = atan(interval->min);
  interval->max = atan(interval->max);
}
