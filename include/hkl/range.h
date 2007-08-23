#ifndef _RANGE_H
#define _RANGE_H


#include "value.h"
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl {

class Range {
  protected:
    hkl::Value _min;

    hkl::Value _current;

    hkl::Value _max;


  public:
    /**
     * @brief The default constructor.
     */
    Range();

    /**
     * @brief constructor of the Range class.
     * 
     * @param min The minimum value of the Range.
     * @param current The current value of the Range.
     * @param max The maximum value of the Range.
     * @throw HKLException if not min < current < max; 
     */
    Range(const hkl::Value & min, const hkl::Value & current, const hkl::Value & max) throw(hkl::HKLException);

    Range(const Range & source);

    /*!
     * \brief Get the _min Value of the Range class.
     * \return The minimum Value.
     */
    inline const hkl::Value & get_min() const;

    /*!
     * \brief Get the _current Value of the Range class.
     * \return The current Value.
     */
    inline const hkl::Value & get_current() const;

    /*!
     * \brief Get the _max Value of the Range class.
     * \return The maximum Value.
     */
    inline const hkl::Value & get_max() const;

    /**
     * @brief Set the _current hkl::Value of the Range class.
     * @param current The hkl::Value to set.
     * @throw An HKLException if the current hkl::Value in not between min and max.
     */
    void set_current(const hkl::Value & current) throw(hkl::HKLException);

    /**
     * @brief Set the _current double of the Range class.
     * @param current The double to set.
     *
     * This method do not check for the validity of the Range. This method
     * is requiered by the simplex affinement.
     */
    void set_current(const double & current);

    /**
     * @brief Set the minimum and the maximum of the Range class.
     * @param min The minimum hkl::Value to set.
     * @param max The maximum hkl::Value to set.
     * @throw HKLException if the new Range is not valid.
     *
     * this method check that the new minimun is not bigger than the current
     * value of the Range and greater than the maximum.
     */
    void set_range(const hkl::Value & min, const hkl::Value & max) throw(hkl::HKLException);

    /**
     * @brief Set the minimum and the maximum of the Range class.
     * @param min The minimum double to set.
     * @param current The current double to set.
     * @param max The maximum double to set.
     * @throw HKLException if the new Range is not valid.
     *
     * this method do not check that the new minimun is not bigger than the current
     * value of the range and greater than the maximum.
     */
    void set(double min, double current, double max);

    /**
     * @brief Set a Range from another one.
     * @param range The Range to set.
     *
     * this method set only the _min, _current, _max Value of the Range.
     */
    void set(const Range & range);

    /**
     * @brief Multiply a Range by another one.
     * @param range The Range to multiply by.
     * @return A Range ref on the Range after the multiplication.
     *
     * This method modify min, current and max to reflect the multiplication.
     */
    
    Range & operator*=(const Range & range);

    /**
     * @brief Multiply a Range by a double value.
     * @param d The double value.
     * @return The Range after the multiplication.
     *
     * This method modify min, current and max to reflect the multiplication.
     */
    
    Range & operator*=(const double & d);

    /**
     * @brief check if the Range contain zero.
     * @return true if zero is include in between min, max.
     */
    
    bool contain_zero() const;

    /*!
     * \brief Are two Range equals ?
     * \param range the Range to compare with.
     */
    
    bool operator==(const Range & range) const;

    /*!
     * \brief print the Range into a flux
     * \param flux The stream to print into.
     */
    std::ostream & printToStream(std::ostream & flux) const;

    /*!
     * \brief Save the Range into a stream.
     * \param flux the stream to save the Range into.
     * \return The stream with the Range.
     */
    std::ostream & toStream(std::ostream & flux) const;

    /*!
     * \brief Restore a Range from a stream.
     * \param flux The stream containing the Range to restore.
     * @todo call update_observers or not ?
     */
    std::istream & fromStream(std::istream & flux);

};
/*!
 * \brief Get the _min Value of the Range class.
 * \return The minimum Value.
 */
inline const hkl::Value & Range::get_min() const 
{
  return _min;
}

/*!
 * \brief Get the _current Value of the Range class.
 * \return The current Value.
 */
inline const hkl::Value & Range::get_current() const 
{
  return _current;
}

/*!
 * \brief Get the _max Value of the Range class.
 * \return The maximum Value.
 */
inline const hkl::Value & Range::get_max() const 
{
  return _max;
}


} // namespace hkl

/*!
 * \brief Overload of the << operator for the Range clas
 * \param flux The ostream to modify.
 * \param range The range to print.
 * 
 * \return the modified ostream 
 */
inline std::ostream &
operator<<(std::ostream & flux, hkl::Range const & range)
{
  return range.printToStream(flux);
}

inline hkl::Range cos(hkl::Range const & range)
{
  hkl::Range res;

  double min = range.get_min().get_value();
  double current = range.get_current().get_value();
  double max = range.get_max().get_value();

  if (max - min >= 2 * hkl::constant::math::pi)
    res.set(-1, cos(current), 1);
  else
    {
      int quad_min = (int)floor(2 * min / hkl::constant::math::pi) % 4;
      if (quad_min < 0)
        quad_min += 4;

      int quad_max = (int)floor(2 * max / hkl::constant::math::pi) % 4;
      if (quad_max < 0)
        quad_max += 4;

      //cout << "quadrant : " << quad_min << ", " << quad_max << endl;
      switch (quad_max)
        {
        case 0:
          switch (quad_min)
            {
            case 0:
              res.set(cos(max), cos(current), cos(min));
              break;
            case 1:
              res.set(-1, cos(current), 1);
              break;
            case 2:
              res.set(cos(min), cos(current), 1);
              break;
            case 3:
              if (cos(min) < cos(max))
                res.set(cos(min), cos(current), 1);
              else
                res.set(cos(max), cos(current), 1);
              break;
            }
          break;
        case 1:
          switch (quad_min)
            {
            case 0:
            case 1:
              res.set(cos(max), cos(current), cos(min));
              break;
            case 2:
              if (cos(min) < cos(max))
                res.set(cos(min), cos(current), 1);
              else
                res.set(cos(max), cos(current), 1);
              break;
            case 3:
              res.set(cos(max), cos(current), 1);
              break;
            }
          break;
        case 2:
          switch (quad_min)
            {
            case 0:
              res.set(-1, cos(current), cos(min));
              break;
            case 1:
              if (cos(min) < cos(max))
                res.set(-1, cos(current), cos(max));
              else
                res.set(-1, cos(current), cos(min));
              break;
            case 2:
              res.set(cos(min), cos(current), cos(max));
              break;
            case 3:
              res.set(-1, cos(current), 1);
              break;
            }
          break;
        case 3:
          switch (quad_min)
            {
            case 0:
              if (cos(min) < cos(max))
                res.set(-1, cos(current), cos(max));
              else
                res.set(-1, cos(current), cos(min));
              break;
            case 1:
              res.set(-1, cos(current), cos(max));
              break;
            case 2:
            case 3:
              res.set(cos(min), cos(current), cos(max));
              break;
            }
          break;
        }
    }
  //cout << "cos   : " << res << endl;
  return res;
}

inline hkl::Range acos(hkl::Range const & range)
{
  double min = acos(range.get_max().get_value());
  double current = acos(range.get_current().get_value());
  double max = acos(range.get_min().get_value());

  return hkl::Range(min, current, max);
}
#endif
