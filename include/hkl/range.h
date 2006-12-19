#ifndef _RANGE_H
#define _RANGE_H

#include <iostream>

#include "HKLException.h"
#include "value.h"
#include "observer.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief A class design to describe a Range
   */
  class Range : public Observable
    {
    public:

      /**
       * @brief The default constructor.
       */
      Range(void);

      /**
       * @brief constructor of the range.
       * 
       * @param current The current value of the range
       * @param min The minimum value for current.
       * @param max The maximum value for current
       * @throw HKLException if not min < current < max; 
       */
      Range(Value const & current, Value const & min, Value const & max) throw (HKLException);

      /**
       * \brief Get the current minimum of the Range
       * \return The minimum
       */
      Value const & get_current(void) const
        {
          return _current;
        }

      /*!
       * \brief Get the current minimum of the Range
       * \return The minimum
       */
      Value const & get_min(void) const
        {
          return _min;
        }

      /*!
       * \brief Get the current maximum of the Range
       * \return The maximum
       */
      Value const & get_max(void) const
        {
          return _max;
        }

      /**
       * @brief Set the current value of the Range.
       * @param current The value to set.
       * @throw An HKLException if the current value in not between min and max.
       */
      void set_current(Value const & current) throw (HKLException);

      /**
       * @brief Set the current Value of the Range.
       * @param current The value to set.
       *
       * This method do not check for the validity of the range. This method
       * is requiered by the simplex affinement.
       */
      void set_current(double const & current);

      /**
       * @brief Set the minimum and the maximum of the Range.
       * @param min The minimumto set.
       * @param max The maximum to set.
       * @throw HKLException if the new Range is not valid.
       *
       * this method check that the new minimun is not bigger than the current
       * value of the range and greater than the maximum.
       */
      void set_range(Value const & min, Value const & max) throw (HKLException);


      /**
       * @brief Set a Range from another one.
       * @param range The Range use to set the vlue of the current Range.
       */
      void set(Range const & range);

      /**
       * @brief Set all the values of the range.
       * @param min The minimum value of the Range.
       * @param current The current value of the Range.
       * @param max The maximum value of the Range.
       *
       * This method do not check that the range is valid: min <= current <= max.
       */
      void set(double min, double current, double max);

      /**
       * @brief Multiply a Range by aother Range.
       * @param range The Range.
       * @return The Range after the multiplication.
       *
       * This method modify min, current and max to reflect the multiplication.
       */
      Range & operator *=(Range const & range);

      /**
       * @brief Multiply a Range by a double value.
       * @param range The double value.
       * @return The Range after the multiplication.
       *
       * This method modify min, current and max to reflect the multiplication.
       */
      Range & operator *=(double const & range);

      /**
       * @brief check if the interval contain zero.
       * @return true if zero is include in between min, max.
       */
      bool contain_zero(void) const;

      /*!
       * \brief Are two Range equals ?
       * \param range the Range to compare with
       */
      bool operator ==(Range const & range) const;

      /*!
       * \brief print the Range into a flux
       * \param flux The stream to print into.
       */
      ostream & printToStream(ostream & flux) const;

      /*!
       * \brief Save the Range into a stream.
       * \param flux the stream to save the Range into.
       * \return The stream with the Range.
       */
      ostream & toStream(ostream & flux) const;

      /*!
       * \brief Restore a Range from a stream.
       * \param flux The stream containing the Range.
       */
      istream & fromStream(istream & flux);

    private:
      Value _current; //!< the current position in the range.
      Value _min; //!< the minimum of the range.
      Value _max; //!< the maximum of the range.
    };

} // namespace hkl


/*!
 * \brief Overload of the << operator for the Range clas
 * \param flux The ostream to modify.
 * \param range The range to print.
 * 
 * \return the modified ostream 
 */
inline ostream &
operator<<(ostream & flux, hkl::Range const & range)
{
  return range.printToStream(flux);
}

hkl::Range cos(hkl::Range const & range);

hkl::Range acos(hkl::Range const & range);

#endif // _RANGE_H
