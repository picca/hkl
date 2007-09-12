#ifndef _VALUE_H
#define _VALUE_H


#include <ostream>
#include <istream>

#include <cmath>
#include <iomanip>
#include "constant.h"
namespace hkl
  {

  /**
    * @brief the class use to store a double with an unit.
    */
  class Value
    {
    protected:
      double _value;


    public:
      /**
       * @brief The default constructor.
       */
      Value();

      /**
       * @brief A constructor from a double
       */
      Value(const double & value);

      Value(const Value & source);

      /**
       *  @brief The get accessor.
       */
      inline const double & get_value() const;

      /**
       *  @brief The set accessor
       *  @param value the Value to set.
       */
      void set_value(const double & value);

      bool operator==(const Value & value) const;

      bool operator!=(const Value & value) const;

      bool operator<=(const Value & value) const;

      bool operator>=(const Value & value) const;

      bool operator<(const Value & value) const;

      bool operator>(const Value & value) const;

      Value & operator+=(const Value & value);

      Value & operator-=(const Value & value);

      Value & operator*=(const Value & value);

      Value & operator/=(const Value & value);

      Value operator+(const Value & value) const;

      Value operator-(const Value & value) const;

      Value operator*(const Value & value) const;

      Value operator/(const Value & value) const;

      /*!
       * \brief print the Value into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /*!
       * \brief Save the Value into a stream.
       * \param flux the stream to save the Value into.
       * \return The stream with the Value.
       */
      std::ostream & toStream(std::ostream & flux) const;

      /*!
       * \brief Restore a Value from a stream.
       * \param flux The stream containing the Value to restore.
       */
      std::istream & fromStream(std::istream & flux);

    };
  /**
   *  @brief The get accessor.
   */
  inline const double & Value::get_value() const
    {
      return _value;
    }


} // namespace hkl

/**
 * \brief Surcharge de l'operateur << pour la class Value
 * @param flux
 * @param m
 * @return
 */
inline std::ostream & operator << (std::ostream & flux, hkl::Value const & value)
{
  return value.printToStream(flux);
}

inline hkl::Value
fabs(hkl::Value const & value)
{
  hkl::Value res(fabs(value.get_value()));
  return res;
}

inline hkl::Value
cos(hkl::Value const & value)
{
  hkl::Value res(cos(value.get_value()));
  return res;
}

inline double
operator/(double d, hkl::Value const & value)
{
  return d / value.get_value();
}

inline double
operator*(double d, hkl::Value const & value)
{
  return d * value.get_value();
}
#endif
