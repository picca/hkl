#ifndef _VALUE_H
#define _VALUE_H

#include <iostream>
#include <cmath>

#include "constants.h"

using namespace std;

namespace hkl {

  /*!
   * \brief A class design to store a scalar value.
   */
  class Value
  {
    public:

      /** 
      * @brief The default constructor.
      */
      Value(void);

      /** 
      * @brief Another constructor
      * 
      * @param value The value to set.
      */
      Value(double value);

      /*!
       * \brief Get the current value of the #Value
       * \return The value
       */
      double const & get_value(void) const {return _value;}

      /*!
       * \brief Set the current value of the #Value
       * \param value The value to set.
       */
      void set_value(double value) {_value = value;}

      /*!
       * \brief Are two Value equals ?
       * \param value the Value to compare with
       * \return true if both are equals, false otherwise.
       */
      bool operator ==(Value const & value) const;

      /** 
      * @brief is it <= value
      * @param value The value to compare with.
      * @return true if *this < value.
      */
      bool operator <=(Value const & value) const;

      /*!
       * \brief Add a value to another
       * \param value The Value to add
       * \return The modified value
       */
      Value & operator +=(Value const & value);

      /*!
       * \brief Substract a value to another
       * \param value The Value to substract
       * \return The modified value
       */
      Value & operator -=(Value const & value);

      /*!
       * \brief Multiply a value by a double
       * \param d The Value to multiply by.
       * \return The modified value.
       */
      Value & operator *=(Value const & value);

      /*!
       * \brief Divide a value by a double
       * \param d The Value to divide by.
       * \return The modified value.
       */
      Value & operator /=(Value const & value);

      /*!
       * \brief Add a value to another
       * \param value The Value to add
       * \return The modified value
       */
      Value operator +(Value const & value) const;

      /*!
       * \brief Substract a value to another
       * \param value The Value to substract
       * \return The modified value
       */
      Value operator -(Value const & value) const;

      /*!
       * \brief Multiply a value by a double
       * \param d The Value to multiply by.
       * \return The modified value.
       */
      Value operator *(Value const & value) const;

      /*!
       * \brief Divide a value by a double
       * \param d The Value to divide by.
       * \return The modified value.
       */
      Value operator /(Value const & value) const;

      /*!
       * \brief print the Value into a flux
       * \param flux The stream to print into.
       * \return The modified flux.
       */
      ostream & printToStream(ostream & flux) const;
       
      /*!
       * \brief Save the Value into a stream.
       * \param flux the stream to save the Value into.
       * \return The stream with the Value.
       */
      ostream & toStream(ostream & flux) const;
    
      /*!
       * \brief Restore a Value from a stream.
       * \param flux The stream containing the Value.
       * \return The modified stream.
       */
      istream & fromStream(istream & flux);
      
    protected:
      double _value; //!< the value.
  };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the Value class
 * \param flux The flux to write into.
 * \param value The Value to stream.
 * \return The modified flux.
 */
inline ostream &
operator<<(ostream & flux, hkl::Value const & value)
{
    return value.printToStream(flux);
}

#endif // _VALUE_H
