#ifndef _VALUE_H
#define _VALUE_H

#include <math.h>
#include <vector>
#include <iomanip>
#include <iostream>

#include "object.h"
#include "mystring.h"
#include "myvector.h"
#include "constants.h"

using namespace std;

namespace hkl {

  /**
   * @brief A class design to describe a rotation axe
   */
  class Value : public Object
  {
    public:

      /**
       * @brief default constructor
       */
      Value();

      /**
       * @brief constructor
       * @param name The name of the Value
       * @param value the value to put into m_value.
       */
      Value(MyString const & name, double value);

      /**
       * @brief Copy constructor
       * @param value A #Value to copy from.
       */
      Value(Value const & value);

      /**
       * @brief The default destructor
       */
      virtual ~Value(void);

      /**
       * @brief Get the current value of the #Value
       * @return The value
       */
      double const & get_value() const {return m_value;}

      /**
       * @brief Set the current value of the #Value
       * @param value The value to set.
       */
      void set_value(double value) {m_value = value;}

      /**
       * @brief Are two #Value equals ?
       * @param value the #Value to compare with
       * @return The comparison between the values.
       */
      bool operator ==(Value const & value) const;

      /**
       * @brief Add a value to another
       * @param value The #Value to add
       * @return The modified value
       */
      Value & operator +=(Value const & value);

      /**
       * @brief Substract a value to another
       * @param value The #Value to substract
       * @return The modified value
       */
      Value & operator -=(Value const & value);

      /**
       * @brief Multiply a value by a double
       * @param d The double to multiply by.
       * @return The modified value.
       */
      Value & operator *=(double const & d);

      /**
       * @brief Divide a value by a double
       * @param d The double to divide by.
       * @return The modified value.
       */
      Value & operator /=(double const & d);

      /**
       * @brief print the Value into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      ostream & printToStream(ostream & flux) const;
       
      /**
       * \brief Save the Value into a stream.
       * \param flux the stream to save the Value into.
       * \return The stream with the Value.
       */
      ostream & toStream(ostream & flux) const;
    
      /**
       * \brief Restore a Value from a stream.
       * \param flux The stream containing the Value.
       */
      istream & fromStream(istream & flux);
      
    private:
      double m_value; //< the value
  };

  typedef MyVector<Value> ValueList;

} // namespace hkl

/**
 * @brief Overload of the << operator for the #Value class
 * @param flux The flux to write into.
 * @param value The #Value to stream.
 * @return The modified flux.
 */
ostream & operator<<(ostream & flux, hkl::Value const & value); 

#endif // _VALUE_H
