#ifndef _MYSTRING_H_
#define _MYSTRING_H_

#include <string>
#include <iostream>

#include "HKLException.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief Class use to store a string with a clever serialisation mechanism
   */
  class MyString : public string
    {
    public:
      MyString(void); //!< Default constructor

      /**
       * @brief Constructor from a MyString
       * @param myString The copy from.
       */
      MyString(MyString const & myString);

      /**
       * @brief Copy constructor
       * @param s The MyString to copy from.
       */
      MyString(char const * s);

      /**
       * @brief Save the content of the MyString into a stream.
       * @param flux The stream.
       * @return The modified stream.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore the MyString from a stream previously filled with toStream.
       * @param flux The stream.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);
    };

} // namespace hkl

#endif //_MYSTRING_H_
