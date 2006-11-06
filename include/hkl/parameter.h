#ifndef _PARAMETER_H
#define _PARAMETER_H

#include <iostream>

#include "portability.h"

#include "myvector.h"
#include "range.h"
#include "object.h"
#include "HKLException.h"

using namespace std;

namespace hkl
  {

  /**
   * \brief A class design to describe a named range to store parameter values.
   *
   * Once the parameter is created you can not change its name nor its description.
   */
  class Parameter : public ObjectReadOnly, public Range
    {
    public:

      Parameter(MyString const & name, MyString const & description,
                Value const & current, Value const & min, Value const & max) throw (HKLException);

      /*!
       * \brief Are two Range equals ?
       * \param range the Range to compare with
       */
      bool operator ==(Parameter const & parameter) const;

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
    };

#ifdef MSVC6
  typedef MyStarVector<Parameter *> ParameterList;
#else
  typedef MyVector<Parameter *> ParameterList;
#endif

} // namespace hkl

/*!
 * \brief Overload of the << operator for the Parameter class
 * \param flux The ostream to modify.
 * \param parameter The Parameter to print.
 * 
 * \return the modified ostream 
 */
inline ostream &
operator<<(ostream & flux, hkl::Parameter const & parameter)
{
  return parameter.printToStream(flux);
}

#endif // _PARAMETER_H
