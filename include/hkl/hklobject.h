#ifndef _HKLOBJECT_H_
#define _HKLOBJECT_H_

#include <iostream>

#include "object.h"
#include "mystring.h"
#include "parameter.h"
#include "HKLException.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief Class used to store a object with scalars parameters.
   */
  class HKLObject : public ObjectReadOnly
    {
    public:

      /**
      * @brief The default constructor
      * 
      * @param name The name of the HKLObject.
      * @param description The description of the HKLObject.
      * @throw HKLException if the name and/or the description are wrong. 
      */
      HKLObject(MyString const & name, MyString const & description) throw (HKLException);

      /*!
       * \brief get the ValueList of the ObjectWithParameters.
       * \return The ValueList of the ObjectWithParameters.
       */
      ParameterList & parameters(void)
      {
        return _parameters;
      }

      /*!
       * \brief Are two ObjectWithParameters equals ?
       * \param objectWithParameters the ObjectWithParameters to compare with.
       * \return True if both are equals, false otherwise.
       */
      bool operator ==(HKLObject const & hklObject) const;

      /*!
       * \brief print the ObjectWithParameters into a flux
       * \param flux The stream to print into.
       * \return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /*!
       * \brief Save the ObjectWithParameters into a stream.
       * \param flux the stream to save the ObjectWithParameters into.
       * \return The stream with the ObjectWithParameters.
       */
      ostream & toStream(ostream & flux) const;

      /*!
       * \brief Restore an ObjectWithParameters from a stream.
       * \param flux The stream containing the ObjectWithParameters.
       * \return The modified stream.
       */
      istream & fromStream(istream & flux);

    private:

      ParameterList _parameters; //!< values store in the object.
    };

} // namespace hkl

inline ostream &
operator << (ostream & flux, hkl::HKLObject const & hklObject)
{
  return hklObject.printToStream(flux);
}

#endif // _HKLOBJECT_H_
