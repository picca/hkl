#ifndef _PARAMETER_H
#define _PARAMETER_H

#include "object.h"
#include "range.h"

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
              Value const & min, Value const & current, Value const & max) throw (HKLException);

    /*!
     * \brief Are two Parameter equals ?
     * \param parameter the Parameter to compare with
     */
    bool operator ==(Parameter const & parameter) const;

    /*!
     * \brief print the Parameter into a flux
     * \param flux The stream to print into.
     */
    ostream & printToStream(ostream & flux) const;

    /*!
     * \brief Save the Parameter into a stream.
     * \param flux the stream to save the Parameter into.
     * \return The stream with the Parameter.
     */
    ostream & toStream(ostream & flux) const;

    /*!
     * \brief Restore a Parameter from a stream.
     * \param flux The stream containing the Parameter.
     */
    istream & fromStream(istream & flux);
};

} // namespace hkl

/*!
 * \brief Overload of the << operator for the Parameter class
 * \param flux The ostream to modify.
 * \param parameter The Parameter to print.
 * \return the modified ostream 
 */
inline ostream &
operator<<(ostream & flux, hkl::Parameter const & parameter)
{
    return parameter.printToStream(flux);
}

#endif // _PARAMETER_H
