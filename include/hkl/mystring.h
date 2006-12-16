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
class MyString
{
public:
    MyString(void); //!< Default constructor

    /*!
     * \brief Constructor from a string
     * \param s The orifianl string
     */
    MyString(MyString const & s);

    /*!
     * \brief Copy constructor
     * \param myString The MyString to copy from.
     */
    MyString(char const * myString);

    unsigned int size(void) const;

    ostream & printToStream(ostream & flux) const;

    bool operator<(MyString const & myString) const;

    bool operator==(MyString const & myString) const;

    /*!
     * \brief Save the content of the MyString into a stream.
     * \param flux The stream.
     * \return The modified stream.
     */
    ostream & toStream(ostream & flux) const;

    /*!
     * \brief Restore the MyString from a stream previously filled with toStream.
     * \param flux The stream.
     * \return The modified stream.
     */
    istream & fromStream(istream & flux);
private:
    string _string;
};

} // namespace hkl

static ostream &
operator <<(ostream & flux, hkl::MyString const & myString)
{
    return myString.printToStream(flux);
}
#endif //_MYSTRING_H_
