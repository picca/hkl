#ifndef _MYSTRING_H
#define _MYSTRING_H


#include <string>

#include <iostream>
using namespace std;

namespace hkl {

/**
  * @brief the class use to store a double with an unit.
  */
class MyString : public std::string {
  public:
    /**
     * @brief The default constructor.
     */
    MyString();

    /**
     * @brief A constructor from a double
     */
    MyString(char const * s);

    /**
     * @brief A constructor from a double
     */
    MyString(std::string const & s);

    /**
     * @brief A constructor from a double
     */
    MyString(const MyString & s);

    /**
     * @brief print on a stream the content of the MyString
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the MyString from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};

} // namespace hkl
#endif
