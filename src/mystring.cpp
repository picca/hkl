#include "portability.h"

#include "mystring.h"

namespace hkl {

    MyString::MyString(void)
    {}

    MyString::MyString(char const * mystring) : _string(mystring)
    {}

    MyString::MyString(MyString const & s) : _string(s._string)
    {}

    unsigned int
    MyString::size(void) const 
      {
        return _string.size();
      }

    bool
    MyString::operator <(MyString const & myString) const
    {
      return _string < myString._string;
    }

    bool
    MyString::operator ==(MyString const & myString) const
    {
      return _string == myString._string;
    }

    ostream &
    MyString::printToStream(ostream & flux) const
    {
      flux << _string;
      return flux;
    }

    /**
     * @brief print on a stream the content of the MyString
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & 
    MyString::toStream(ostream  & flux) const
      {
        flux << " " << _string.size()
        << " " << _string;
        return flux;
      }

    /**
     * @brief restore the content of the MyString from an istream
     * @param flux the istream.
     * @return the modified istream.
     */
    istream &
    MyString::fromStream(istream  & flux)
      {
        unsigned int size;

        flux >> size;
        //remove the first space
        flux.get();
        char * chaine = (char *)malloc(size+1);
        flux.read(chaine, size);
        _string.assign(chaine, size);
        free(chaine);
        //cout << "* " << dynamic_cast<stringstream &>(flux).str();
        return flux;
      }

} // namespace hkl
