#include "portability.h"

#include "mystring.h"

namespace hkl
  {

  MyString::MyString(void) : string()
  {}

  MyString::MyString(char const * s) : string(s)
  {}

  MyString::MyString(MyString const & myString) : string(myString)
  {}

  /**
   * @brief print on a stream the content of the MyString
   * @param flux the ostream to modify.
   * @return the modified ostream
   */
  ostream &
  MyString::toStream(ostream  & flux) const
    {
      flux << " " << string::size()
      << " " << string::c_str();
      return flux;
    }

  /**
   * @brief restore the content of the MyString from an istream
   * @param flux the istream.
   * @return the modified istream.
   * @todo problem of security here.
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
    string::assign(chaine, size);
    free(chaine);
    return flux;
  }

} // namespace hkl
