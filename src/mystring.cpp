
#include "mystring.h"

namespace hkl {

/**
 * @brief The default constructor.
 */
MyString::MyString() :
  string()
{
  // Bouml preserved body begin 00022882
  // Bouml preserved body end 00022882
}

/**
 * @brief A constructor from a double
 */
MyString::MyString(char const * s) :
  string(s)
{
  // Bouml preserved body begin 00022902
  // Bouml preserved body end 00022902
}

/**
 * @brief A constructor from a double
 */
MyString::MyString(std::string const & s) :
  string(s)
{
  // Bouml preserved body begin 00023282
  // Bouml preserved body end 00023282
}

/**
 * @brief A constructor from a double
 */
MyString::MyString(const hkl::MyString & s) :
  string(s) 
{
  // Bouml preserved body begin 00023302
  // Bouml preserved body end 00023302
}

/**
 * @brief print on a stream the content of the MyString
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
ostream & MyString::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00023182
      flux << " " << string::size()
           << " " << string::c_str();
      return flux;
  // Bouml preserved body end 00023182
}

/**
 * @brief restore the content of the MyString from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
istream & MyString::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00023202
      unsigned int size;
      
      flux >> size;
      //remove the first space
      flux.get();
      char * chaine = (char *)malloc(size+1);
      flux.read(chaine, size);
      string::assign(chaine, size);
      free(chaine);
      return flux;
  // Bouml preserved body end 00023202
}


} // namespace hkl
