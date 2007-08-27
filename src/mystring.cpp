
#include "mystring.h"

namespace hkl {

/**
 * @brief The default constructor.
 */
MyString::MyString() :
#if ( _MSC_VER && _MSC_VER <= 1200)
  string()
#else
  std::string()
#endif
{
  // Bouml preserved body begin 00022882
  // Bouml preserved body end 00022882
}

/**
 * @brief A constructor from a double
 */
MyString::MyString(char const * s) :
  std::string(s)
{
  // Bouml preserved body begin 00022902
  // Bouml preserved body end 00022902
}

/**
 * @brief A constructor from a double
 */
MyString::MyString(std::string const & s) :
  std::string(s)
{
  // Bouml preserved body begin 00023282
  // Bouml preserved body end 00023282
}

/**
 * @brief A constructor from a double
 */
MyString::MyString(const hkl::MyString & s) :
  std::string(s) 
{
  // Bouml preserved body begin 00023302
  // Bouml preserved body end 00023302
}

/**
 * @brief print on a stream the content of the MyString
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
std::ostream & MyString::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00023182
      flux << " " << std::string::size()
           << " " << std::string::c_str();
      return flux;
  // Bouml preserved body end 00023182
}

/**
 * @brief restore the content of the MyString from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
std::istream & MyString::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00023202
      unsigned int size;
      
      flux >> size;
      //remove the first space
      flux.get();
      char * chaine = (char *)malloc(size+1);
      flux.read(chaine, size);
      std::string::assign(chaine, size);
      free(chaine);
      return flux;
  // Bouml preserved body end 00023202
}


} // namespace hkl
