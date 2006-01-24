#ifndef _MYSTRING_H_
#define _MYSTRING_H_

#include <string>
#include <iostream>

#include "HKLException.h"

using namespace std;

namespace hkl {

  class MyString : public string
  {
    public:
      MyString(void);
      MyString(char const * mystring);
      ostream & toStream(ostream & flux) const;
      istream & fromStream(istream & flux);
  };
  
} // namespace hkl

#endif //_MYSTRING_H_
