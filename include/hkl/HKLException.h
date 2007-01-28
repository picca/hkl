#ifndef _HKLEXCEPTION_H
#define _HKLEXCEPTION_H


#include <string>
using namespace std;
#include <vector>
using namespace std;

#include <sstream>

#define HKLEXCEPTION(a,b)  do {\
    std::ostringstream location;\
    location << __FILE__ << " line : " << __LINE__;\
    throw HKLException((a),(b),location.str());\
} while(0)

namespace hkl {

enum ErrorSeverity {
  WARN,
  ERR,
  PANIC
};
class Error {
  public:
    string reason;

    string desc;

    string origin;

    int severity;

    Error();

    Error(const char * _reason, const char * _desc, const char * _origin, int _severity = ERR);

    Error(const string & _reason, const string & _desc, const string & _origin, int _severity = ERR);

    Error(const Error & error);

    virtual ~Error();

    Error & operator=(const Error & error);

};
typedef vector<Error> ErrorList;
class HKLException {
  public:
    ErrorList errors;

    HKLException();

    HKLException(const char * _reason, const char * _desc, const char * _origin, int _severity = ERR);

    HKLException(const string & _reason, const string & _desc, const string & _origin, int _severity = ERR);

    HKLException(const HKLException & _src);

    ~HKLException();

    HKLException & operator=(const HKLException & _src);

    void push_error(char * _reason, char * _desc, char * _origin, int _severity = ERR);

    void push_error(string _reason, string _desc, string _origin, int _severity = ERR);

    void push_error(const Error & _error);

};

} // namespace hkl
#endif
