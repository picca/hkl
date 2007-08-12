#ifndef _HKLEXCEPTION_H
#define _HKLEXCEPTION_H


#include <string>

#include <vector>


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
    std::string reason;

    std::string desc;

    std::string origin;

    int severity;

    Error();

    Error(const char * _reason, const char * _desc, const char * _origin, int _severity = ERR);

    Error(const std::string & _reason, const std::string & _desc, const std::string & _origin, int _severity = ERR);

    Error(const Error & error);

    virtual ~Error();

    Error & operator=(const Error & error);

};
typedef std::vector<hkl::Error> ErrorList;
class HKLException {
  public:
    hkl::ErrorList errors;

    HKLException();

    HKLException(const char * _reason, const char * _desc, const char * _origin, int _severity = ERR);

    HKLException(const std::string & _reason, const std::string & _desc, const std::string & _origin, int _severity = ERR);

    HKLException(const HKLException & _src);

    ~HKLException();

    HKLException & operator=(const HKLException & _src);

    void push_error(char * _reason, char * _desc, char * _origin, int _severity = ERR);

    void push_error(std::string _reason, std::string _desc, std::string _origin, int _severity = ERR);

    void push_error(const hkl::Error & _error);

};

} // namespace hkl
#endif
