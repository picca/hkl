
#include "HKLException.h"

namespace hkl {

Error::Error() :
  reason ("unknown"),
  desc ("unknown error"),
  origin ("unknown"),
  severity (ERR)
{
}

Error::Error(const char * _reason, const char * _desc, const char * _origin, int _severity) :
  reason (_reason),
  desc (_desc),
  origin (_origin),
  severity (_severity)
{
}

Error::Error(const string & _reason, const string & _desc, const string & _origin, int _severity) : 
  reason (_reason),
  desc (_desc),
  origin (_origin),
  severity (_severity)
{
}

Error::Error(const Error & error) :
    reason (error.reason),
    desc (error.desc),
    origin (error.origin),
    severity (error.severity)
{
}

Error::~Error() 
{
}

Error & Error::operator=(const Error & error) 
{
  if (this == &error)
    return *this;
  
  this->reason = error.reason;
  this->desc = error.desc;
  this->origin = error.origin;
  this->severity = error.severity;
  
  return *this;
}

HKLException::HKLException() :
  errors(0)
{
  this->push_error(Error());
}

HKLException::HKLException(const char * _reason, const char * _desc, const char * _origin, int _severity) :
  errors(0) 
{
#ifdef DEBUG
  std::cout << std::endl
  << _reason << std::endl
  << _desc << std::endl
  << _origin << std::endl;
#endif
  this->push_error(Error(_reason, _desc, _origin, _severity));
}

HKLException::HKLException(const string & _reason, const string & _desc, const string & _origin, int _severity) :
  errors(0)
{
#ifdef DEBUG
  std::cout << std::endl
  << _reason << std::endl
  << _desc << std::endl
  << _origin << std::endl;
#endif
  this->push_error(Error(_reason, _desc, _origin, _severity));
}

HKLException::HKLException(const HKLException & _src) :
  errors(0)
{
  for (unsigned int i = 0; i < _src.errors.size();  i++)
    this->push_error(_src.errors[i]);
}

HKLException::~HKLException() 
{
  this->errors.clear();
}

HKLException & HKLException::operator=(const HKLException & _src) 
{
  if (this == &_src)
    return *this;
  
  this->errors.clear();
  
  for (unsigned int i = 0; i < _src.errors.size();  i++)
    this->push_error(_src.errors[i]);
  
  return *this;
}

void HKLException::push_error(char * _reason, char * _desc, char * _origin, int _severity) 
{
  this->errors.push_back(Error(_reason, _desc, _origin, _severity));
}

void HKLException::push_error(string _reason, string _desc, string _origin, int _severity) 
{
  this->errors.push_back(Error(_reason, _desc, _origin, _severity));
}

void HKLException::push_error(const Error & _error) 
{
  this->errors.push_back(_error);
}


} // namespace hkl
