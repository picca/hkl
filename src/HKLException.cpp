
#include "HKLException.h"


#define DEBUG
#ifdef DEBUG
# include <iostream>
#endif
namespace hkl {

Error::Error() :
  reason ("unknown"),
  desc ("unknown error"),
  origin ("unknown"),
  severity (ERR)
{
  // Bouml preserved body begin 0001F553
  // Bouml preserved body end 0001F553
}

Error::Error(const char * _reason, const char * _desc, const char * _origin, int _severity) :
  reason (_reason),
  desc (_desc),
  origin (_origin),
  severity (_severity)
{
  // Bouml preserved body begin 0001F5D3
  // Bouml preserved body end 0001F5D3
}

Error::Error(const std::string & _reason, const std::string & _desc, const std::string & _origin, int _severity) : 
  reason (_reason),
  desc (_desc),
  origin (_origin),
  severity (_severity)
{
  // Bouml preserved body begin 0001F653
  // Bouml preserved body end 0001F653
}

Error::Error(const hkl::Error & error) :
    reason (error.reason),
    desc (error.desc),
    origin (error.origin),
    severity (error.severity)
{
  // Bouml preserved body begin 0001F6D3
  // Bouml preserved body end 0001F6D3
}

Error::~Error() 
{
  // Bouml preserved body begin 0001F753
  // Bouml preserved body end 0001F753
}

hkl::Error & Error::operator=(const hkl::Error & error) 
{
  // Bouml preserved body begin 0001F7D3
      if (this == &error)
        return *this;
      
      this->reason = error.reason;
      this->desc = error.desc;
      this->origin = error.origin;
      this->severity = error.severity;
      
      return *this;
  // Bouml preserved body end 0001F7D3
}

HKLException::HKLException() :
  errors(0)
{
  // Bouml preserved body begin 0001F853
      this->push_error(Error());
  // Bouml preserved body end 0001F853
}

HKLException::HKLException(const char * _reason, const char * _desc, const char * _origin, int _severity) :
  errors(0) 
{
  // Bouml preserved body begin 0001F582
#ifdef DEBUG
      std::cout << std::endl
      << _reason << std::endl
      << _desc << std::endl
      << _origin << std::endl;
#endif
      this->push_error(Error(_reason, _desc, _origin, _severity));
  // Bouml preserved body end 0001F582
}

HKLException::HKLException(const std::string & _reason, const std::string & _desc, const std::string & _origin, int _severity) :
  errors(0)
{
  // Bouml preserved body begin 0001F602
#ifdef DEBUG
      std::cout << std::endl
      << _reason << std::endl
      << _desc << std::endl
      << _origin << std::endl;
#endif
      this->push_error(Error(_reason, _desc, _origin, _severity));
  // Bouml preserved body end 0001F602
}

HKLException::HKLException(const hkl::HKLException & _src) :
  errors(0)
{
  // Bouml preserved body begin 0001F482
      for (unsigned int i = 0; i < _src.errors.size();  i++)
        this->push_error(_src.errors[i]);
  // Bouml preserved body end 0001F482
}

HKLException::~HKLException() 
{
  // Bouml preserved body begin 0001F402
      this->errors.clear();
  // Bouml preserved body end 0001F402
}

hkl::HKLException & HKLException::operator=(const hkl::HKLException & _src) 
{
  // Bouml preserved body begin 0001F502
      if (this == &_src)
        return *this;
      
      this->errors.clear();
      
      for (unsigned int i = 0; i < _src.errors.size();  i++)
        this->push_error(_src.errors[i]);
      
      return *this;
  // Bouml preserved body end 0001F502
}

void HKLException::push_error(char * _reason, char * _desc, char * _origin, int _severity) 
{
  // Bouml preserved body begin 0001F682
      this->errors.push_back(Error(_reason, _desc, _origin, _severity));
  // Bouml preserved body end 0001F682
}

void HKLException::push_error(std::string _reason, std::string _desc, std::string _origin, int _severity) 
{
  // Bouml preserved body begin 0001F702
      this->errors.push_back(Error(_reason, _desc, _origin, _severity));
  // Bouml preserved body end 0001F702
}

void HKLException::push_error(const hkl::Error & _error) 
{
  // Bouml preserved body begin 0001F782
      this->errors.push_back(_error);
  // Bouml preserved body end 0001F782
}


} // namespace hkl
