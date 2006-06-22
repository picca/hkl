# include "HKLException.h"

Error::Error(void) :
  reason ("unknown"),
  desc ("unknown error"),
  origin ("unknown"),
  severity (ERR)
{}

Error::Error(const char * _reason,
             const char * _desc,
             const char * _origin,
             int _severity) :
  reason (_reason),
  desc (_desc),
  origin (_origin),
  severity (_severity)
{}

Error::Error(const std::string & _reason,
             const std::string & _desc,
             const std::string & _origin,
             int _severity) :
  reason (_reason),
  desc (_desc),
  origin (_origin),
  severity (_severity)
{}

Error::Error(const Error & _src) :
  reason (_src.reason),
  desc (_src.desc),
  origin (_src.origin),
  severity (_src.severity)
{}

Error::~Error(void)
{}

Error &
Error::operator=(const Error & _src) 
{
    if (this == &_src)
        return *this;

    this->reason = _src.reason;
    this->desc = _src.desc;
    this->origin = _src.origin;
    this->severity = _src.severity;

    return *this;
}

HKLException::HKLException(void) :
  errors(0)
{
    this->push_error(Error());
}

HKLException::HKLException(const char *_reason,
                           const char *_desc,
                           const char *_origin,
                           int _severity) :
  errors(0)
{
    this->push_error(Error(_reason, _desc, _origin, _severity));
}

HKLException::HKLException(const std::string & _reason,
                           const std::string & _desc,
                           const std::string & _origin,
                           int _severity) :
  errors(0)
{
    this->push_error(_reason, _desc, _origin, _severity);
}

HKLException::HKLException(const HKLException & _src) :
  errors(0)
{
    for (unsigned int i = 0; i < _src.errors.size();  i++)
        this->push_error(_src.errors[i]);
}

HKLException &
HKLException::operator=(const HKLException & _src) 
{
    if (this == &_src)
        return *this;

    this->errors.clear();

    for (unsigned int i = 0; i < _src.errors.size();  i++)
        this->push_error(_src.errors[i]);

    return *this;
}

HKLException::~HKLException(void)
{
    this->errors.clear();
}


void
HKLException::push_error(const char * _reason,
                         const char * _desc,
                         const char * _origin, 
                         int _severity)
{
    this->errors.push_back(Error(_reason, _desc, _origin, _severity));
}

void
HKLException::push_error(const std::string & _reason,
                         const std::string & _desc,
                         const std::string & _origin, 
                         int _severity)
{
    this->errors.push_back(Error(_reason, _desc, _origin, _severity));
}

void
HKLException::push_error(const Error & _error)
{
    this->errors.push_back(_error);
}
