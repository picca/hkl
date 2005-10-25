#include "objectwithparameters.h"

namespace hkl {
  
  ObjectWithParameters::ObjectWithParameters(void) :
    Object()
  {}

  ObjectWithParameters::~ObjectWithParameters(void)
  {}

  bool
  ObjectWithParameters::operator == (ObjectWithParameters const & objectWithParameters) const
  {
    return Object::operator==(objectWithParameters)
      && m_valueList == objectWithParameters.m_valueList;
  }

  std::ostream & 
  ObjectWithParameters::printToStream(std::ostream & flux) const
  { 
    Object::printToStream(flux);
    flux
      << m_valueList << std::endl;

    return flux;
  }

  std::vector<std::string>
  ObjectWithParameters::getParameterNames(void) const
  {
    return m_valueList.getNames();
  }
  
  double const &
  ObjectWithParameters::getParameterValue(std::string const & name) const
  {
    return m_valueList[name].get_value();
  }

  void
  ObjectWithParameters::setParameterValue(std::string const & name, double const & value)
  {
    m_valueList[name].set_value(value);
  }

  void
  ObjectWithParameters::addParameter(std::string const & name) throw (HKLException)
  {
    m_valueList.add(Value(name, 0.));
  }

} // namespace hkl

std::ostream &
operator << (std::ostream & flux, hkl::ObjectWithParameters const & objectWithParameters)
{
  return objectWithParameters.printToStream(flux);
}
