#include "objectwithparameters.h"

namespace hkl {
  
  ObjectWithParameters::ObjectWithParameters(void)
  : Object()
  {}

  ObjectWithParameters::ObjectWithParameters(ObjectWithParameters const & objectWithParameters)
    : Object(objectWithParameters)
  {}
    
  ObjectWithParameters::~ObjectWithParameters(void)
  {}

  bool
  ObjectWithParameters::operator == (ObjectWithParameters const & objectWithParameters) const
  {
    return Object::operator==(objectWithParameters)
      && m_valueList == objectWithParameters.m_valueList;
  }

  ostream & 
  ObjectWithParameters::printToStream(ostream & flux) const
  { 
    Object::printToStream(flux);
    flux << m_valueList << endl;

    return flux;
  }

  vector<string>
  ObjectWithParameters::getParametersNames(void) const
  {
    return m_valueList.getNames();
  }
  
  double const &
  ObjectWithParameters::getParameterValue(string const & name) const
  {
    return m_valueList[name].get_value();
  }

  void
  ObjectWithParameters::setParameterValue(string const & name, double const & value)
  {
    m_valueList[name].set_value(value);
  }

  void
  ObjectWithParameters::addParameter(string const & name) throw (HKLException)
  {
    m_valueList.add(Value(name, 0.));
  }

} // namespace hkl

ostream &
operator << (ostream & flux, hkl::ObjectWithParameters const & objectWithParameters)
{
  return objectWithParameters.printToStream(flux);
}
