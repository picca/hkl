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

    vector<MyString>
    ObjectWithParameters::getParametersNames(void) const
      {
        return m_valueList.getNames();
      }

    double const &
    ObjectWithParameters::getParameterValue(MyString const & name) const
      {
        return m_valueList[name].get_value();
      }

    void
    ObjectWithParameters::setParameterValue(MyString const & name, double const & value)
      {
        m_valueList[name].set_value(value);
      }

    void
    ObjectWithParameters::addParameter(MyString const & name) throw (HKLException)
      {
        m_valueList.add(Value(name, 0.));
      }

    ostream &
    ObjectWithParameters::toStream(ostream & flux) const
      {
        Object::toStream(flux);
        m_valueList.toStream(flux);

        return flux;    
      }

    istream &
    ObjectWithParameters::fromStream(istream & flux)
      {
        Object::fromStream(flux);
        m_valueList.fromStream(flux);

        return flux;
      }

} // namespace hkl

ostream &
operator << (ostream & flux, hkl::ObjectWithParameters const & objectWithParameters)
{
    return objectWithParameters.printToStream(flux);
}
