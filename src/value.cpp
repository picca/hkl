#include "value.h"

namespace hkl {

    Value::Value(void)
    : Object()
      {}

    Value::Value(MyString const & name, double value) throw (HKLException)
    : Object(name),
    m_value(value)
      {}

    Value::Value(Value const & value)
    : Object(value),
    m_value(value.m_value)
      {}

    Value::~Value(void)
      {}


    bool
    Value::operator == (Value const & value) const
      {
        return Object::operator==(value)
        && fabs(m_value - value.m_value) < constant::math::epsilon_1;
      }


    Value &
    Value::operator += (Value const & value)
      {
        m_value += value.m_value;
        return *this;
      }

    Value &
    Value::operator -= (Value const & value)
      {
        m_value -= value.m_value;
        return *this;
      }

    Value &
    Value::operator /= (double const & d)
      {
        m_value /= d;
        return *this;
      }

    Value &
    Value::operator *= (double const & d)
      {
        m_value *= d;
        return *this;
      }

    ostream & 
    Value::printToStream(ostream & flux) const
      {  
        Object::printToStream(flux);

        flux  << " Value: " << m_value << endl;

        return flux;
      }

    ostream &
    Value::toStream(ostream & flux) const
      {
        Object::toStream(flux);
        flux << setprecision(constant::math::precision) << " " << m_value;

        return flux;    
      }

    istream &
    Value::fromStream(istream & flux)
      {
        Object::fromStream(flux);
        flux >> setprecision(constant::math::precision) >> m_value;

        return flux;
      }

} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::Value const & value)
{
    return value.printToStream(flux);
}
