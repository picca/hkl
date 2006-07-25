#include "object.h"

#include <sstream>

namespace hkl {

    Object::Object(void)
    : m_name("no name"),
    m_description("no description")
      {}

    Object::Object(MyString const & name) throw (HKLException)
    : m_description("no description")
      {
        set_name(name);
      }

    Object::Object(MyString const & name, MyString const & description) throw (HKLException)
      {
        set_name(name);
        set_description(description);
      }

    Object::Object(Object const & object) :
      m_name(object.m_name),
      m_description(object.m_description)
    {}

    Object::~Object(void)
      {}

    bool
    Object::operator == (Object const & object) const
      {
        return m_name == object.m_name
        && m_description == object.m_description;
      }

    void
    Object::set_name(MyString const & name) throw (HKLException)
      {
        //verification that the name is a valid name.
        if (name.size() == 0)
          {
            ostringstream reason;
            reason << "Can not set the " << typeid(*this).name() << " name with an empty name";
            HKLEXCEPTION(reason.str(),
                         "Please set with a non empty name");
          }
        else
            m_name = name;
      }

    void
    Object::set_description(MyString const & description) throw (HKLException)
      {
        //verification that the name is a valid name.
        if (description.size() == 0)
          {
            ostringstream reason;
            reason << "Can not set the " << typeid(*this).name() << " description with an empty description";
            HKLEXCEPTION(reason.str(),
                         "Please set with a non empty description");
          }
        else
            m_description = description;
      }

    ostream & 
    Object::printToStream(ostream & flux) const
      { 
        flux
        << " Name: " << m_name << endl
        << " Description: " << m_description << endl;

        return flux;
      }

    ostream &
    Object::toStream(ostream & flux) const throw (HKLException)
      {
        m_name.toStream(flux);
        m_description.toStream(flux);

        return flux;    
      }

    istream &
    Object::fromStream(istream & flux)
      {
        m_name.fromStream(flux);
        m_description.fromStream(flux);

        return flux;
      }

} // namespace hkl

ostream &
operator << (ostream & flux, hkl::Object const & object)
{
    return object.printToStream(flux);
}
