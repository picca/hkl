#include "axe.h"

namespace hkl {
  
  Axe::Axe(void)
  {}
  
  Axe::Axe(MyString const & name, svector const & axe, int direction)
        : Range(name, 0., 1., -1.),
          m_axe(axe)
  {
    set_direction(direction);
  }
  
  Axe::Axe(Axe const & axe)
        : Range(axe.get_name(), axe.get_value(), axe.get_min(), axe.get_max()),
          m_axe(axe.m_axe),
          m_direction(axe.m_direction)
  {}
  
  Axe::~Axe(void)
  {}
  
  void 
  Axe::set_direction(int direction)
  {
    //penser à l'exception lorsque direction = 0;
    if (direction > 0) m_direction = 1;
    if (direction < 0) m_direction = -1;
  }
  
  bool
  Axe::operator ==(Axe const & axe) const
  {
    return Range::operator==(axe)
            && m_axe == axe.m_axe
            && m_direction == axe.m_direction;
  }
  
  Quaternion
  Axe::asQuaternion(void) const
  {
    double const & angle = get_value() * m_direction / 2.; 
    double s_angle = sin(angle) / m_axe.norm2(); 
    
    return Quaternion(cos(angle), s_angle * m_axe[0], s_angle * m_axe[1], s_angle * m_axe[2]);
  }
  
  ostream & 
  Axe::printToStream(ostream & flux) const
  {
    flux  << "\"";
    //flux.width(12);
    flux << get_name();
    flux << "\""
         << m_axe << ", ";
    flux << showpoint << showpos;
    flux << "Sens de rotation: " << m_direction << ", "
      << "Minimum: " << get_min() *  constant::math::radToDeg << ", "
      << "Value: " << get_value() * constant::math::radToDeg << ", "
      << "Maximum: " << get_max() * constant::math::radToDeg << endl;
    flux << noshowpoint << noshowpos << dec;
  
    return flux;
  }

  ostream &
  Axe::toStream(ostream & flux) const
  {
    Range::toStream(flux);
    m_axe.toStream(flux);
    flux << m_direction << endl;

    return flux;    
  }

  istream &
  Axe::fromStream(istream & flux)
  {
    Range::fromStream(flux);
    m_axe.fromStream(flux);
    flux >> m_direction;
    
    return flux;
  }
  
} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::Axe const & axe)
{
  return axe.printToStream(flux);
}
