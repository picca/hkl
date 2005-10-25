#include "axe.h"

namespace hkl {

Axe::Axe(std::string const & name, svector const & axe, int direction)
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

std::ostream & 
Axe::printToStream(std::ostream & flux) const
{
  flux  << "Axe: \"" << get_name() << "\"\t"
    << m_axe << ", ";
  flux << std::showpoint << std::showpos;
  flux << "Sens de rotation: " << m_direction << ", "
    << "Minimum: " << get_min() *  constant::math::radToDeg << ", "
    << "Value: " << get_value() * constant::math::radToDeg << ", "
    << "Maximum: " << get_max() * constant::math::radToDeg << std::endl;
  flux << std::noshowpoint << std::noshowpos << std::dec;

  return flux;
}

} // namespace hkl

std::ostream &
operator <<(std::ostream & flux, hkl::Axe const & axe)
{
  return axe.printToStream(flux);
}
