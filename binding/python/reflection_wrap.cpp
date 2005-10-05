#include "reflection_wrap.h"

Reflection_wrap::Reflection_wrap()
{
};

Reflection_wrap::Reflection_wrap(Reflection const& r)
{
  m_aC = r.get_angleConfiguration();
  m_source = r.get_source();
  m_relevance = r.get_relevance();
  m_h = r.get_h();
  m_k = r.get_k();
  m_l = r.get_l();
  m_flag = r.get_flag();
}

list
Reflection_wrap::getAngles()
{
  std::vector<std::string> liste = get_angleConfiguration().getAxesNames();
  double radToDeg = mathematicalConstants::convertAnglesToDegrees();
  
  unsigned int i;
  unsigned int nb_axes = liste.size();
  list angleList;
  
  for(i=0;i<nb_axes;i++)
    angleList.append(get_angleConfiguration().getAxe(liste[i]).getAngle()*radToDeg);
  
  return angleList;
}
