#include "mode.h"

namespace hkl {

  Mode::~Mode(void) {}

  std::vector<std::string> const
  Mode::getParametersNames(void) const
  {
    return m_parameterList.getNames();
  }

    double
  Mode::getParameterValue(std::string const & name) const throw (HKLException)
  {
    try{
      return m_parameterList[name].get_value();
    } catch (HKLException const &){
      throw;
    }
  }
    void
  Mode::setParameterValue(std::string const & name, double value) throw (HKLException)
  {
    try{
      m_parameterList[name].set_value(value);
    } catch (HKLException const &){
      throw;
    }
  }

  //*************************** PROTECTED FUNCTIONS ****************************************

  Mode::Mode(void) {}

    void
  Mode::_addParameter(std::string const & name) throw (HKLException)
  {
    ValueList::iterator iter = m_parameterList.begin();
    ValueList::iterator end = m_parameterList.end();
    while(iter != end){
      if (iter->get_name() == name){
        std::ostringstream reason;
        reason << "The Parameter \"" << name << "\" already exist";
        throw HKLException(reason.str(),
                           "Please choose an other name",
                           "Mode::_addParameter");
      }
      ++iter;
    }
    m_parameterList.push_back(Value(name, 0.));
  }

  double
  Mode::_atan2(double s, double c) const
  {
    double angle;

    if (fabs(s) < constant::math::epsilon_0) s = 0.;
    if (fabs(c) < constant::math::epsilon_0) c = 0.;
    angle = atan2(s, c);    
    if (fabs(angle) < constant::math::epsilon_0) angle = 0.;
    return angle;
  }

    double
  Mode::_asin(double s) const throw (HKLException)
  { 
    double angle;
    if (fabs(s) - 1. > constant::math::epsilon_0)
      throw HKLException("sine bigger than 1.",
                         "",
                         "Mode::_asin");
    else
      angle = asin(s);

    if (fabs(angle) < constant::math::epsilon_0) angle = 0.;

    return angle;
  }

  std::ostream &
  Mode::printToStream(std::ostream& flux) const
  { 
    flux << "Mode: " << get_name() << std::endl;
    return flux;
  }

} // namespace hkl

std::ostream &
operator <<(std::ostream & flux, hkl::Mode const & mode)
{ 
  return mode.printToStream(flux);
}
