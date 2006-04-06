#ifndef _DIFFRACTOMETER_WRAP_H_
#define _DIFFRACTOMETER_WRAP_H_

#include <boost/python.hpp>
#include "diffractometer.h"

using namespace boost::python;
using namespace hkl;
using namespace std;

class Diffractometer_wrap : public Diffractometer
{
  public:
   
    // axes
    list getAxesNames(void);
    
    double getAxeValue(string const & name);
    
    void setAxeValue(string const & name, double angle); 
  
    //pseudoAxes
    list getPseudoAxesNames(void);

    str getPseudoAxeDescription(string const & name);

    list getPseudoAxeParametersNames(string const & name);

    double getPseudoAxeParameterValue(string const & pseudoAxe_name, string const & parameter_name);

    void setPseudoAxeParameterValue(string const & pseudoAxe_name, string const & parameter_name, double value);

    double getPseudoAxeValue(string const & name);
    
    void setPseudoAxeValue(string const & name, double value);

    // crystals
    void setCurrentCrystal(string const & name);

    list getCrystalNames(void);
  
    str getCurrentCrystalName(void);
  
    void setCrystalLattice(std::string const & name, 
                           double a, double b, double c,
                           double alpha, double beta, double gamma);
  
    tuple getCrystalLattice(std::string const & name);
     
    tuple getCrystalReciprocalLattice(std::string const & name);
 
    tuple getCrystalParameterValues(std::string const & crystal_name, std::string const & parameter_name);

    void setCrystalParameterValues(std::string const & crystal_name, std::string const & parameter_name,
                                   double value, double min, double max,
                                   bool flag);
    
    numeric::array getCrystal_UB(std::string const & name);

    // reflections
    double getCrystalReflectionAxeAngle(std::string const & crystal_name, unsigned int index, std::string const & axe_name);
    
    tuple getCrystalReflectionParameters(std::string const & name, unsigned int index);

  
    //modes
    list getModeNames(void);

    str getCurrentModeName(void);

    str getModeDescription(std::string const & name);
  
    list getModeParametersNames(std::string const & name);
    
    void setModeParameterValue(string const & mode_name, string const & parameter_name, double value);
    
    double getModeParameterValue(string const & mode_name, string const & parameter_name);

    // Affinement
    list getAffinementNames(void);

    // calculation
    tuple computeHKL(void);

  protected:
    // Constructor
    Diffractometer_wrap(void);

};

#endif
