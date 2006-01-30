#ifndef _DIFFRACTOMETER_EULERIAN4C_WRAP_H_
#define _DIFFRACTOMETER_EULERIAN4C_WRAP_H_

#include <boost/python.hpp>
#include "diffractometer_eulerian4C.h"

using namespace boost::python;
using namespace hkl;
using namespace std;

class Diffractometer_Eulerian4C_wrap : public diffractometer::Eulerian4C
{
  public:
   
    // Constructor
    Diffractometer_Eulerian4C_wrap(void);

    // axes
    list getAxesNames(void);
    
    double getAxeValue(std::string const & name);
    
    void setAxeValue(std::string const & name, double angle); 
  
    // crystals
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
   
    // Affinement
    list getAffinementNames(void);

    // calculation
    tuple computeHKL(void);
};

#endif
