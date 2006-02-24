#include "diffractometer_eulerian4C_wrap.h"

Diffractometer_Eulerian4C_wrap::Diffractometer_Eulerian4C_wrap()
: diffractometer::Eulerian4C()
{}

//axes
list 
Diffractometer_Eulerian4C_wrap::getAxesNames()
{
    vector<string> AxesNameList = diffractometer::Eulerian4C::getAxesNames();
    unsigned int nb_axes = AxesNameList.size();
    list nameList;

    for(unsigned int i=0;i<nb_axes;i++)
        nameList.append(AxesNameList[i].c_str());

    return nameList;
}

double
Diffractometer_Eulerian4C_wrap::getAxeValue(string const & name)
{ 
    return diffractometer::Eulerian4C::getAxeValue(name) * constant::math::radToDeg;
}

void
Diffractometer_Eulerian4C_wrap::setAxeValue(string const & name, double angle)
{
    diffractometer::Eulerian4C::setAxeValue(name, angle * constant::math::degToRad);
}

// pseudoaxes
list 
Diffractometer_Eulerian4C_wrap::getPseudoAxesNames()
{
    vector<string> pseudoAxesNameList = diffractometer::Eulerian4C::getPseudoAxesNames();
    unsigned int nb_axes = pseudoAxesNameList.size();
    list nameList;

    for(unsigned int i=0;i<nb_axes;i++)
        nameList.append(pseudoAxesNameList[i].c_str());

    return nameList;
}

str
Diffractometer_Eulerian4C_wrap::getPseudoAxeDescription(string const & name)
{
    return str(diffractometer::Eulerian4C::getPseudoAxeDescription(name));
}

list
Diffractometer_Eulerian4C_wrap::getPseudoAxeParametersNames(string const & name)
{
    vector<string> const & parametersNames = diffractometer::Eulerian4C::getPseudoAxeParametersNames(name);
    unsigned int nb_parameters = parametersNames.size();

    list l;
    for(unsigned int i=0; i<nb_parameters; i++)
        l.append(str(parametersNames[i]));

    return l;
}

double
Diffractometer_Eulerian4C_wrap::getPseudoAxeParameterValue(string const & pseudoAxe_name,
                                                           string const & parameter_name)
{
    return diffractometer::Eulerian4C::getPseudoAxeParameterValue(pseudoAxe_name,parameter_name) * constant::math::radToDeg;
}

void
Diffractometer_Eulerian4C_wrap::setPseudoAxeParameterValue(string const & pseudoAxe_name,
                                                           string const & parameter_name,
                                                           double value)
{
    diffractometer::Eulerian4C::setPseudoAxeParameterValue(pseudoAxe_name,
                                                           parameter_name,
                                                           value * constant::math::degToRad); 
}

double
Diffractometer_Eulerian4C_wrap::getPseudoAxeValue(string const & name)
{
    return diffractometer::Eulerian4C::getPseudoAxeValue(name) * constant::math::radToDeg;
}

void
Diffractometer_Eulerian4C_wrap::setPseudoAxeValue(string const & name,
                                                  double value)
{
    diffractometer::Eulerian4C::setPseudoAxeValue(name,
                                                  value * constant::math::degToRad); 
}

// crystals
list
Diffractometer_Eulerian4C_wrap::getCrystalNames()
{
    CrystalList::iterator i = m_crystalList.begin();
    CrystalList::iterator end = m_crystalList.end();
    list l;

    while (i != end){
        l.append(str(i->first.c_str()));
        ++i;
    }

    return l;
}

str
Diffractometer_Eulerian4C_wrap::getCurrentCrystalName()
{
    return str(diffractometer::Eulerian4C::getCurrentCrystalName());
}

void
Diffractometer_Eulerian4C_wrap::setCurrentCrystal(string const & name)
{
    diffractometer::Eulerian4C::setCurrentCrystal(name);
}

void
Diffractometer_Eulerian4C_wrap::setCrystalLattice(string const & name,
                                                  double a, double b, double c,
                                                  double alpha, double beta, double gamma)
{
    diffractometer::Eulerian4C::setCrystalLattice(name, a, b, c,
                                                  alpha * constant::math::degToRad,
                                                  beta * constant::math::degToRad,
                                                  gamma * constant::math::degToRad);
}

tuple
Diffractometer_Eulerian4C_wrap::getCrystalLattice(string const & name)
{
    double a, b, c;
    double alpha, beta, gamma;

    diffractometer::Eulerian4C::getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
    alpha *= constant::math::radToDeg;
    beta *= constant::math::radToDeg;
    gamma *= constant::math::radToDeg;

    return make_tuple(a, b, c, alpha, beta, gamma);
}

tuple
Diffractometer_Eulerian4C_wrap::getCrystalReciprocalLattice(string const & name)
{
    double a, b, c;
    double alpha, beta, gamma;

    diffractometer::Eulerian4C::getCrystalReciprocalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
    alpha *= constant::math::radToDeg;
    beta *= constant::math::radToDeg;
    gamma *= constant::math::radToDeg;

    return make_tuple(a, b, c, alpha, beta, gamma);
}

tuple
Diffractometer_Eulerian4C_wrap::getCrystalParameterValues(string const & crystal_name,
                                                          string const & parameter_name)
{
    double value, min , max;
    bool flag;

    diffractometer::Eulerian4C::getCrystalParameterValues(crystal_name, parameter_name, &value, &min, &max, &flag);
    if (parameter_name == "alpha" || parameter_name == "beta" || parameter_name == "gamma"){
        value *= constant::math::radToDeg;
        min *= constant::math::radToDeg;
        max *= constant::math::radToDeg;
    }

    return make_tuple(value, min, max, flag);
}

void
Diffractometer_Eulerian4C_wrap::setCrystalParameterValues(std::string const & crystal_name,
                                                          std::string const & parameter_name,
                                                          double value, double min, double max,
                                                          bool flag)
{
    if (parameter_name == "alpha" || parameter_name == "beta" || parameter_name == "gamma"){
        value *= constant::math::degToRad;
        min *= constant::math::degToRad;
        max *= constant::math::degToRad;
    }
    diffractometer::Eulerian4C::setCrystalParameterValues(crystal_name, parameter_name, value, min, max, flag);
}

numeric::array
Diffractometer_Eulerian4C_wrap::getCrystal_UB(std::string const & name)
{
    smatrix M = diffractometer::Eulerian4C::getCrystal_UB(name);

    numeric::array U(make_tuple(
                                make_tuple(M.get(0,0),M.get(0,1),M.get(0,2)),
                                make_tuple(M.get(1,0),M.get(1,1),M.get(1,2)),
                                make_tuple(M.get(2,0),M.get(2,1),M.get(2,2))), "Float32");

    return U;
}

// reflections
tuple
Diffractometer_Eulerian4C_wrap::getCrystalReflectionParameters(std::string const & name, unsigned int index)
{
    double h, k, l;
    int relevance;
    bool flag;

    diffractometer::Eulerian4C::getCrystalReflectionParameters(name, index,
                                                               &h, &k, &l,
                                                               &relevance, &flag);
    return make_tuple(h, k, l, relevance, flag);
}

double
Diffractometer_Eulerian4C_wrap::getCrystalReflectionAxeAngle(std::string const & crystal_name, 
                                                             unsigned int index,
                                                             std::string const & axe_name)
{
    return diffractometer::Eulerian4C::getCrystalReflectionAxeAngle(crystal_name, index, axe_name) * constant::math::radToDeg;
}

// modes
list
Diffractometer_Eulerian4C_wrap::getModeNames(void)
{
    vector<string> modeNames = diffractometer::Eulerian4C::getModeNames();
    unsigned int nb_modes = modeNames.size();  

    list l;
    for(unsigned int i=0; i< nb_modes; i++)
        l.append(str(modeNames[i]));

    return l;
}

str
Diffractometer_Eulerian4C_wrap::getCurrentModeName(void)
{
    return str(diffractometer::Eulerian4C::getCurrentModeName());
}

str
Diffractometer_Eulerian4C_wrap::getModeDescription(string const & name)
{
    return str(diffractometer::Eulerian4C::getModeDescription(name));
}

list
Diffractometer_Eulerian4C_wrap::getModeParametersNames(string const & name)
{
    vector<string> const & parametersNames = diffractometer::Eulerian4C::getModeParametersNames(name);
    unsigned int nb_parameters = parametersNames.size();

    list l;
    for(unsigned int i=0; i<nb_parameters; i++)
        l.append(str(parametersNames[i]));

    return l;
}

void
Diffractometer_Eulerian4C_wrap::setModeParameterValue(string const & mode_name,
                                                      string const & parameter_name,
                                                      double value)
{
    diffractometer::Eulerian4C::setModeParameterValue(mode_name,
                                                      parameter_name,
                                                      value*constant::math::degToRad); 
}

double
Diffractometer_Eulerian4C_wrap::getModeParameterValue(string const & mode_name,
                                                      string const & parameter_name)
{
    return diffractometer::Eulerian4C::getModeParameterValue(mode_name,parameter_name) * constant::math::radToDeg;
}

// Affinement

list
Diffractometer_Eulerian4C_wrap::getAffinementNames(void)
{
    vector<string> affinementNames = diffractometer::Eulerian4C::getAffinementNames();
    unsigned int nb_methods = affinementNames.size();  

    list l;
    for(unsigned int i=0; i< nb_methods; i++)
        l.append(str(affinementNames[i]));

    return l;
}


// calculations
tuple
Diffractometer_Eulerian4C_wrap::computeHKL()
{
    double h,k,l;
    diffractometer::Eulerian4C::computeHKL(&h, &k, &l);
    return make_tuple(h,k,l);
}
