#include "diffractometer_wrap.h"

Diffractometer_wrap::Diffractometer_wrap(void)
: Diffractometer()
{}

//axes
list 
Diffractometer_wrap::getAxesNames(void)
{
    vector<string> AxesNameList = Diffractometer::getAxesNames();
    unsigned int nb_axes = AxesNameList.size();
    list nameList;

    for(unsigned int i=0;i<nb_axes;i++)
        nameList.append(AxesNameList[i].c_str());

    return nameList;
}

double
Diffractometer_wrap::getAxeValue(string const & name)
{ 
    return Diffractometer::getAxeValue(name) * constant::math::radToDeg;
}

void
Diffractometer_wrap::setAxeValue(string const & name, double angle)
{
    Diffractometer::setAxeValue(name, angle * constant::math::degToRad);
}

// pseudoaxes
list 
Diffractometer_wrap::getPseudoAxesNames(void)
{
    vector<string> pseudoAxesNameList = Diffractometer::getPseudoAxesNames();
    unsigned int nb_axes = pseudoAxesNameList.size();
    list nameList;

    for(unsigned int i=0;i<nb_axes;i++)
        nameList.append(pseudoAxesNameList[i].c_str());

    return nameList;
}

str
Diffractometer_wrap::getPseudoAxeDescription(string const & name)
{
    return str(Diffractometer::getPseudoAxeDescription(name));
}

list
Diffractometer_wrap::getPseudoAxeParametersNames(string const & name)
{
    vector<string> const & parametersNames = Diffractometer::getPseudoAxeParametersNames(name);
    unsigned int nb_parameters = parametersNames.size();

    list l;
    for(unsigned int i=0; i<nb_parameters; i++)
        l.append(str(parametersNames[i]));

    return l;
}

double
Diffractometer_wrap::getPseudoAxeParameterValue(string const & pseudoAxe_name,
                                                string const & parameter_name)
{
    return Diffractometer::getPseudoAxeParameterValue(pseudoAxe_name,parameter_name) * constant::math::radToDeg;
}

void
Diffractometer_wrap::setPseudoAxeParameterValue(string const & pseudoAxe_name,
                                                string const & parameter_name,
                                                double value)
{
    Diffractometer::setPseudoAxeParameterValue(pseudoAxe_name,
                                               parameter_name,
                                               value * constant::math::degToRad); 
}

double
Diffractometer_wrap::getPseudoAxeValue(string const & name)
{
    return Diffractometer::getPseudoAxeValue(name) * constant::math::radToDeg;
}

void
Diffractometer_wrap::setPseudoAxeValue(string const & name,
                                       double value)
{
    Diffractometer::setPseudoAxeValue(name,
                                      value * constant::math::degToRad); 
}

// crystals
list
Diffractometer_wrap::getCrystalNames(void)
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
Diffractometer_wrap::getCurrentCrystalName(void)
{
    return str(Diffractometer::getCurrentCrystalName());
}

void
Diffractometer_wrap::setCurrentCrystal(string const & name)
{
    Diffractometer::setCurrentCrystal(name);
}

void
Diffractometer_wrap::setCrystalLattice(string const & name,
                                       double a, double b, double c,
                                       double alpha, double beta, double gamma)
{
    Diffractometer::setCrystalLattice(name, a, b, c,
                                      alpha * constant::math::degToRad,
                                      beta * constant::math::degToRad,
                                      gamma * constant::math::degToRad);
}

tuple
Diffractometer_wrap::getCrystalLattice(string const & name)
{
    double a, b, c;
    double alpha, beta, gamma;

    Diffractometer::getCrystalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
    alpha *= constant::math::radToDeg;
    beta *= constant::math::radToDeg;
    gamma *= constant::math::radToDeg;

    return make_tuple(a, b, c, alpha, beta, gamma);
}

tuple
Diffractometer_wrap::getCrystalReciprocalLattice(string const & name)
{
    double a, b, c;
    double alpha, beta, gamma;

    Diffractometer::getCrystalReciprocalLattice(name, &a, &b, &c, &alpha, &beta, &gamma);
    alpha *= constant::math::radToDeg;
    beta *= constant::math::radToDeg;
    gamma *= constant::math::radToDeg;

    return make_tuple(a, b, c, alpha, beta, gamma);
}

tuple
Diffractometer_wrap::getCrystalParameterValues(string const & crystal_name,
                                               string const & parameter_name)
{
    double value, min , max;
    bool flag;

    Diffractometer::getCrystalParameterValues(crystal_name, parameter_name, &value, &min, &max, &flag);
    if (parameter_name == "alpha" || parameter_name == "beta" || parameter_name == "gamma"){
        value *= constant::math::radToDeg;
        min *= constant::math::radToDeg;
        max *= constant::math::radToDeg;
    }

    return make_tuple(value, min, max, flag);
}

void
Diffractometer_wrap::setCrystalParameterValues(std::string const & crystal_name,
                                               std::string const & parameter_name,
                                               double value, double min, double max,
                                               bool flag)
{
    if (parameter_name == "alpha" || parameter_name == "beta" || parameter_name == "gamma"){
        value *= constant::math::degToRad;
        min *= constant::math::degToRad;
        max *= constant::math::degToRad;
    }
    Diffractometer::setCrystalParameterValues(crystal_name, parameter_name, value, min, max, flag);
}

numeric::array
Diffractometer_wrap::getCrystal_UB(std::string const & name)
{
    smatrix M = Diffractometer::getCrystal_UB(name);

    numeric::array U(make_tuple(
                                make_tuple(M.get(0,0),M.get(0,1),M.get(0,2)),
                                make_tuple(M.get(1,0),M.get(1,1),M.get(1,2)),
                                make_tuple(M.get(2,0),M.get(2,1),M.get(2,2))), "Float32");

    return U;
}

// reflections
tuple
Diffractometer_wrap::getCrystalReflectionParameters(std::string const & name, unsigned int index)
{
    double h, k, l;
    int relevance;
    bool flag;

    Diffractometer::getCrystalReflectionParameters(name, index,
                                                   &h, &k, &l,
                                                   &relevance, &flag);
    return make_tuple(h, k, l, relevance, flag);
}

double
Diffractometer_wrap::getCrystalReflectionAxeAngle(std::string const & crystal_name, 
                                                  unsigned int index,
                                                  std::string const & axe_name)
{
    return Diffractometer::getCrystalReflectionAxeAngle(crystal_name, index, axe_name) * constant::math::radToDeg;
}

// modes
list
Diffractometer_wrap::getModeNames(void)
{
    vector<string> modeNames = Diffractometer::getModeNames();
    unsigned int nb_modes = modeNames.size();  

    list l;
    for(unsigned int i=0; i< nb_modes; i++)
        l.append(str(modeNames[i]));

    return l;
}

str
Diffractometer_wrap::getCurrentModeName(void)
{
    return str(Diffractometer::getCurrentModeName());
}

str
Diffractometer_wrap::getModeDescription(string const & name)
{
    return str(Diffractometer::getModeDescription(name));
}

list
Diffractometer_wrap::getModeParametersNames(string const & name)
{
    vector<string> const & parametersNames = Diffractometer::getModeParametersNames(name);
    unsigned int nb_parameters = parametersNames.size();

    list l;
    for(unsigned int i=0; i<nb_parameters; i++)
        l.append(str(parametersNames[i]));

    return l;
}

void
Diffractometer_wrap::setModeParameterValue(string const & mode_name,
                                           string const & parameter_name,
                                           double value)
{
    Diffractometer::setModeParameterValue(mode_name,
                                          parameter_name,
                                          value*constant::math::degToRad); 
}

double
Diffractometer_wrap::getModeParameterValue(string const & mode_name,
                                           string const & parameter_name)
{
    return Diffractometer::getModeParameterValue(mode_name,parameter_name) * constant::math::radToDeg;
}

// Affinement

list
Diffractometer_wrap::getAffinementNames(void)
{
    vector<string> affinementNames = Diffractometer::getAffinementNames();
    unsigned int nb_methods = affinementNames.size();  

    list l;
    for(unsigned int i=0; i< nb_methods; i++)
        l.append(str(affinementNames[i]));

    return l;
}


// calculations
tuple
Diffractometer_wrap::computeHKL(void)
{
    double h,k,l;
    Diffractometer::computeHKL(h, k, l);
    return make_tuple(h,k,l);
}
