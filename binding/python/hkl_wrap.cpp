#include <boost/python.hpp>

using namespace boost::python;

#include "diffractometer_wrap.h"
#include "diffractometer_kappa_wrap.h"
#include "diffractometer_eulerian4C_wrap.h"
#include "diffractometer_kappa4C_wrap.h"

BOOST_PYTHON_MODULE(libhkl)
{
  class_<Diffractometer_wrap>("Diffractometer_Eulerian4C", no_init)
    .def("setWaveLength", &Diffractometer_wrap::setWaveLength)
    .def("getWaveLength", &Diffractometer_wrap::getWaveLength)
    
    .def("getAxesNames", &Diffractometer_wrap::getAxesNames)
    .def("setAxeValue", &Diffractometer_wrap::setAxeValue)
    .def("getAxeValue", &Diffractometer_wrap::getAxeValue)
  
    .def("getPseudoAxesNames", &Diffractometer_wrap::getPseudoAxesNames)
    .def("getPseudoAxeDescription", &Diffractometer_wrap::getPseudoAxeDescription)
    .def("getPseudoAxeParametersNames", &Diffractometer_wrap::getPseudoAxeParametersNames)
    .def("getPseudoAxeParameterValue", &Diffractometer_wrap::getPseudoAxeParameterValue)
    .def("setPseudoAxeParameterValue", &Diffractometer_wrap::setPseudoAxeParameterValue)
    .def("initializePseudoAxe", &Diffractometer_wrap::initializePseudoAxe)
    .def("getPseudoAxeIsValid", &Diffractometer_wrap::getPseudoAxeIsValid)
    .def("getPseudoAxeValue", &Diffractometer_wrap::getPseudoAxeValue)
    .def("setPseudoAxeValue", &Diffractometer_wrap::setPseudoAxeValue)

    .def("getCrystalNames", &Diffractometer_wrap::getCrystalNames)
    .def("getCurrentCrystalName", &Diffractometer_wrap::getCurrentCrystalName)
    .def("setCurrentCrystal", &Diffractometer_wrap::setCurrentCrystal)
    .def("addNewCrystal", &Diffractometer_wrap::addNewCrystal)
    .def("getCrystalLattice", &Diffractometer_wrap::getCrystalLattice)
    .def("setCrystalLattice", &Diffractometer_wrap::setCrystalLattice)
    .def("getCrystalReciprocalLattice", &Diffractometer_wrap::getCrystalReciprocalLattice)
    .def("getCrystalParameterValues", &Diffractometer_wrap::getCrystalParameterValues)
    .def("setCrystalParameterValues", &Diffractometer_wrap::setCrystalParameterValues)
    .def("getCrystal_UB", &Diffractometer_wrap::getCrystal_UB)
    .def("getCrystalFitness", &Diffractometer_wrap::getCrystalFitness)
    .def("delCrystal", &Diffractometer_wrap::delCrystal)
    .def("copyCrystalAsNew", &Diffractometer_wrap::copyCrystalAsNew)
    
    .def("getCrystalNumberOfReflection", &Diffractometer_wrap::getCrystalNumberOfReflection)
    .def("addCrystalReflection", &Diffractometer_wrap::addCrystalReflection)
    .def("getCrystalReflectionAxeAngle", &Diffractometer_wrap::getCrystalReflectionAxeAngle)
    .def("setCrystalReflectionParameters", &Diffractometer_wrap::setCrystalReflectionParameters)
    .def("getCrystalReflectionParameters", &Diffractometer_wrap::getCrystalReflectionParameters)
    .def("delCrystalReflection", &Diffractometer_wrap::delCrystalReflection)
    .def("copyCrystalReflectionFromTo", &Diffractometer_wrap::copyCrystalReflectionFromTo)
    
    .def("getModeNames", &Diffractometer_wrap::getModeNames)
    .def("getCurrentModeName", &Diffractometer_wrap::getCurrentModeName)
    .def("setCurrentMode", &Diffractometer_wrap::setCurrentMode)
    .def("getModeDescription", & Diffractometer_wrap::getModeDescription)
    .def("getModeParametersNames", &Diffractometer_wrap::getModeParametersNames)
    .def("getModeParameterValue", &Diffractometer_wrap::getModeParameterValue)
    .def("setModeParameterValue", &Diffractometer_wrap::setModeParameterValue)
    
    .def("getAffinementNames", &Diffractometer_wrap::getAffinementNames)
    .def("getAffinementMaxIteration", &Diffractometer_wrap::getAffinementMaxIteration)
    .def("setAffinementMaxIteration", &Diffractometer_wrap::setAffinementMaxIteration)
    .def("getAffinementIterations", &Diffractometer_wrap::getAffinementIterations)
    .def("affineCrystal", &Diffractometer_wrap::affineCrystal)
    
    .def("computeU", &Diffractometer_wrap::computeU)
    .def("computeHKL", &Diffractometer_wrap::computeHKL)
    .def("computeAngles", &Diffractometer_wrap::computeAngles)  
    ;
  
  class_<Diffractometer_Kappa_wrap, bases<Diffractometer_wrap> >("Diffractometer_Kappa", no_init);
  
  class_<Diffractometer_Eulerian4C_wrap, bases<Diffractometer_wrap> >("Diffractometer_Eulerian4C");
  
  class_<Diffractometer_Kappa4C_wrap, bases<Diffractometer_wrap> >("Diffractometer_Kappa4C", init<double>());
}
