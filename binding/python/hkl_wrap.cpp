#include <boost/python.hpp>

using namespace boost::python;

#include "diffractometer_eulerian4C_wrap.h"

BOOST_PYTHON_MODULE(libhkl)
{
  class_<Diffractometer_Eulerian4C_wrap>("Diffractometer_Eulerian4C")
    .def("setWaveLength", &Diffractometer_Eulerian4C_wrap::setWaveLength)
    .def("getWaveLength", &Diffractometer_Eulerian4C_wrap::getWaveLength)
    
    .def("getAxesNames", &Diffractometer_Eulerian4C_wrap::getAxesNames)
    .def("setAxeValue", &Diffractometer_Eulerian4C_wrap::setAxeValue)
    .def("getAxeValue", &Diffractometer_Eulerian4C_wrap::getAxeValue)
  
    .def("getPseudoAxesNames", &Diffractometer_Eulerian4C_wrap::getPseudoAxesNames)
    .def("getPseudoAxeDescription", &Diffractometer_Eulerian4C_wrap::getPseudoAxeDescription)
    .def("getPseudoAxeParametersNames", &Diffractometer_Eulerian4C_wrap::getPseudoAxeParametersNames)
    .def("getPseudoAxeParameterValue", &Diffractometer_Eulerian4C_wrap::getPseudoAxeParameterValue)
    .def("setPseudoAxeParameterValue", &Diffractometer_Eulerian4C_wrap::setPseudoAxeParameterValue)
    .def("initializePseudoAxe", &Diffractometer_Eulerian4C_wrap::initializePseudoAxe)
    .def("getPseudoAxeIsValid", &Diffractometer_Eulerian4C_wrap::getPseudoAxeIsValid)
    .def("getPseudoAxeValue", &Diffractometer_Eulerian4C_wrap::getPseudoAxeValue)
    .def("setPseudoAxeValue", &Diffractometer_Eulerian4C_wrap::setPseudoAxeValue)

    .def("getCrystalNames", &Diffractometer_Eulerian4C_wrap::getCrystalNames)
    .def("getCurrentCrystalName", &Diffractometer_Eulerian4C_wrap::getCurrentCrystalName)
    .def("setCurrentCrystal", &Diffractometer_Eulerian4C_wrap::setCurrentCrystal)
    .def("addNewCrystal", &Diffractometer_Eulerian4C_wrap::addNewCrystal)
    .def("getCrystalLattice", &Diffractometer_Eulerian4C_wrap::getCrystalLattice)
    .def("setCrystalLattice", &Diffractometer_Eulerian4C_wrap::setCrystalLattice)
    .def("getCrystalReciprocalLattice", &Diffractometer_Eulerian4C_wrap::getCrystalReciprocalLattice)
    .def("getCrystalParameterValues", &Diffractometer_Eulerian4C_wrap::getCrystalParameterValues)
    .def("setCrystalParameterValues", &Diffractometer_Eulerian4C_wrap::setCrystalParameterValues)
    .def("getCrystal_UB", &Diffractometer_Eulerian4C_wrap::getCrystal_UB)
    .def("getCrystalFitness", &Diffractometer_Eulerian4C_wrap::getCrystalFitness)
    .def("delCrystal", &Diffractometer_Eulerian4C_wrap::delCrystal)
    .def("copyCrystalAsNew", &Diffractometer_Eulerian4C_wrap::copyCrystalAsNew)
    
    .def("getCrystalNumberOfReflection", &Diffractometer_Eulerian4C_wrap::getCrystalNumberOfReflection)
    .def("addCrystalReflection", &Diffractometer_Eulerian4C_wrap::addCrystalReflection)
    .def("getCrystalReflectionAxeAngle", &Diffractometer_Eulerian4C_wrap::getCrystalReflectionAxeAngle)
    .def("setCrystalReflectionParameters", &Diffractometer_Eulerian4C_wrap::setCrystalReflectionParameters)
    .def("getCrystalReflectionParameters", &Diffractometer_Eulerian4C_wrap::getCrystalReflectionParameters)
    .def("delCrystalReflection", &Diffractometer_Eulerian4C_wrap::delCrystalReflection)
    .def("copyCrystalReflectionFromTo", &Diffractometer_Eulerian4C_wrap::copyCrystalReflectionFromTo)
    
    .def("getModeNames", &Diffractometer_Eulerian4C_wrap::getModeNames)
    .def("getCurrentModeName", &Diffractometer_Eulerian4C_wrap::getCurrentModeName)
    .def("setCurrentMode", &Diffractometer_Eulerian4C_wrap::setCurrentMode)
    .def("getModeDescription", & Diffractometer_Eulerian4C_wrap::getModeDescription)
    .def("getModeParametersNames", &Diffractometer_Eulerian4C_wrap::getModeParametersNames)
    .def("getModeParameterValue", &Diffractometer_Eulerian4C_wrap::getModeParameterValue)
    .def("setModeParameterValue", &Diffractometer_Eulerian4C_wrap::setModeParameterValue)
    
    .def("getAffinementNames", &Diffractometer_Eulerian4C_wrap::getAffinementNames)
    .def("getAffinementMaxIteration", &Diffractometer_Eulerian4C_wrap::getAffinementMaxIteration)
    .def("setAffinementMaxIteration", &Diffractometer_Eulerian4C_wrap::setAffinementMaxIteration)
    .def("getAffinementIterations", &Diffractometer_Eulerian4C_wrap::getAffinementIterations)
    .def("affineCrystal", &Diffractometer_Eulerian4C_wrap::affineCrystal)
    
    .def("computeU", &Diffractometer_Eulerian4C_wrap::computeU)
    .def("computeHKL", &Diffractometer_Eulerian4C_wrap::computeHKL)
    .def("computeAngles", &Diffractometer_Eulerian4C_wrap::computeAngles)  
    ;
}
