//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/source.cpp,v $

//

// Project:      HKL Library

//

// Description:  C++ source code for the class source

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.4 $

//

// $Log: source.cpp,v $
// Revision 1.4  2005/02/08 15:51:05  picca
// update the documenattion
//
// Revision 1.3  2005/01/27 09:23:53  delos
// Commentaires pour CVS en tete des fichiers
//

//

//

// copyleft :       Synchrotron SOLEIL

//                  L'Orme des Merisiers

//                  Saint-Aubin - BP 48

//                  91192 GIF-sur-YVETTE CEDEX

//

//-======================================================================
#include "source.h"
#include <iostream>

source::source()
{
  m_waveLength = 0.;
  m_undulatorGap = 0.;
  m_monochromatorAngle = 0.;
}

source::source(const source &S)
{
  m_waveLength = S.m_waveLength;
  m_undulatorGap = S.m_undulatorGap;
  m_monochromatorAngle = S.m_monochromatorAngle;
}

source::source(double _waveLength, double _monoAngle, double _undGap)
{
  m_waveLength = _waveLength;
  m_undulatorGap = _undGap;
  m_monochromatorAngle = _monoAngle;
}

void source::setWaveLength(double _wl)
{
  m_waveLength = _wl;
}

double source::getWaveLength() const
{
  return m_waveLength;
}

double source::getMonochromatorAngle() const
{
  return m_monochromatorAngle;
}

double source::getUndulatorGap() const
{
  return m_undulatorGap;
}

void source::printOnScreen() const
{
  std::cout << std::endl << "CLASS source";
  std::cout << std::endl
    << "Wave length = " << m_waveLength << '\t'
    << "Monochromator angle = " << m_monochromatorAngle << '\t'
    << "Undulator gap = " << m_undulatorGap << std::endl;
}
