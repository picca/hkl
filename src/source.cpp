#include "source.h"
#include <iostream.h>

source::source(double _waveLength, double _monoAngle, double _undGap)
{
  m_waveLength = _waveLength;
  m_undulatorGap = _undGap;
  m_monochromatorAngle = _monoAngle;
}

source::source(source& S)
{
  m_waveLength = S.m_waveLength;
  m_undulatorGap = S.m_undulatorGap;
  m_monochromatorAngle = S.m_monochromatorAngle;
}

source::source()
{
  m_waveLength = 0.;
  m_undulatorGap = 0.;
  m_monochromatorAngle = 0.;
}

void source::printOnScreen() const
{
  cout << endl << "CLASS source";
  cout << endl
    << "Wave length = " << m_waveLength << '\t'
    << "Monochromator angle = " << m_monochromatorAngle << '\t'
    << "Undulator gap = " << m_undulatorGap << endl;
}
