// The class source to define a light ray and its main characteristics.

#ifndef SOURCE
#define SOURCE

class source
{
private:
  // The wave length plays a significant role in diffractometry
  // computations and can be varied.
  double m_waveLength;
  // Derive from the wave length and the monochromator type.
  // Refer the Bragg relation : 2D.sin(theta) = lembda
  double m_monochromatorAngle;
  //char*  m_monochromatorType;
  // self explanatory.
  double m_undulatorGap;

public:
  source();

  source(source& S);

  source(double,double,double);

  void setWaveLength(double _wl)
  {m_waveLength = _wl;}

  double getWaveLength() const
  {return m_waveLength;}

  double getMonochromatorAngle() const
  {return m_monochromatorAngle;}

  double getUndulatorGap() const
  {return m_undulatorGap;}

  void printOnScreen() const;

};

#endif