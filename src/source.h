/// The class source defines a light ray and its main characteristics.

#ifndef SOURCE
#define SOURCE

class source
{
private:
  /// The wave length plays a significant role in diffractometry computations and can be varied.
  /// \brief Has to be defined in a consistent way with the crystal units.
  double m_waveLength;
  /// Refer to the Bragg relation : 2D.sin(theta) = lembda
  /// \brief Derive from the wave length and the monochromator type.
  double m_monochromatorAngle;
  //char*  m_monochromatorType;
  /// Self explanatory.
  double m_undulatorGap;

public:
  source();

  /// Check if S units are consistent with the crystal units for the diffractometry computations.
  source(source& S);

  /// _waveLength unit must be consistent with the crystal length units.
  source(
    double _waveLength,
    double _monoAngle,
    double _undGap);

  /// _wl unit must be consistent with the crystal length units.
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
