//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/source.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class source

// (Delos Vincent) - 26 janv. 2005

//

// $Author: delos $

//

// $Revision: 1.9 $

//

// $Log: source.h,v $
// Revision 1.9  2005/01/27 09:23:53  delos
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
/// The class source defines a light ray and its main characteristics.

#ifndef SOURCE
#define SOURCE

class source
{
private:
  /// The wave length plays a significant role in diffractometry computations and can be varied.
  /// \brief Has to be defined in a consistent way with the crystal units.
  double m_waveLength;
  /// Refer to the Bragg relation : 2D.sin(theta) = lambda
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
