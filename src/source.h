//+======================================================================

// $Source: /usr/local/CVS/Libraries/HKL/src/Attic/source.h,v $

//

// Project:      HKL Library

//

// Description:  Header file for the class source

// (Delos Vincent) - 26 janv. 2005

//

// $Author: picca $

//

// $Revision: 1.10 $

//

// $Log: source.h,v $
// Revision 1.10  2005/02/08 15:51:05  picca
// update the documenattion
//
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

#ifndef SOURCE_H
#define SOURCE_H

/**
 * \brief describe the source parameters
 * 
 * The class source defines a light ray and its main characteristics.
 */
class source
{
private:
  /**
   * \brief The wave length of the beam.
   * Has to be defined in a consistent way with the crystal units.
   * 
   * The wave length plays a significant role in diffractometry computations and can be varied.
   * Has to be defined in a consistent way with the crystal units.
   */
  double m_waveLength;
  
  /**
   * \brief Derive from the wave length and the monochromator type.
   *
   * Refer to the Bragg relation : 2D.sin(theta) = lambda
   */
  double m_monochromatorAngle;
  
  //char*  m_monochromatorType;
  
  /**
   * \brief the gap of the ondulator
   *
   * containe the gap of the ondulator
   */
  double m_undulatorGap;

public:
  /**
   * \brief Default Constructor of the source
   *
   * Create a new source with all is privates parameters set to zero.
   */
  source();

  /**
   * \brief Copy constructor
   * <b>Check if S units are consistent with the crystal units for the diffractometry computations</b>.
   * \param S #source the source to copy from.
   *
   * Create a new source by copying the source S.
   * <b>Check if S units are consistent with the crystal units for the diffractometry computations</b>.
   */
  source(const source &S);

  /**
   * \brief constructor from parameters
   * <b>_waveLength unit must be consistent with the crystal length units</b>.
   * \param _waveLength the wavelength of the beam
   * \param _monoAngle the angle of the monochromator
   * \param _undGap The gap of the undulator
   *
   * Create a new source from the parameters.
   * <b>_waveLength unit must be consistent with the crystal length units</b>.
   */
  source( double _waveLength, double _monoAngle, double _undGap);

  /**
   * \brief set the wavelength
   * <b>_wl unit must be consistent with the crystal length units</b>.
   * \param _wl the wavelength
   *
   * Set the wavelength of the source
   * <b>_wl unit must be consistent with the crystal length units</b>.
   */
  void setWaveLength(double _wl);

  /**
   * \brief get the wavelength
   * \return the wavelength
   *
   * Get the wavelength of the source
   */
  double getWaveLength() const;

  /**
   * \brief get the monochromator angle
   * \return the monochromator angle
   *
   * Get the monochromator angle
   */
  double getMonochromatorAngle() const;

  /**
   * \brief get the undulator gap
   * \return the undulator gap
   *
   * Get the undulator gap
   */
  double getUndulatorGap() const;

  /**
   * \brief print on screen the source parameters.
   *
   * 
   * Print on screen the source parameters.
   */
  void printOnScreen() const;

};

#endif
