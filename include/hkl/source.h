#ifndef SOURCE_H
#define SOURCE_H

#include <iostream>
#include <iomanip>

#include "svecmat.h"
#include "quaternion.h"
#include "value.h"

using namespace std;

namespace hkl
  {

  /**
   * @brief describe the source parameters
   * 
   * The class source defines a light ray and its main characteristics.
   */
  class Source
    {
    public:
      /**
       * @brief Default Constructor of the source
       *
       * Create a new source with all is privates parameters set to zero.
       * After this you must set the waveLength before using it, with the
       * setWaveLength method.
       */
      Source(void);

      /**
       * @brief Copy constructor
       * @param source the %Source to copy from.
       *
       * Create a new source by copying the source S.
       * <b>Check if S units are consistent with the crystal units for the diffractometry computations</b>.
       */
      Source(Source const & source);

      /**
       * @brief Constructor from parameters
       * @param waveLength the wavelength of the beam.
       * @param direction the X-rays beam direction. This parameter is normalize by
       * the methode  
       *
       * Create a new source from the parameters.
       * <b>_waveLength unit must be consistent with the crystal length units</b>.
       */
      Source(Value const & waveLength, svector const & direction);

      Value const & get_waveLength(void) const
        {
          return _waveLength;
        } //!< Get the waveLength of the source.
      svector const & get_direction(void) const
        {
          return _direction;
        } //!< Get the direction of the source.
      Quaternion const & get_qi(void) const
        {
          return _qi;
        } //!< Get the incomming wave quaternion.

      /**
       * @brief overload of the == operator for the source class
       * @param source The %Source we want to compare.
       * @return The comparison with the %Source S. 
       */
      bool operator == (Source const & source) const;

      /**
       * @brief set the wavelength
       * @param waveLength the wavelength
       * \exception if wavelength == 0.
       *
       * Set the wavelength of the source
       * <b>wl unit must be consistent with the crystal length units</b>.
       */
      void setWaveLength(Value const & waveLength) throw (HKLException);

      /**
       * @brief Set the direction.
       * @param direction
       *
       * The direction is normalize by the methode
       */
      void setDirection(svector const & direction) throw (HKLException);

      /**
       * @brief Get the ki vector
       */
      svector getKi(void) const;

      /**
       * \brief set the ki vector
       */
      void setKi(svector const & ki) throw (HKLException);

      /**
       * \brief Save the Source into a stream.
       * \param flux the stream to save the Source into.
       * \return The stream with the Source.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * \brief Save the Source into a stream.
       * \param flux the stream to save the Source into.
       * \return The stream with the Source.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * \brief Restore a Source from a stream.
       * \param flux The stream containing the Source.
       */
      istream & fromStream(istream & flux);

    private:
      /**
       * @brief The wave length of the beam.
       * Has to be defined in a consistent way with the crystal units.
       * 
       * The wave length plays a significant role in diffractometry computations and can be varied.
       * Has to be defined in a consistent way with the crystal units.
       */
      Value _waveLength;
      svector _direction; //!< The direction of the incomming beam.
      Quaternion _qi; //!< The incomming wave vector
    };

} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class source
 * @param flux The ostream to print into.
 * @param source The Source to print. 
 * @return the modified flux.
 */
inline ostream &
operator << (ostream & flux, hkl::Source const & source)
{
  return source.printToStream(flux);
}

#endif // SOURCE_H
