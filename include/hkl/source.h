#ifndef _SOURCE_H
#define _SOURCE_H


#include "HKLException.h"
#include <ostream>
#include <istream>

#include "value.h"
#include "svector.h"
#include "quaternion.h"
namespace hkl
  {

  class Source
    {
    protected:
      hkl::Value _waveLength;

      hkl::svector _direction;

      hkl::Quaternion _qi;


    public:
      /**
       * @brief Default Constructor.
       *
       * Create a new Source with all is privates parameters set to zero.
       * After this you must set the waveLength before using it, with the
       * setWaveLength method.
       */
      Source();

      /**
       * @brief Constructor from parameters
       * @param waveLength the wavelength of the beam.
       * @param direction the X-rays beam direction. This parameter is normalize.
       *
       * Create a new Source from the parameters.
       * <b>_waveLength unit must be consistent with the crystal length units</b>.
       */
      Source(const hkl::Value & waveLength, const hkl::svector & direction);

      inline const hkl::Value & get_waveLength() const;

      inline const hkl::svector & get_direction() const;

      inline const hkl::Quaternion & get_qi() const;

      /**
       * @brief set the wavelength
       * @param waveLength the wavelength to set.
       * @exception HKLException if waveLength == 0.
       *
       * Set the waveLength of the source
       * <b>wl unit must be consistent with the crystal length units</b>.
       */
      void setWaveLength(const hkl::Value & waveLength) throw(hkl::HKLException);

      /**
       * @brief Set the _{p0} of the Source.
       * @param direction to set
       *
       * The direction is normalize.
       */
      void setDirection(const hkl::svector & direction) throw(hkl::HKLException);

      /**
       * @brief Get the ki vector
       */
      hkl::svector getKi() const;

      /**
       * @brief set the ki vector
       */
      void setKi(const hkl::svector & ki) throw(hkl::HKLException);

      /**
       * \brief Are two Source equals ?
       * \param source the Source to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const Source & source) const;

      /**
       * @brief print the Source into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the Source
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the Source from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };
  inline const hkl::Value & Source::get_waveLength() const
    {
      return _waveLength;
    }

  inline const hkl::svector & Source::get_direction() const
    {
      return _direction;
    }

  inline const hkl::Quaternion & Source::get_qi() const
    {
      return _qi;
    }


} // namespace hkl
/**
 * @brief Surcharge de l'operateur << pour la class Source
 * @param flux The ostream to print into.
 * @param source The Source to print.
 * @return the modified flux.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::Source const & source)
{
  return source.printToStream(flux);
}
#endif
