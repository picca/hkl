#ifndef _MODE_H_
#define _MODE_H_

#include <iostream>

#include "hklobject.h"
#include "svecmat.h"
#include "convenience.h"

using namespace std;

namespace hkl
  {

  class Mode : public HKLObject
    {
    public:
      virtual ~Mode(void);

      /*!
       * \brief The main function to get a sample of angles from (h,k,l).
       * \param h The scaterring vector first element.
       * \param k The scaterring vector second element.
       * \param l The scaterring vector third element.
       * \param UB The product of the orientation matrix U by the crystal matrix B.
       * \param[out] geometry The Geometry to compute.
       */
      virtual void computeAngles(Value const & h, Value const & k, Value const & l,
                                 smatrix const & UB) const = 0;

    protected:

      Mode(MyString const & name, MyString const & description);
    };

  /*!
   * \brief This class defines how to use a diffractomer.
   */
  template<typename T>
  class ModeTemp : public Mode
    {
    public:

      /*!
       * @brief Default constructor - protected to make sure this class is abstract.
       */
      ModeTemp(MyString const & name, MyString const & description, T & geometry) :
          Mode(name, description),
          _geometry(geometry)
      {}

      virtual ~ModeTemp(void)
      {}

      /**
       * \brief Print the state of the current Mode on a ostream.
       * \param flux
       * \return the flux modified.
       */
      ostream & printToStream(ostream & flux) const
        {
          flux << "Mode: " << get_name() << std::endl;
          return flux;
        }

    public:

      typedef T value_type;

    protected:

      T & _geometry;

      /**
       * @brief Check if the parameter are ok to compute the geometry configuration.
       * 
       * @param h 
       * @param k 
       * @param l 
       * @param UB 
       * @param geometry 
       * @throw HKLException if one of the parameters is wrong.
       * @return true if parameters are ok, false otherwise.
       */
      bool _parametersAreOk(Value const & h, Value const & k, Value const & l, smatrix const & UB) const throw (HKLException)
      {
        // Check [h,k,l]
        if (fabs(h.get_value()) < constant::math::epsilon_0
            && fabs(k.get_value()) < constant::math::epsilon_0
            && fabs(l.get_value()) < constant::math::epsilon_0)
          HKLEXCEPTION("Cannot compute the geometry axes values of the [0,0,0] reflection.",
                       "Please set an non-null [h,k,l]");

        // check the wave length
        if (_geometry.get_source().get_waveLength().get_value() < constant::math::epsilon_0)
          HKLEXCEPTION("Cannot compute the geometry axes values with a null wave length.",
                       "Please set an non-null wavelength.");

        if (UB == smatrix())
          HKLEXCEPTION("Cannot compute the geometry axes values with a null UB matrix",
                       "please set a correct UB matrix.");

        return true;
      }

      /**
       * @brief Compute theta correspondig to thoses parameters.
       * 
       * @param h 
       * @param k 
       * @param l 
       * @param UB 
       * @param geometry 
       * @throw HKLException if the reflection is unreachable
       * @return The theta.
       */
      void _computeThetaAndHphi(Value const & h, Value const & k, Value const & l,
                                smatrix const & UB, double & theta, svector & hphi) const throw (HKLException)
      {
        // Calcule de Theta
        hphi = UB * svector(h.get_value(),k.get_value(),l.get_value());
        try
          {
            double lambda = _geometry.get_source().get_waveLength().get_value();
            theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          }
        catch (const HKLException &)
          {
            HKLEXCEPTION("Unreachable reflection with this energy.",
                         "Please change h k l values or the energy.");
          }
      }
    };

} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << for the Mode class
 * \param flux The flux to modifie 
 * \param mode The mode to stream.
 * \return The modified flux.
 */
template<typename T>
ostream &
operator << (ostream & flux, hkl::ModeTemp<T> const & mode)
{
  return mode.printToStream(flux);
};

#endif // _MODE_H_
