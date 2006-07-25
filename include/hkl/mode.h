#ifndef _MODE_H_
#define _MODE_H_

#include <iostream>

#include "svecmat.h"
#include "geometry.h"
#include "convenience.h"
#include "HKLException.h"
#include "objectwithparameters.h"

using namespace std;

namespace hkl {

    /*!
     * \brief This class defines how to use a diffractomer.
     */
    template<typename T>
    class Mode : public ObjectWithParameters
      {
      public:

        Mode(void); //!< Default constructor - protected to make sure this class is abstract.

        Mode(Mode const & mode); //!< Copy constructor

        virtual ~Mode(void); //!< The default constructor

        /*!
         * \brief The main function to get a sample of angles from (h,k,l).
         * \param h The scaterring vector first element.
         * \param k The scaterring vector second element.
         * \param l The scaterring vector third element.
         * \param UB The product of the orientation matrix U by the crystal matrix B.
         * \param[out] geometry The Geometry to compute.
         */
        virtual void computeAngles(double h, double k, double l,
                                   smatrix const & UB,
                                   T & geometry) const = 0;

        /*!
         * \brief Print the state of the current Mode on a ostream.
         * \param flux
         * \return the flux modified.
         */
        ostream & printToStream(ostream & flux) const;

      public:

        typedef T value_type;

protected:
        bool _parametersAreOk(double const & h, double const & k, double const & l, smatrix const & UB, T & geometry) const throw (HKLException);

        void _computeThetaAndHphi(double const & h, double const & k, double const & l, smatrix const & UB, T const & geometry,
                                  double & theta, svector & hphi) const throw (HKLException);
      };

    template<typename T>
    Mode<T>::Mode(void) :
      ObjectWithParameters()
    {}

    template<typename T>
    Mode<T>::Mode(Mode const & mode) :
      ObjectWithParameters(mode)
    {}

    template<typename T>
    Mode<T>::~Mode(void)
      {}

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
    template<typename T>
    bool
    Mode<T>::_parametersAreOk(double const & h, double const & k, double const & l, smatrix const & UB, T & geometry) const throw (HKLException)
      {
        // Check [h,k,l]
        if (fabs(h) < constant::math::epsilon_0
            && fabs(k) < constant::math::epsilon_0
            && fabs(l) < constant::math::epsilon_0)
            HKLEXCEPTION("Cannot compute the geometry axes values of the [0,0,0] reflection.",
                         "Please set an non-null [h,k,l]");

        // check the wave length
        if (geometry.get_source().get_waveLength() < constant::math::epsilon_0)
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
    template<typename T>
    void
    Mode<T>::_computeThetaAndHphi(double const & h, double const & k, double const & l, smatrix const & UB, T const & geometry, double & theta, svector & hphi) const throw (HKLException)
      {
        // Calcule de Theta
        hphi = UB * svector(h,k,l);
        try
          {
            double lambda = geometry.get_source().get_waveLength();
            theta = convenience::asin(hphi.norm2() * lambda / constant::physic::tau / 2.);
          } 
        catch (const HKLException &)
          {
            HKLEXCEPTION("Unreachable reflection with this energy.",
                         "Please change h k l values or the energy.");
          }
      }

    template<typename T>
    ostream &
    Mode<T>::printToStream(ostream & flux) const
      { 
        flux << "Mode: " << get_name() << std::endl;
        return flux;
      }

} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << for the Mode class
 * \param flux The flux to modifie 
 * \param mode The mode to stream.
 * \return The modified flux.
 */
template<typename T>
ostream & 
operator << (ostream & flux, hkl::Mode<T> const & mode)
{
    return mode.printToStream(flux);
};

#endif // _MODE_H_
