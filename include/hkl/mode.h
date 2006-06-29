#ifndef _MODE_H_
#define _MODE_H_

#include <iostream>

#include "svecmat.h"
#include "geometry.h"
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

        Mode(void); //!< Default constructor - protected to make sure this class is abstract.

        Mode(Mode const & mode); //!< Copy constructor
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
