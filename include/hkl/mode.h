#ifndef _MODE_H_
#define _MODE_H_

#include "config.h"

#include <iostream>

#include "mymap.h"
#include "value.h"
#include "svecmat.h"
#include "myvector.h"
#include "geometry.h"
#include "HKLException.h"
#include "objectwithparameters.h"

using namespace std;

namespace hkl {
  /*!
   * \brief This class defines how to use a diffractomer.
   */
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
                                 Geometry & geometry) const = 0;
      
      /*!
       * \brief Print the state of the current Mode on a ostream.
       * \param flux
       * \return the flux modified.
       */
      ostream & printToStream(ostream & flux) const;
      
    public:

      Mode(void); //!< Default constructor - protected to make sure this class is abstract.
  };

#ifdef MSVC6
  typedef MyStarMap<Mode*> ModeList;
#else
  typedef MyMap<Mode*> ModeList; //!< \typedef a MyMap containing pointers of Mode.
#endif

  namespace mode {

    /** 
     * @brief A dummy mode just for testing purpose.
     */
      class Dummy : public Mode
      {
      public:
        
        Dummy(void);

        virtual ~Dummy(void);
        
        void computeAngles(double h, double k, double l,
                           smatrix const & UB,
                           Geometry & geometry) const;
      };
  }
  
} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << for the Mode class
 * \param flux The flux to modifie 
 * \param mode The mode to stream.
 * \return The modified flux.
 */
ostream & operator << (ostream & flux, hkl::Mode const & mode);

#endif // _MODE_H_
