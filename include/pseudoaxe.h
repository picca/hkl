#ifndef _PSEUDOAXE_H_
#define _PSEUDOAXE_H_

#include "config.h"

#include <iostream>

#include "objectwithparameters.h"

namespace hkl {

  class Geometry;

  /**
   * \brief A class design to describe a pseudomotor.
   */
  class PseudoAxe : public ObjectWithParameters
  {
    public:
      /**
       * \brief Default constructor.
       */
      PseudoAxe(void);

      /**
       * \brief Copy constructor.
       * \param pseudoAxe A PseudoAxe to copy from.
       */
      PseudoAxe(PseudoAxe const & pseudoAxe);

      /**
       * \brief The default destructor
       */
      virtual ~PseudoAxe(void);

      /**
       * \brief Are two PseudoAxe equals ?
       * \param pseudoAxe The PseudoAxe to compare with.
       */
      bool operator ==(PseudoAxe const & pseudoAxe) const;

      /**
       * \brief print the PseudoAxe into a flux
       * \param flux The stream to print into.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * \brief Initialize the PseudoAxe from the Geometry.
       * \param geometry The configuration to save for calculation.
       */
      virtual void init(Geometry const & geometry) = 0;
          
      /**
       * \brief get the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real #Axe
       * \return the position of the PseudoAxe.
       */
      virtual double const get_value(Geometry const & geometry) const = 0;

      /**
       * \brief set the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real #Axe
       * \param value the value to set.
       */
      virtual void set_value(Geometry & geometry, double value) throw (HKLException) = 0;
  };

#ifdef VCPP6
#include "mymap.h"
  typedef MyStarMap<PseudoAxe*> PseudoAxeList;
#else
#include "myvector.h"
  typedef MyVector<PseudoAxe*> PseudoAxeList;
#endif

} // namespace hkl

/**
 * \brief Overload of the << operator for the #PseudoAxe class
 */
std::ostream & operator<<(std::ostream & flux, hkl::PseudoAxe const & pseudoAxe); 


#endif // _PSEUDOAXE_H_
