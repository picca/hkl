#ifndef _REFLECTION_H_
#define _REFLECTION_H_

#include "geometry.h"
#include "enums.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief The class reflection defines a configuration where a diffraction occurs. It
   * 
   * is defined by a set of angles, the 3 integers associated to the reciprocal
   * lattice and its relevance to make sure we only take into account significant
   * reflections.
   * @todo rewrite the reflection description.
   */
  class Reflection
    {
    public:

      /**
       * @brief Create a clone of the current Reflection
       * @return A pointer on the newly created Reflection.
       *
       * do not forget to release the memory at the end with delete.
       */
      virtual Reflection * clone(void) const = 0;

      /**
       * @brief The default destructor.
       */
      virtual ~Reflection(void);

      /**
       * @brief compare two Reflections.
       * @param reflection the reflection to compare with.
       * @return true if both are equals, false neither.
       */
      bool operator == (Reflection const & reflection) const;

      /**
       * @brief Get a constant reference on the geometry part of the Reflection.
       * @return the geometry part of the Reflecion.
       */
      Geometry const & get_geometry(void) const
        {
          return _geometry;
        }

      /**
       * @brief Get a constant reference on the hkl scattering vector store to fasten the affinement calculation.
       * @return The hkl coordinates in the last Axe coordinates.
       */
      svector const & get_hkl_phi(void) const
        {
          return _hkl_phi;
        }

      /**
       * @brief Get a constant reference on the hkl scattering vector store in the Reflection.
       * @return The hkl coordinates in the crystal coordinates.
       */
      svector const & get_hkl(void) const
        {
          return _hkl;
        }

      /**
       * @brief Get a constant reference on the flag store in the Reflection.
       * @return The flag of the Reflection.
       *
       * the flag is true when we use the reflection in the affinement, false otherwise.
       */
      bool const & flag(void) const
        {
          return _flag;
        }

      /**
       * @brief Get a reference on the flag store in the Reflection.
       * @return The flag of the Reflection.
       *
       * The flag is true when we use the reflection in the affinement, false otherwise.
       * with this method we can taggle the flag value.
       */
      bool & flag(void)
      {
        return _flag;
      }

      /**
       * @brief Set the hkl scattering vector store in the Reflection.
       * @param hkl The scattering vector in the crystal coordinates to store in the Reflection.
       */
      void set_hkl(svector const & hkl)
      {
        _hkl = hkl;
      }

      /**
       * @brief compute the theoretical angle beetween two hkl vectors.
       * @param hkl The second scattering vector to compare with the Reflection internal hkl.
       * @return the angle between the two hkl.
       * @todo Maybe move this in the Sample and add a computeAngle(Reflection const & reflection)
       * @todo add the mathematical formula.
       */
      Value computeAngle(svector const & hkl) const;

      /**
       * @brief Check if two reflections are colinear.
       * @param reflection The reflection to compare with.
       * @return true if the reflections are colinear, false otherwise.
       * @todo Add the mathematical formula.
       */
      bool isColinear(Reflection const & reflection) const;

      /**
       * @brief Methode use to print on a stream a Reflection.
       * @param flux The stream to put the Reflection into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Methode use to store a Reflection in a stream.
       * @param flux The stream to put the Reflection into.
       * @return The modified stream.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Methode use to restore a Reflection from a stream.
       * @param flux The stream to get the Reflection from.
       * @return The modified stream.
       */
      istream & fromStream(istream & flux);

    protected:
      Geometry _geometry; //!< The corresponding Geometry.
      svector _hkl; //!< the scattering vector store in the Reflection.
      bool _flag; //!< is the reflection use for calculation.
      svector _hkl_phi; //!< The hkl vector in the last axes repere (use to fasten the affinement).

      /**
       * @brief Create a Reflection.
       * 
       * @param geometry The Geometry of the reflection
       * @param hkl The hkl scattering vactor.
       * @param flag if the reflection must be use during calculation.
       * @throw HKLException if the geometry is not valid.
       */
      Reflection(Geometry const & geometry,
                 svector const & hkl,
                 bool const & flag) throw (HKLException);

      /**
       * @brief The copy contructor.
       * @param reflection The reflection to Copy fro.
       */
      Reflection(Reflection const & reflection);
    };

  /**
   * this enum is use to qualify the reflection relevance.
   * @todo move to the enum.h file
   */
  enum Relevance
  {
    notVerySignificant = 0, //!< not very significant reflection
    Significant, //!< significant reflection
    VerySignificant, //!< very significant reflection
    Best //!< Best reflection
  };
} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class reflection
 * @param flux The flux to print into
 * @param reflection The Reflection to print.
 */
static ostream &
operator << (ostream & flux, hkl::Reflection const & reflection)
{
  return reflection.printToStream(flux);
}

#endif // _REFLECTION_H_
