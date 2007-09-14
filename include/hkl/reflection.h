#ifndef _REFLECTION_H
#define _REFLECTION_H


#include "geometry.h"
#include "svecmat.h"
#include "value.h"
#include <ostream>
#include <istream>

namespace hkl
  {

  enum ReflectionType
  {
    REFLECTION_MONOCRYSTAL
  };
  class Reflection
    {
    protected:
      hkl::Geometry _geometry;

      hkl_svector _hkl;

      bool _flag;

      hkl_svector _hkl_phi;

      /**
       * @brief Create a Reflection.
       *
       * @param geometry The hkl::Geometry of the reflection
       * @param hkl The hkl scattering vactor.
       * @param flag if the reflection must be use during calculation.
       * @throw HKLException if the geometry is not valid.
       */

      Reflection(const hkl::Geometry & geometry, hkl_svector const * hkl, bool flag);


    public:
      virtual ~Reflection();

      /**
       * @brief Create a clone of the current Reflection
       * @return A pointer on the newly created Reflection.
       *
       * do not forget to release the memory at the end with delete.
       */

      virtual Reflection * clone() const = 0;

      /**
       * @brief Get a constant reference on the geometry part of the Reflection.
       * @return the geometry part of the Reflecion.
       */

      inline const hkl::Geometry & get_geometry() const;

      /**
       * @brief Get a constant reference on the hkl scattering vector store in the Reflection.
       * @return The hkl coordinates in the crystal coordinates.
       */

      inline const hkl_svector * get_hkl() const;

      /**
       * @brief Set the hkl scattering vector store in the Reflection.
       * @param hkl The scattering vector in the crystal coordinates to store in the Reflection.
       */

      void set_hkl(hkl_svector const * hkl);

      /**
       * @brief Get a constant reference on the hkl scattering vector store to fasten the affinement calculation.
       * @return The hkl coordinates in the last Axe coordinates.
       */

      inline const hkl_svector * get_hkl_phi() const;

      /**
       * @brief Get a constant reference on the flag store in the Reflection.
       * @return The flag of the Reflection.
       *
       * the flag is true when we use the reflection in the affinement, false otherwise.
       */

      const bool & flag() const;

      /**
       * @brief Get a constant reference on the flag store in the Reflection.
       * @return The flag of the Reflection.
       *
       * the flag is true when we use the reflection in the affinement, false otherwise.
       */

      bool & flag();

      /**
       * @brief compute the theoretical angle beetween two hkl vectors.
       * @param hkl The second scattering vector to compare with the Reflection internal hkl.
       * @return the angle between the two hkl.
       * @todo Maybe move this in the Sample and add a computeAngle(Reflection const & reflection)
       * @todo add the mathematical formula.
       */

      hkl::Value computeAngle(const hkl_svector * hkl) const;

      /**
       * @brief Check if two reflections are colinear.
       * @param reflection The reflection to compare with.
       * @return true if the reflections are colinear, false otherwise.
       * @todo Add the mathematical formula.
       */

      bool isColinear(const Reflection & reflection) const;

      /**
       * \brief Are two Reflection equals ?
       * \param reflection the Reflection to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const Reflection & reflection) const;

      /**
       * @brief print the Reflection into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the Reflection
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the Reflection from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };
  /**
   * @brief Get a constant reference on the geometry part of the Reflection.
   * @return the geometry part of the Reflecion.
   */

  inline const hkl::Geometry & Reflection::get_geometry() const
    {
      return _geometry;
    }

  /**
   * @brief Get a constant reference on the hkl scattering vector store in the Reflection.
   * @return The hkl coordinates in the crystal coordinates.
   */

  inline hkl_svector const * Reflection::get_hkl() const
    {
      return &_hkl;
    }

  /**
   * @brief Get a constant reference on the hkl scattering vector store to fasten the affinement calculation.
   * @return The hkl coordinates in the last Axe coordinates.
   */

  inline hkl_svector const * Reflection::get_hkl_phi() const
    {
      return &_hkl_phi;
    }


} // namespace hkl
/**
 * @brief Surcharge de l'operateur << pour la class reflection
 * @param flux The flux to print into
 * @param reflection The Reflection to print.
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::Reflection const & reflection)
{
  return reflection.printToStream(flux);
}
#endif
