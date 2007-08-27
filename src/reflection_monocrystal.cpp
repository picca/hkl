
#include "reflection_monocrystal.h"
#include "geometry.h"
#include "svector.h"

namespace hkl {

namespace reflection {

/**
 * @brief The default constructor.
 * 
 * @param geometry The geometry use to initialize the geometry store in the reflections.
 * @param hkl the scattering vector of the reflection. 
 * @param flag the falg of the reflection (related to the affinement).
 * @throw HKLException if the geometry is not valid. 
 */

MonoCrystal::MonoCrystal(const hkl::Geometry & geometry, const hkl::svector & hkl, bool flag) throw(hkl::HKLException) :
  Reflection( geometry, hkl, flag )
{
  // Bouml preserved body begin 0002DE02
  // do not forgot to update _hkl_phi
    _hkl_phi = _geometry.get_sample_rotation_matrix().transpose() * _geometry.get_Q();
  // Bouml preserved body end 0002DE02
}

MonoCrystal::~MonoCrystal() 
{
  // Bouml preserved body begin 00034502
  // Bouml preserved body end 00034502
}

/**
 * @brief The copy constructor
 * @param reflection  The Reflection to copy.
 */

MonoCrystal::MonoCrystal(const hkl::reflection::MonoCrystal & source) :
  Reflection(source) 
{
  // Bouml preserved body begin 0002DE82
  // Bouml preserved body end 0002DE82
}

/**
 * @brief clone the reflection.
 * @return a cloned reflection.
 */

hkl::Reflection * MonoCrystal::clone() const 
{
  // Bouml preserved body begin 0002DF02
      return new MonoCrystal(*this);
  // Bouml preserved body end 0002DF02
}


} // namespace hkl::reflection

} // namespace hkl
