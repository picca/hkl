
#include "reflection_monocrystal.h"
#include "geometry.h"
#include "svecmat.h"

namespace hkl
  {

  namespace reflection
    {

    /**
     * @brief The default constructor.
     *
     * @param geometry The geometry use to initialize the geometry store in the reflections.
     * @param hkl the scattering vector of the reflection.
     * @param flag the falg of the reflection (related to the affinement).
     * @throw HKLException if the geometry is not valid.
     */

    MonoCrystal::MonoCrystal(const hkl::Geometry & geometry, hkl_svector const * hkl, bool flag) throw(hkl::HKLException) :
        Reflection( geometry, hkl, flag )
    {
      // do not forgot to update _hkl_phi
      hkl_smatrix R;

      _geometry.get_sample_rotation_matrix(&R);
      _geometry.get_Q(&_hkl_phi);

      ::hkl_smatrix_transpose(&R);
      ::hkl_smatrix_times_svector(&R, &_hkl_phi);
    }

    MonoCrystal::~MonoCrystal()
    {
    }

    /**
     * @brief The copy constructor
     * @param reflection  The Reflection to copy.
     */

    MonoCrystal::MonoCrystal(const hkl::reflection::MonoCrystal & source) :
        Reflection(source)
    {
    }

    /**
     * @brief clone the reflection.
     * @return a cloned reflection.
     */

    hkl::Reflection * MonoCrystal::clone() const
      {
        return new MonoCrystal(*this);
      }


  } // namespace hkl::reflection

} // namespace hkl
