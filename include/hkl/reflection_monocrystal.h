#ifndef _REFLECTION_MONOCRYSTAL_H_
#define _REFLECTION_MONOCRYSTAL_H_

#include "reflection.h"

using namespace std;

namespace hkl {
    namespace reflection {

        class MonoCrystal : public Reflection
        {
        public:

          MonoCrystal(Geometry const & geometry,
                      Value const & h,
                      Value const & k,
                      Value const & l,
                      bool const & flag);

          Reflection * clone(void) const;

          MonoCrystal(MonoCrystal const & reflection);

          virtual ~MonoCrystal(void);

          svector const & get_hkl_phi(void) const {return _hkl_phi;}

          ostream & printToStream(ostream & flux) const;

          bool operator == (MonoCrystal const & reflection) const;

          ostream & toStream(ostream & flux) const;

          istream & fromStream(istream & flux);

        protected:

          svector _hkl_phi;
        };

    } // namespace reflection
} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << pour la class reflection
 * \param flux The flux to print into
 * \param r
 */
static ostream &
operator << (ostream & flux, hkl::reflection::MonoCrystal const & reflection)
{
    return reflection.printToStream(flux);
}

#endif // _REFLECTION_MONOCRYSTAL_H_
