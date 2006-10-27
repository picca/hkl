#ifndef _REFLECTION_H_
#define _REFLECTION_H_

#include "geometry.h"
#include "enums.h"

using namespace std;

namespace hkl {

    /*!
     * \brief The class reflection defines a configuration where a diffraction occurs. It
     * 
     * is defined by a set of angles, the 3 integers associated to the reciprocal
     * lattice and its relevance to make sure we only take into account significant
     * reflections.
     */
    class Reflection
      {
      public:

        virtual Reflection * clone(void) const = 0;

        virtual ~Reflection(void);

        bool operator == (Reflection const & reflection) const;

        Geometry const & get_geometry(void) const {return _geometry;}

        Value & h(void) {return _h;}

        Value & k(void) {return _k;}

        Value & l(void) {return _l;}

        bool & flag(void) {return _flag;}

        Value const & h(void) const {return _h;}

        Value const & k(void) const {return _k;}

        Value const & l(void) const {return _l;}

        bool const & flag(void) const {return _flag;}

        svector getHKL(void) const;

        Value computeAngle(Value const & h2, Value const & k2, Value const & l2) const;
        
        bool isColinear(Reflection const & reflection) const;
        
        ostream & printToStream(ostream & flux) const;

        ostream & toStream(ostream & flux) const;
        
        istream & fromStream(istream & flux);

      protected:
        Geometry _geometry; //!< The corresponding Geometry.
        Value _h; //!< The first of the three numbers (h,k,l).
        Value _k; //!< The second of the three numbers (h,k,l).
        Value _l; //!< The third of the three numbers (h,k,l).
        bool _flag; //!< is the reflection use for calculation.

        Reflection(Geometry const & geometry,
                   Value const & h,
                   Value const & k,
                   Value const & l,
                   bool const & flag);

        Reflection(Reflection const & reflection);
      };

    enum Relevance
      {
        notVerySignificant = 0, //!< not very significant reflection
        Significant, //!< significant reflection
        VerySignificant, //!< very significant reflection
        Best //!< Best reflection
      };
} // namespace hkl

/*!
 * \brief Surcharge de l'operateur << pour la class reflection
 * \param flux The flux to print into
 * \param r
 */
static ostream &
operator << (ostream & flux, hkl::Reflection const & reflection)
{
    return reflection.printToStream(flux);
}

#endif // _REFLECTION_H_
