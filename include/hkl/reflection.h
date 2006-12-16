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
 */
class Reflection
{
public:

    virtual Reflection * clone(void) const = 0;

    virtual ~Reflection(void);

    bool operator == (Reflection const & reflection) const;

    Geometry const & get_geometry(void) const
    {
        return _geometry;
    }

    svector const & get_hkl_phi(void) const
    {
        return _hkl_phi;
    }

    svector const & get_hkl(void) const
    {
        return _hkl;
    }

    bool const & flag(void) const
    {
        return _flag;
    }

    bool & flag(void)
    {
        return _flag;
    }

    void set_hkl(svector const & hkl)
    {
        _hkl = hkl;
    }

    Value computeAngle(svector const & hkl) const;

    bool isColinear(Reflection const & reflection) const;

    ostream & printToStream(ostream & flux) const;

    ostream & toStream(ostream & flux) const;

    istream & fromStream(istream & flux);

protected:
    Geometry _geometry; //!< The corresponding Geometry.
    svector _hkl;
    bool _flag; //!< is the reflection use for calculation.
    svector _hkl_phi; //!< The hkl vector in the last axes repere.

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
    * 
    * @param reflection The reflection to Copy fro.
    */
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
