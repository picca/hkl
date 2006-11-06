#ifndef _SAMPLE_H_
#define _SAMPLE_H_

#include "object.h"
#include "lattice.h"
#include "fitparameterlist.h"
#include "reflectionlist.h"

using namespace std;

namespace hkl
  {

  /**
   * @brief Class which store the Sample parameters
   *
   * Class Sample to store direct and reciprocal lattice 
   * parameters and the matrix to move from the reciprocal
   * lattice to the cristal cartesian system.
   * References :
   *
   * William R. Busing and Henri A. Levy "Angle calculation 
   * for 3- and 4- Circle X-ray and  Neutron Diffractometer" (1967)
   * <A HREF="http://journals.iucr.org/index.html"> Acta
   * Cryst.</A>, <B>22</B>, 457-464.
   *
   * A.J.C. Wilson "X-Ray Optics, The Diffraction of X-Rays
   * By Finite and Imperfect Crystals"
   * (1962) John Wiley & Sons Inc., 14-17.
   */
  class Sample : public FitParameterList, public Object
    {

    public:

      virtual ~Sample(void);

      virtual Sample * clone(void) const = 0;

      virtual smatrix const get_UB(void) = 0; //!< get the m_B %smatrix

      virtual SampleType type(void) const = 0;

      Lattice & lattice(void)
      {
        return _lattice;
      } //!< return the lattice parameters.

      ReflectionList & reflections(void)
      {
        return *_reflections;
      } //!< get the reflectionList

      virtual bool operator == (Sample const & sample) const;

      ostream & printToStream(ostream & flux) const;

      virtual ostream & toStream(ostream & flux) const;

      virtual istream & fromStream(istream & flux);

    protected:
      Geometry & _geometry; //!< The geometry use when adding reflection.
      Lattice _lattice; //!< The lattice.
      ReflectionList * _reflections; //!< the reflection list associated with this crystal

      Sample(Geometry & geometry, MyString const & name);

      Sample(Sample const & sample);

    };

} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class cristal
 * @param flux 
 * @param C 
 * @return 
 */
static ostream &
operator << (ostream & flux, hkl::Sample const & sample)
{
  return sample.printToStream(flux);
}

#endif // _SAMPLE_H_
