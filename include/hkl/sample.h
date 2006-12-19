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

      /**
       * @brief the default destructor
       */
      virtual ~Sample(void);

      /**
       * @brief Clone the current Sample.
       * @return A pointer on the cloned sample.
       */
      virtual Sample * clone(void) const = 0;

      /**
       * @brief Get the UB matrix of the Sample.
       * @return The UB matrix.
       */
      virtual smatrix const get_UB(void) = 0; //!< get the m_B %smatrix

      /**
       * @brief Get the type of the Sample.
       * 
       * @return The Sample type.
       *
       * this method is use during the toStream and fromStream process.
       */
      virtual SampleType type(void) const = 0;

      /**
       * @brief Get the Lattice of the Sample.
       * 
       * @return A reference on the Lattice.
       */
      Lattice & lattice(void)
      {
        return _lattice;
      }

      /**
       * @brief Get the reflections associated with the Sample.
       * 
       * @return A reference on thoses reflections.
       */
      ReflectionList & reflections(void)
      {
        return *_reflections;
      }

      /**
       * @brief Are two samples equals ?
       * @param sample the Sample to compare with.
       * @return True if both are equals, false otherwise.
       */
      virtual bool operator == (Sample const & sample) const;

      /**
       * @brief print the Sample into a flux
       * @param flux The stream to print into.
       * @return The modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Serialize the Sample.
       * @param  flux The stream to save the Sample into.
       * @return the flux with the Sample serialized. 
       */
      virtual ostream & toStream(ostream & flux) const;

      /**
       * @brief UnSerialize the Sample.
       * 
       * @param flux The stream to load the Sample from.
       * 
       * @return The flux without the Sample un-serialized.
       */
      virtual istream & fromStream(istream & flux);

    protected:
      Geometry & _geometry; //!< The geometry use when adding reflection.
      Lattice _lattice; //!< The lattice.
      ReflectionList * _reflections; //!< the reflection list associated with this crystal

      /**
       * @brief The default constructor
       * @param geometry The geometry use to create the Reflections.
       * @param name The name of the Reflections.
       */
      Sample(Geometry & geometry, MyString const & name);

      /**
       * @brief The copy constructor.
       * @param sample The Sample to copy.
       */
      Sample(Sample const & sample);

    };

} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class cristal
 * @param flux The ostream to print into.
 * @param sample The Sample to print 
 * @return 
 */
static ostream &
operator << (ostream & flux, hkl::Sample const & sample)
{
  return sample.printToStream(flux);
}

#endif // _SAMPLE_H_
