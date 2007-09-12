#ifndef _SAMPLE_H
#define _SAMPLE_H


#include "fitparameterlist.h"
#include "object.h"
#include "lattice.h"
#include <string>
#include "svector.h"
#include <ostream>
#include <istream>

#include "reflectionlist.h"
namespace hkl
  {
  class Geometry;
}
namespace hkl
  {
  class ReflectionList;
}

namespace hkl
  {

  enum SampleType
  {
    SAMPLE_MONOCRYSTAL
  };
  class Sample : public hkl::FitParameterList, public hkl::Object
    {
    protected:
      hkl::Geometry & _geometry;

      hkl::Lattice _lattice;

      hkl::ReflectionList * _reflections;

      /**
       * @brief The default constructor
       * @param geometry The geometry use to create the Reflections.
       * @param name The name of the Reflections.
       */

      Sample(hkl::Geometry & geometry, const std::string & name);


    public:
      virtual ~Sample();


    protected:
      Sample(const Sample & source);


    public:
      /**
       * @brief Clone the current Sample.
       * @return A pointer on the cloned sample.
       */

      virtual Sample * clone() const = 0;

      /**
       * @brief Get the UB matrix of the Sample.
       * @return The UB matrix.
       */

      virtual hkl::smatrix get_UB() = 0;

      /**
       * @brief Get the type of the Sample.
       *
       * @return The Sample type.
       *
       * this method is use during the toStream and fromStream process.
       */

      virtual hkl::SampleType get_type() = 0;

      /**
       * @brief Get the Lattice of the Sample.
       *
       * @return A reference on the Lattice.
       */

      hkl::Lattice & lattice();

      /**
       * @brief Get the reflections associated with the Sample.
       *
       * @return A reference on thoses reflections.
       */

      hkl::ReflectionList & reflections();

      /**
       * \brief Are two Sample equals ?
       * \param sample the Sample to compare with.
       * \return true if both are equals flase otherwise.
       */
      bool operator==(const Sample & sample) const;

      /**
       * @brief print the Sample into a flux
       * @param flux The stream to print into.
       * @return The modified flux.
       */
      std::ostream & printToStream(std::ostream & flux) const;

      /**
       * @brief print on a stream the content of the Sample
       * @param flux the ostream to modify.
       * @return the modified ostream
       */
      std::ostream & toStream(std::ostream & flux) const;

      /**
       * @brief restore the content of the Sample from an istream
       * @param flux the istream.
       * @return the modified istream.
       * @todo problem of security here.
       */
      std::istream & fromStream(std::istream & flux);

    };

} // namespace hkl
/**
 * @brief Surcharge de l'operateur << pour la class cristal
 * @param flux The ostream to print into.
 * @param sample The Sample to print
 * @return
 */
inline std::ostream &
operator << (std::ostream & flux, hkl::Sample const & sample)
{
  return sample.printToStream(flux);
}

#endif
