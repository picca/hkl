#ifndef _SAMPLE_MONOCRYSTAL_H
#define _SAMPLE_MONOCRYSTAL_H


#include "sample.h"
#include "svecmat.h"
#include <string>
#include "HKLException.h"
#include <ostream>
#include <istream>

namespace hkl
  {
  class FitParameter;
}
namespace hkl
  {
  class Geometry;
}

namespace hkl
  {

  namespace sample
    {

    class MonoCrystal : public hkl::Sample
      {
      protected:
        hkl_smatrix _U;

        hkl::FitParameter * _euler_x;

        hkl::FitParameter * _euler_y;

        hkl::FitParameter * _euler_z;


      public:
        /**
         * @brief The default constructor.
         * @param geometry the geometry use to fill reflections.
         * @param name The name of the sample.
         */

        MonoCrystal(hkl::Geometry & geometry, const std::string & name);

        /**
         * @brief The default destructor.
         */

        virtual ~MonoCrystal();

        /**
         * @brief The copy constructor.
         * @param sample The sample to copy from.
         */

        MonoCrystal(const MonoCrystal & source);

        /**
         * @brief Clone the current Sample.
         * @return A pointer on the cloned sample.
         */

        virtual hkl::Sample * clone() const;

        inline hkl_smatrix const * get_U() const;

        /**
         * @brief Get the UB matrix of the Sample.
         * @return The UB matrix.
         */

        virtual void get_UB(hkl_smatrix * UB);

        /**
         * @brief Get the type of the Sample.
         *
         * @return The Sample type.
         *
         * this method is use during the toStream and fromStream process.
         */

        virtual hkl::SampleType get_type();

        /**
         * @brief Compute the orientation matrix from two non colinear reflections.
         *
         * @param index1 The index of the first reflection.
         * @param index2 The index of the second reflection.
         */

        void computeU(unsigned int index1, unsigned int index2) throw(hkl::HKLException);

        bool ready_to_fit() const;

        double fitness() throw(hkl::HKLException);

        bool fitness(double & fitness);

        /**
         * @brief Randomize the crystal
         */

        void randomize();

        void update();

        /**
         * \brief Are two MonoCrystal equals ?
         * \param sample the MonoCrystal to compare with.
         * \return true if both are equals flase otherwise.
         */
        bool operator==(const MonoCrystal & sample) const;

      };
    inline hkl_smatrix const * MonoCrystal::get_U() const
      {
        return &_U;
      }


  } // namespace hkl::sample

} // namespace hkl
#endif
