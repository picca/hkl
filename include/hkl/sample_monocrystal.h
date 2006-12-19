#ifndef _SAMPLE_MONOCRYSTAL_H_
#define _SAMPLE_MONOCRYSTAL_H_

#include "sample.h"

using namespace std;

namespace hkl
  {
  namespace sample
    {

    class MonoCrystal : public Sample
      {

      public:
        /**
         * @brief The default constructor.
         * @param geometry the geometry use to fill reflections.
         * @param name The name of the sample.
         */
        MonoCrystal(Geometry & geometry, MyString const & name);

        /**
         * @brief The copy constructor.
         * @param sample The sample to copy from.
         */
        MonoCrystal(MonoCrystal const & sample);

        /**
         * @brief The default destructor.
         */
        virtual ~MonoCrystal(void);

        /**
         * @brief clone the sample.
         * @return A cloned sample.
         */
        Sample * clone(void) const;

        /**
         * @brief Get the U matrix of the mono-crystal.
         * @return the U matrix.
         */
        smatrix const & get_U(void) const
          {
            return _U;
          }

        /**
         * @brief get the UB matrix.
         * @return The UB matrix.
         */
        smatrix const get_UB(void)
        {
          bool status;
          return _U * _lattice.get_B(status);
        }

        /**
         * @brief get the type of the sample.
         * @return the type of the sample.
         */
        SampleType type(void) const
          {
            return SAMPLE_MONOCRYSTAL;
          }

        /**
         * @brief Compute the orientation matrix from two non colinear reflections.
         *
         * @param index1 The index of the first reflection.
         * @param index2 The index of the second reflection.
         */
        void computeU(unsigned int index1, unsigned int index2) throw (HKLException);

        bool ready_to_fit(void) const throw (HKLException);

        double fitness(void) throw (HKLException);

        /**
         * @brief Compute the leastSquare of the crystal.
         * @return the variance.
         */
        bool fitness(double & fitness);

        /**
         * @brief Randomize the crystal
         */
        void randomize(void);

        void update(void);

        /**
         * @brief overload of the == operator for the cristal class
         * @param sample The crystal we want to compare.
         */
        bool operator == (MonoCrystal const & sample) const;

        ostream & toStream(ostream & flux) const;

        istream & fromStream(istream & flux);

      protected:

        smatrix _U; //!< The orientation matrix.


      private:

        FitParameter * _euler_x; //!< the parameter use to fit the mono-crystal and use to compute U
        FitParameter * _euler_y; //!< the parameter use to fit the mono-crystal and use to compute U
        FitParameter * _euler_z; //!< the parameter use to fit the mono-crystal and use to compute U
      };

  } // namespace sample
} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class cristal
 * @param flux The ostream to print into.
 * @param sample The sample to print 
 * @return 
 */
static ostream &
operator << (ostream & flux, hkl::sample::MonoCrystal const & sample)
{
  return sample.printToStream(flux);
}

#endif // _SAMPLE_MONOCRYSTAL_H_
