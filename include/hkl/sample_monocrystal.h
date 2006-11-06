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
        MonoCrystal(Geometry & geometry, MyString const & name);

        MonoCrystal(MonoCrystal const & sample);

        virtual ~MonoCrystal(void);

        Sample * clone(void) const;

        smatrix const & get_U(void) const
          {
            return _U;
          }

        smatrix const get_UB(void)
        {
          return _U * _lattice.get_B();
        }

        SampleType type(void) const
          {
            return SAMPLE_MONOCRYSTAL;
          }

        void computeU(unsigned int index1, unsigned int index2) throw (HKLException);

        double fitness(void) throw (HKLException);

        void randomize(void);

        bool operator == (MonoCrystal const & sample) const;

        ostream & toStream(ostream & flux) const;

        istream & fromStream(istream & flux);

      protected:

        smatrix _U; //!< The orientation matrix.

      private:

        FitParameter * _euler_x;
        FitParameter * _euler_y;
        FitParameter * _euler_z;
      };

  } // namespace sample
} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class cristal
 * @param flux 
 * @param C 
 * @return 
 */
static ostream &
operator << (ostream & flux, hkl::sample::MonoCrystal const & sample)
{
  return sample.printToStream(flux);
}

#endif // _SAMPLE_MONOCRYSTAL_H_
