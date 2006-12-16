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
        bool status;
        return _U * _lattice.get_B(status);
    }

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

    FitParameter * _euler_x;
    FitParameter * _euler_y;
    FitParameter * _euler_z;
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
