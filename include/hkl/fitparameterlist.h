#ifndef _FITPARAMETERLIST_H_
#define _FITPARAMETERLIST_H_

#include <math.h>

#include "fitparameter.h"

using namespace std;

namespace hkl {

    /**
     * @brief A class design to describe a FitParameterList for the simplex methode
     */

    class FitParameterList
    {
    public:

      virtual ~FitParameterList(void);

      bool operator==(FitParameterList const & fitParameterList) const;

      ostream & printToStream(ostream & flux) const;

      ostream & toStream(ostream & flux) const;

      istream & fromStream(istream & flux);

      unsigned int size(void) const;

      FitParameter & operator[](MyString const & name) throw (HKLException);

      unsigned int size_to_fit(void) const;

      vector<FitParameter *>::iterator begin(void);

      vector<FitParameter *>::iterator end(void);

      vector<FitParameter *>::const_iterator begin(void) const;

      vector<FitParameter *>::const_iterator end(void) const;

      /*!
       * @brief Randomize all the fitParameter of the FitParameterList.
       */
      virtual void randomize(void) = 0;

      /*!
       * @brief Calculation of the fitness.
       * @return the fitness calculated from the fitParameters.
       */
      virtual double fitness(void) throw (HKLException) = 0;

    protected:
      vector<FitParameter *> _parameters;
    };

} // namespace hkl

/*!
 * @brief Overload of the << operator for the %FitParameterList class
 * @param flux
 * @param fitParameterList
 * @return the modified flux.
 */
ostream & operator<<(ostream & flux, hkl::FitParameterList const & fitParameterList); 

#endif // _FITPARAMETERLIST_H_
