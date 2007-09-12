#ifndef _AFFINEMENT_SIMPLEX_H
#define _AFFINEMENT_SIMPLEX_H


#include "affinement.h"
#include "HKLException.h"
#include <valarray>
using namespace std;

namespace hkl
  {
  class FitParameterList;
}

namespace hkl
  {

  namespace affinement
    {

    class Simplex : public hkl::Affinement
      {
      public:
        Simplex();

        virtual ~Simplex();

        /**
         * @brief fit the data using the simplex method.
         * @param fitParameterList the hkl::FitParameterList to fit.
         *
         * This function modify the vertex.
         */

        virtual void fit(hkl::FitParameterList & fitParameterList) throw(hkl::HKLException);


      protected:
        void _updateParameterListFromVertex(const hkl::FitParameterList & fitParameterList, valarray<double> & parameterList);

        void _updateVertexFromParameterList(hkl::FitParameterList & fitParameterList, const valarray<double> & parameterList);

      };

  } // namespace hkl::affinement

} // namespace hkl
#endif
