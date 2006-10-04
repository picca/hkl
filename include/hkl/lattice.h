#ifndef _LATTICE_H_
#define _LATTICE_H_

#include "svecmat.h"
#include "fitparameter.h"

using namespace std;

namespace hkl {

    class Lattice
      {

      public:

        /** 
         * @brief The default constructor.
         */
        Lattice(void);

        /** 
         * @brief The copy constructor.
         * 
         * @param lattice 
         */
        Lattice(Lattice const & lattice);

        /** 
         * @brief The default destructor.
         */
        virtual ~Lattice(void);

        FitParameter & a(void) {return *_a;}
        FitParameter & b(void) {return *_b;}
        FitParameter & c(void) {return *_c;}
        FitParameter & alpha(void) {return *_alpha;}
        FitParameter & beta(void) {return *_beta;}
        FitParameter & gamma(void) {return *_gamma;}

        smatrix const & get_B(void) throw (HKLException); //!< get the m_B smatrix

        Lattice const reciprocal(void) const throw (HKLException);

        void randomize(void);

        bool operator == (Lattice const & lattice) const;

        ostream & printToStream(ostream & flux) const;

        ostream & toStream(ostream & flux) const;

        istream & fromStream(istream & flux);

      protected:
        FitParameter * _a;
        FitParameter * _b;
        FitParameter * _c;
        FitParameter * _alpha;
        FitParameter * _beta;
        FitParameter * _gamma;

        smatrix _B;

        void _computeB(void) throw (HKLException);

      private:
        double _old_a;
        double _old_b;
        double _old_c;
        double _old_alpha;
        double _old_beta;
        double _old_gamma;
      };

} // namespace hkl

/**
 * @brief Surcharge de l'operateur << pour la class Lattice
 * @param flux 
 * @param C 
 * @return 
 */
inline ostream &
operator << (ostream & flux, hkl::Lattice const & lattice)
{ 
    return lattice.printToStream(flux);
}

#endif // _LATTICE_H_
