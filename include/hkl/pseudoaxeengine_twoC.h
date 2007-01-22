#ifndef _PSEUDOAXEENGINE_TWOC_H_
#define _PSEUDOAXEENGINE_TWOC_H_

#include "pseudoaxeengine.h"
#include "geometry_twoC.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace twoC
      {
      namespace vertical
        {

        /**
         * @brief The "th2th" pseudoAxeEngine
         */
        class Th2th : public PseudoAxeEngineTemp<geometry::twoC::Vertical>
          {
          public:

            Th2th(geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Th2th(void); //!< Default destructor.

            void initialize(void) throw (HKLException);

            void update(void) throw (HKLException);

            void set(void) throw (HKLException);

            ostream & toStream(ostream & flux) const;

            istream & fromStream(istream & flux);

          private:
            Axe * _omega; //!< The omega Axe
            Axe * _tth; //!< The tth Axe
            double _omega0; //!< The omega value after initialization.
            double _tth0; //!< The tth value after initialization.

            Range _th2th_r; //!< The read part of the PseudoAxe.
            Range _th2th_w; //!< The write part of the PseudoAxe.

            PseudoAxe * _th2th; //!< The th2th PseudoAxe.
          };

        class Q2th : public PseudoAxeEngineTemp<geometry::twoC::Vertical>
          {
          public:

            Q2th(geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Q2th(void); //!< Default destructor.

            void initialize(void) throw (HKLException);

            void update(void) throw (HKLException);

            void set(void) throw (HKLException);

            ostream & toStream(ostream & flux) const;

            istream & fromStream(istream & flux);

          private:
            Axe * _omega; //!< The omega Axe
            Axe * _tth; //!< The tth Axe
            double _omega0; //!< The omega value after initialization.
            double _tth0; //!< The tth value after initialization.

            Range _q2th_r; //!< The read part of the PseudoAxe.
            Range _q2th_w; //!< The write part of the PseudoAxe.

            PseudoAxe * _q2th; //!< The q2th PseudoAxe.
          };

        class Q : public PseudoAxeEngineTemp<geometry::twoC::Vertical>
          {
          public:

            Q(geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Q(void); //!< Default destructor.

            void initialize(void) throw (HKLException);

            void update(void) throw (HKLException);

            void set(void) throw (HKLException);

            ostream & toStream(ostream & flux) const;

            istream & fromStream(istream & flux);

          private:
            Axe * _tth; //!< The real pseudoAxe engine.

            Range _q_r; //!< The read part of the PseudoAxe.
            Range _q_w; //!< The write part of the PseudoAxe.

            PseudoAxe * _q; //!< The q PseudoAxe.
          };

      } // namespace vertical.
    } // namespace twoC.
  } // namespace pseudoAxeEngine.
} // namespace hkl.

#endif // _PSEUDOAXEENGINE_TWOC_H_
