#ifndef _PSEUDOAXE_TWOC_H_
#define _PSEUDOAXE_TWOC_H_

#include "pseudoaxe.h"
#include "geometry_twoC.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace twoC
      {
      namespace vertical
        {

        /**
         * @brief The "th2th" pseudoAxe
         */
        class Th2th : public PseudoAxeTemp<geometry::twoC::Vertical>
          {
          public:

            Th2th(geometry::twoC::Vertical & geometry); //!< Default constructor.

            void initialize(void) throw (HKLException);

            void update(void);

            void set_current(Value const & value) throw (HKLException);

            ostream & toStream(ostream & flux) const;

            istream & fromStream(istream & flux);

          private:
            Axe * _omega; //!< The omega Axe
            Axe * _tth; //!< The tth Axe
            double _omega0; //!< The omega value after initialization.
            double _tth0; //!< The tth value after initialization.
          };

        class Q2th : public PseudoAxeTemp<geometry::twoC::Vertical>
          {
          public:

            Q2th(geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Q2th(void); //!< Default destructor.

            void initialize(void) throw (HKLException);

            void update(void);

            void set_current(Value const & value) throw (HKLException);

            ostream & toStream(ostream & flux) const;

            istream & fromStream(istream & flux);

          private:
            Axe * _omega; //!< The omega Axe
            Axe * _tth; //!< The tth Axe
            double _omega0; //!< The omega value after initialization.
            double _tth0; //!< The tth value after initialization.
          };

        class Q : public PseudoAxeTemp<geometry::twoC::Vertical>
          {
          public:

            Q(geometry::twoC::Vertical & geometry); //!< Default constructor.

            virtual ~Q(void); //!< Default destructor.

            void initialize(void) throw (HKLException);

            void update(void);

            void set_current(Value const & value) throw (HKLException);

          private:
            Axe * _tth; //!< The real pseudoAxe engine.
          };

      } // namespace vertical.
    } // namespace twoC.
  } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_TWOC_H_
