#ifndef _PSEUDOAXE_TWOC_H_
#define _PSEUDOAXE_TWOC_H_

#include "pseudoaxe.h"
#include "geometry_twoC.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace twoC {
            namespace vertical {

              /** 
               * @brief The "th2th" pseudoAxe
               */
                class Th2th : public PseudoAxe<geometry::twoC::Vertical>
                {
                public:

                  Th2th(geometry::twoC::Vertical & geometry); //!< Default constructor.

                  virtual ~Th2th(void); //!< Default destructor.

                  double get_min(void) const;

                  double get_max(void) const;

                  double get_value(void) throw (HKLException);

                  void set_value(double const &) throw (HKLException);

                  bool isValid(void) throw (HKLException);
                };

                class Q2th : public PseudoAxe<geometry::twoC::Vertical>
                {
                public:

                  Q2th(geometry::twoC::Vertical & geometry); //!< Default constructor.

                  virtual ~Q2th(void); //!< Default destructor.

                  double get_min(void)const ;

                  double get_max(void) const;

                  double get_value(void) throw (HKLException);

                  void set_value(double const & value) throw (HKLException);

                  bool isValid(void) throw (HKLException);
                };

                class Q : public PseudoAxe<geometry::twoC::Vertical>
                {
                public:

                  Q(geometry::twoC::Vertical & geometry); //!< Default constructor.

                  virtual ~Q(void); //!< Default destructor.

                  double get_min(void) const;

                  double get_max(void) const;

                  double get_value(void) throw (HKLException);

                  void set_value(double const & value) throw (HKLException);

                  bool isValid(void) throw (HKLException);
                };

            } // namespace vertical.
        } // namespace twoC.
    } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_TWOC_H_
