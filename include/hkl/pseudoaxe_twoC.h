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

                  Th2th(void); //!< Default constructor.

                  virtual ~Th2th(void); //!< Default destructor.

                  /** 
                   * @brief Initialize the pseudoAxe
                   * 
                   * @param geometry The geometry to store.
                   * @throw HKLException No 
                   */
                  void initialize(geometry::twoC::Vertical const & geometry) throw (HKLException);

                  bool get_isValid(geometry::twoC::Vertical const & geometry) const;

                  double get_value(geometry::twoC::Vertical const & geometry) const throw (HKLException);

                  void set_value(geometry::twoC::Vertical & geometry, double const & value) const throw (HKLException);

                };
                
                class Q2th : public PseudoAxe<geometry::twoC::Vertical>
                {
                public:

                  Q2th(void); //!< Default constructor.

                  virtual ~Q2th(void); //!< Default destructor.

                  /** 
                   * @brief Initialize the pseudoAxe.
                   * 
                   * @param geometry The geometry to store
                   * @throw HKLException if the geometry wave Length is null.
                   */
                  void initialize(geometry::twoC::Vertical const & geometry) throw (HKLException);

                  bool get_isValid(geometry::twoC::Vertical const & geometry) const;

                  double get_value(geometry::twoC::Vertical const & geometry) const throw (HKLException);

                  void set_value(geometry::twoC::Vertical & geometry, double const & value) const throw (HKLException);

                };
                
                class Q : public PseudoAxe<geometry::twoC::Vertical>
                {
                public:

                  Q(void); //!< Default constructor.

                  virtual ~Q(void); //!< Default destructor.

                  /** 
                   * @brief Initialize the pseudoAxe.
                   * 
                   * @param geometry The geometry to store
                   * @throw HKLException if the geometry wave Length is null.
                   */
                  void initialize(geometry::twoC::Vertical const & geometry) throw (HKLException);

                  bool get_isValid(geometry::twoC::Vertical const & geometry) const;

                  double get_value(geometry::twoC::Vertical const & geometry) const throw (HKLException);

                  void set_value(geometry::twoC::Vertical & geometry, double const & value) const throw (HKLException);

                };
                
            } // namespace vertical.
        } // namespace twoC.
    } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_TWOC_H_
