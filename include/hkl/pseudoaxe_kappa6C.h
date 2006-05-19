#ifndef _PSEUDOAXE_KAPPA6C_H_
#define _PSEUDOAXE_KAPPA6C_H_

#include "config.h"

#include "pseudoaxe.h"
#include "geometry_kappa6C.h"
#include "pseudoaxe_kappa4C.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace kappa6C {
            namespace kappa4C {

                /*!
                 * This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
                 */
                class Vertical
                {
                public:

                  virtual ~Vertical(void); //!< The destructor

                protected:
                  mutable geometry::kappa4C::Vertical * m_K4C; //!< geometry use to do the conversion between K6C and K4CV

                  Vertical(double const & alpha); //!< Default constructor - protected to make sure this class is abstract.
                };

                namespace vertical {

                    /*!
                     * The kappa 4-circle diffractometer Omega pseudoAxe.
                     */
                    class Omega :
                      public pseudoAxe::kappa6C::kappa4C::Vertical,
#ifdef MSVC6
                      public PseudoAxe
#else
                      public pseudoAxe::kappa4C::vertical::Omega
#endif
                    {
                    public:

                      Omega(double const & alpha); //!< Default constructor.

                      virtual ~Omega(void); //!< Default destructor.

                      void initialize(Geometry const & geometry) throw (HKLException);

                      bool get_isValid(Geometry const & geometry) const;

                      double get_value(Geometry const & geometry) const throw (HKLException);

                      void set_value(Geometry & geometry, double const & value) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable pseudoAxe::kappa4C::vertical::Omega * m_omega;
#endif
                    };

                    /*!
                     * The kappa 4-circle diffractometer Omega pseudoAxe.
                     */
                    class Chi :
                      public pseudoAxe::kappa6C::kappa4C::Vertical,
#ifdef MSVC6
                      public PseudoAxe
#else
                      public pseudoAxe::kappa4C::vertical::Chi
#endif
                    {
                    public:

                      Chi(double const & alpha); //!< Default constructor.

                      virtual ~Chi(void); //!< Default destructor.

                      void initialize(Geometry const & geometry) throw (HKLException);

                      bool get_isValid(Geometry const & geometry) const;

                      double get_value(Geometry const & geometry) const throw (HKLException);

                      void set_value(Geometry & geometry, double const & value) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable pseudoAxe::kappa4C::vertical::Chi * m_chi;
#endif
                    };

                    /*!
                     * The kappa 4-circle diffractometer Omega pseudoAxe.
                     */
                    class Phi :
                      public pseudoAxe::kappa6C::kappa4C::Vertical,
#ifdef MSVC6
                      public PseudoAxe
#else
                      public pseudoAxe::kappa4C::vertical::Phi
#endif
                    {
                    public:

                      Phi(double const & alpha); //!< Default constructor.

                      virtual ~Phi(void); //!< Default destructor.

                      void initialize(Geometry const & geometry) throw (HKLException);

                      bool get_isValid(Geometry const & geometry) const;

                      double get_value(Geometry const & geometry) const throw (HKLException);

                      void set_value(Geometry & geometry, double const & value) const throw (HKLException);
#ifdef MSVC6
                    protected:
                      mutable pseudoAxe::kappa4C::vertical::Phi * m_phi;
#endif
                    };

                    /** 
                     * @brief The psi pseudoAxe bas on the eulerian4C::vertical::Psi pseudoAxe.
                     */
                    class Psi :
                      public pseudoAxe::kappa6C::kappa4C::Vertical,
#ifdef MSVC6
                      public PseudoAxe
#else
                      public pseudoAxe::kappa4C::vertical::Psi
#endif
                    {
                    public:
                      Psi(double const & alpha); //!< Default constructor.

                      virtual ~Psi(void); //!< Default destructor.

                      void initialize(Geometry const & geometry) throw (HKLException);

                      bool get_isValid(Geometry const & geometry) const;

                      double get_value(Geometry const & geometry) const throw (HKLException);

                      void set_value(Geometry & geometry, double const & value) const throw (HKLException);

#ifdef MSVC6
                    protected:
                      mutable pseudoAxe::kappa4C::vertical::Psi * m_psi;
#endif
                    };

                } // namespace vertical
            } // namespace kappa4C
        } // namespace kappa6C
    } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
