#ifndef _PSEUDOAXE_KAPPA4C_H_
#define _PSEUDOAXE_KAPPA4C_H_

#include "config.h"

#include "pseudoaxe.h"
#include "geometry_kappa4C.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace kappa4C {

            /**
             * @brief This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
             */
            class Vertical : public PseudoAxe
            {
            public:

              virtual ~Vertical(void); //!< The destructor

              //virtual void initialize(Geometry const & geometry) = 0;

              //virtual bool get_isValid(Geometry const & geometry) const = 0;

              //virtual double get_value(Geometry const & geometry) = 0;

              //virtual void set_value(Geometry & geometry, double const & value) throw (HKLException) = 0;

              /**
               * @brief Save the pseudoaxe::Eulerian4C into a stream.
               *
               * @param flux the stream to save the pseudoaxe::Eulerian4C into.
               * @return The stream with the pseudoaxe::Eulerian4C.
               */
              ostream & toStream(ostream & flux) const;

              /**
               * @brief Restore a pseudoaxe::Eulerian4C from a stream.
               *
               * @param flux The stream containing the pseudoaxe::Eulerian4C.
               * @return The modified stream.
               */
              istream & fromStream(istream & flux);

            protected:
              geometry::kappa4C::Vertical * m_geometry_K4C; //!< The geometry use to initialize the pseudoaxe.

              Vertical(double alpha); //!< Default constructor - protected to make sure this class is abstract.
            };

            namespace vertical {

                /**
                 * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
                 */
                class Omega : public kappa4C::Vertical
                {
                public:

                  Omega(double alpha); //!< Default constructor.

                  virtual ~Omega(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);
                };

                /**
                 * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
                 */
                class Chi : public kappa4C::Vertical
                {
                public:

                  Chi(double alpha); //!< Default constructor.

                  virtual ~Chi(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);
                };

                /**
                 * @brief The kappa 4-circle diffractometer Omega pseudoAxe.
                 */
                class Phi : public kappa4C::Vertical
                {
                public:

                  Phi(double alpha); //!< Default constructor.

                  virtual ~Phi(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);
                };

                /** 
                 * @brief The psi pseudoAxe bas on the eulerian4C::vertical::Psi pseudoAxe.
                 */
                class Psi :
#ifdef MSVC6
                  public PseudoAxe
#else
                  public pseudoAxe::eulerian4C::vertical::Psi
#endif
                {
                public:
                  Psi(double alpha); //!< Default constructor.

                  virtual ~Psi(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                protected:
                  mutable geometry::eulerian4C::Vertical m_E4C; //!< geometry use to convert from E4C <-> K4C.
#ifdef MSVC6
                  mutable pseudoAxe::eulerian4C::vertical::Psi m_psi;
#endif
                };

            } // namespace vertical
        } // namespace kappa4C
    } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
