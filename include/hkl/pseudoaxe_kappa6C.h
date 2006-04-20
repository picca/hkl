#ifndef _PSEUDOAXE_KAPPA6C_H_
#define _PSEUDOAXE_KAPPA6C_H_

#include "pseudoaxe.h"
#include "geometry_kappa6C.h"
#include "pseudoaxe_kappa4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace kappa6C {
            namespace kappa4C {

                /*!
                 * This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
                 */
                class Vertical : public virtual PseudoAxe
                {
                public:

                  virtual ~Vertical(void); //!< The destructor

                  virtual void initialize(Geometry const & geometry) = 0;

                  virtual bool get_isValid(Geometry const & geometry) const = 0;

                  virtual double const get_value(Geometry const & geometry) = 0;

                  virtual void set_value(Geometry & geometry, double const & value) throw (HKLException) = 0;

                  /*!
                   * \brief Save the pseudoaxe::Eulerian4C into a stream.
                   * \param flux the stream to save the pseudoaxe::Eulerian4C into.
                   * \return The stream with the pseudoaxe::Eulerian4C.
                   */
                  ostream & toStream(ostream & flux) const;

                  /*!
                   * \brief Restore a pseudoaxe::Eulerian4C from a stream.
                   * \param flux The stream containing the pseudoaxe::Eulerian4C.
                   * \return The modified stream.
                   */
                  istream & fromStream(istream & flux);

                protected:
                  geometry::Kappa6C * m_geometry_K6C;

                  Vertical(double alpha); //!< Default constructor - protected to make sure this class is abstract.
                };

                namespace vertical {
                    /*!
                     * The kappa 4-circle diffractometer Omega pseudoAxe.
                     */
                    class Omega : public pseudoAxe::kappa4C::vertical::Omega, public Vertical
                    {
                    public:

                      Omega(double alpha); //!< Default constructor.

                      virtual ~Omega(void); //!< Default destructor.

                      void initialize(Geometry const & geometry);

                      bool get_isValid(Geometry const & geometry) const;

                      double const get_value(Geometry const & geometry);

                      void set_value(Geometry & geometry, double const & value) throw (HKLException);
                    };

                    /*!
                     * The kappa 4-circle diffractometer Omega pseudoAxe.
                     */
                    class Chi : public pseudoAxe::kappa4C::vertical::Chi, public Vertical
                    {
                    public:

                      Chi(double alpha); //!< Default constructor.

                      virtual ~Chi(void); //!< Default destructor.

                      void initialize(Geometry const & geometry);

                      bool get_isValid(Geometry const & geometry) const;

                      double const get_value(Geometry const & geometry);

                      void set_value(Geometry & geometry, double const & value) throw (HKLException);
                    };

                    /*!
                     * The kappa 4-circle diffractometer Omega pseudoAxe.
                     */
                    class Phi : public pseudoAxe::kappa4C::vertical::Phi, public Vertical
                    {
                    public:

                      Phi(double alpha); //!< Default constructor.

                      virtual ~Phi(void); //!< Default destructor.

                      void initialize(Geometry const & geometry);

                      bool get_isValid(Geometry const & geometry) const;

                      double const get_value(Geometry const & geometry);

                      void set_value(Geometry & geometry, double const & value) throw (HKLException);
                    };


                } // namespace vertical
            } // namespace kappa4C
        } // namespace kappa6C
    } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
