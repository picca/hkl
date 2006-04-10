#ifndef _PSEUDOAXE_KAPPA4C_H_
#define _PSEUDOAXE_KAPPA4C_H_

#include "pseudoaxe.h"
#include "geometry_kappa4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {

        /*!
         * This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
         */
        class Kappa4C : public PseudoAxe
        {
        public:

          virtual ~Kappa4C(void); //!< The destructor

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
          geometry::Kappa4C * m_geometry_K4C; //!< The geometry use to initialize the pseudoaxe.

          double m_omega; //!< value of the omega PseudoAxe
          double m_chi; //!< value of the omega PseudoAxe
          double m_phi; //!< value of the omega PseudoAxe

          Kappa4C(double alpha); //!< Default constructor - protected to make sure this class is abstract.
        };

        namespace kappa4C {
            /*!
             * The kappa 4-circle diffractometer Omega pseudoAxe.
             */
            class Omega : public Kappa4C
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
            class Chi : public Kappa4C
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
            class Phi : public Kappa4C
            {
            public:

              Phi(double alpha); //!< Default constructor.

              virtual ~Phi(void); //!< Default destructor.

              void initialize(Geometry const & geometry);

              bool get_isValid(Geometry const & geometry) const;

              double const get_value(Geometry const & geometry);

              void set_value(Geometry & geometry, double const & value) throw (HKLException);
            };

        } // namespace eulerian4C
    } // namespace pseudoAxe
} // namespace hkl

#endif // _PSEUDOAXE_EULERIAN4C_H_
