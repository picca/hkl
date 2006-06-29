#ifndef _PSEUDOAXE_EULERIAN6C_H_
#define _PSEUDOAXE_EULERIAN6C_H_

#include "pseudoaxe.h"
#include "geometry_eulerian6C.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian6C {

            class Tth : public PseudoAxe<geometry::Eulerian6C>
            {
            public:

              Tth(void); //!< Default constructor.

              Tth(Tth const & pseudoAxe); //!< Copy constructor

              virtual ~Tth(void); //!< Default destructor.

              void initialize(geometry::Eulerian6C const & geometry) throw (HKLException);

              bool get_isValid(geometry::Eulerian6C const & geometry) const;

              double get_value(geometry::Eulerian6C const & geometry) const throw (HKLException);

              void set_value(geometry::Eulerian6C & geometry, double const & value) const throw (HKLException);

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

            private:
              svector m_axe;
            };

            class Q : public PseudoAxe<geometry::Eulerian6C>
            {
            public:

              Q(void); //!< Default constructor.

              virtual ~Q(void); //!< Default destructor.

              void initialize(geometry::Eulerian6C const & geometry) throw (HKLException);

              bool get_isValid(geometry::Eulerian6C const & geometry) const;

              double get_value(geometry::Eulerian6C const & geometry) const throw (HKLException);

              void set_value(geometry::Eulerian6C & geometry, double const & value) const throw (HKLException);

            private:

              mutable pseudoAxe::eulerian6C::Tth m_tth;
            };

            namespace eulerian4C {
                namespace vertical {

                    typedef DerivedPseudoAxe<pseudoAxe::eulerian4C::vertical::Psi, geometry::Eulerian6C> Psi;

                } // namespace vertical
            } // namespace eulerian4C
        } // namespace eulerian6C.
    } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_EULERIAN6C_H_
