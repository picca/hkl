#ifndef _PSEUDOAXE_EULERIAN6C_H_
#define _PSEUDOAXE_EULERIAN6C_H_

#include "pseudoaxe.h"
#include "geometry_eulerian6C.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace eulerian6C
      {

      class Tth : public PseudoAxeTemp<geometry::Eulerian6C>
        {
        public:

          Tth(geometry::Eulerian6C &); //!< Default constructor.

          Tth(Tth const &); //!< Copy constructor

          virtual ~Tth(void); //!< Default destructor.

          void initialize(void) throw (HKLException);

          void update(void);

          void set_current(Value const &) throw (HKLException);

          /*!
           * \brief Save the pseudoaxe::Eulerian4C into a stream.
           * \param flux the stream to save the pseudoaxe::Eulerian4C into.
           * \return The stream with the pseudoaxe::Eulerian4C.
           */
          ostream & toStream(ostream &) const;

          /*!
           * \brief Restore a pseudoaxe::Eulerian4C from a stream.
           * \param flux The stream containing the pseudoaxe::Eulerian4C.
           * \return The modified stream.
           */
          istream & fromStream(istream &);

        private:
          Axe * & _gamma;
          Axe * & _delta;
          double _gamma0;
          double _delta0;
          svector _axe0;

          Parameter * _direction;

          void _minmax(Range & range, Range const & gamma, Range const & delta);
        };

      class Q : public PseudoAxeTemp<geometry::Eulerian6C>
        {
        public:

          Q(geometry::Eulerian6C &); //!< Default constructor.

          virtual ~Q(void); //!< Default destructor.

          void initialize(void) throw (HKLException);

          void update(void);

          void set_current(Value const &) throw (HKLException);

          bool isValid(void) throw (HKLException);

        private:
          Axe * & _gamma;
          Axe * & _delta;

          mutable pseudoAxe::eulerian6C::Tth * m_tth;
        };

      namespace eulerian4C
        {
        namespace vertical
          {

          typedef DerivedPseudoAxe<pseudoAxe::eulerian4C::vertical::Psi, geometry::Eulerian6C> Psi;

        } // namespace vertical
      } // namespace eulerian4C
    } // namespace eulerian6C.
  } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_EULERIAN6C_H_
