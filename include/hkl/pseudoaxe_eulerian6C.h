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

          void set_current(Value const & value) throw (HKLException);

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
          Axe * & _gamma; //!< The gamma axe use for calculation.
          Axe * & _delta; //!< The delta axe use for calculation.
          double _gamma0; //!< The gamma value after initialization.
          double _delta0;  //!< The delta value after initialization
          svector _axe0; //!< The normal of the scatering plan after initialization.

          Parameter * _direction; //!< The direction parameter use to indicate in gamma=0, delta=0 if we use the horizontal or vertical plane for the diffraction.

          /**
           * @brief this method compute the min max range depending on the omega (min, max) and delta (min, max)
           * @param[out] range to update. 
           * @param gamma Axe
           * @param delta Axe.
           */
          void _minmax(Range & range, Range const & gamma, Range const & delta);
        };

      class Q : public PseudoAxeTemp<geometry::Eulerian6C>
        {
        public:

          Q(geometry::Eulerian6C &); //!< Default constructor.

          virtual ~Q(void); //!< Default destructor.

          void initialize(void) throw (HKLException);

          void update(void);

          void set_current(Value const & value) throw (HKLException);

          bool isValid(void) throw (HKLException);

        private:
          Axe * & _gamma; //!< The gamma axe use for calculation.
          Axe * & _delta; //!< The delta axe use for calculation.

          mutable pseudoAxe::eulerian6C::Tth * m_tth; //!< The real PseudoAxe use for the calculation.
        };

      namespace eulerian4C
        {
        namespace vertical
          {

          typedef DerivedPseudoAxe<pseudoAxe::eulerian4C::vertical::Psi, geometry::Eulerian6C> Psi; //!< DerivedPseudoAxe from the eulerian4C pseudoAxes.

        } // namespace vertical
      } // namespace eulerian4C
    } // namespace eulerian6C.
  } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_EULERIAN6C_H_
