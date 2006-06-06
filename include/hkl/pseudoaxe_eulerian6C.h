#ifndef _PSEUDOAXE_EULERIAN6C_H_
#define _PSEUDOAXE_EULERIAN6C_H_

#include "pseudoaxe.h"
#include "geometry_eulerian6C.h"
#include "pseudoaxe_eulerian4C.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace eulerian6C {

            /**
             * @brief This class defines the PseudoAxe for all the 4 circles Eulerian diffractometers.
             */
            class Eulerian6C : public PseudoAxe
            {
            public:

              virtual ~Eulerian6C(void); //!< The destructor

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
              geometry::Eulerian6C m_geometry; //!< The geometry use to initialize the pseudoaxe.

              Eulerian6C(void); //!< Default constructor - protected to make sure this class is abstract.
            };

            class Tth : public Eulerian6C
            {
            public:

              Tth(void); //!< Default constructor.

              virtual ~Tth(void); //!< Default destructor.

              void initialize(Geometry const & geometry) throw (HKLException);

              bool get_isValid(Geometry const & geometry) const;

              double get_value(Geometry const & geometry) const throw (HKLException);

              void set_value(Geometry & geometry, double const & value) const throw (HKLException);

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

            class Q :
#ifdef MSVC6
              public PseudoAxe
#else
              public pseudoAxe::eulerian6C::Tth
#endif

                {
                public:

                  Q(void); //!< Default constructor.

                  virtual ~Q(void); //!< Default destructor.

                  void initialize(Geometry const & geometry) throw (HKLException);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                private:
#ifdef MSVC6
                  mutable pseudoAxe::eulerian6C::Tth m_tth;
#endif
                };

            namespace eulerian4C {
                namespace vertical {

                    /**
                     * @brief The eulerian 4-circle Vertical diffractometer Psi pseudoAxe.
                     */
                    class Psi :
#ifdef MSVC6
                      public PseudoAxe
#else
                      public pseudoAxe::eulerian4C::vertical::Psi
#endif

                        {
                        public:

                          Psi(void); //!< Default constructor.

                          virtual ~Psi(void); //!< Default destructor.

                          void initialize(Geometry const & geometry) throw (HKLException);

                          bool get_isValid(Geometry const & geometry) const;

                          double get_value(Geometry const & geometry) const throw (HKLException);

                          void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                        private:
                          mutable geometry::eulerian4C::Vertical m_E4CV;
#ifdef MSVC6
                          mutable pseudoAxe::eulerian4C::vertical::Psi m_psi;
#endif
                        };

                } // namespace vertical
            } // namespace eulerian4C
        } // namespace eulerian6C.
    } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_EULERIAN6C_H_
