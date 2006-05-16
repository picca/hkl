#ifndef _PSEUDOAXE_TWOC_H_
#define _PSEUDOAXE_TWOC_H_

#include "pseudoaxe.h"
#include "geometry_twoC.h"

using namespace std;

namespace hkl {
    namespace pseudoAxe {
        namespace twoC {

            /**
             * @brief This class defines the PseudoAxe for all the TwoC  Vertical diffractometers.
             */
            class Vertical : public PseudoAxe
              {
              public:

                virtual ~Vertical(void); //!< The destructor

                //virtual void initialize(Geometry const & geometry) = 0;

                //virtual bool get_isValid(Geometry const & geometry) const = 0;

                //virtual double const get_value(Geometry const & geometry) = 0;

                //virtual void set_value(Geometry & geometry, double const & value) throw (HKLException) = 0;

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
                geometry::twoC::Vertical m_geometry; //!< The geometry use to initialize the pseudoaxe.

                Vertical(void); //!< Default constructor - protected to make sure this class is abstract.
              };

            namespace vertical {

                class Th2th : public Vertical
                {
                public:

                  Th2th(void); //!< Default constructor.

                  virtual ~Th2th(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                };
                
                class Q2th : public Vertical
                {
                public:

                  Q2th(void); //!< Default constructor.

                  virtual ~Q2th(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                };
                
                class Q : public Vertical
                {
                public:

                  Q(void); //!< Default constructor.

                  virtual ~Q(void); //!< Default destructor.

                  void initialize(Geometry const & geometry);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                };
                
            } // namespace vertical.
        } // namespace twoC.
    } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_TWOC_H_
