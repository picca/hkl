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

              /** 
               * @brief The "th2th" pseudoAxe
               */
                class Th2th : public Vertical
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
                  void initialize(Geometry const & geometry) throw (HKLException);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                };
                
                class Q2th : public Vertical
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
                  void initialize(Geometry const & geometry) throw (HKLException);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                };
                
                class Q : public Vertical
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
                  void initialize(Geometry const & geometry) throw (HKLException);

                  bool get_isValid(Geometry const & geometry) const;

                  double get_value(Geometry const & geometry) const throw (HKLException);

                  void set_value(Geometry & geometry, double const & value) const throw (HKLException);

                };
                
            } // namespace vertical.
        } // namespace twoC.
    } // namespace pseudoAxe.
} // namespace hkl.

#endif // _PSEUDOAXE_TWOC_H_
