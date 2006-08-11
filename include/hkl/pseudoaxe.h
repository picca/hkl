#ifndef _PSEUDOAXE_H_
#define _PSEUDOAXE_H_

#include <iostream>

#include "geometry.h"
#include "HKLException.h"
#include "objectwithparameters.h"

using namespace std;

namespace hkl {

    class PseudoAxeInterface
      {
      public:
        /**
         * \brief Initialize the PseudoAxe from the Geometry.
         * \param geometry The configuration to save for calculation.
         */
        virtual void initialize(void) throw (HKLException) = 0;

        /**
         * \brief Uninitialize the PseudoAxe from the Geometry.
         */
        virtual void uninitialize(void) = 0;

        /**
         * \brief get the current value of the PseudoAxe.
         * \param geometry the Geometry containing the real Axe
         * \return the position of the PseudoAxe.
         */
        virtual double get_value(void) const throw (HKLException) = 0;

        /**
         * \brief set the current value of the PseudoAxe.
         * \param geometry the Geometry containing the real Axe
         * \param value The value to set.
         * \throw HKLException if the pseudoAxe is not ready to be set.
         */
        virtual void set_value(double const & value) throw (HKLException) = 0;
      };

    /*!
     * \brief A class design to describe a pseudoaxe from a geometry type
     */
    template<typename T>
    class PseudoAxe : public PseudoAxeInterface, public ObjectWithParameters
      {      
      public:

        virtual ~PseudoAxe(void);

        virtual void uninitialize(void) {m_wasInitialized = false;}

        PseudoAxe & operator=(PseudoAxe const &) { return *this;}

        /** 
         * @brief Is a PseudoAxe valid ?
         * 
         * @return The validity of the PseudoAxe for the current Geometry.
         *
         * A pseudoAxe is valid when its value can be compute and when the meaning
         * of this value is coherant with the initialization of the pseudoAxe.
         */
        virtual bool get_isValid(void) const = 0;

        ostream & printToStream(ostream & flux) const;

        ostream & toStream(ostream & flux) const;

        istream & fromStream(istream & flux);

      protected:
        T & m_geometry; //!< geometry connected to the pseudoAxe.
        T m_geometry0; //!< Geometry used to store the initialisation of the pseudoAxe.
        bool m_wasInitialized; //!< Tell if the PseudoAxe was initialized before using it.

        PseudoAxe(T &);

        PseudoAxe(PseudoAxe const & pseudoAxe);

      public:
        typedef T value_type;
      };

    /**
     * The default constructor - protected to make sure this class is abstract.
     */
    template<typename T>
    PseudoAxe<T>::PseudoAxe(T & geometry) :
      ObjectWithParameters(),
      m_geometry(geometry),
      m_wasInitialized(false)
    {}

    /**
     * The default copy constructor.
     */
    template<typename T>
    PseudoAxe<T>::PseudoAxe(PseudoAxe const & pseudoAxe) :
      ObjectWithParameters(pseudoAxe),
      m_geometry(pseudoAxe.m_geometry),
      m_geometry0(pseudoAxe.m_geometry0),
      m_wasInitialized(pseudoAxe.m_wasInitialized)
    {}

    /**
     * The default destructor.
     */
    template<typename T>
    PseudoAxe<T>::~PseudoAxe(void)
      {}

    template<typename T>
    ostream &
    PseudoAxe<T>::printToStream(ostream & flux) const
      {
        flux << m_geometry0;
        flux << m_wasInitialized;
        return flux;
      }

    template<typename T>
    ostream &
    PseudoAxe<T>::toStream(ostream & flux) const
      {
        m_geometry0.toStream(flux);
        flux << " " << m_wasInitialized << endl;
        return flux;
      }

    template<typename T>
    istream &
    PseudoAxe<T>::fromStream(istream & flux)
      {
        m_geometry0.fromStream(flux);
        flux >> m_wasInitialized;
        return flux;
      }

} // namespace hkl

/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
template<typename T>
ostream &
operator<<(ostream & flux, hkl::PseudoAxe<T> const & pseudoAxe)
{
    return pseudoAxe.printToStream(flux);
}

#endif // _PSEUDOAXE_H_
