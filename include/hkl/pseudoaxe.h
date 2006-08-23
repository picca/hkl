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
         * @brief Get the writable state of the pseudoAxe.
         *
         * During the get_value and set_value method, the peusoAxe can be set unwritable.
         */
        virtual bool get_writable(void) const = 0;

        /** 
        * @brief Get the initialization state of the pseudoAxe
        * 
        * @return True if the pseudoAxe was initialized. False otherwise.
        */
        virtual bool get_initialized(void) const = 0;

        /**
         * @brief Initialize the pseudoAxe.
         */
        virtual void initialize(void) throw (HKLException) = 0;

        /**
         * @brief Initialize the pseudoAxe.
         */
        virtual void uninitialize(void) = 0;

        /** 
         * @brief get the minimum value of the pseudoAxe.
         * @return The minimum value of the pseudoAxe.
         *
         * If there is no minimum This method return -INF
         */
        virtual double get_min(void) const = 0;

        /** 
         * @brief get the maximum value of the pseudoAxe.
         * @return The maximum value of the pseudoAxe.
         *
         * If there is no maximum This method return +INF
         */
        virtual double get_max(void) const = 0;

        /**
         * \brief get the current value of the PseudoAxe.
         * \param geometry the Geometry containing the real Axe
         * \return the position of the PseudoAxe.
         *
         * This function can set the writable flag of the pseudoAxe depending
         * on condition of the related geometry.
         */
        virtual double get_value(void) throw (HKLException) = 0;

        /**
         * \brief set the current value of the PseudoAxe.
         * \param geometry the Geometry containing the real Axe
         * \param value The value to set.
         * \throw HKLException if the pseudoAxe is not ready to be set.
         */
        virtual void set_value(double const & value) throw (HKLException) = 0;
      };

    /**
     * \brief A class design to describe a pseudoaxe from a geometry type
     */
    template<typename T>
    class PseudoAxe : public PseudoAxeInterface, public ObjectWithParameters
      {      
      public:

        virtual ~PseudoAxe(void)
          {}

        bool get_writable(void) const {return m_writable;}

        bool get_initialized(void) const {return m_initialized;}

        void uninitialize(void)
          {
            m_initialized = false;
            m_writable = false;
          }

        void initialize(void) throw (HKLException)
          {
            if (m_geometry.isValid())
              {
                m_geometry0 = m_geometry;
                m_initialized = true;
                m_writable = true;
              }
          }

        PseudoAxe & operator=(PseudoAxe const &)
          {
            return *this;
          }

        /** 
         * @brief Is a PseudoAxe valid ?
         * 
         * @return The validity of the PseudoAxe for the current Geometry.
         *
         * A pseudoAxe is valid when its value can be compute and when the meaning
         * of this value is coherant with the initialization of the pseudoAxe.
         */
        bool isValid(void) throw (HKLException)
          {
            if (m_initialized)
                m_writable = true;
            else
              {
                m_writable = false;
                HKLEXCEPTION("The pseudoAxe was not initialized.", "Please initialize it.");
              }
            return true;
          }

        ostream & printToStream(ostream & flux) const
          {
            flux << m_geometry0;
            flux << m_writable;
            flux << m_initialized;
            return flux;
          }

        ostream & toStream(ostream & flux) const
          {
            m_geometry0.toStream(flux);
            flux << " " << m_writable
            << " " << m_initialized << endl;
            return flux;
          }

        istream & fromStream(istream & flux)
          {
            m_geometry0.fromStream(flux);
            flux >> m_writable >> m_initialized;
            return flux;
          }

      protected:
        T & m_geometry; //!< geometry connected to the pseudoAxe.
        T m_geometry0; //!< Geometry used to store the initialisation of the pseudoAxe.
        bool m_writable; //!< Tell if we can write on the pseudoAxe.
        bool m_initialized; //!< Tell if the PseudoAxe was initialized.

        PseudoAxe(T & geometry) :
          ObjectWithParameters(),
          m_geometry(geometry),
          m_writable(false),
          m_initialized(false)
        {}

        PseudoAxe(PseudoAxe const & pseudoAxe) :
          ObjectWithParameters(pseudoAxe),
          m_geometry(pseudoAxe.m_geometry),
          m_geometry0(pseudoAxe.m_geometry0),
          m_writable(pseudoAxe.m_writable),
          m_initialized(pseudoAxe.m_initialized)
        {}

      public:
        typedef T value_type;
      };

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
