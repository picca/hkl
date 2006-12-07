#ifndef _PSEUDOAXE_H_
#define _PSEUDOAXE_H_

#include <iostream>

#include "geometry.h"
#include "HKLException.h"
#include "hklobject.h"

using namespace std;

namespace hkl
  {

  class PseudoAxe : public HKLObject, public Observer
    {
    public:

      virtual ~PseudoAxe(void);

      /**
       * @brief Get the initialization state of the pseudoAxe
       * 
       * @return True if the pseudoAxe was initialized. False otherwise.
       */
      bool get_initialized(void) const
        {
          return _initialized;
        }

      /**
       * @brief Get the writable state of the pseudoAxe.
       *
       * During the get_value and set_value method, the peusoAxe can be set unwritable.
       */
      bool get_writable(void) const
        {
          return _writable;
        }

      /**
       * @brief Get the readable state of the pseudoAxe.
       *
       * During the get_value and set_value method, the peusoAxe can be set unreadable.
       */
      bool get_readable(void) const
        {
          return _readable;
        }

      /**
       * @brief Initialize the pseudoAxe.
       */
      virtual void initialize(void) throw (HKLException) = 0;

      /**
       * @brief Initialize the pseudoAxe.
       */
      void uninitialize(void);

      /**
       * @brief get the minimum value of the pseudoAxe.
       * @return The minimum value of the pseudoAxe.
       *
       * If there is no minimum This method return -INF
       */
      Value const & get_min(void);

      /**
       * @brief get the maximum value of the pseudoAxe.
       * @return The maximum value of the pseudoAxe.
       *
       * If there is no maximum This method return +INF
       */
      Value const & get_max(void);

      /**
       * \brief get the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \return the position of the PseudoAxe.
       *
       * This function can set the writable flag of the pseudoAxe depending
       * on condition of the related geometry.
       */
      Value const & get_current(void);

      /**
       * \brief set the current value of the PseudoAxe.
       * \param geometry the Geometry containing the real Axe
       * \param value The value to set.
       * \throw HKLException if the pseudoAxe is not ready to be set.
       */
      virtual void set_current(Value const & value) throw (HKLException) = 0;

      bool operator==(PseudoAxe const & pseudoAxe) const;

      ostream & printToStream(ostream & flux) const;

      ostream & toStream(ostream & flux) const;

      istream & fromStream(istream & flux);

    protected:

      Range _range;
      bool _initialized;
      bool _readable;
      bool _writable;


      PseudoAxe(MyString const & name, MyString const & description);

      PseudoAxe(PseudoAxe const & pseudoAxe);
    };

  /**
   * \brief A class design to describe a pseudoaxe from a geometry type
   */
  template<typename T>
  class PseudoAxeTemp : public PseudoAxe
    {
    public:

      virtual ~PseudoAxeTemp(void)
      {}

      void initialize(void) throw (HKLException)
      {
        _geometry0 = _geometry;
        _initialized = true;
        _writable = true;
      }

      PseudoAxeTemp & operator=(PseudoAxeTemp const & pseudoaxe)
      {
        return *this;
      }

      bool operator==(PseudoAxeTemp const & pseudoAxe) const
        {
          return PseudoAxe::operator==(pseudoAxe)
                 && _geometry0 == pseudoAxe._geometry0;
        }

      ostream & printToStream(ostream & flux) const
        {
          PseudoAxe::printToStream(flux);
          flux << _geometry0;
          return flux;
        }

      ostream & toStream(ostream & flux) const
        {
          PseudoAxe::toStream(flux);
          _geometry0.toStream(flux);
          return flux;
        }

      istream & fromStream(istream & flux)
      {
        PseudoAxe::fromStream(flux);
        _geometry0.fromStream(flux);
        return flux;
      }

    protected:
      T & _geometry; //!< geometry connected to the pseudoAxe.
      T _geometry0; //!< Geometry used to store the initialisation of the pseudoAxe.

      PseudoAxeTemp(T & geometry, MyString const & name, MyString const & description) :
          PseudoAxe(name, description),
          _geometry(geometry)
      {}

      PseudoAxeTemp(PseudoAxeTemp const & pseudoAxeTemp) :
          PseudoAxe(pseudoAxeTemp),
          _geometry(pseudoAxeTemp._geometry),
          _geometry0(pseudoAxeTemp._geometry0)
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
operator<<(ostream & flux, hkl::PseudoAxeTemp<T> const & pseudoAxeTemp)
{
  return pseudoAxeTemp.printToStream(flux);
}

#endif // _PSEUDOAXE_H_
