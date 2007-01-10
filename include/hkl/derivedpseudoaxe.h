#ifndef _DERIVEDPSEUDOAXE_H_
#define _DERIVEDPSEUDOAXE_H_

#include <iostream>

#include "pseudoaxe.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief A class design to describe a pseudoaxe from a geometry type
   */
  template<typename T, typename C>
  class DerivedPseudoAxe : public PseudoAxeTemp<C>
    {
    public:

      /**
       * @brief the default constructor
       * @param geometry The Geometry use to compute the pseudo axe range.
       * @param name The name of the DerivedPseudoAxe.
       * @param description the description of the DerivedPseudoAxe.
       */
      DerivedPseudoAxe(C & geometry, MyString const & name, MyString const & description) :
          PseudoAxeTemp<C>(geometry, name, description)
      {
        _pseudoAxe = new T(_gconv);
        PseudoAxeTemp<C>::_parameters = _pseudoAxe->parameters();

        //update the observable.
        AxeMap & axes = geometry.axes();
        AxeMap::iterator iter = axes.begin();
        AxeMap::iterator end = axes.end();
        while(iter != end)
          {
            iter->second.add_observer(this);
            ++iter;
          }
        PseudoAxeTemp<C>::connect();
        update();
      }

      /**
       * @brief The copy constructor
       * @param derivedPseudoAxe The DerivedPseudoAxe to copy.
       * @todo Make the cange to be sure a copy do not produce a segfault.
       */
      DerivedPseudoAxe(DerivedPseudoAxe const & derivedPseudoAxe) :
          PseudoAxeTemp<C>(derivedPseudoAxe),
          _gconv(derivedPseudoAxe._gconv),
          _pseudoAxe(derivedPseudoAxe._pseudoAxe)
      {
        // for now if we made a copy seg fault due to try to delete two times the same pointer _pseudoAxe.
      }
      /**
       * @brief The default destructor.
       */
      virtual ~DerivedPseudoAxe(void)
      {
        delete _pseudoAxe;
      }

      /**
       * @brief Get the initialize state of the DerivedPseudoAxe.
       */
      bool get_initialized(void) const
        {
          return _pseudoAxe->get_initialized();
        }

      /**
       * @brief Get the readable state of the DerivedPseudoAxe.
       */
      bool get_readable(void) const
        {
          return _pseudoAxe->get_readable();
        }

      /**
       * @brief Get the writable state of the DerivedPseudoAxe.
       */
      bool get_writable(void) const
        {
          return _pseudoAxe->get_writable();
        }

      /**
       * @brief Initialize the DerivedPseudoAxe.
       */
      void initialize(void) throw (HKLException)
      {
        _pseudoAxe->unconnect();
        _gconv.setFromGeometry(PseudoAxeTemp<C>::_geometry, false);
        _pseudoAxe->connect();
        _pseudoAxe->initialize();
      }


      /**
       * @brief Uninitialize the DerivedPseudoAxe.
       */
      void uninitialize(void)
      {
        _pseudoAxe->uninitialize();
      }

      void update(void)
      {
        if (PseudoAxeTemp<C>::_connected)
          {
            _pseudoAxe->unconnect();
            _gconv.setFromGeometry(PseudoAxeTemp<C>::_geometry, false);
            _pseudoAxe->connect();
            _pseudoAxe->update();
          }
      }

      /**
       * @brief Get the minimum value of the DerivedPseudoAxe.
       */
      Value const & get_min(void) throw (HKLException)
      {
        return _pseudoAxe->get_min();
      }


      /**
       * @brief Get the maximum value of the DerivedPseudoAxe.
       */
      Value const & get_max(void) throw (HKLException)
      {
        return _pseudoAxe->get_max();
      }

      /**
       * @brief Get the current value of the DerivedPseudoAxe.
       */
      Value const & get_current(void) throw (HKLException)
      {
        return _pseudoAxe->get_current();
      }


      /**
       * @brief set the current value of the PseudoAxe.
       * @param value The value to set.
       * @throw HKLException if the pseudoAxe is not ready to be set.
       */
      void set_current(Value const & value) throw (HKLException)
      {
        _pseudoAxe->unconnect();
        _gconv.setFromGeometry(PseudoAxeTemp<C>::_geometry, false);
        _pseudoAxe->connect();
        _pseudoAxe->set_current(value);
        PseudoAxeTemp<C>::unconnect();
        PseudoAxeTemp<C>::_geometry.setFromGeometry(_gconv, false);
        PseudoAxeTemp<C>::connect();
      }

      /**
       * @brief Print a DerivedPseudoAxe in a stream.
       * @param flux the stream to print into.
       * @return the modified stream.
       */
      ostream & printToStream(ostream & flux) const
        {
          PseudoAxeTemp<C>::printToStream(flux);
          flux << _pseudoAxe;
          return flux;
        }

      /**
       * @brief Store a DerivedPseudoAxe in a stream.
       * @param flux the stream use to store the PseudoAxe.
       * @return the modified stream.
       */
      ostream & toStream(ostream & flux) const
        {
          PseudoAxeTemp<C>::toStream(flux);
          _pseudoAxe->toStream(flux);
          return flux;
        }

      /**
       * @brief Restore a DerivedPseudoAxe from a stream.
       * @param flux The stream to restore from.
       * @return the modified stream.
       */
      istream & fromStream(istream & flux)
      {
        PseudoAxeTemp<C>::fromStream(flux);
        _pseudoAxe->fromStream(flux);
        return flux;
      }

    private:
      mutable typename T::value_type _gconv; //!< The geometry used to do the conversion.
      mutable T * _pseudoAxe; //!< The pseudoAxe use to do the calculation.
    };

} // namespace hkl

/**
 * @brief Overload of the << operator for the PseudoAxe class
 * @param flux the stream to print into.
 * @param derivedPseudoAxe The DerivedPseudoAxe to send into the stream.
 */
template<typename T, typename C>
ostream &
operator<<(ostream & flux, hkl::DerivedPseudoAxe<T, C> const & derivedPseudoAxe)
{
  return derivedPseudoAxe.printToStream(flux);
}



#endif // _DERIVEDPSEUDOAXE_H_
