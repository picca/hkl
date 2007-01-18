#ifndef _DERIVEDPSEUDOAXEENGINE_H_
#define _DERIVEDPSEUDOAXEENGINE_H_

#include "pseudoaxeengine.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief A class design to describe a pseudoaxe from a geometry type
   */
  template<typename T, typename C>
  class DerivedPseudoAxeEngine : public PseudoAxeEngineTemp<C>
    {
    public:

      /**
       * @brief the default constructor
       * @param geometry The Geometry use to compute the pseudo axe range.
       * @param name The name of the DerivedPseudoAxe.
       * @param description the description of the DerivedPseudoAxe.
       */
      DerivedPseudoAxeEngine(C & geometry) :
          PseudoAxeEngineTemp<C>(geometry, false, false, false)
      {
        _pseudoAxeEngine = new T(_gconv);
        PseudoAxeEngineTemp<C>::_parameters = _pseudoAxeEngine->parameters();
        PseudoAxeEngineTemp<C>::_pseudoAxes = _pseudoAxeEngine->pseudoAxes();

        //fill the pseudoAxes with the right engine.
        vector<PseudoAxe *>::iterator pseudoAxe_it = PseudoAxeEngineTemp<C>::_pseudoAxes.begin();
        vector<PseudoAxe *>::iterator pseudoAxe_end = PseudoAxeEngineTemp<C>::_pseudoAxes.end();
        while (pseudoAxe_it != pseudoAxe_end)
          {
            (*pseudoAxe_it)->set_engine(this);
            ++pseudoAxe_it;
          }

        //update the observable.
        AxeMap & axes = geometry.axes();
        AxeMap::iterator iter = axes.begin();
        AxeMap::iterator end = axes.end();
        while(iter != end)
          {
            iter->second.add_observer(this);
            ++iter;
          }
        PseudoAxeEngineTemp<C>::connect();
        update();
      }

      /**
       * @brief The copy constructor
       * @param derivedPseudoAxe The DerivedPseudoAxe to copy.
       * @todo Make the cange to be sure a copy do not produce a segfault.
       */
      DerivedPseudoAxeEngine(DerivedPseudoAxeEngine const & derivedPseudoAxeEngine) :
          PseudoAxeEngineTemp<C>(derivedPseudoAxeEngine),
          _gconv(derivedPseudoAxeEngine._gconv),
          _pseudoAxeEngine(derivedPseudoAxeEngine._pseudoAxe)
      {
        // for now if we made a copy seg fault due to try to delete two times the same pointer _pseudoAxe.
      }
      /**
       * @brief The default destructor.
       */
      virtual ~DerivedPseudoAxeEngine(void)
      {
        delete _pseudoAxeEngine;
      }

      /**
       * @brief Get the initialize state of the DerivedPseudoAxe.
       */
      bool const get_initialized(void) const
        {
          return _pseudoAxeEngine->get_initialized();
        }

      /**
       * @brief Get the readable state of the DerivedPseudoAxe.
       */
      bool const get_readable(void) const
        {
          return _pseudoAxeEngine->get_readable();
        }

      /**
       * @brief Get the writable state of the DerivedPseudoAxe.
       */
      bool const get_writable(void) const
        {
          return _pseudoAxeEngine->get_writable();
        }

      /**
       * @brief Initialize the DerivedPseudoAxe.
       */
      void initialize(void) throw (HKLException)
      {
        _pseudoAxeEngine->unconnect();
        _gconv.setFromGeometry(PseudoAxeEngineTemp<C>::_geometry, false);
        _pseudoAxeEngine->connect();
        _pseudoAxeEngine->initialize();
      }


      /**
       * @brief Uninitialize the DerivedPseudoAxe.
       */
      void uninitialize(void)
      {
        _pseudoAxeEngine->uninitialize();
      }

      void update(void)
      {
        if (PseudoAxeEngineTemp<C>::_connected)
          {
            _pseudoAxeEngine->unconnect();
            _gconv.setFromGeometry(PseudoAxeEngineTemp<C>::_geometry, false);
            _pseudoAxeEngine->connect();
            _pseudoAxeEngine->update();
          }
      }

      /**
       * @brief set the current value of the PseudoAxe.
       * @param value The value to set.
       * @throw HKLException if the pseudoAxe is not ready to be set.
       */
      void set(void) throw (HKLException)
        {
          _pseudoAxeEngine->unconnect();
          _gconv.setFromGeometry(PseudoAxeEngineTemp<C>::_geometry, false);
          _pseudoAxeEngine->connect();
          _pseudoAxeEngine->set();
          PseudoAxeEngineTemp<C>::unconnect();
          PseudoAxeEngineTemp<C>::_geometry.setFromGeometry(_gconv, false);
          PseudoAxeEngineTemp<C>::connect();
        }

      /**
       * @brief Print a DerivedPseudoAxe in a stream.
       * @param flux the stream to print into.
       * @return the modified stream.
       */
      ostream & printToStream(ostream & flux) const
        {
          PseudoAxeEngineTemp<C>::printToStream(flux);
          flux << _pseudoAxeEngine;
          return flux;
        }

      /**
       * @brief Store a DerivedPseudoAxe in a stream.
       * @param flux the stream use to store the PseudoAxe.
       * @return the modified stream.
       */
      ostream & toStream(ostream & flux) const
        {
          PseudoAxeEngineTemp<C>::toStream(flux);
          _pseudoAxeEngine->toStream(flux);
          return flux;
        }

      /**
       * @brief Restore a DerivedPseudoAxe from a stream.
       * @param flux The stream to restore from.
       * @return the modified stream.
       */
      istream & fromStream(istream & flux)
      {
        PseudoAxeEngineTemp<C>::fromStream(flux);
        _pseudoAxeEngine->fromStream(flux);
        return flux;
      }

    private:
      mutable typename T::value_type _gconv; //!< The geometry used to do the conversion.
      mutable T * _pseudoAxeEngine; //!< The pseudoAxe use to do the calculation.
    };

} // namespace hkl

/**
 * @brief Overload of the << operator for the PseudoAxe class
 * @param flux the stream to print into.
 * @param derivedPseudoAxe The DerivedPseudoAxe to send into the stream.
 */
template<typename T, typename C>
ostream &
operator<<(ostream & flux, hkl::DerivedPseudoAxeEngine<T, C> const & derivedPseudoAxeEngine)
{
  return derivedPseudoAxeEngine.printToStream(flux);
}



#endif // _DERIVEDPSEUDOAXE_H_
