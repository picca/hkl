#ifndef _DERIVED_PSEUDOAXEENGINE_H
#define _DERIVED_PSEUDOAXEENGINE_H


#include "pseudoaxeengine.h"
#include "HKLException.h"
#include <iostream>
using namespace std;
#include "samplelist.h"

#include "pseudoaxe.h"
namespace hkl {

namespace pseudoAxeEngine {

template<class T, class C>
class Derived : public hkl::PseudoAxeEngineTemp<T> {
  private:
    mutable typename C::value_type * _gconv;

    mutable  C * _pseudoAxeEngine;


  public:
    Derived(T & geometry);

    virtual ~Derived();

    /**
     * @brief Get the initialization state of the pseudoAxe.
     */
    virtual bool is_initialized() const;

    /**
     * @brief Get the readable state of the pseudoAxe
     * During the get_value and set_value method, the peusoAxe can be set unreadable.
     */
    virtual bool is_readable() const;

    /**
     * @brief Get the writable state of the pseudoAxe.
     * During the get_value and set_value method, the peusoAxe can be set unwritable.
     */
    virtual bool is_writable() const;

    /**
     * @brief Initialize the pseudoAxe.
     *
     * This method must be call before using a pseudoAxe.
     */
    virtual void initialize() throw(hkl::HKLException);

    /**
     * @brief Un-Initialize the pseudoAxe.
     * This method must be call to un-initialize a pseudoAxe.
     */
    virtual void uninitialize();

    virtual void update();

    /**
     * @brief set the current value of the PseudoAxe.
     * @throw HKLException if the pseudoAxe is not ready to be set.
     */
    virtual void set() throw(hkl::HKLException);

    virtual void set_write_from_read();

    /**
     * @brief print the Derived into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the Derived
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the Derived from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};
template<class T, class C>
Derived<T, C>::Derived(T & geometry) :
  PseudoAxeEngineTemp<T>(geometry, false, false, false)  
{
  // Bouml preserved body begin 00033982
      _pseudoAxeEngine = new C(*_gconv);
      PseudoAxeEngineTemp<T>::_parameters = _pseudoAxeEngine->parameters();
      PseudoAxeEngineTemp<T>::_pseudoAxes = _pseudoAxeEngine->pseudoAxes();
      
      //fill the pseudoAxes with the right engine.
      PseudoAxeList::iterator pseudoAxe_it = PseudoAxeEngineTemp<T>::_pseudoAxes.begin();
      PseudoAxeList::iterator pseudoAxe_end = PseudoAxeEngineTemp<T>::_pseudoAxes.end();
      while (pseudoAxe_it != pseudoAxe_end)
        {
          (*pseudoAxe_it)->set_engine(this);
          ++pseudoAxe_it;
        }
      
      //update the observable.
      hkl::AxeList & axes = geometry.axes();
      hkl::AxeList::iterator iter = axes.begin();
      hkl::AxeList::iterator end = axes.end();
      while(iter != end)
        {
          (*iter)->add_observer(this);
          ++iter;
        }

      // fill the _relatedAxes.
      iter = axes.begin();
      while(iter != end)
        {
          PseudoAxeEngineTemp<T>::_relatedAxes.push_back(*iter);
          ++iter;
        }
      
      PseudoAxeEngineTemp<T>::connect();
      update();
  // Bouml preserved body end 00033982
}

template<class T, class C>
Derived<T, C>::~Derived() 
{
  // Bouml preserved body begin 00033A02
      delete _pseudoAxeEngine;
  // Bouml preserved body end 00033A02
}

/**
 * @brief Get the initialization state of the pseudoAxe.
 */
template<class T, class C>
bool Derived<T, C>::is_initialized() const 
{
  // Bouml preserved body begin 00033A82
      return _pseudoAxeEngine->is_initialized();
  // Bouml preserved body end 00033A82
}

/**
 * @brief Get the readable state of the pseudoAxe
 * During the get_value and set_value method, the peusoAxe can be set unreadable.
 */
template<class T, class C>
bool Derived<T, C>::is_readable() const 
{
  // Bouml preserved body begin 00033B02
      return _pseudoAxeEngine->is_readable();
  // Bouml preserved body end 00033B02
}

/**
 * @brief Get the writable state of the pseudoAxe.
 * During the get_value and set_value method, the peusoAxe can be set unwritable.
 */
template<class T, class C>
bool Derived<T, C>::is_writable() const 
{
  // Bouml preserved body begin 00033B82
      return _pseudoAxeEngine->is_writable();
  // Bouml preserved body end 00033B82
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
template<class T, class C>
void Derived<T, C>::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00033C02
      _pseudoAxeEngine->unconnect();
      _gconv->setFromGeometry(PseudoAxeEngineTemp<T>::_geometry, false);
      _pseudoAxeEngine->connect();
      _pseudoAxeEngine->initialize();
  // Bouml preserved body end 00033C02
}

/**
 * @brief Un-Initialize the pseudoAxe.
 * This method must be call to un-initialize a pseudoAxe.
 */
template<class T, class C>
void Derived<T, C>::uninitialize() 
{
  // Bouml preserved body begin 00033C82
      _pseudoAxeEngine->uninitialize();
  // Bouml preserved body end 00033C82
}

template<class T, class C>
void Derived<T, C>::update() 
{
  // Bouml preserved body begin 00033D02
      if (PseudoAxeEngineTemp<T>::_connected)
        {
          _pseudoAxeEngine->unconnect();
          _gconv->setFromGeometry(PseudoAxeEngineTemp<T>::_geometry, false);
          _pseudoAxeEngine->connect();
          _pseudoAxeEngine->update();
        }
  // Bouml preserved body end 00033D02
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
template<class T, class C>
void Derived<T, C>::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00033D82
      _pseudoAxeEngine->unconnect();
      _gconv->setFromGeometry(PseudoAxeEngineTemp<T>::_geometry, false);
      _pseudoAxeEngine->connect();
      _pseudoAxeEngine->set();
      PseudoAxeEngineTemp<T>::unconnect();
      PseudoAxeEngineTemp<T>::_geometry.setFromGeometry(*_gconv, false);
      PseudoAxeEngineTemp<T>::connect();
  // Bouml preserved body end 00033D82
}

template<class T, class C>
void Derived<T, C>::set_write_from_read() 
{
  // Bouml preserved body begin 00038782
      _pseudoAxeEngine->set_write_from_read();
  // Bouml preserved body end 00038782
}

/**
 * @brief print the Derived<T, C> into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
template<class T, class C>
ostream & Derived<T, C>::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00033E02
      PseudoAxeEngineTemp<T>::printToStream(flux);
      flux << _pseudoAxeEngine;
      
      return flux;
  // Bouml preserved body end 00033E02
}

/**
 * @brief print on a stream the content of the Derived<T, C>
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T, class C>
ostream & Derived<T, C>::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00033E82
      PseudoAxeEngineTemp<T>::toStream(flux);
      _pseudoAxeEngine->toStream(flux);
      return flux;
  // Bouml preserved body end 00033E82
}

/**
 * @brief restore the content of the Derived<T, C> from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
template<class T, class C>
istream & Derived<T, C>::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00033F02
      PseudoAxeEngineTemp<T>::fromStream(flux);
      _pseudoAxeEngine->fromStream(flux);
      return flux;
  // Bouml preserved body end 00033F02
}

template<class T, class C>
class DerivedWithSample : public hkl::PseudoAxeEngineWithSampleTemp<T> {
  private:
    mutable typename C::value_type * _gconv;

    mutable  C  * _pseudoAxeEngineWithSample;


  public:
    DerivedWithSample(T & geometry, hkl::SampleList *& samples);

    virtual ~DerivedWithSample();

    /**
     * @brief Get the initialization state of the pseudoAxe.
     */
    virtual bool is_initialized() const;

    /**
     * @brief Get the readable state of the pseudoAxe
     * During the get_value and set_value method, the peusoAxe can be set unreadable.
     */
    virtual bool is_readable() const;

    /**
     * @brief Get the writable state of the pseudoAxe.
     * During the get_value and set_value method, the peusoAxe can be set unwritable.
     */
    virtual bool is_writable() const;

    /**
     * @brief Initialize the pseudoAxe.
     *
     * This method must be call before using a pseudoAxe.
     */
    virtual void initialize() throw(hkl::HKLException);

    /**
     * @brief Un-Initialize the pseudoAxe.
     * This method must be call to un-initialize a pseudoAxe.
     */
    virtual void uninitialize();

    virtual void update();

    /**
     * @brief set the current value of the PseudoAxe.
     * @throw HKLException if the pseudoAxe is not ready to be set.
     */
    virtual void set() throw(hkl::HKLException);

    virtual void set_write_from_read();

    /**
     * @brief print the DerivedWithSample into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    ostream & printToStream(ostream & flux) const;

    /**
     * @brief print on a stream the content of the DerivedWithSample
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    ostream & toStream(ostream & flux) const;

    /**
     * @brief restore the content of the DerivedWithSample from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    istream & fromStream(istream & flux);

};
template<class T, class C>
DerivedWithSample<T, C>::DerivedWithSample(T & geometry, hkl::SampleList *& samples) :
  PseudoAxeEngineWithSampleTemp<T>(geometry, samples, false, false, false)   
{
  // Bouml preserved body begin 00038C02
          _pseudoAxeEngineWithSample = new C(*_gconv, samples);
          PseudoAxeEngineWithSampleTemp<T>::_parameters = _pseudoAxeEngineWithSample->parameters();
          PseudoAxeEngineWithSampleTemp<T>::_pseudoAxes = _pseudoAxeEngineWithSample->pseudoAxes();
          
          //fill the pseudoAxes with the right engine.
          PseudoAxeList::iterator pseudoAxe_it = PseudoAxeEngineTemp<T>::_pseudoAxes.begin();
          PseudoAxeList::iterator pseudoAxe_end = PseudoAxeEngineTemp<T>::_pseudoAxes.end();
          while (pseudoAxe_it != pseudoAxe_end)
            {
              (*pseudoAxe_it)->set_engine(this);
              ++pseudoAxe_it;
            }
          
          //update the observable.
          hkl::AxeList & axes = geometry.axes();
          hkl::AxeList::iterator iter = axes.begin();
          hkl::AxeList::iterator end = axes.end();
          while(iter != end)
            {
              (*iter)->add_observer(this);
              ++iter;
            }
          
          // fill the _relatedAxes.
          iter = axes.begin();
          while(iter != end)
            {
              PseudoAxeEngineTemp<T>::_relatedAxes.push_back(*iter);
              ++iter;
            }
          
          PseudoAxeEngineTemp<T>::connect();
          update();
  // Bouml preserved body end 00038C02
}

template<class T, class C>
DerivedWithSample<T, C>::~DerivedWithSample() 
{
  // Bouml preserved body begin 00038C82
        delete _pseudoAxeEngineWithSample;
  // Bouml preserved body end 00038C82
}

/**
 * @brief Get the initialization state of the pseudoAxe.
 */
template<class T, class C>
bool DerivedWithSample<T, C>::is_initialized() const 
{
  // Bouml preserved body begin 00038D02
  return _pseudoAxeEngineWithSample->is_initialized();
  // Bouml preserved body end 00038D02
}

/**
 * @brief Get the readable state of the pseudoAxe
 * During the get_value and set_value method, the peusoAxe can be set unreadable.
 */
template<class T, class C>
bool DerivedWithSample<T, C>::is_readable() const 
{
  // Bouml preserved body begin 00038D82
        return _pseudoAxeEngineWithSample->is_readable();
  // Bouml preserved body end 00038D82
}

/**
 * @brief Get the writable state of the pseudoAxe.
 * During the get_value and set_value method, the peusoAxe can be set unwritable.
 */
template<class T, class C>
bool DerivedWithSample<T, C>::is_writable() const 
{
  // Bouml preserved body begin 00038E02
        return _pseudoAxeEngineWithSample->is_writable();
  // Bouml preserved body end 00038E02
}

/**
 * @brief Initialize the pseudoAxe.
 *
 * This method must be call before using a pseudoAxe.
 */
template<class T, class C>
void DerivedWithSample<T, C>::initialize() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00038E82
        _pseudoAxeEngineWithSample->unconnect();
        _gconv->setFromGeometry(PseudoAxeEngineWithSampleTemp<T>::_geometry, false);
        _pseudoAxeEngineWithSample->connect();
        _pseudoAxeEngineWithSample->initialize();
  // Bouml preserved body end 00038E82
}

/**
 * @brief Un-Initialize the pseudoAxe.
 * This method must be call to un-initialize a pseudoAxe.
 */
template<class T, class C>
void DerivedWithSample<T, C>::uninitialize() 
{
  // Bouml preserved body begin 00038F02
        _pseudoAxeEngineWithSample->uninitialize();
  // Bouml preserved body end 00038F02
}

template<class T, class C>
void DerivedWithSample<T, C>::update() 
{
  // Bouml preserved body begin 00038F82
        if (PseudoAxeEngineTemp<T>::_connected)
          {
            _pseudoAxeEngineWithSample->unconnect();
            _gconv->setFromGeometry(PseudoAxeEngineWithSampleTemp<T>::_geometry, false);
            _pseudoAxeEngineWithSample->connect();
            _pseudoAxeEngineWithSample->update();
          }
  // Bouml preserved body end 00038F82
}

/**
 * @brief set the current value of the PseudoAxe.
 * @throw HKLException if the pseudoAxe is not ready to be set.
 */
template<class T, class C>
void DerivedWithSample<T, C>::set() throw(hkl::HKLException) 
{
  // Bouml preserved body begin 00039002
        _pseudoAxeEngineWithSample->unconnect();
        _gconv->setFromGeometry(PseudoAxeEngineWithSampleTemp<T>::_geometry, false);
        _pseudoAxeEngineWithSample->connect();
        _pseudoAxeEngineWithSample->set();
        PseudoAxeEngineWithSampleTemp<T>::unconnect();
        PseudoAxeEngineWithSampleTemp<T>::_geometry.setFromGeometry(*_gconv, false);
        PseudoAxeEngineWithSampleTemp<T>::connect();
  // Bouml preserved body end 00039002
}

template<class T, class C>
void DerivedWithSample<T, C>::set_write_from_read() 
{
  // Bouml preserved body begin 00039082
        _pseudoAxeEngineWithSample->set_write_from_read();
  // Bouml preserved body end 00039082
}

/**
 * @brief print the DerivedWithSample<T, C> into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
template<class T, class C>
ostream & DerivedWithSample<T, C>::printToStream(ostream & flux) const 
{
  // Bouml preserved body begin 00039102
        PseudoAxeEngineWithSampleTemp<T>::printToStream(flux);
        flux << _pseudoAxeEngineWithSample;
        
        return flux;
  // Bouml preserved body end 00039102
}

/**
 * @brief print on a stream the content of the DerivedWithSample<T, C>
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T, class C>
ostream & DerivedWithSample<T, C>::toStream(ostream & flux) const 
{
  // Bouml preserved body begin 00039182
        PseudoAxeEngineWithSampleTemp<T>::toStream(flux);
        _pseudoAxeEngineWithSample->toStream(flux);
        return flux;
  // Bouml preserved body end 00039182
}

/**
 * @brief restore the content of the DerivedWithSample<T, C> from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
template<class T, class C>
istream & DerivedWithSample<T, C>::fromStream(istream & flux) 
{
  // Bouml preserved body begin 00039202
        PseudoAxeEngineWithSampleTemp<T>::fromStream(flux);
        _pseudoAxeEngineWithSample->fromStream(flux);
        return flux;
  // Bouml preserved body end 00039202
}


} // namespace hkl::pseudoAxeEngine

} // namespace hkl
/**
 * @brief Overload of the << operator for the PseudoAxe class
 * @param flux the stream to print into.
 * @param derivedPseudoAxe The DerivedPseudoAxe to send into the stream.
 */
template<typename T, typename C>
ostream &
operator<<(ostream & flux, hkl::pseudoAxeEngine::Derived<T, C> const & derivedPseudoAxeEngine)
{
  return derivedPseudoAxeEngine.printToStream(flux);
}
#endif
