#ifndef _PSEUDOAXEENGINE_H
#define _PSEUDOAXEENGINE_H


#include "hklobject.h"
#include "axe.h"
#include "pseudoaxelist.h"
#include "HKLException.h"
#include "observer.h"
#include <ostream>
#include <istream>
#include "samplelist.h"

namespace hkl { class PseudoAxe; } 

namespace hkl {

class PseudoAxeEngine : public hkl::HKLObject {
  protected:
    PseudoAxeEngine();


  public:
    /**
     * @brief The default destructor.
     */
    virtual ~PseudoAxeEngine();

    /**
     * @brief Get the related axes of the PseudoAxeEngine.
     * @return The related axes of the PseudoAxeEngine.
     */
    virtual AxeList & relatedAxes() = 0;

    /**
     * @brief Get the PseudoAxeList axes of the PseudoAxeEngine.
     * @return The PseudoAxeList axes of the PseudoAxeEngine.
     */
    virtual hkl::PseudoAxeList & pseudoAxes() = 0;

    /**
     * @brief Get the initialization state of the pseudoAxe.
     */
    virtual bool is_initialized() const = 0;

    /**
     * @brief Get the readable state of the pseudoAxe
     * During the get_value and set_value method, the peusoAxe can be set unreadable.
     */
    virtual bool is_readable() const = 0;

    /**
     * @brief Get the writable state of the pseudoAxe.
     * During the get_value and set_value method, the peusoAxe can be set unwritable.
     */
    virtual bool is_writable() const = 0;

    /**
     * @brief Initialize the pseudoAxe.
     *
     * This method must be call before using a pseudoAxe.
     */
    virtual void initialize() throw(hkl::HKLException) = 0;

    /**
     * @brief Un-Initialize the pseudoAxe.
     * This method must be call to un-initialize a pseudoAxe.
     */
    virtual void uninitialize() = 0;

    /**
     * @brief set the current value of the PseudoAxe.
     * @throw HKLException if the pseudoAxe is not ready to be set.
     */
    virtual void set() throw(hkl::HKLException) = 0;


  protected:
    /**
     * @brief Set the read part of a PseudoAxe without calling the set methode of the engine
     * @param pseudoAxe the hkl::PseudoAxe to set
     * @param min the minimum value to set
     * @param current the current value to set
     * @param consign the consign value to set
     * @param max The maximum value to set
     */
    void set_pseudoAxe(hkl::PseudoAxe * pseudoAxe, double min, double current, double consign, double & max);

};
template<class T>
class PseudoAxeEngineTemp : public hkl::PseudoAxeEngine, public hkl::Observer {
  protected:
    hkl::AxeList _relatedAxes;

    T & _geometry;

    bool _initialized;

    bool _readable;

    bool _writable;

    hkl::PseudoAxeList _pseudoAxes;


  public:
    typedef T value_type;


  protected:
    /**
     * @brief The default constructor.
     * @param geometry The Geometry use to compute the pseudoAxes values.
     * @param initialized the initial state of the PseudoAxeEngine.
     * @param readable The initial readable state of the PseudoAxeEngine.
     * @param writable The initial writable state of the PseudoAxeEngine.
     * @todo be sure to be consistant with ModeTemp.
     */
    PseudoAxeEngineTemp(T & geometry, bool initialized, bool readable, bool writable);


  public:
    virtual ~PseudoAxeEngineTemp();

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
     * @brief Get the PseudoAxeList axes of the PseudoAxeEngine.
     * @return The PseudoAxeList axes of the PseudoAxeEngine.
     */
    virtual hkl::PseudoAxeList & pseudoAxes();

    /**
     * @brief Get the related axes of the PseudoAxeEngine.
     * @return The related axes of the PseudoAxeEngine.
     */
    virtual AxeList & relatedAxes();

    /**
     * @brief Un-Initialize the pseudoAxe.
     * This method must be call to un-initialize a pseudoAxe.
     */
    virtual void uninitialize();

    /**
     * \brief Are two PseudoAxeEngineTemp equals ?
     * \param pseudoAxeEngineTemp the PseudoAxeEngineTemp<T> to compare with.
     * \return true if both are equals flase otherwise.
     */
    bool operator==(const PseudoAxeEngineTemp<T> & pseudoAxeEngineTemp) const;

    /**
     * @brief print the PseudoAxeEngineTemp into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    std::ostream & printToStream(std::ostream & flux) const;

    /**
     * @brief print on a stream the content of the PseudoAxeEngineTemp
     * @param flux the ostream to modify.
     * @return the modified ostream
     */
    std::ostream & toStream(std::ostream & flux) const;

    /**
     * @brief restore the content of the PseudoAxeEngineTemp from an istream
     * @param flux the istream.
     * @return the modified istream.
     * @todo problem of security here.
     */
    std::istream & fromStream(std::istream & flux);

};
/**
 * @brief The default constructor.
 * @param geometry The Geometry use to compute the pseudoAxes values.
 * @param initialized the initial state of the PseudoAxeEngine.
 * @param readable The initial readable state of the PseudoAxeEngine.
 * @param writable The initial writable state of the PseudoAxeEngine.
 * @todo be sure to be consistant with ModeTemp.
 */
template<class T>
PseudoAxeEngineTemp<T>::PseudoAxeEngineTemp(T & geometry, bool initialized, bool readable, bool writable) :
  PseudoAxeEngine(),
  Observer(),
  _geometry(geometry),
  _initialized(initialized),
  _readable(readable),
  _writable(writable)
{
  // Bouml preserved body begin 00030702
  // Bouml preserved body end 00030702
}

template<class T>
PseudoAxeEngineTemp<T>::~PseudoAxeEngineTemp() 
{
  // Bouml preserved body begin 00030782
  // Bouml preserved body end 00030782
}

/**
 * @brief Get the initialization state of the pseudoAxe.
 */
template<class T>
bool PseudoAxeEngineTemp<T>::is_initialized() const 
{
  // Bouml preserved body begin 00030802
      return _initialized;
  // Bouml preserved body end 00030802
}

/**
 * @brief Get the readable state of the pseudoAxe
 * During the get_value and set_value method, the peusoAxe can be set unreadable.
 */
template<class T>
bool PseudoAxeEngineTemp<T>::is_readable() const 
{
  // Bouml preserved body begin 00030902
      return _readable;
  // Bouml preserved body end 00030902
}

/**
 * @brief Get the writable state of the pseudoAxe.
 * During the get_value and set_value method, the peusoAxe can be set unwritable.
 */
template<class T>
bool PseudoAxeEngineTemp<T>::is_writable() const 
{
  // Bouml preserved body begin 00030982
      return _writable;
  // Bouml preserved body end 00030982
}

/**
 * @brief Get the PseudoAxeList axes of the PseudoAxeEngine.
 * @return The PseudoAxeList axes of the PseudoAxeEngine.
 */
template<class T>
hkl::PseudoAxeList & PseudoAxeEngineTemp<T>::pseudoAxes() 
{
  // Bouml preserved body begin 00031882
      return _pseudoAxes;
  // Bouml preserved body end 00031882
}

/**
 * @brief Get the related axes of the PseudoAxeEngine.
 * @return The related axes of the PseudoAxeEngine.
 */
template<class T>
AxeList & PseudoAxeEngineTemp<T>::relatedAxes() 
{
  // Bouml preserved body begin 00030A02
      return _relatedAxes;
  // Bouml preserved body end 00030A02
}

/**
 * @brief Un-Initialize the pseudoAxe.
 * This method must be call to un-initialize a pseudoAxe.
 */
template<class T>
void PseudoAxeEngineTemp<T>::uninitialize() 
{
  // Bouml preserved body begin 00030B02
      _initialized = false;
      _writable = false;
  // Bouml preserved body end 00030B02
}

/**
 * \brief Are two PseudoAxeEngineTemp<T> equals ?
 * \param pseudoAxeEngineTemp the hkl::PseudoAxeEngineTemp<T> to compare with.
 * \return true if both are equals flase otherwise.
 */
template<class T>
bool PseudoAxeEngineTemp<T>::operator==(const hkl::PseudoAxeEngineTemp<T> & pseudoAxeEngineTemp) const 
{
  // Bouml preserved body begin 00030B82
      return HKLObject::operator==(pseudoAxeEngineTemp);
  // Bouml preserved body end 00030B82
}

/**
 * @brief print the PseudoAxeEngineTemp<T> into a flux
 * @param flux The stream to print into.
 * @return The modified flux.
 */
template<class T>
std::ostream & PseudoAxeEngineTemp<T>::printToStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00030C02
      PseudoAxeEngine::printToStream(flux);
      flux << " initialized : " << _initialized << std::endl;
      flux << " readable : " << _readable << std::endl;
      flux << " writable : " << _writable << std::endl;
      return flux;
  // Bouml preserved body end 00030C02
}

/**
 * @brief print on a stream the content of the PseudoAxeEngineTemp<T>
 * @param flux the ostream to modify.
 * @return the modified ostream
 */
template<class T>
std::ostream & PseudoAxeEngineTemp<T>::toStream(std::ostream & flux) const 
{
  // Bouml preserved body begin 00030C82
      PseudoAxeEngine::toStream(flux);
      flux << " " << _initialized;
      flux << " " << _readable;
      flux << " " << _writable << std::endl;
      return flux;
  // Bouml preserved body end 00030C82
}

/**
 * @brief restore the content of the PseudoAxeEngineTemp<T> from an istream
 * @param flux the istream.
 * @return the modified istream.
 * @todo problem of security here.
 */
template<class T>
std::istream & PseudoAxeEngineTemp<T>::fromStream(std::istream & flux) 
{
  // Bouml preserved body begin 00030D02
      PseudoAxeEngine::fromStream(flux);
      flux >> _initialized >> _readable >> _writable;
      return flux;
  // Bouml preserved body end 00030D02
}

template<class T>
class PseudoAxeEngineWithSampleTemp : public hkl::PseudoAxeEngineTemp<T> {
  protected:
    hkl::SampleList * & _samples;


  public:
    PseudoAxeEngineWithSampleTemp(T & geometry, hkl::SampleList *& samples, bool initialized, bool readable, bool writable);

    virtual ~PseudoAxeEngineWithSampleTemp();

};
template<class T>
PseudoAxeEngineWithSampleTemp<T>::PseudoAxeEngineWithSampleTemp(T & geometry, hkl::SampleList *& samples, bool initialized, bool readable, bool writable) :
  hkl::PseudoAxeEngineTemp<T>(geometry, initialized, readable, writable),
  _samples(samples)  
{
  // Bouml preserved body begin 00038982
  // Bouml preserved body end 00038982
}

template<class T>
PseudoAxeEngineWithSampleTemp<T>::~PseudoAxeEngineWithSampleTemp() 
{
  // Bouml preserved body begin 00038A02
  // Bouml preserved body end 00038A02
}


} // namespace hkl
/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
template<typename T>
inline std::ostream &
operator<<(std::ostream & flux, hkl::PseudoAxeEngineTemp<T> const & pseudoAxeEngine)
{
  return pseudoAxeEngine.printToStream(flux);
}

#endif
