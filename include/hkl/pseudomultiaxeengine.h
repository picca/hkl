#ifndef _PSEUDOMULTIAXEENGINE_H_
#define _PSEUDOMULTIAXEENGINE_H_

#include "pseudomultiaxe.h"
#include "pseudoaxelist.h"

using namespace std;

namespace hkl
  {

  class PseudoMultiAxeEngine : public HKLObject, public Observer
    {
    public:

      /**
       * @brief The default destructor.
       */
      virtual ~PseudoMultiAxeEngine(void);

      PseudoAxeList & pseudoAxes(void)
      {
        return _pseudoAxes;
      }

      /**
       * @brief Get the initialization state of the pseudoAxe
       * @return True if the pseudoAxe was initialized. False otherwise.
       */
      virtual bool get_initialized(void) const
        {
          return _initialized;
        }

      /**
       * @brief Get the writable state of the pseudoAxe.
       * During the get_value and set_value method, the peusoAxe can be set unwritable.
       */
      virtual bool get_writable(void) const
        {
          return _writable;
        }

      /**
       * @brief Get the readable state of the pseudoAxe.
       *
       * During the get_value and set_value method, the peusoAxe can be set unreadable.
       */
      virtual bool get_readable(void) const
        {
          return _readable;
        }

      /**
       * @brief Initialize the pseudoAxe.
       *
       * This method must be call before using a pseudoAxe.
       */
      virtual void initialize(void) throw (HKLException) = 0;

      /**
       * @brief Un-Initialize the pseudoAxe.
       *
       * This method must be call to un-initialize a pseudoAxe.
       */
      virtual void uninitialize(void);

      /**
       * @brief set the current value of the PseudoAxe.
       * @throw HKLException if the pseudoAxe is not ready to be set.
       */
      virtual void update(void) throw (HKLException) = 0;

      /**
       * @brief set the current value of the PseudoAxe.
       * @throw HKLException if the pseudoAxe is not ready to be set.
       */
      virtual void set(void) throw (HKLException) = 0;

      /**
       * @brief compare two PseudoAxes.
       * @param pseudoAxe the pseudoAxe to compare with.
       * @return true, if bothh are identical, false otherwise.
       */
      bool operator==(PseudoMultiAxeEngine const & pseudoMultiAxeEngine) const;

      /**
       * @brief Print a PseudoAxe in a stream.
       * @param flux the stream to print into.
       * @return the modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Store a PseudoAxe in a stream.
       * @param flux the stream use to store the PseudoAxe.
       * @return the modified stream.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore a PseudoAxe from a stream.
       * @param flux The stream to restore from.
       * @return the modified stream.
       */
      istream & fromStream(istream & flux);

    protected:

      bool _initialized; //!< The initialized state of the PseudoAxe.
      bool _readable; //!< The readable state of the PseudoAxe.
      bool _writable; //!< The writable state of the PseudoAxe.
      PseudoAxeList _pseudoAxes;

      /**
       * @brief The default constructor -- protected to be sure the calss is abstract.
       * @param name the name of the PseudoAxe.
       * @param description the description of the PseudoAxe.
       */
      PseudoMultiAxeEngine(void);

      /**
       * @brief The default constructor
       * @param pseudoAxe The PseudoAxe to copy.
       */
      PseudoMultiAxeEngine(PseudoMultiAxeEngine const & pseudoMultiAxeEngine);
    };

  /**
   * \brief A class design to describe a pseudoaxe from a geometry type
   */
  template<typename T>
  class PseudoMultiAxeEngineTemp : public PseudoMultiAxeEngine
    {
    public:

      /**
       * @brief The default destructor.
       */
      virtual ~PseudoMultiAxeEngineTemp(void)
      {}

      /**
       * @brief Initialize the pseudoAxe.
       *
       * This method must be call before using a pseudoAxe.
       */
      void initialize(void) throw (HKLException)
      {
        _geometry0 = _geometry;
        _initialized = true;
        _writable = true;
      }

      /**
       * @brief Assign a pseudoAxeTemp to another one.
       * @param pseudoaxe The PseudoAxeTemp to assign.
       * @return The modified PseudoAxeTemp.
       */
      PseudoMultiAxeEngineTemp & operator=(PseudoMultiAxeEngineTemp const & pseudoaxe)
      {
        return *this;
      }

      /**
       * @brief compare two PseudoAxeTemp.
       * @param pseudoAxe the pseudoAxeTemp to compare with.
       * @return true, if bothh are identical, false otherwise.
       */
      bool operator==(PseudoMultiAxeEngineTemp const & pseudoMultiAxeEngine) const
        {
          return PseudoMultiAxeEngine::operator==(pseudoMultiAxeEngine)
                 && _geometry0 == pseudoMultiAxeEngine._geometry0;
        }

      /**
       * @brief Print a PseudoAxeTemp in a stream.
       * @param flux the stream to print into.
       * @return the modified stream.
       */
      ostream & printToStream(ostream & flux) const
        {
          PseudoMultiAxeEngine::printToStream(flux);
          flux << _geometry0;
          return flux;
        }

      /**
       * @brief Store a PseudoAxeTemp in a stream.
       * @param flux the stream use to store the PseudoAxeTemp.
       * @return the modified stream.
       */
      ostream & toStream(ostream & flux) const
        {
          PseudoMultiAxeEngine::toStream(flux);
          _geometry0.toStream(flux);
          return flux;
        }

      /**
       * @brief Restore a PseudoAxeTemp from a stream.
       * @param flux The stream to restore from.
       * @return the modified stream.
       */
      istream & fromStream(istream & flux)
      {
        PseudoMultiAxeEngine::fromStream(flux);
        _geometry0.fromStream(flux);
        return flux;
      }

    protected:
      T & _geometry; //!< geometry connected to the pseudoAxe.
      T _geometry0; //!< Geometry used to store the initialisation of the pseudoAxe.

      /**
       * @brief The default constructor.
       * @param geometry The Geometry use to compute the pseudoAxes values.
       * @param name The name of the PseudoAxeTemp.
       * @param description The description of the PseudoAxeTemp.
       * @todo be sure to be consistant with ModeTemp.
       */
      PseudoMultiAxeEngineTemp(T & geometry) :
          PseudoMultiAxeEngine(),
          _geometry(geometry)
      {}

      /**
       * @brief A copy constructor
       * @param pseudoAxeTemp The PseudoAxeTemp to copy.
       */
      PseudoMultiAxeEngineTemp(PseudoMultiAxeEngineTemp const & pseudoMultiAxeEngineTemp) :
          PseudoMultiAxeEngine(pseudoMultiAxeEngineTemp),
          _geometry(pseudoMultiAxeEngineTemp._geometry),
          _geometry0(pseudoMultiAxeEngineTemp._geometry0)
      {}

    public:
      typedef T value_type; //!< The type of the geometry use in this PseudoAxe.
    };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
template<typename T>
inline ostream &
operator<<(ostream & flux, hkl::PseudoMultiAxeEngineTemp<T> const & pseudoMultiAxeEngineTemp)
{
  return pseudoMultiAxeEngineTemp.printToStream(flux);
}

#endif // _PSEUDOAXE_H_
