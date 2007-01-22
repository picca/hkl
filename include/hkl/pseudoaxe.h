#ifndef _PSEUDOAXE_H_
#define _PSEUDOAXE_H_

#include "object.h"
#include "range.h"
#include "parameterlist.h"

using namespace std;

namespace hkl
  {

  // forward declaration
  class PseudoAxeEngine;

  /**
   * \brief A class design to describe a PseudoAxe.
   */
  class PseudoAxe : public ObjectReadOnly
    {
    public:

      /**
       * @brief The default constructor.
       * @param name The name of the PseudoAxeTemp.
       * @param description The description of the PseudoAxeTemp.
       * @param read The read part of the PseudoAxe.
       * @param write The write part of the PseudoAxe.
       * @param engine The engine use to compute the pseudoAxes value.
       * @todo be sure to be consistant with ModeTemp.
       */
      PseudoAxe(MyString const & name, MyString const & description, Range const & read, Range & write, PseudoAxeEngine * engine);

      /**
       * @brief The default destructor.
       */
      virtual ~PseudoAxe(void);

      /**
       * @brief Initialize the pseudoAxe.
       * This method must be call before using a pseudoAxe.
       */
      void initialize(void) throw (HKLException);

      /** 
       * @brief uninitialize the PseudoAxe.
       * Uninitialize a PseudoAxe if you do not whant to use it.
       */
      void uninitialize(void);

      /** 
       * @brief Get the initialized state of the PseudoAxe.
       * @return A bool fill with the initialized state of the PseudoAxe.
       */
      bool get_initialized(void) const;

      /** 
       * @brief Get the readable state of the PseudoAxe.
       * @return A bool fill with the readable state of the PseudoAxe.
       */
      bool get_readable(void) const;

      /** 
       * @brief Get the writable state of the PseudoAxe.
       * @return A bool fill with the writable state of the PseudoAxe.
       */
      bool get_writable(void) const;

      /** 
       * @brief Get the min Value of the PseudoAxe.
       * @return A Value fill with the minimum value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      Value const & get_min(void) const throw (HKLException);

      /** 
       * @brief Get the max Value of the PseudoAxe.
       * @return A Value fill with the maximum value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      Value const & get_max(void) const throw (HKLException);

      /** 
       * @brief Get the current Value of the PseudoAxe.
       * @return A Value fill with the current value of the PseudoAxe.
       * @throw HKLException if the PseudoAxe is not readable.
       */
      Value const & get_current(void) const throw (HKLException);

      /** 
       * @brief Set the current value of the PseudoAxe.
       * @param value The Value to set. 
       * @throw HKLException If the PseudoAxe is not writable. 
       * 
       * This method set the write part of the pseudoAxe and compute
       * the corresponding geometry using the engine. 
       */
      void set_current(Value const & value) throw (HKLException);
      
      /** 
       * @brief Set the engine use by the PseudoAxe.
       * @param engine The engine to set.
       *
       * This method is only use by the DerivedPseudoAxeEngine to modify
       * the engine part of the PseudoAxe.
       */
      void set_engine(PseudoAxeEngine * engine);

      /** 
       * @brief Get the ParameterList  of the PseudoAxe. 
       * @return The ParameterList of the PseudoAxe.
       */
      ParameterList & parameters(void)
      {
        return _parameters;
      }

      /**
       * @brief compare two PseudoAxeTemp.
       * @param pseudoAxe the pseudoAxeTemp to compare with.
       * @return true, if bothh are identical, false otherwise.
       */
      bool operator==(PseudoAxe const & pseudoAxe) const;

      /**
       * @brief Print a PseudoAxeTemp in a stream.
       * @param flux the stream to print into.
       * @return the modified stream.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * @brief Store a PseudoAxeTemp in a stream.
       * @param flux the stream use to store the PseudoAxeTemp.
       * @return the modified stream.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * @brief Restore a PseudoAxeTemp from a stream.
       * @param flux The stream to restore from.
       * @return the modified stream.
       */
      istream & fromStream(istream & flux);

    protected:
      Range const & _read; //!< The read part of the PseudoAxe (update by the engine)
      Range & _write; //!< The write part of the PseudoAxe (use by engine to compute the geometry)
      PseudoAxeEngine * _engine; //!< The engine use for computation.
      ParameterList & _parameters; //!< The ParameterList of the pseudaxe.
    };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
inline ostream &
operator<<(ostream & flux, hkl::PseudoAxe const & pseudoAxe)
{
  return pseudoAxe.printToStream(flux);
}

#endif // _PSEUDOAXE_H_
