#ifndef _PSEUDOMULTIAXE_H_
#define _PSEUDOMULTIAXE_H_

#include "object.h"
#include "range.h"

using namespace std;

namespace hkl
  {

  // forward declaration
  class PseudoAxeEngine;

  /**
   * \brief A class design to describe a pseudomultiaxe from a geometry type
   */
  class PseudoMultiAxe : public ObjectReadOnly
    {
    public:

      /**
       * @brief The default constructor.
       * @param geometry The Geometry use to compute the pseudoAxes values.
       * @param name The name of the PseudoAxeTemp.
       * @param description The description of the PseudoAxeTemp.
       * @todo be sure to be consistant with ModeTemp.
       */
      PseudoMultiAxe(MyString const & name, MyString const & description, Range & range, PseudoAxeEngine * engine);

      /**
       * @brief The default destructor.
       */
      virtual ~PseudoMultiAxe(void);

      /**
       * @brief Initialize the pseudoAxe.
       *
       * This method must be call before using a pseudoAxe.
       */
      void initialize(void) throw (HKLException);

      void uninitialize(void);
      bool get_initialized(void) const;
      bool get_readable(void) const;
      bool get_writable(void) const;
      Value const & get_min(void) const throw (HKLException);
      Value const & get_max(void) const throw (HKLException);
      Value const & get_current(void) const throw (HKLException);
      void set_current(Value const & value) throw (HKLException);

      /**
       * @brief compare two PseudoAxeTemp.
       * @param pseudoAxe the pseudoAxeTemp to compare with.
       * @return true, if bothh are identical, false otherwise.
       */
      bool operator==(PseudoMultiAxe const & pseudoMultiAxe) const;

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
      Range & _range;
      PseudoAxeEngine * _engine;
    };

} // namespace hkl

/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
inline ostream &
operator<<(ostream & flux, hkl::PseudoMultiAxe const & pseudoMultiAxeTemp)
{
  return pseudoMultiAxeTemp.printToStream(flux);
}

#endif // _PSEUDOMULTIAXE_H_
