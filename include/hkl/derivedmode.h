#ifndef _DERIVEDMODE_H_
#define _DERIVEDMODE_H_

#include "mode.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief A class design to describe a mode derived from another one.
   */
  template<typename T, typename C>
  class DerivedMode : public ModeTemp<C>
    {
    public:

      /**
       * @brief The default constructor.
       * @param name The name of the DerivedMode.
       * @param description The description of the DerivedMode.
       * @param geometry The Geometry for the computation.
       */
      DerivedMode(MyString const & name, MyString const & description, C & geometry) :
          ModeTemp<C>(name, description, geometry)
      {
        _mode = new T("derived", "real mode", _gconv);
        ModeTemp<C>::_parameters = _mode->parameters();
      }

      /**
       * @brief The default destructor.
       */
      virtual ~DerivedMode(void)
      {
        delete _mode;
      }

      void computeAngles(Value const & h, Value const & k, Value const & l,
                         smatrix const & UB) const
        {
          _gconv.setFromGeometry(ModeTemp<C>::_geometry, true);
          _mode->computeAngles(h, k, l, UB);
          ModeTemp<C>::_geometry.setFromGeometry(_gconv, true);
        }

    protected:
      mutable typename T::value_type _gconv; //!< The geometry used to do the conversion.
      mutable T * _mode; //!< The real calculus mode.

    };

} // namespace hkl

#endif // _DERIVEDMODE_H_
