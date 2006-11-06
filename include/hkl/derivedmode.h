#ifndef _DERIVEDMODE_H_
#define _DERIVEDMODE_H_

#include <iostream>

#include "mode.h"
#include "geometry.h"
#include "HKLException.h"

using namespace std;

namespace hkl
  {

  /*!
   * \brief A class design to describe a mode derived from another one.
   */
  template<typename T, typename C>
  class DerivedMode : public Mode<C>
    {
    public:

      DerivedMode(void); //!< The default constructor - protected to make sure this class is abstract.

      DerivedMode(DerivedMode const & derivedMode); //!<The default copy constructor.

      virtual ~DerivedMode(void); //!< The default destructor.

      virtual void computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 C & geometry) const;

    private:
      mutable typename T::value_type m_gconv; //!< The geometry used to do the conversion.
      mutable T m_mode; //!< The real calculus mode.
    };

  template<typename T, typename C>
  DerivedMode<T, C>::DerivedMode(void) :
      Mode<C>()
  {
    set_name(m_mode.get_name());
    set_description(m_mode.get_description());
    set_valueList(m_mode.get_valueList());
  }

  template<typename T, typename C>
  DerivedMode<T, C>::DerivedMode(DerivedMode const & derivedMode) :
      Mode<C>(derivedMode),
      m_mode(derivedMode.m_mode)
  {}

  template<typename T, typename C>
  DerivedMode<T, C>::~DerivedMode(void)
  {}

  template<typename T, typename C>
  void
  DerivedMode<T, C>::computeAngles(double h, double k, double l,
                                   smatrix const & UB,
                                   C & geometry) const
    {
      m_gconv.setFromGeometry(geometry, true);
      m_mode.set_valueList(Mode<C>::get_valueList());
      m_mode.computeAngles(h, k, l, UB, m_gconv);
      geometry.setFromGeometry(m_gconv, true);
    }

} // namespace hkl

#endif // _DERIVEDMODE_H_
