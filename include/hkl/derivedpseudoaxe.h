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

    DerivedPseudoAxe(C &, MyString const & name, MyString const & description); //!< The default constructor - protected to make sure this class is abstract.

    DerivedPseudoAxe(DerivedPseudoAxe const & derivedPseudoAxe); //!<The default copy constructor.

    virtual ~DerivedPseudoAxe(void); //!< The default destructor.

    bool get_initialized(void);

    bool get_readable(void);

    bool get_writable(void);

    /**
     * \brief Initialize the PseudoAxe from the Geometry.
     */
    void initialize(void) throw (HKLException);

    /**
     * @brief Uninitialize the pseudoAxe.
     */
    void uninitialize(void);

    void update(void);

    Value const & get_min(void) throw (HKLException);

    Value const & get_max(void) throw (HKLException);

    Value const & get_current(void) throw (HKLException);

    /**
     * \brief set the current value of the PseudoAxe.
     * \param value The value to set.
     * \throw HKLException if the pseudoAxe is not ready to be set.
     */
    void set_current(Value const & value) throw (HKLException);

    ostream & printToStream(ostream & flux) const;

    ostream & toStream(ostream & flux) const;

    istream & fromStream(istream & flux);

private:
    mutable typename T::value_type _gconv; //!< The geometry used to do the conversion.
    mutable T * _pseudoAxe; //!< The pseudoAxe use to do the calculation.

};

template<typename T, typename C>
DerivedPseudoAxe<T, C>::DerivedPseudoAxe(C & geometry, MyString const & name, MyString const & description) :
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

template<typename T, typename C>
DerivedPseudoAxe<T, C>::DerivedPseudoAxe(DerivedPseudoAxe const & derivedPseudoAxe) :
        PseudoAxeTemp<C>(derivedPseudoAxe),
        _gconv(derivedPseudoAxe._gconv),
        _pseudoAxe(derivedPseudoAxe._pseudoAxe)
{
    // for now if we made a copy seg fault due to try to delete two times the same pointer _pseudoAxe.
}

template<typename T, typename C>
DerivedPseudoAxe<T, C>::~DerivedPseudoAxe(void)
{
    delete _pseudoAxe;
}

template<typename T, typename C>
bool
DerivedPseudoAxe<T, C>::get_initialized(void)
{
    return _pseudoAxe->get_initialized();
}

template<typename T, typename C>
bool
DerivedPseudoAxe<T, C>::get_writable(void)
{
    return _pseudoAxe->get_writable();
}

template<typename T, typename C>
bool
DerivedPseudoAxe<T, C>::get_readable(void)
{
    return _pseudoAxe->get_readable();
}

template<typename T, typename C>
Value const &
DerivedPseudoAxe<T, C>::get_min(void) throw (HKLException)
{
    return _pseudoAxe->get_min();
}

template<typename T, typename C>
Value const &
DerivedPseudoAxe<T, C>::get_max(void) throw (HKLException)
{
    return _pseudoAxe->get_max();
}

template<typename T, typename C>
Value const &
DerivedPseudoAxe<T, C>::get_current(void) throw (HKLException)
{
    return _pseudoAxe->get_current();
}

template<typename T, typename C>
void
DerivedPseudoAxe<T, C>::initialize(void) throw (HKLException)
{
    _pseudoAxe->unconnect();
    _gconv.setFromGeometry(PseudoAxeTemp<C>::_geometry, false);
    _pseudoAxe->connect();
    _pseudoAxe->initialize();
}

template<typename T, typename C>
void
DerivedPseudoAxe<T, C>::uninitialize(void)
{
    _pseudoAxe->uninitialize();
}

template<typename T, typename C>
void
DerivedPseudoAxe<T, C>::update()
{
    if (PseudoAxeTemp<C>::_connected)
    {
        _pseudoAxe->unconnect();
        _gconv.setFromGeometry(PseudoAxeTemp<C>::_geometry, false);
        _pseudoAxe->connect();
        _pseudoAxe->update();
    }
}

template<typename T, typename C>
void
DerivedPseudoAxe<T, C>::set_current(Value const & value) throw (HKLException)
{
    _pseudoAxe->unconnect();
    _gconv.setFromGeometry(PseudoAxeTemp<C>::_geometry, false);
    _pseudoAxe->connect();
    _pseudoAxe->set_current(value);
    PseudoAxeTemp<C>::unconnect();
    PseudoAxeTemp<C>::_geometry.setFromGeometry(_gconv, false);
    PseudoAxeTemp<C>::connect();
}

template<typename T, typename C>
ostream &
DerivedPseudoAxe<T, C>::printToStream(ostream & flux) const
{
    PseudoAxeTemp<C>::printToStream(flux);
    flux << _pseudoAxe;
    return flux;
}

template<typename T, typename C>
ostream &
DerivedPseudoAxe<T, C>::toStream(ostream & flux) const
{
    PseudoAxeTemp<C>::toStream(flux);
    _pseudoAxe->toStream(flux);
    return flux;
}

template<typename T, typename C>
istream &
DerivedPseudoAxe<T, C>::fromStream(istream & flux)
{
    PseudoAxeTemp<C>::fromStream(flux);
    _pseudoAxe->fromStream(flux);
    return flux;
}

} // namespace hkl

/*!
 * \brief Overload of the << operator for the PseudoAxe class
 */
template<typename T, typename C>
ostream &
operator<<(ostream & flux, hkl::DerivedPseudoAxe<T, C> const & derivedPseudoAxe)
{
    return derivedPseudoAxe.printToStream(flux);
}



#endif // _DERIVEDPSEUDOAXE_H_
