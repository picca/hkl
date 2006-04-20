#include "config.h"
#include "pseudoaxe_kappa6C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace kappa6C {
            namespace kappa4C {

                Vertical::Vertical(double alpha) :
                  PseudoAxe()
                {}

                Vertical::~Vertical(void)
                  {}

                ostream &
                Vertical::toStream(ostream & flux) const
                  {
                    PseudoAxe::toStream(flux);
                    m_geometry_K6C->toStream(flux);

                    return flux;
                  }

                istream &
                Vertical::fromStream(istream & flux)
                  {
                    PseudoAxe::fromStream(flux);
                    m_geometry_K6C->fromStream(flux);

                    return flux;
                  }

                namespace vertical {

                    /*******************/
                    /* OMEGA PSEUDOAXE */
                    /*******************/
                    Omega::Omega(double alpha) :
                      pseudoAxe::kappa4C::vertical::Omega(alpha),
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha)
                    {
                      set_name ("omega_v");
                      set_description ("This is the value of an equivalent eulerian geometry.");
                    }

                    Omega::~Omega(void)
                      {}

                    void
                    Omega::initialize(Geometry const & geometry)
                      {
                        set_wasInitialized(true);
                      }

                    bool
                    Omega::get_isValid(Geometry const & geometry) const
                      {
                        return true;
                      }

                    double const 
                    Omega::get_value(Geometry const & geometry)
                      {
                        m_geometry_K4C->setFromGeometry(static_cast<geometry::Kappa6C const &>(geometry));
#ifdef MSVC6
                        return ((pseudoAxe::kappa4C::vertical::Omega *)this)->get_value(*m_geometry_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Omega::get_value(*m_geometry_K4C);
#endif
                      }

                    void
                    Omega::set_value(Geometry & geometry,
                                     double const & value) throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_K4C->setFromGeometry(*m_geometry_K6C);

#ifdef MSVC6
                        ((pseudoAxe::kappa4C::vertical::Omega *)this)->set_value(*m_geometry_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Omega::set_value(*m_geometry_K4C, value);
#endif

                        m_geometry_K6C->setFromGeometry(*m_geometry_K4C);
                      }

                    /*****************/
                    /* CHI PSEUDOAXE */
                    /*****************/
                    Chi::Chi(double alpha) :
                      pseudoAxe::kappa4C::vertical::Chi(alpha),
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha)
                    {
                      set_name ("chi_v");
                      set_description ("This is the value of an equivalent eulerian geometry.");
                    }

                    Chi::~Chi(void)
                      {}

                    void
                    Chi::initialize(Geometry const & geometry)
                      {
                        set_wasInitialized(true);
                      }

                    bool
                    Chi::get_isValid(Geometry const & geometry) const
                      {
                        return true;
                      }

                    double const 
                    Chi::get_value(Geometry const & geometry)
                      {
                        m_geometry_K4C->setFromGeometry(static_cast<geometry::Kappa6C const &>(geometry));
#ifdef MSVC6
                        return ((pseudoAxe::kappa4C::vertical::Chi *)this)->get_value(*m_geometry_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Chi::get_value(*m_geometry_K4C);
#endif
                      }

                    void
                    Chi::set_value(Geometry & geometry,
                                   double const & value) throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_K4C->setFromGeometry(*m_geometry_K6C);

#ifdef MSVC6
                        ((pseudoAxe::kappa4C::vertical::Chi *)this)->set_value(*m_geometry_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Chi::set_value(*m_geometry_K4C, value);
#endif

                        m_geometry_K6C->setFromGeometry(*m_geometry_K4C);
                      }

                    /*****************/
                    /* PHI PSEUDOAXE */
                    /*****************/
                    Phi::Phi(double alpha) :
                      pseudoAxe::kappa4C::vertical::Phi(alpha),
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha)
                    {
                      set_name ("phi_v");
                      set_description ("This is the value of an equivalent eulerian geometry.");
                    }

                    Phi::~Phi(void)
                      {}

                    void
                    Phi::initialize(Geometry const & geometry)
                      {
                        set_wasInitialized(true);
                      }

                    bool
                    Phi::get_isValid(Geometry const & geometry) const
                      {
                        return true;
                      }

                    double const 
                    Phi::get_value(Geometry const & geometry)
                      {
                        m_geometry_K4C->setFromGeometry(static_cast<geometry::Kappa6C const &>(geometry));
#ifdef MSVC6
                        return ((pseudoAxe::kappa4C::vertical::Phi *)this)->get_value(*m_geometry_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Phi::get_value(*m_geometry_K4C);
#endif
                      }

                    void
                    Phi::set_value(Geometry & geometry,
                                   double const & value) throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_K4C->setFromGeometry(*m_geometry_K6C);

#ifdef MSVC6
                        ((pseudoAxe::kappa4C::vertical::Phi *)this)->set_value(*m_geometry_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Phi::set_value(*m_geometry_K4C, value);
#endif

                        m_geometry_K6C->setFromGeometry(*m_geometry_K4C);
                      }

                } // namespace vertical
            } // namespace kappa4C
        } // namespace kappa6C
    } // namespace pseudoAxe
} // namespace hkl
