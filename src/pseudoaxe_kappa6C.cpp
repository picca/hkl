#include "config.h"

#include "pseudoaxe_kappa6C.h"
#include "convenience.h"

namespace hkl {
    namespace pseudoAxe {
        namespace kappa6C {
            namespace kappa4C {

                Vertical::Vertical(double const & alpha)
                  {
                    m_K4C = new geometry::kappa4C::Vertical(alpha);
                  }

                Vertical::~Vertical(void)
                  {
                    delete m_K4C;
                  }

                namespace vertical {

                    /*******************/
                    /* OMEGA PSEUDOAXE */
                    /*******************/
                    Omega::Omega(double const & alpha) :
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha),
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::kappa4C::vertical::Omega(alpha)
#endif
                    {
                      set_name ("omega_v");
#ifdef MSVC6
                      m_omega = new pseudoAxe::kappa4C::vertical::Omega(alpha);
                      set_description(m_omega->get_description());
                      set_valueList(m_omega->get_valueList());
#endif
                    }

                    Omega::~Omega(void)
                      {
#ifdef MSVC6
                        delete m_omega;
#endif
                      }

                    void
                    Omega::initialize(Geometry const & geometry)
                      {
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_omega->set_valueList(get_valueList());
                        m_omega->initialize(*m_K4C);
#else
                        pseudoAxe::kappa4C::vertical::Omega::initialize(*m_K4C);
#endif
                      }

                    bool
                    Omega::get_isValid(Geometry const & geometry) const
                      {
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_omega->set_valueList(get_valueList());
                        return m_omega->get_isValid(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Omega::get_isValid(*m_K4C);
#endif
                      }

                    double
                    Omega::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_omega->set_valueList(get_valueList());
                        return m_omega->get_value(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Omega::get_value(*m_K4C);
#endif
                      }

                    void
                    Omega::set_value(Geometry & geometry,
                                     double const & value) const throw (HKLException)
                      {
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_omega->set_valueList(get_valueList());
                        m_omega->set_value(*m_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Omega::set_value(*m_K4C, value);
#endif

                        geometry.setFromGeometry(*m_K4C, false);
                      }

                    /*****************/
                    /* CHI PSEUDOAXE */
                    /*****************/
                    Chi::Chi(double const & alpha) :
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha),
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::kappa4C::vertical::Chi(alpha)
#endif
                    {
                      set_name ("chi_v");
#ifdef MSVC6
                      m_chi = new pseudoAxe::kappa4C::vertical::Chi(alpha);
                      set_description(m_chi->get_description());
                      set_valueList(m_chi->get_valueList());
#endif
                    }

                    Chi::~Chi(void)
                      {
#ifdef MSVC6
                        delete m_chi;
#endif
                      }

                    void
                    Chi::initialize(Geometry const & geometry)
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_chi->set_valueList(get_valueList());
                        m_chi->initialize(*m_K4C);
#else
                        pseudoAxe::kappa4C::vertical::Chi::initialize(*m_K4C);
#endif
                      }

                    bool
                    Chi::get_isValid(Geometry const & geometry) const
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_chi->set_valueList(get_valueList());
                        return m_chi->get_isValid(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Chi::get_isValid(*m_K4C);
#endif
                      }

                    double
                    Chi::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        //m_K4C->setFromGeometry(static_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_chi->set_valueList(get_valueList());
                        return m_chi->get_value(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Chi::get_value(*m_K4C);
#endif
                      }

                    void
                    Chi::set_value(Geometry & geometry,
                                   double const & value) const throw (HKLException)
                      {
                        geometry::Kappa6C & K6C = static_cast<geometry::Kappa6C &>(geometry);
                        //m_K4C->setFromGeometry(K6C);
                        m_K4C->setFromGeometry(geometry, false);

#ifdef MSVC6
                        m_chi->set_valueList(get_valueList());
                        m_chi->set_value(*m_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Chi::set_value(*m_K4C, value);
#endif

                        K6C.setFromGeometry(*m_K4C, false);
                      }

                    /*****************/
                    /* PHI PSEUDOAXE */
                    /*****************/
                    Phi::Phi(double const & alpha) :
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha),
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::kappa4C::vertical::Phi(alpha)
#endif
                    {
                      set_name ("phi_v");
#ifdef MSVC6
                      m_phi = new pseudoAxe::kappa4C::vertical::Phi(alpha);
                      set_description(m_phi->get_description());
                      set_valueList(m_phi->get_valueList());
#endif
                    }

                    Phi::~Phi(void)
                      {
#ifdef MSVC6
                        delete m_phi;
#endif
                      }

                    void
                    Phi::initialize(Geometry const & geometry)
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_phi->set_valueList(get_valueList());
                        m_phi->initialize(*m_K4C);
#else
                        pseudoAxe::kappa4C::vertical::Phi::initialize(*m_K4C);
#endif
                      }

                    bool
                    Phi::get_isValid(Geometry const & geometry) const
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_phi->set_valueList(get_valueList());
                        return m_phi->get_isValid(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Phi::get_isValid(*m_K4C);
#endif
                      }

                    double
                    Phi::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        //m_K4C->setFromGeometry(static_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_phi->set_valueList(get_valueList());
                        return m_phi->get_value(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Phi::get_value(*m_K4C);
#endif
                      }

                    void
                    Phi::set_value(Geometry & geometry,
                                   double const & value) const throw (HKLException)
                      {
                        geometry::Kappa6C & K6C = static_cast<geometry::Kappa6C &>(geometry);
                        //m_K4C->setFromGeometry(K6C);
                        m_K4C->setFromGeometry(geometry, false);

#ifdef MSVC6
                        m_phi->set_valueList(get_valueList());
                        m_phi->set_value(*m_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Phi::set_value(*m_K4C, value);
#endif

                        K6C.setFromGeometry(*m_K4C, false);
                      }

                    /*******/
                    /* PSI */
                    /*******/
                    Psi::Psi(double const & alpha) :
                      pseudoAxe::kappa6C::kappa4C::Vertical(alpha),
#ifdef MSVC6
                      PseudoAxe()
#else
                      pseudoAxe::kappa4C::vertical::Psi(alpha)
#endif
                    {
                      set_name("psi_v");
#ifdef MSVC6
                      m_psi = new pseudoAxe::kappa4C::vertical::Psi(alpha);
                      set_description(m_psi->get_description());
                      set_valueList(m_psi->get_valueList());
#endif
                    }

                    Psi::~Psi(void)
                      {
#ifdef MSVC6
                        delete m_psi;
#endif
                      }

                    void
                    Psi::initialize(Geometry const & geometry)
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi->set_valueList(m_psi->get_valueList());
                        m_psi->initialize(*m_K4C);
#else
                        pseudoAxe::kappa4C::vertical::Psi::initialize(*m_K4C);
#endif
                      }

                    bool
                    Psi::get_isValid(Geometry const & geometry) const
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi->set_valueList(m_psi->get_valueList());
                        return m_psi->get_isValid(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Psi::get_isValid(*m_K4C);
#endif
                      }

                    double
                    Psi::get_value(Geometry const & geometry) const throw (HKLException)
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi->set_valueList(m_psi->get_valueList());
                        return m_psi->get_value(*m_K4C);
#else
                        return pseudoAxe::kappa4C::vertical::Psi::get_value(*m_K4C);
#endif
                      }

                    void
                    Psi::set_value(Geometry & geometry,
                                   double const & value) const throw (HKLException)
                      {
                        //m_K4C->setFromGeometry(dynamic_cast<geometry::Kappa6C const &>(geometry));
                        m_K4C->setFromGeometry(geometry, false);
#ifdef MSVC6
                        m_psi->set_valueList(m_psi->get_valueList());
                        m_psi->set_value(*m_K4C, value);
#else
                        pseudoAxe::kappa4C::vertical::Psi::set_value(*m_K4C, value);
#endif
                        dynamic_cast<geometry::Kappa6C &>(geometry).setFromGeometry(*m_K4C, false);
                      }

                } // namespace vertical
            } // namespace kappa4C
        } // namespace kappa6C
    } // namespace pseudoAxe
} // namespace hkl
