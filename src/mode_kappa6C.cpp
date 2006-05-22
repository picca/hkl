#include "config.h"
#include "geometry_eulerian4C.h"
#include "geometry_kappa6C.h"
#include "mode_kappa6C.h"

namespace hkl {
    namespace mode {
        namespace kappa6C {
            namespace eulerian4C {

                Vertical::Vertical(void)
                  {}

                Vertical::~Vertical(void)
                  {}

                namespace vertical {

                    /*************************/
                    /* VERTICAL 4C BISSECTOR */
                    /*************************/
                    Bissector::Bissector(void)
#ifndef MSVC6                      
                    : mode::eulerian4C::vertical::Bissector()
#endif                      
                      {
                        set_name("Vertical Eulerian 4C Bissector");
#ifdef MSVC6
                        set_description(m_mode.get_description());
                        set_valueList(m_mode.get_valueList());
#endif
                      }

                    Bissector::~Bissector(void)
                      {}

                    void 
                    Bissector::computeAngles(double h, double k, double l,
                                             smatrix const & UB,
                                             Geometry & geometry) const throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_E4C.setFromGeometry(*m_geometry_K6C, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                        mode::eulerian4C::vertical::Bissector::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                        m_geometry_K6C->setFromGeometry(m_geometry_E4C, true);
                        return;
                      }

                    /***************************/
                    /* VERTICAL 4C DELTA THETA */
                    /***************************/
                    Delta_Theta::Delta_Theta()
#ifndef MSVC6                      
                    : mode::eulerian4C::vertical::Delta_Theta()
#endif                      
                      {
                        set_name("Vertical Eulerian 4C Delta Theta");
#ifdef MSVC6
                        set_description(m_mode.get_description());
                        set_valueList(m_mode.get_valueList());
#endif
                      }

                    Delta_Theta::~Delta_Theta() {}

                    void 
                    Delta_Theta::computeAngles(double h, double k, double l,
                                               smatrix const & UB,
                                               Geometry & geometry) const throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_E4C.setFromGeometry(*m_geometry_K6C, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                        mode::eulerian4C::vertical::Delta_Theta::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                        m_geometry_K6C->setFromGeometry(m_geometry_E4C, true);
                        return;
                      }

                    /******************************/
                    /* VERTICAL 4C CONSTANT OMEGA */
                    /******************************/
                    Constant_Omega::Constant_Omega()
#ifndef MSVC6                      
                    : mode::eulerian4C::vertical::Constant_Omega()
#endif                      
                      {
                        set_name("Vertical Eulerian 4C Constant Omega");
#ifdef MSVC6
                        set_description(m_mode.get_description());
                        set_valueList(m_mode.get_valueList());
#endif
                      }

                    Constant_Omega::~Constant_Omega() {}

                    void 
                    Constant_Omega::computeAngles(double h, double k, double l,
                                                  smatrix const & UB,
                                                  Geometry & geometry) const throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_E4C.setFromGeometry(*m_geometry_K6C, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                        mode::eulerian4C::vertical::Constant_Omega::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                        m_geometry_K6C->setFromGeometry(m_geometry_E4C, true);
                        return;
                      }

                    /****************************/
                    /* VERTICAL 4C CONSTANT CHI */
                    /****************************/
                    Constant_Chi::Constant_Chi()
#ifndef MSVC6                      
                    : mode::eulerian4C::vertical::Constant_Chi()
#endif                      
                      {
                        set_name("Vertical Eulerian 4C Constant Chi");
#ifdef MSVC6
                        set_description(m_mode.get_description());
                        set_valueList(m_mode.get_valueList());
#endif
                      }

                    Constant_Chi::~Constant_Chi() {}

                    void 
                    Constant_Chi::computeAngles(double h, double k, double l,
                                                smatrix const & UB,
                                                Geometry & geometry) const throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_E4C.setFromGeometry(*m_geometry_K6C, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                        mode::eulerian4C::vertical::Constant_Chi::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                        m_geometry_K6C->setFromGeometry(m_geometry_E4C, true);
                        return;
                      }

                    /****************************/
                    /* VERTICAL 4C CONSTANT PHI */
                    /****************************/
                    Constant_Phi::Constant_Phi()
#ifndef MSVC6                      
                    : mode::eulerian4C::vertical::Constant_Phi()
#endif                      
                      {
                        set_name("Vertical Eulerian 4C Constant Phi");
#ifdef MSVC6
                        set_description(m_mode.get_description());
                        set_valueList(m_mode.get_valueList());
#endif
                      }

                    Constant_Phi::~Constant_Phi() {}

                    void 
                    Constant_Phi::computeAngles(double h, double k, double l,
                                                smatrix const & UB,
                                                Geometry & geometry) const throw (HKLException)
                      {
                        m_geometry_K6C = static_cast<geometry::Kappa6C *>(&geometry);
                        m_geometry_E4C.setFromGeometry(*m_geometry_K6C, true);
#ifdef MSVC6
                        m_mode.set_valueList(get_valueList());
                        m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                        mode::eulerian4C::vertical::Constant_Phi::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                        m_geometry_K6C->setFromGeometry(m_geometry_E4C, true);
                        return;
                      }

                } // namespace vertical
            } // namespace eulerian4C
        } // namesapce kappa6C
    } // name space mode
} // namespace hkl
