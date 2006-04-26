#include "mode_kappa4C.h"

namespace hkl {
    namespace mode {
        namespace kappa4C {

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
                  set_name("Eulerian 4C Bissector");
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
                    geometry::kappa4C::Vertical * K4C = (geometry::kappa4C::Vertical *)(&geometry);
                    m_geometry_E4C.setFromGeometry(*K4C);
#ifdef MSVC6
                    m_mode.set_valueList(get_valueList());
                    m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                    mode::eulerian4C::vertical::Bissector::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                    K4C->setFromGeometry(m_geometry_E4C);
                    return;
                  }

                /***************************/
                /* VERTICAL 4C DELTA THETA */
                /***************************/
                Delta_Theta::Delta_Theta(void)
#ifndef MSVC6
                  : mode::eulerian4C::vertical::Delta_Theta()
#endif
                {
                  set_name("Eulerian 4C Delta Theta");
#ifdef MSVC6
                  set_description(m_mode.get_description());
                  set_valueList(m_mode.get_valueList());
#endif
                }

                Delta_Theta::~Delta_Theta(void)
                  {}

                void 
                Delta_Theta::computeAngles(double h, double k, double l,
                                           smatrix const & UB,
                                           Geometry & geometry) const throw (HKLException)
                  {
                    geometry::kappa4C::Vertical * K4C = (geometry::kappa4C::Vertical *)(&geometry);
                    m_geometry_E4C.setFromGeometry(*K4C);
#ifdef MSVC6
                    m_mode.set_valueList(get_valueList());
                    m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                    mode::eulerian4C::vertical::Delta_Theta::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                    K4C->setFromGeometry(m_geometry_E4C);
                  }

                /******************************/
                /* VERTICAL 4C CONSTANT OMEGA */
                /******************************/
                Constant_Omega::Constant_Omega(void)
#ifndef MSVC6
                  : mode::eulerian4C::vertical::Constant_Omega()
#endif
                {
                  set_name("Eulerian 4C Constant Omega");
#ifdef MSVC6
                  set_description(m_mode.get_description());
                  set_valueList(m_mode.get_valueList());
#endif
                }

                Constant_Omega::~Constant_Omega(void)
                  {}

                void 
                Constant_Omega::computeAngles(double h, double k, double l,
                                              smatrix const & UB,
                                              Geometry & geometry) const throw (HKLException)
                  {
                    geometry::kappa4C::Vertical * K4C = (geometry::kappa4C::Vertical *)(&geometry);
                    m_geometry_E4C.setFromGeometry(*K4C);
#ifdef MSVC6
                    m_mode.set_valueList(get_valueList());
                    m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                    mode::eulerian4C::vertical::Constant_Omega::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                    K4C->setFromGeometry(m_geometry_E4C);
                  }

                /****************************/
                /* VERTICAL 4C CONSTANT CHI */
                /****************************/
                Constant_Chi::Constant_Chi(void)
#ifndef MSVC6
                  : mode::eulerian4C::vertical::Constant_Chi()
#endif
                {
                  set_name("Eulerian 4C Constant Chi");
#ifdef MSVC6
                  set_description(m_mode.get_description());
                  set_valueList(m_mode.get_valueList());
#endif
                }

                Constant_Chi::~Constant_Chi(void)
                  {}

                void 
                Constant_Chi::computeAngles(double h, double k, double l,
                                            smatrix const & UB,
                                            Geometry & geometry) const throw (HKLException)
                  {
                    geometry::kappa4C::Vertical * K4C = (geometry::kappa4C::Vertical *)(&geometry);
                    m_geometry_E4C.setFromGeometry(*K4C);
#ifdef MSVC6
                    m_mode.set_valueList(get_valueList());
                    m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                    mode::eulerian4C::vertical::Constant_Chi::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                    K4C->setFromGeometry(m_geometry_E4C);
                  }

                /****************************/
                /* VERTICAL 4C CONSTANT PHI */
                /****************************/
                Constant_Phi::Constant_Phi(void)
#ifndef MSVC6
                  : mode::eulerian4C::vertical::Constant_Phi()
#endif
                {
                  set_name("Eulerian 4C Constant Phi");
#ifdef MSVC6
                  set_description(m_mode.get_description());
                  set_valueList(m_mode.get_valueList());
#endif
                }

                Constant_Phi::~Constant_Phi(void)
                  {}

                void 
                Constant_Phi::computeAngles(double h, double k, double l,
                                            smatrix const & UB,
                                            Geometry & geometry) const throw (HKLException)
                  {
                    geometry::kappa4C::Vertical * K4C = (geometry::kappa4C::Vertical *)(&geometry);
                    m_geometry_E4C.setFromGeometry(*K4C);
#ifdef MSVC6
                    m_mode.set_valueList(get_valueList());
                    m_mode.computeAngles(h, k, l, UB, m_geometry_E4C);
#else
                    mode::eulerian4C::vertical::Constant_Phi::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
                    K4C->setFromGeometry(m_geometry_E4C);
                  }

            } // namespace vertical
        } // namespace kappa4C
    } // name space mode
} // namespace hkl
