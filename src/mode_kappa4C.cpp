#include "mode_kappa4C.h"

namespace hkl {
  namespace mode {

    Kappa4C::Kappa4C(void)
      {}

    Kappa4C::~Kappa4C(void)
      {}

    namespace kappa4C {

        Bissector::Bissector(void) :
          mode::eulerian4C::Bissector()
        {
          set_name("Eulerian 4C Bissector");
        }

        Bissector::~Bissector(void)
          {}

        void 
        Bissector::computeAngles(double h, double k, double l,
                                 smatrix const & UB,
                                 Geometry & geometry) const throw (HKLException)
        {
          geometry::Kappa4C * K4C = (geometry::Kappa4C *)(&geometry);
          m_geometry_E4C.setFromK4C(*K4C);
#ifdef VCPP6
          ((mode::eulerian4C::Bissector *)this)->computeAngles(h, k, l, UB, m_geometry_E4C);
#else
          mode::eulerian4C::Bissector::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
          K4C->setFromE4C(m_geometry_E4C);
          return;
        }

        /*****************************/
        /* HORIZONTAL 4C DELTA THETA */
        /*****************************/
        Delta_Theta::Delta_Theta() :
          mode::eulerian4C::Delta_Theta()
        {
          set_name("Eulerian 4C Delta Theta");
        }

        Delta_Theta::~Delta_Theta() {}

        void 
        Delta_Theta::computeAngles(double h, double k, double l,
                                   smatrix const & UB,
                                   Geometry & geometry) const throw (HKLException)
          {
            geometry::Kappa4C * K4C = (geometry::Kappa4C *)(&geometry);
            m_geometry_E4C.setFromK4C(*K4C);
#ifdef VCPP6
            ((mode::eulerian4C::Delta_Theta *)this)->computeAngles(h, k, l, UB, m_geometry_E4C);
#else
            mode::eulerian4C::Delta_Theta::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
            K4C->setFromE4C(m_geometry_E4C);
          }

        /********************************/
        /* HORIZONTAL 4C CONSTANT OMEGA */
        /********************************/
        Constant_Omega::Constant_Omega() :
          mode::eulerian4C::Constant_Omega()
        {
          set_name("Eulerian 4C Constant Omega");
        }

        Constant_Omega::~Constant_Omega() {}

        void 
        Constant_Omega::computeAngles(double h, double k, double l,
                                      smatrix const & UB,
                                      Geometry & geometry) const throw (HKLException)
          {
            geometry::Kappa4C * K4C = (geometry::Kappa4C *)(&geometry);
            m_geometry_E4C.setFromK4C(*K4C);
#ifdef VCPP6
            ((mode::eulerian4C::Constant_Omega *)this)->computeAngles(h, k, l, UB, m_geometry_E4C);
#else
            mode::eulerian4C::Constant_Omega::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
            K4C->setFromE4C(m_geometry_E4C);
          }

        /******************************/
        /* HORIZONTAL 4C CONSTANT CHI */
        /******************************/
        Constant_Chi::Constant_Chi() :
          mode::eulerian4C::Constant_Chi()
        {
          set_name("Eulerian 4C Constant Chi");
        }

        Constant_Chi::~Constant_Chi() {}

        void 
        Constant_Chi::computeAngles(double h, double k, double l,
                                    smatrix const & UB,
                                    Geometry & geometry) const throw (HKLException)
          {
            geometry::Kappa4C * K4C = (geometry::Kappa4C *)(&geometry);
            m_geometry_E4C.setFromK4C(*K4C);
#ifdef VCPP6
            ((mode::eulerian4C::Constant_Chi *)this)->computeAngles(h, k, l, UB, m_geometry_E4C);
#else
            mode::eulerian4C::Constant_Chi::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
            K4C->setFromE4C(m_geometry_E4C);
          }

        /******************************/
        /* HORIZONTAL 4C CONSTANT PHI */
        /******************************/
        Constant_Phi::Constant_Phi() :
          mode::eulerian4C::Constant_Phi()
        {
          set_name("Eulerian 4C Constant Phi");
        }

        Constant_Phi::~Constant_Phi() {}

        void 
        Constant_Phi::computeAngles(double h, double k, double l,
                                    smatrix const & UB,
                                    Geometry & geometry) const throw (HKLException)
          {
            geometry::Kappa4C * K4C = (geometry::Kappa4C *)(&geometry);
            m_geometry_E4C.setFromK4C(*K4C);
#ifdef VCPP6
            ((mode::eulerian4C::Constant_Phi *)this)->computeAngles(h, k, l, UB, m_geometry_E4C);
#else
            mode::eulerian4C::Constant_Phi::computeAngles(h, k, l, UB, m_geometry_E4C);
#endif
            K4C->setFromE4C(m_geometry_E4C);
          }

    } // namespace kappa4C
  } // name space mode
} // namespace hkl
