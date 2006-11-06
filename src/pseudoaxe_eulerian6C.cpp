#include "pseudoaxe_eulerian6C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace eulerian6C
      {

      /*****************/
      /* TTH PSEUDOAXE */
      /*****************/
      Tth::Tth(geometry::Eulerian6C & geometry) : PseudoAxe<geometry::Eulerian6C>(geometry)
      {
        set_name("2theta");
        set_description ("2theta = 2 * theta.\n");
        addParameter("direction", 1., "Prefered mode when gamma=0 and delta=0\n  Vertical=1(default).\n  Horizontal=0.");
      }

      Tth::Tth(Tth const & pseudoAxe) :
          PseudoAxe<geometry::Eulerian6C>(pseudoAxe),
          m_axe(pseudoAxe.m_axe)
      {}

      Tth::~Tth(void)
      {}

      ostream &
      Tth::toStream(ostream & flux) const
        {
          PseudoAxe<geometry::Eulerian6C>::toStream(flux);
          m_axe.toStream(flux);

          return flux;
        }

      istream &
      Tth::fromStream(istream & flux)
      {
        PseudoAxe<geometry::Eulerian6C>::fromStream(flux);
        m_axe.fromStream(flux);

        return flux;
      }

      double
      Tth::get_min(void) const
        {
          double min = 0;
          if (m_initialized)
            min = -constant::math::pi;
          return min;
        }

      double
      Tth::get_max(void) const
        {
          double max = 0;
          if (m_initialized)
            max = constant::math::pi;
          return max;
        }

      void
      Tth::initialize(void) throw (HKLException)
      {
        if (m_geometry.isValid())
          {
            PseudoAxe<geometry::Eulerian6C>::initialize();
            svector ki0 = m_geometry0.get_source().getKi();
            svector kf0 = m_geometry0.getKf();
            m_axe = ki0.vectorialProduct(kf0);
            if (m_axe == svector())
              {
                if (getParameterValue("direction") == 1.)
                  m_axe = svector(0, -1, 0);
                else
                  m_axe = svector(0, 0, 1);
              }
            else
              m_axe = m_axe.normalize();
          }
      }

      bool
      Tth::isValid(void) throw (HKLException)
      {
        bool valid = false;
        m_writable = false;
        if (m_geometry.isValid() && m_initialized)
          {
            // compute the sign of value for the initialized pseudoAxe.
            svector ki = m_geometry.get_source().getKi();
            svector kf = m_geometry.getKf();
            svector axe = (ki.vectorialProduct(kf));
            if (axe.norm2() > constant::math::epsilon_0) // we are close to 0, pi or -pi
              {
                axe = axe.normalize();
                // si axe est l'opposé de m_axe change le signe de value.
                if ((fabs(axe[X] + m_axe[X]) < constant::math::epsilon_1)
                    && (fabs(axe[Y] + m_axe[Y]) < constant::math::epsilon_1)
                    && (fabs(axe[Z] + m_axe[Z]) < constant::math::epsilon_1)
                    ||
                    (fabs(axe[X] - m_axe[X]) < constant::math::epsilon_1)
                    && (fabs(axe[Y] - m_axe[Y]) < constant::math::epsilon_1)
                    && (fabs(axe[Z] - m_axe[Z]) < constant::math::epsilon_1))
                  {
                    m_writable = true;
                    valid = true;
                  }
              }
            else
              {
                m_writable = true;
                valid = true;
              }
          }
        return valid;
      }

      double
      Tth::get_value(void) throw (HKLException)
      {
        double gamma = m_geometry.get_axe("gamma").get_value();
        double delta = m_geometry.get_axe("delta").get_value();
        double value = acos(cos(gamma)*cos(delta));
        if (Tth::isValid())
          {
            // compute the sign of value for the initialized pseudoAxe.
            svector ki = m_geometry.get_source().getKi();
            svector kf = m_geometry.getKf();
            svector axe = (ki.vectorialProduct(kf));
            //!< @todo ki = kf  and ki = -kf
            if (axe.norm2() < constant::math::epsilon_0) // we are close to 0, pi or -pi
              value = 0;
            else
              {
                axe = axe.normalize();
                // si axe est l'opposé de m_axe change le signe de value.
                if ((fabs(axe[X] + m_axe[X]) < constant::math::epsilon_1)
                    && (fabs(axe[Y] + m_axe[Y]) < constant::math::epsilon_1)
                    && (fabs(axe[Z] + m_axe[Z]) < constant::math::epsilon_1))
                  {
                    value *= -1;
                  }
              }
          }
        return value;
      }

      void
      Tth::set_value(double const & value) throw (HKLException)
      {
        if (Tth::isValid())
          {
            svector ki = m_geometry.get_source().getKi();
            svector kf = ki.rotatedAroundVector(m_axe, value);

            // 1st solution
            double gamma1 = atan2(kf[Y], kf[X]);
            double delta1 = atan2(kf[Z], sqrt(kf[X]*kf[X]+kf[Y]*kf[Y]));
            geometry::Eulerian6C g1(0, 0, 0, 0, gamma1, delta1);

            // 2nd solution
            double gamma2 = atan2(-kf[Y], -kf[X]);
            double delta2 = atan2(kf[Z], -sqrt(kf[X]*kf[X]+kf[Y]*kf[Y]));
            geometry::Eulerian6C g2(0, 0, 0, 0, gamma2, delta2);

            // keep the closest one.
            if (m_geometry.getDistance(g1) < m_geometry.getDistance(g2))
              {
                m_geometry.get_axe("gamma").set_value(gamma1);
                m_geometry.get_axe("delta").set_value(delta1);
              }
            else
              {
                m_geometry.get_axe("gamma").set_value(gamma2);
                m_geometry.get_axe("delta").set_value(delta2);
              }
          }
      }

      /***************/
      /* Q PSEUDOAXE */
      /***************/
      Q::Q(geometry::Eulerian6C & geometry) :
          PseudoAxe<geometry::Eulerian6C>(geometry)
      {
        m_tth = new pseudoAxe::eulerian6C::Tth(geometry);
        set_name("q");
        set_description ("q = 2 * tau * sin(theta) / lambda");
        set_valueList(m_tth->get_valueList());
      }

      Q::~Q(void)
      {
        delete m_tth;
      }

      void
      Q::initialize(void) throw (HKLException)
      {
        m_tth->set_valueList(get_valueList());
        m_tth->initialize();
        // this code is not executed if m_tth can not be initialize. (throw)
        PseudoAxe<geometry::Eulerian6C>::initialize();
      }

      double
      Q::get_min(void) const
        {
          try
            {
              m_geometry.isValid();
            }
          catch (HKLException &)
            {
              return 0;
            }
          double lambda = m_geometry.get_source().get_waveLength();
          double tth_min = m_tth->get_min();
          double tth_max = m_tth->get_max();

          m_tth->set_valueList(get_valueList());
          return 2 * constant::physic::tau * sin(tth_min) / lambda;
        }

      double
      Q::get_max(void) const
        {
          try
            {
              m_geometry.isValid();
            }
          catch (HKLException &)
            {
              return 0;
            }
          double lambda = m_geometry.get_source().get_waveLength();
          double tth_min = m_tth->get_min();
          double tth_max = m_tth->get_max();

          m_tth->set_valueList(get_valueList());
          return 2 * constant::physic::tau * sin(tth_max) / lambda;
        }

      bool
      Q::isValid(void) throw (HKLException)
      {
        m_tth->set_valueList(get_valueList());
        // check if m_tth is valid
        m_tth->isValid();
        // if ok, verification of the geometry validity.
        try
          {
            m_geometry.isValid();
          }
        catch (HKLException &)
          {
            m_writable = false;
            throw;
          }
        m_writable = true;
        return true;
      }

      double
      Q::get_value(void) throw (HKLException)
      {
        if (Q::isValid())
          {
            m_tth->set_valueList(get_valueList());
            double theta = m_tth->get_value() / 2.;
            double lambda = m_geometry.get_source().get_waveLength();
            return 2 * constant::physic::tau * sin(theta) / lambda;
          }
      }

      void
      Q::set_value(double const & value) throw (HKLException)
      {
        if (Q::isValid())
          {
            double lambda = m_geometry.get_source().get_waveLength();
            double two_theta = 2 * asin(value * lambda / (2 * constant::physic::tau));
            m_tth->set_valueList(get_valueList());
            m_tth->set_value(two_theta);
          }
      }

    } // namespace eulerian6C
  } // namespace pseudoAxe
} // namespace hkl
