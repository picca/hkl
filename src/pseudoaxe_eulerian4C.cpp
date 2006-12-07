#include "pseudoaxe_eulerian4C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoAxe
    {
    namespace eulerian4C
      {
      namespace vertical
        {

        /*****************/
        /* PSI PSEUDOAXE */
        /*****************/
        Psi::Psi(geometry::eulerian4C::Vertical & geometry) :
            PseudoAxeTemp<geometry::eulerian4C::Vertical>(geometry, "psi", "psi is the angle of rotation around the Q vector."),
            _omega(_geometry.omega()),
            _chi(_geometry.chi()),
            _phi(_geometry.phi()),
            _tth(_geometry.tth())
        {
          _omega->add_observer(this);
          _chi->add_observer(this);
          _phi->add_observer(this);
          _tth->add_observer(this);
          connect();
          update();
        }

        void
        Psi::initialize(void) throw (HKLException)
        {
          svector Q0 = _geometry.getQ();
          double norm2 = Q0.norm2();
          if (norm2 > constant::math::epsilon_0)
            {
              Q0 /= norm2;
              _Q0 = Q0;
              _qpsi0 = _geometry.getSampleQuaternion();
              _initialized = true;
              _writable = true;
              _readable = true;
              update();
            }
          else
            {
              ostringstream reason;
              reason << "Cannot initialize the \"" << get_name() << "\" PseudoAxe when the Q vector is null.";
              HKLEXCEPTION(reason.str(), "Check the wave length.");
            }
        }

        void
        Psi::uninitialize(void)
        {
          _initialized = false;
          _writable = false;
          _readable = false;
        }

        bool
        Psi::isValid(void) throw (HKLException)
        {
          bool valid = false;
          if (_initialized)
            {
              svector Q(_geometry.getQ());
              double norm2 = Q.norm2();
              // check that |Q| is non-null
              if (norm2 > constant::math::epsilon_0)
                {
                  Q /= norm2;
                  if (Q == _Q0)
                    {
                      Quaternion q(_geometry.getSampleQuaternion());
                      q *= _qpsi0.conjugate();

                      svector axe(q.getAxe());
                      //if axe = (0,0,0), we get back to the initial position so return true.
                      if (axe == svector())
                        valid = true;
                      else
                        valid = axe.vectorialProduct(_Q0) == svector();
                    }
                }
            }
          if (valid)
            _writable = true;
          else
            {
              _writable = false;
              HKLEXCEPTION("The current geometry is not compatible with the pseudoAxe initialisation.","Please re-initialize it.");
            }
          return valid;
        }

        void
        Psi::update(void)
        {
          if (_connected)
            {
              double min;
              double max;
              double current;

              _writable = false;
              _readable = false;
              if (_initialized)
                {
                  // update the min max
                  min = -constant::math::pi;
                  max = constant::math::pi;

                  // compute the current position
                  svector Q(_geometry.getQ());
                  double norm2 = Q.norm2();

                  // check that |Q| is non-null
                  if (norm2 > constant::math::epsilon_0)
                    {
                      Quaternion qpsi(_geometry.getSampleQuaternion());
                      qpsi *= _qpsi0.conjugate();

                      Q /= norm2;
                      if (Q == _Q0)
                        {
                          svector axe(qpsi.getAxe());
                          //if axe = (0,0,0), we get back to the initial position so return true.
                          if (axe == svector())
                            {
                              current = 0;
                              _writable = true;
                              _readable = true;
                            }
                          else if (axe.vectorialProduct(_Q0) == svector())
                            {
                              // if the sample rotation is compatible with a rotation around _Q0
                              svector psi_axe(_Q0);
                              // getAngleAndAxe update the axe so need to do a copy of _Q0 before
                              qpsi.getAngleAndAxe(current, psi_axe);
                              _writable = true;
                              _readable = true;
                            }
                        }
                    }
                }
              else
                {
                  min = max = current = 0;
                }
              _range.set(min, current, max);
            }
        }


        void
        Psi::set_current(Value const & value) throw (HKLException)
        {
          if (Psi::isValid())
            {
              Quaternion q(value.get_value(), _Q0);
              q *= _qpsi0;
              smatrix M = q.asMatrix();

              double omega;
              double chi;
              double phi;
              double tth;
              if (fabs (M.get(0, 1)) < constant::math::epsilon_0
                  && fabs (M.get(1, 0)) < constant::math::epsilon_0
                  && fabs (M.get(2, 1)) < constant::math::epsilon_0
                  && fabs (M.get(1, 2)) < constant::math::epsilon_0)
                {
                  omega = _omega->get_current().get_value();
                  if (M.get (1, 1) > 0)
                    {
                      chi = 0;
                      phi = atan2(M.get(2, 0), M.get(0, 0)) - omega;
                    }
                  else
                    {
                      chi = constant::math::pi;
                      phi = omega - atan2(M.get(2, 0), M.get(0, 0));
                    }
                  _chi->set_current(chi);
                  _phi->set_current(phi);
                }
              else
                {
                  //1st solution 0<chi<pi
                  omega = convenience::atan2(-M.get(0, 1), M.get(2, 1));
                  chi = convenience::atan2(sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                  phi = convenience::atan2(-M.get(1, 0), -M.get(1, 2));
                  tth = _geometry._tth->get_current().get_value();
                  geometry::eulerian4C::Vertical g1(omega, chi, phi, tth);

                  //2nd solution -pi<chi<0
                  omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
                  chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                  phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
                  geometry::eulerian4C::Vertical g2(omega, chi, phi, tth);

                  double d1 = _geometry.getDistance(g1);
                  double d2 = _geometry.getDistance(g2);
                  if (d1 < d2)
                    {
                      _omega->set_current(g1._omega->get_current().get_value());
                      _chi->set_current(g1._chi->get_current().get_value());
                      _phi->set_current(g1._phi->get_current().get_value());
                    }
                  else
                    {
                      _omega->set_current(g2._omega->get_current().get_value());
                      _chi->set_current(g2._chi->get_current().get_value());
                      _phi->set_current(g2._phi->get_current().get_value());
                    }
                  // update the read part after connection
                  _tth->set_current(tth);
                }
            }
        }

        ostream &
        Psi::toStream(ostream & flux) const
          {
            PseudoAxeTemp<geometry::eulerian4C::Vertical>::toStream(flux);
            _Q0.toStream(flux);
            _qpsi0.toStream(flux);

            return flux;
          }

        istream &
        Psi::fromStream (istream & flux)
        {
          PseudoAxeTemp<geometry::eulerian4C::Vertical>::fromStream(flux);
          _Q0.fromStream(flux);
          _qpsi0.fromStream(flux);

          return flux;
        }

      } // namespace vertical
    } // namespace eulerian4C
  } // namespace pseudoAxe
} // namespace hkl
