#include "pseudoaxeengine_eulerian4C.h"
#include "convenience.h"

namespace hkl
  {
  namespace pseudoAxeEngine
    {
    namespace eulerian4C
      {
      namespace vertical
        {

        /*****************/
        /* PSI PSEUDOAXE */
        /*****************/
        Psi::Psi(geometry::eulerian4C::Vertical & geometry) :
            PseudoAxeEngineTemp<geometry::eulerian4C::Vertical>(geometry, false, false, false),
            _omega(_geometry.omega()),
            _chi(_geometry.chi()),
            _phi(_geometry.phi()),
            _tth(_geometry.tth())
        {
          // set the range
          double min = -constant::math::pi;
          double max = constant::math::pi;
          _psi_r.set_range(min, max);
          _psi_w.set_range(min, max);

          // add all the PseudoAxes
          _psi = new PseudoAxe( "psi", "psi is the angle of rotation around the Q vector", _psi_r, _psi_w, this);
          _pseudoAxes.push_back(_psi);

          // add observer to observable
          _omega->add_observer(this);
          _chi->add_observer(this);
          _phi->add_observer(this);
          _tth->add_observer(this);
          connect();
          Psi::update();

          // update the write part from the read part for the first time.
          _psi_w.set_current(_psi_r.get_current());
        }

        Psi::~Psi(void)
        {
          delete _psi;
        }

        void
        Psi::initialize(void) throw (HKLException)
        {
          svector Q0 = _geometry.getQ();
          double norm2 = Q0.norm2();
          if (norm2 > constant::math::epsilon)
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
              if (norm2 > constant::math::epsilon)
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
                  if (norm2 > constant::math::epsilon)
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
              _psi_r.set(min, current, max);
            }
        }


        void
        Psi::set(void) throw (HKLException)
          {
            if (Psi::isValid())
              {
                Quaternion q(_psi_w.get_current().get_value(), _Q0);
                q *= _qpsi0;
                smatrix M = q.asMatrix();

                double omega;
                double chi;
                double phi;
                double tth;
                if (fabs (M.get(0, 1)) < constant::math::epsilon
                    && fabs (M.get(1, 0)) < constant::math::epsilon
                    && fabs (M.get(2, 1)) < constant::math::epsilon
                    && fabs (M.get(1, 2)) < constant::math::epsilon)
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
                    unconnect();
                    _chi->set_current(chi);
                    _phi->set_current(phi);
                    connect();
                    update();
                  }
                else
                  {
                    //1st solution 0<chi<pi
                    omega = convenience::atan2(-M.get(0, 1), M.get(2, 1));
                    chi = convenience::atan2(sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                    phi = convenience::atan2(-M.get(1, 0), -M.get(1, 2));
                    tth = _geometry.tth()->get_current().get_value();
                    geometry::eulerian4C::Vertical g1(omega, chi, phi, tth);

                    //2nd solution -pi<chi<0
                    omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
                    chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
                    phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
                    geometry::eulerian4C::Vertical g2(omega, chi, phi, tth);

                    double d1 = _geometry.getDistance(g1);
                    double d2 = _geometry.getDistance(g2);
                    unconnect();
                    if (d1 < d2)
                      {
                        _omega->set_current(g1.omega()->get_current().get_value());
                        _chi->set_current(g1.chi()->get_current().get_value());
                        _phi->set_current(g1.phi()->get_current().get_value());
                        _tth->set_current(tth);
                      }
                    else
                      {
                        _omega->set_current(g2.omega()->get_current().get_value());
                        _chi->set_current(g2.chi()->get_current().get_value());
                        _phi->set_current(g2.phi()->get_current().get_value());
                        _tth->set_current(tth);
                      }
                    connect();
                    update();
                    // update the read part after connection
                  }
              }
          }

        ostream &
        Psi::toStream(ostream & flux) const
          {
            PseudoAxeEngineTemp<geometry::eulerian4C::Vertical>::toStream(flux);
            _Q0.toStream(flux);
            _qpsi0.toStream(flux);
            _psi_r.toStream(flux);
            _psi_w.toStream(flux);

            return flux;
          }

        istream &
        Psi::fromStream (istream & flux)
        {
          PseudoAxeEngineTemp<geometry::eulerian4C::Vertical>::fromStream(flux);
          _Q0.fromStream(flux);
          _qpsi0.fromStream(flux);
          _psi_r.fromStream(flux);
          _psi_w.fromStream(flux);

          return flux;
        }

      } // namespace vertical
    } // namespace eulerian4C
  } // namespace pseudoAxe
} // namespace hkl
