
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "axe_rotation.h"
#include "parameter.h"
#include "pseudoaxe.h"
#include "samplelist.h"

namespace hkl
  {

  namespace eulerian4C
    {

    namespace vertical
      {

      namespace pseudoAxeEngine
        {

        Psi::Psi(hkl::eulerian4C::vertical::Geometry & geometry, hkl::SampleList *& samples)  :
            PseudoAxeEngineWithSampleTemp<hkl::eulerian4C::vertical::Geometry>(geometry, samples, false, false, false),
            _omega(geometry.omega()),
            _chi(geometry.chi()),
            _phi(geometry.phi()),
            _tth(geometry.tth())
        {
          // Add the parameter
          _desorientation = new Parameter("Desorientation", "The maximum desorientation allowed before the pseudoAxe inactivation.",
                                          0 * constant::math::degToRad, 1e-2 * constant::math::degToRad, 180 * constant::math::degToRad);
          _parameters.add(_desorientation);

          // add all the PseudoAxes
          _psi = new PseudoAxe("psi", "angle of rotation around the Q vector", this);
          _pseudoAxes.push_back(_psi);

          // add observer to observable
          _omega->add_observer(this);
          _chi->add_observer(this);
          _phi->add_observer(this);
          _tth->add_observer(this);

          //fill the relatedAxes
          _relatedAxes.push_back(_omega);
          _relatedAxes.push_back(_chi);
          _relatedAxes.push_back(_phi);
          _relatedAxes.push_back(_tth);

          this->connect();
          Psi::update();
        }

        Psi::~Psi()
        {
          delete _desorientation;
          delete _psi;
        }

        /**
         * @brief Initialize the pseudoAxe.
         *
         * This method must be call before using a pseudoAxe.
         */
        void Psi::initialize() throw(hkl::HKLException)
        {
          svector Q0 = _geometry.get_Q();
          double norm2 = Q0.norm2();
          if (norm2 > constant::math::epsilon)
            {
              Q0 /= norm2;
              _Q0 = Q0;
              _qpsi0 = _geometry.get_sample_quaternion();
              _initialized = true;
              Psi::update();
            }
          else
            {
              std::ostringstream reason;
              reason << "Cannot initialize the \"" << get_name() << "\" PseudoAxe when the Q vector is null.";
              HKLEXCEPTION(reason.str(), "Check the wave length.");
            }
        }

        /**
         * @brief Un-Initialize the pseudoAxe.
         * This method must be call to un-initialize a pseudoAxe.
         */
        void Psi::uninitialize()
        {
          _initialized = false;
          _writable = false;
          _readable = false;
        }

        void Psi::update()
        {
          if (_connected)
            {
              if (_initialized)
                {
                  double min = -constant::math::pi;
                  double max = constant::math::pi;
                  double current;
                  double consign;

                  // compute the current position
                  hkl::svector Q(_geometry.get_Q());
                  hkl::Quaternion qpsi(_geometry.get_sample_quaternion());
                  bool shit;
                  this->compute_psi(Q, qpsi, current, shit, shit);

                  // compute the consign position
                  Q = _geometry.get_Q_consign();
                  qpsi = _geometry.get_sample_quaternion_consign();
                  this->compute_psi(Q, qpsi, consign, _readable, _writable);

                  this->set_pseudoAxe(_psi, min, current, consign, max);
                }
            }
        }

        /**
         * @brief set the current value of the PseudoAxe.
         * @throw HKLException if the pseudoAxe is not ready to be set.
         */
        void Psi::set() throw(hkl::HKLException)
        {
          Quaternion q(_psi->get_consign().get_value(), _Q0);
          q *= _qpsi0;
          smatrix M = q.asMatrix();

          double omega;
          double chi;
          double phi;
          double tth;
          if (fabs (M.get(0, 1)) < constant::math::epsilon
              && fabs (M.get(1, 0)) < constant::math::epsilon
              && fabs (M.get(2, 1)) < constant::math::epsilon
              && fabs (M.get(1, 2)) < constant::math::epsilon) // chi = 0
            {
              omega = _omega->get_consign().get_value();
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
              this->unconnect();
              _chi->set_consign(chi);
              _phi->set_consign(phi);
              this->connect();
              Psi::update();
            }
          else  // chi != 0
            {
              //1st solution 0<chi<pi
              omega = convenience::atan2(-M.get(0, 1), M.get(2, 1));
              chi = convenience::atan2(sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
              phi = convenience::atan2(-M.get(1, 0), -M.get(1, 2));
              tth = _geometry.tth()->get_consign().get_value();
              hkl::eulerian4C::vertical::Geometry g1(omega, chi, phi, tth);

              //2nd solution -pi<chi<0
              omega = convenience::atan2(M.get(0, 1), -M.get(2, 1));
              chi = convenience::atan2(-sqrt(M.get(0, 1) * M.get(0, 1) + M.get(2, 1) * M.get(2, 1)), M.get(1, 1));
              phi = convenience::atan2(M.get(1, 0), M.get(1, 2));
              hkl::eulerian4C::vertical::Geometry g2(omega, chi, phi, tth);

              double d1 = _geometry.get_distance_consign(g1);
              double d2 = _geometry.get_distance_consign(g2);
              this->unconnect();
              if (d1 < d2)
                {
                  _omega->set_consign(g1.omega()->get_consign().get_value());
                  _chi->set_consign(g1.chi()->get_consign().get_value());
                  _phi->set_consign(g1.phi()->get_consign().get_value());
                  _tth->set_consign(tth);
                }
              else
                {
                  _omega->set_consign(g2.omega()->get_consign().get_value());
                  _chi->set_consign(g2.chi()->get_consign().get_value());
                  _phi->set_consign(g2.phi()->get_consign().get_value());
                  _tth->set_consign(tth);
                }
              this->connect();
              Psi::update();
            }
        }

        /**
         * @brief print on a stream the content of the Psi
         * @param flux the ostream to modify.
         * @return the modified ostream
         */
        std::ostream & Psi::toStream(std::ostream & flux) const
          {
            PseudoAxeEngineTemp<hkl::eulerian4C::vertical::Geometry>::toStream(flux);
            _Q0.toStream(flux);
            _qpsi0.toStream(flux);
            _desorientation->toStream(flux);

            return flux;
          }

        /**
         * @brief restore the content of the Psi from an istream
         * @param flux the istream.
         * @return the modified istream.
         * @todo problem of security here.
         */
        std::istream & Psi::fromStream(std::istream & flux)
        {
          PseudoAxeEngineTemp<hkl::eulerian4C::vertical::Geometry>::fromStream(flux);
          _Q0.fromStream(flux);
          _qpsi0.fromStream(flux);
          _desorientation->fromStream(flux);

          return flux;
        }

        /**
         * @brief Compute the value of the Psi pseudoAxe for a given Q and qpsi.
         * @param Q The Q vector (modified by the method do not forget to do a copy before using it if needed)
         * @param q The qpsi Quaternion (modified by the method do not forget to do a copy before using it if needed.).
         * @param value The psi value.
         * @param readable The readability of the pseudoAxe.
         * @param writable The writability of the pseudoAxe.
         */
        void Psi::compute_psi(hkl::svector & Q, hkl::Quaternion & q, double & value, bool & readable, bool & writable)
        {
          readable = false;
          writable = false;
          double norm2 = Q.norm2();

          // check that |Q| is non-null
          if (norm2 > constant::math::epsilon)
            {
              q *= _qpsi0.conjugate();

              // check that the desorientation of the Q vector (detector angles) did not changed.
              Q /= norm2;
              double const & desorientation_max = _desorientation->get_current().get_value();
              if (Q.angle(_Q0) < desorientation_max)
                {
                  // compute the axe of rotation of the sample quaternion.
                  svector axe(q.getAxe());
                  //if axe = (0,0,0), we get back to the initial position so return true.
                  if (axe == svector())
                    {
                      value = 0;
                      readable = true;
                      writable = true;
                    }
                  else
                    {
                      // angle return a positiv number between 0 and pi
                      double desorientation = axe.angle(_Q0);
                      if (desorientation < desorientation_max
                          || fabs(desorientation - constant::math::pi) < desorientation_max)
                        {
                          // if the sample rotation is compatible with a rotation around _Q0
                          svector psi_axe(_Q0);
                          // getAngleAndAxe update the axe so need to do a copy of _Q0 before
                          q.getAngleAndAxe(value, psi_axe);
                          readable = true;
                          writable = true;
                        }
                    }
                }
            }
        }


      } // namespace hkl::eulerian4C::vertical::pseudoAxeEngine

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
