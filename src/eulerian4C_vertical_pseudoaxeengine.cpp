#include "config.h"
#include "eulerian4C_vertical_pseudoaxeengine.h"
#include "axe_rotation.h"
#include "parameter.h"
#include "pseudoaxe.h"
#include "samplelist.h"
#include "convenience.h"

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
                                          0 * HKL_DEGTORAD, 1e-2 * HKL_DEGTORAD, 180 * HKL_DEGTORAD);
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
          hkl_svector Q0; // a temporary Q vector

          _geometry.get_Q(&Q0);
          // check if the vector is compatible with an initialization.
          if (::hkl_svector_normalize(&Q0))
            {
              _Q0 = Q0;
              // compute the initial sample quaternion
              _geometry.get_sample_quaternion(&_qpsi0);
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
                  double min = -M_PI;
                  double max = M_PI;
                  double current;
                  double consign;
                  hkl_svector Q;
                  hkl_quaternion qpsi;
                  bool shit;

                  // compute the current position
                  _geometry.get_Q(&Q);
                  _geometry.get_sample_quaternion(&qpsi);
                  this->compute_psi(&Q, &qpsi, current, shit, shit);

                  // compute the consign position
                  _geometry.get_Q_consign(&Q);
                  _geometry.get_sample_quaternion_consign(&qpsi);
                  this->compute_psi(&Q, &qpsi, consign, _readable, _writable);

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
          hkl_quaternion q;
          hkl_smatrix M;
          double omega;
          double chi;
          double phi;
          double tth;

          ::hkl_quaternion_from_angle_and_axe(&q, _psi->get_consign().get_value(), &_Q0);
          ::hkl_quaternion_times_quaternion(&q, &_qpsi0);
          ::hkl_quaternion_to_smatrix(&q, &M);

          if (fabs (M.data[0][1]) < HKL_EPSILON
              && fabs (M.data[1][0]) < HKL_EPSILON
              && fabs (M.data[2][1]) < HKL_EPSILON
              && fabs (M.data[1][2]) < HKL_EPSILON) // chi = 0
            {
              omega = _omega->get_consign().get_value();
              if (M.data[1][1] > 0)
                {
                  chi = 0;
                  phi = atan2(M.data[2][0], M.data[0][0]) - omega;
                }
              else
                {
                  chi = M_PI;
                  phi = omega - atan2(M.data[2][0], M.data[0][0]);
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
              omega = convenience::atan2(-M.data[0][1], M.data[2][1]);
              chi = convenience::atan2(sqrt(M.data[0][1] * M.data[0][1] + M.data[2][1] * M.data[2][1]), M.data[1][1]);
              phi = convenience::atan2(-M.data[1][0], -M.data[1][2]);
              tth = _geometry.tth()->get_consign().get_value();
              hkl::eulerian4C::vertical::Geometry g1(omega, chi, phi, tth);

              //2nd solution -pi<chi<0
              omega = convenience::atan2(M.data[0][1], -M.data[2][1]);
              chi = convenience::atan2(-sqrt(M.data[0][1] * M.data[0][1] + M.data[2][1] * M.data[2][1]), M.data[1][1]);
              phi = convenience::atan2(M.data[1][0], M.data[1][2]);
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
         * @brief Compute the value of the Psi pseudoAxe for a given Q and qpsi.
         * @param Q The Q vector (modified by the method do not forget to do a copy before using it if needed)
         * @param q The qpsi Quaternion (modified by the method do not forget to do a copy before using it if needed.).
         * @param value The psi value.
         * @param readable The readability of the pseudoAxe.
         * @param writable The writability of the pseudoAxe.
         */
        void Psi::compute_psi(hkl_svector * Q, hkl_quaternion * q, double & value, bool & readable, bool & writable)
        {
          readable = false;
          writable = false;
          
          if (::hkl_svector_normalize(Q))
            {
              double desorientation;
              double desorientation_max;

              desorientation_max = _desorientation->get_current().get_value();
              desorientation = ::hkl_svector_angle(Q, &_Q0);

              if (desorientation < desorientation_max)
                {
                  hkl_svector axe;
                  hkl_quaternion tmp;

                  // q *= conjugate(_qpsi0)
                  tmp = _qpsi0;
                  ::hkl_quaternion_conjugate(&tmp);
                  ::hkl_quaternion_times_quaternion(q, &tmp);

                  // compute the axe of rotation of the sample quaternion.
                  ::hkl_quaternion_to_angle_and_axe(q, &value, &axe);

                  //if value = 0, we get back to the initial position so return true.
                  if (value == 0)
                    {
                      readable = true;
                      writable = true;
                    }
                  else
                    {
                      // angle return a positiv number between 0 and pi
                      // so compute the desorientation between the quaternion axe
                      // and the Q vector.
                      desorientation = ::hkl_svector_angle(&axe, &_Q0);
                      if (desorientation < desorientation_max)
                        {
                          readable = true;
                          writable = true;
                        }
                      else if( fabs(desorientation - M_PI) < desorientation_max)
                        {
                          readable = true;
                          writable = true;
                          value *= -1;
                        }
                    }
                }
            }
        }


      } // namespace hkl::eulerian4C::vertical::pseudoAxeEngine

    } // namespace hkl::eulerian4C::vertical

  } // namespace hkl::eulerian4C

} // namespace hkl
