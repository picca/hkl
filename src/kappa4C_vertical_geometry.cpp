
#include "kappa4C_vertical_geometry.h"
#include "axe_rotation.h"
#include "twoC_vertical_geometry.h"
#include "eulerian4C_vertical_geometry.h"
#include "eulerian6C_geometry.h"
#include "kappa6C_geometry.h"

namespace hkl {

namespace kappa4C {

namespace vertical {

/**
 * @brief Default constructor
 * @param alpha the alpha angle of the kappa geometry
 */
Geometry::Geometry(double alpha) :
  hkl::geometry::Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", alpha) 
{
  // Bouml preserved body begin 0002AC82
    _source.setDirection(svector(1,0,0));

    // add the sample holder
    hkl::Holder * holder = _holders.add();
    _komega = holder->add_rotation("komega", svector(0., -1., 0.));
    _kappa = holder->add_rotation("kappa", svector(0., -cos(_alpha), -sin(_alpha)));
    _kphi = holder->add_rotation("kphi", svector(0., -1., 0.));

    // add the detector holder
    holder = _holders.add();
    _tth = holder->add_rotation("tth", svector(0., -1., 0.));
  // Bouml preserved body end 0002AC82
}

/**
 * @brief Another constructor.
 * @param alpha the alpha angle of the kappa geometry.
 * @param komega the first angle value.
 * @param kappa the second angle value.
 * @param kphi the third angle value.
 * @param tth the fourth angle value.
 */
Geometry::Geometry(double alpha, double komega, double kappa, double kphi, double tth) :
  hkl::geometry::Kappa("Kappa 4 circles vertical", "The Cristal beamline (synchrotron-soleil) kappa 4 circles diffractometer.", alpha)
{
  // Bouml preserved body begin 0002AD02
    _source.setDirection(svector(1,0,0));

    // add the sample holder
    hkl::Holder * holder = _holders.add();
    _komega = holder->add_rotation("komega", svector(0., -1., 0.));
    _kappa = holder->add_rotation("kappa", svector(0., -cos(_alpha), -sin(_alpha)));
    _kphi = holder->add_rotation("kphi", svector(0., -1., 0.));

    // add the detector holder
    holder = _holders.add();
    _tth = holder->add_rotation("tth", svector(0., -1., 0.));

    this->set_angles(komega, kappa, kphi, tth);
    this->set_angles_consign(komega, kappa, kphi, tth);
  // Bouml preserved body end 0002AD02
}

Geometry::~Geometry() 
{
  // Bouml preserved body begin 00034202
  // Bouml preserved body end 00034202
}

/**
 * @brief Copy Constructor.
 */
Geometry::Geometry(const hkl::kappa4C::vertical::Geometry & geometry) :
  hkl::geometry::Kappa(geometry)
{
  // Bouml preserved body begin 0002AD82
    _komega = static_cast<hkl::axe::Rotation *>(_holders.axes()["komega"]);
    _kappa = static_cast<hkl::axe::Rotation *>(_holders.axes()["kappa"]);
    _kphi = static_cast<hkl::axe::Rotation *>(_holders.axes()["kphi"]);
    _tth = static_cast<hkl::axe::Rotation *>(_holders.axes()["tth"]);
  // Bouml preserved body end 0002AD82
}

/**
 * @brief Get the _komega Axe.
 * @return A pointer on the _komega Axe.
 */
hkl::axe::Rotation * Geometry::komega() 
{
  // Bouml preserved body begin 0002AE82
      return _komega;
  // Bouml preserved body end 0002AE82
}

/**
 * @brief Get the _kappa Axe.
 * @return A pointer on the _kappa Axe.
 */
hkl::axe::Rotation * Geometry::kappa() 
{
  // Bouml preserved body begin 0002AF02
      return _kappa;
  // Bouml preserved body end 0002AF02
}

/**
 * @brief Get the _kphi Axe.
 * @return A pointer on the _kphi Axe.
 */
hkl::axe::Rotation * Geometry::kphi() 
{
  // Bouml preserved body begin 0002AF82
      return _kphi;
  // Bouml preserved body end 0002AF82
}

/**
 * @brief Get the _tth Axe.
 * @return A pointer on the _tth Axe.
 */
hkl::axe::Rotation * Geometry::tth() 
{
  // Bouml preserved body begin 0002B002
      return _tth;
  // Bouml preserved body end 0002B002
}

/**
 * @brief Get the _komega Axe.
 * @return A pointer on the _komega Axe.
 */
const hkl::axe::Rotation * Geometry::komega() const 
{
  // Bouml preserved body begin 0002B082
      return _komega;
  // Bouml preserved body end 0002B082
}

/**
 * @brief Get the _kappa Axe.
 * @return A pointer on the _kappa Axe.
 */
const hkl::axe::Rotation * Geometry::kappa() const 
{
  // Bouml preserved body begin 0002B102
      return _kappa;
  // Bouml preserved body end 0002B102
}

/**
 * @brief Get the _kphi Axe.
 * @return A pointer on the _kphi Axe.
 */
const hkl::axe::Rotation * Geometry::kphi() const 
{
  // Bouml preserved body begin 0002B182
      return _kphi;
  // Bouml preserved body end 0002B182
}

/**
 * @brief Get the _tth Axe.
 * @return A pointer on the _tth Axe.
 */
const hkl::axe::Rotation * Geometry::tth() const 
{
  // Bouml preserved body begin 0002B202
      return _tth;
  // Bouml preserved body end 0002B202
}

/**
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param komega The value of the "komega" Axe.
 * @param kappa The value of the "kappa" Axe.
 * @param kphi The value of the "kphi" Axe.
 * @param tth The value of the "tth" Axe.
 */
void Geometry::set_angles(double komega, double kappa, double kphi, double tth) 
{
  // Bouml preserved body begin 0002B282
      _komega->set_current(komega);
      _kappa->set_current(kappa);
      _kphi->set_current(kphi);
      _tth->set_current(tth);
  // Bouml preserved body end 0002B282
}

/**
 * @brief Set the angles of the eulerian4CD::Vertical geometry. 
 * @param komega The value of the "komega" Axe.
 * @param kappa The value of the "kappa" Axe.
 * @param kphi The value of the "kphi" Axe.
 * @param tth The value of the "tth" Axe.
 */
void Geometry::set_angles_consign(double komega, double kappa, double kphi, double tth) 
{
  // Bouml preserved body begin 00040B82
    _komega->set_consign(komega);
    _kappa->set_consign(kappa);
    _kphi->set_consign(kphi);
    _tth->set_consign(tth);
  // Bouml preserved body end 00040B82
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::twoC::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::twoC::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B302
  // update the source
    _source = geometry.get_source();

    if (strict)
      {
        _kappa->set_current(0);
        _kphi->set_current(0);

        _kappa->set_consign(0);
        _kphi->set_consign(0);
      }
    _komega->set_current(geometry.omega()->get_current());
    _tth->set_current(geometry.tth()->get_current());

    _komega->set_consign(geometry.omega()->get_consign());
    _tth->set_consign(geometry.tth()->get_consign());
  // Bouml preserved body end 0002B302
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian4C::vertical::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian4C::vertical::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B382
    double const & omega = geometry.omega()->get_current().get_value();
    double const & chi = geometry.chi()->get_current().get_value();
    double const & phi = geometry.phi()->get_current().get_value();
    double komega, kappa, kphi;
    //this line can throw an exception so deport the source modification after.
    hkl::kappa4C::vertical::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi);

    double const & omega_c = geometry.omega()->get_consign().get_value();
    double const & chi_c = geometry.chi()->get_consign().get_value();
    double const & phi_c = geometry.phi()->get_consign().get_value();
    double komega_c, kappa_c, kphi_c;
    //this line can throw an exception so deport the source modification after.
    hkl::kappa4C::vertical::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c);

  // update the source
    _source = geometry.get_source();

    _komega->set_current(komega);
    _kappa->set_current(kappa);
    _kphi->set_current(kphi);
    _tth->set_current(geometry.tth()->get_current());

    _komega->set_consign(komega_c);
    _kappa->set_consign(kappa_c);
    _kphi->set_consign(kphi_c);
    _tth->set_consign(geometry.tth()->get_consign());
  // Bouml preserved body end 0002B382
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::eulerian6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::eulerian6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B402
    if (strict)
      {
        if (geometry.mu()->get_current() != 0
            || geometry.gamma()->get_current() != 0)
          {
            HKLEXCEPTION("\"gamma\" and/or \"mu\" current values are wrong",
                         "\"gamma\" = \"mu\" current values must be set to zero");
          }
        else
          {
            if (geometry.mu()->get_consign() != 0
                || geometry.gamma()->get_consign() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" consign values are wrong",
                             "\"gamma\" = \"mu\" consign values must be set to zero");
              }
          }
      }
    double const & omega = geometry.omega()->get_current().get_value();
    double const & chi = geometry.chi()->get_current().get_value();
    double const & phi = geometry.phi()->get_current().get_value();
    double komega, kappa, kphi;
    // the next line can throw an exception so deport the geometry modification once convertion is ok. 
    hkl::kappa4C::vertical::eulerian_to_kappa(omega, chi, phi, _alpha, komega, kappa, kphi);

    double const & omega_c = geometry.omega()->get_consign().get_value();
    double const & chi_c = geometry.chi()->get_consign().get_value();
    double const & phi_c = geometry.phi()->get_consign().get_value();
    double komega_c, kappa_c, kphi_c;
    // the next line can throw an exception so deport the geometry modification once convertion is ok. 
    hkl::kappa4C::vertical::eulerian_to_kappa(omega_c, chi_c, phi_c, _alpha, komega_c, kappa_c, kphi_c);

    // update the source
    _source = geometry.get_source();

    _komega->set_current(komega);
    _kappa->set_current(kappa);
    _kphi->set_current(kphi);
    _tth->set_current(geometry.delta()->get_current());

    _komega->set_consign(komega_c);
    _kappa->set_consign(kappa_c);
    _kphi->set_consign(kphi_c);
    _tth->set_consign(geometry.delta()->get_consign());

  // Bouml preserved body end 0002B402
}

/**
 * @brief Set an eulerian4C::Vertical Geometry from another Geometry.
 * @param geometry The hkl::kappa6C::Geometry.
 * @param strict false or true if we must not care of the strictness of the conversion.
 * @throw HKLException
 */
void Geometry::setFromGeometry(const hkl::kappa6C::Geometry & geometry, bool strict) throw(hkl::HKLException) 
{
  // Bouml preserved body begin 0002B482
    // update the source
    if (strict)
      {
        if (geometry.mu()->get_current() != 0
            || geometry.gamma()->get_current() != 0)
          {
            HKLEXCEPTION("\"gamma\" and/or \"mu\" current values are wrong",
                         "\"gamma\" = \"mu\" current values must be set to zero");
          }
        else
          {
            if (geometry.mu()->get_consign() != 0
                || geometry.gamma()->get_consign() != 0)
              {
                HKLEXCEPTION("\"gamma\" and/or \"mu\" consign values are wrong",
                             "\"gamma\" = \"mu\" consign values must be set to zero");
              }
          }
      }

    // update the source
    _source = geometry.get_source();

    _komega->set_current(geometry.komega()->get_current());
    _kappa->set_current(geometry.kappa()->get_current());
    _kphi->set_current(geometry.kphi()->get_current());
    _tth->set_current(geometry.delta()->get_current());

    _komega->set_consign(geometry.komega()->get_consign());
    _kappa->set_consign(geometry.kappa()->get_consign());
    _kphi->set_consign(geometry.kphi()->get_consign());
    _tth->set_consign(geometry.delta()->get_consign());
  // Bouml preserved body end 0002B482
}


} // namespace hkl::kappa4C::vertical

} // namespace hkl::kappa4C

} // namespace hkl
