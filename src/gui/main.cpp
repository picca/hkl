#include <gtkmm/main.h>

//#include "axe.h"
#include "diffractometer_kappa6C.h"
#include "diffractometer_eulerian4C.h"

#include "hklwindow.h"

int main(int argc, char *argv[])
{
    hkl::DiffractometerInterface * diffractometer = new hkl::diffractometer::eulerian4C::Vertical();

    diffractometer->setWaveLength(1.54);
    diffractometer->setCrystalLattice("Crystal",
                                      1, 5, 4,
                                      90 * hkl::constant::math::degToRad,
                                      90 * hkl::constant::math::degToRad,
                                      90 * hkl::constant::math::degToRad);

    hkl::Axe & omega = diffractometer->getAxe("omega");
    hkl::Axe & chi = diffractometer->getAxe("chi");
    hkl::Axe & phi = diffractometer->getAxe("phi");
    hkl::Axe & tth = diffractometer->getAxe("2theta");

    omega.set_value(30 * hkl::constant::math::degToRad);
    chi.set_value(0 * hkl::constant::math::degToRad);
    phi.set_value(90 * hkl::constant::math::degToRad);
    tth.set_value(60 * hkl::constant::math::degToRad);
    diffractometer->addCrystalReflection("Crystal", 1., 0., 0., 1, true);

    omega.set_value(30 * hkl::constant::math::degToRad);
    chi.set_value(90 * hkl::constant::math::degToRad);
    phi.set_value(0 * hkl::constant::math::degToRad);
    tth.set_value(60 * hkl::constant::math::degToRad);
    diffractometer->addCrystalReflection("Crystal", 0., 1., 0., 1, true);

    omega.set_value(30 * hkl::constant::math::degToRad);
    chi.set_value(0 * hkl::constant::math::degToRad);
    phi.set_value(0 * hkl::constant::math::degToRad);
    tth.set_value(60 * hkl::constant::math::degToRad);
    diffractometer->addCrystalReflection("Crystal", 0., 0., 1., 1, true);
    
    omega.set_value(60 * hkl::constant::math::degToRad);
    chi.set_value(60 * hkl::constant::math::degToRad);
    phi.set_value(60 * hkl::constant::math::degToRad);
    tth.set_value(60 * hkl::constant::math::degToRad);
    diffractometer->addCrystalReflection("Crystal", 0.625, 0.75, -0.216506350946, 1, true);
    
    omega.set_value(45 * hkl::constant::math::degToRad);
    chi.set_value(45 * hkl::constant::math::degToRad);
    phi.set_value(45 * hkl::constant::math::degToRad);
    tth.set_value(60 * hkl::constant::math::degToRad);
    diffractometer->addCrystalReflection("Crystal", 0.665975615037, 0.683012701892, 0.299950211252, 1, true);

    diffractometer->addNewCrystal("test");
    diffractometer->setCurrentMode("Bissector");

    Gtk::Main kit(argc, argv);
    HKLWindow window(diffractometer);
    kit.run(window);

    return 0;
}
