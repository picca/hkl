#include "mode.h"

namespace hkl {

    Mode::Mode(void) {}

    Mode::~Mode(void) {}

    ostream &
    Mode::printToStream(ostream & flux) const
      { 
        flux << "Mode: " << get_name() << std::endl;
        return flux;
      }

    namespace mode {
        Dummy::Dummy(void) : Mode()
        {
          set_name("Dummy");
          set_description("This mode do nothing. Just for testing purpose");
        }

        Dummy::~Dummy(void)
          {}

        void
        Dummy::computeAngles(double h, double k, double l,
                             smatrix const & UB,
                             Geometry & geometry) const
          {}
    }
} // namespace hkl

ostream &
operator <<(ostream & flux, hkl::Mode const & mode)
{ 
    return mode.printToStream(flux);
}
