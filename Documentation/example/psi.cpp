#include <stdlib.h>
#include <pseudoaxe_eulerian4C.h>

using namespace std;

int main(int argc, char *argv[])
{
  unsigned int i;
  double omega;
  double chi;
  double phi;
  double two_theta;
  
  if (argc < 4)
    {
      omega = 34;
      chi=45;
      phi=77;
      two_theta=-5;
    }
  else 
    {
      omega = atof(argv[1]);
      chi = atof(argv[2]);
      phi = atof(argv[3]);
      two_theta = atof(argv[4]);
    }

  hkl::pseudoAxe::eulerian4C::vertical::Psi psi;
  hkl::geometry::eulerian4C::Vertical geometry;

  geometry.get_axe("omega").set_value(omega * hkl::constant::math::degToRad);  
  geometry.get_axe("chi").set_value(chi * hkl::constant::math::degToRad);
  geometry.get_axe("phi").set_value(phi * hkl::constant::math::degToRad);
  geometry.get_axe("2theta").set_value(two_theta * hkl::constant::math::degToRad); 

  psi.initialize(geometry);
  
  for(i=0;i<360;i++)
    {
      psi.set_value(geometry, i * hkl::constant::math::degToRad);
      cout << i 
	   << " " << geometry.get_axe("omega").get_value() * hkl::constant::math::radToDeg
	   << " " << geometry.get_axe("chi").get_value() * hkl::constant::math::radToDeg
	   << " " << geometry.get_axe("phi").get_value() * hkl::constant::math::radToDeg
	   << " " << geometry.get_axe("2theta").get_value() * hkl::constant::math::radToDeg
	   << endl;
      
    }
}
