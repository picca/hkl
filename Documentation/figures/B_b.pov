#include "colors.inc"    // The include files contain
#include "textures.inc"  // pre-defined scene elements
#include "glass.inc"
#include "metals.inc"
#include "skies.inc"
#include "shapes.inc"
#include "arrows.inc"
#include "bravais.inc"


#declare camera_location = 2 * (x+y+0.5*z);
#declare camera_look_at = <0,0,0>;

background { color Cyan }

// all setup
camera {
  location camera_location
  look_at camera_look_at
}

light_source {
	<10, 0, 0> 
	color White
        parallel
        point_at <0, 0, 0>
}
light_source {
	<0, 10, 0> 
	color White
        parallel
        point_at <0, 0, 0>
}
light_source {
	<0, 0, 10> 
	color White
        parallel
        point_at <0, 0, 0>
}

plane { <1,0,0>, -.5 pigment {color LightWood} }
plane { <0,1,0>, -.5 pigment {color LightWood*.9} }
plane { <0,0,1>, -.5 pigment {color LightWood*.8} }

#declare v1 = <1,0,0>;
#declare v2 = <.31,.95,0>;
#declare v3 = <.4,.5,0.76>;

object{repere_orthonormal(false) }
repere(v1, v2, v3, true)

#macro diffractometre4C()
    Axes
    plan_faisceau
    plan_chi
#end

//diffractometre4C()
