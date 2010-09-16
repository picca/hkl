#include "colors.inc"    // The include files contain
#include "textures.inc"  // pre-defined scene elements
#include "glass.inc"
#include "metals.inc"
#include "skies.inc"
#include "shapes.inc"
#include "arrows.inc"
#include "bravais.inc"

//#declare camera_location = 2 * (x+y+0.5*z);
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
        shadowless
}
light_source {
	<0, 10, 0> 
	color White
        parallel
        point_at <0, 0, 0>
        shadowless
}
light_source {
	<0, 0, 10> 
	color White
        parallel
        point_at <0, 0, 0>
        shadowless
}

plane { <1,0,0>, -.5 pigment {color LightWood} }
plane { <0,1,0>, -.5 pigment {color LightWood*.9} }
plane { <0,0,1>, -.5 pigment {color LightWood*.8} }

#declare b1 = <1,0,0>;
#declare b2 = <.31,.95,0>;
#declare b3 = <.4,.5,0.76>;

#declare a1 = vcross(b2,b3);
#declare a2 = vcross(b3,b1);
#declare a3 = vcross(b1,b2);

//repere orthonorme
arrow(y,Gray)
arrow(z,Gray)

// espace reciproque
arrow(b1,Blue)
arrow(b2,Red)
arrow(b3,Green)

//espce reel
arrow(a1, Cyan)
arrow(a2, Magenta)
arrow(a3, Yellow)


plane { b1, 0 pigment {color Blue transmit .7}}
plane { b2, 0 pigment {color Red transmit .4}}
plane { b3, 0 pigment {color Green transmit .7}}
