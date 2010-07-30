#include "colors.inc"    // The include files contain
#include "textures.inc"  // pre-defined scene elements
#include "glass.inc"
#include "metals.inc"
#include "skies.inc"
#include "shapes.inc"
#include "arrows.inc"
#include "bravais.inc"

// les vecteurs du cristal
#declare O = <0,0,0>;
#declare a = <1,0,0>;
#declare b = <.31,.95,0>;
#declare c = <.4,.5,0.76>;

#declare camera_location = a/2+b/1.5-2*z;
#declare camera_look_at = a/2+b/1.5;


background { color White }

// all setup
camera {
  //fisheye
  orthographic
  location camera_location
  look_at camera_look_at
}

light_source {
  <10, 10, 0> 
  color White
  parallel
  point_at <0, 0, 0>
  shadowless
}
light_source {
  <-10, 10, 10> 
  color White
  parallel
  point_at <0, 0, 0>
  shadowless
}
light_source {
  <-10, 10, -10> 
  color White
  parallel
  point_at <0, 0, 0>
  shadowless
}

plane { <1,0,0>, -.5 pigment {color LightWood} }
plane { <0,1,0>, -.5 pigment {color LightWood*.9} }
plane { <0,0,1>, -.5 pigment {color LightWood*.8} }


//repere orthonorme
// on ne met pas le x pour ne pas etre embêté pour le vecteur a
arrow(y, Gray)
arrow(z, Gray)

cristal_atomes(a, b, c, .05, Blue)
cristal_frame(a, b, c, .005, Gray)

//vecteurs du cristal
arrow(a, Blue)
arrow(b, Red)
arrow(c, Green)

sector(a, b,Green,.2)
sector(b, c,Blue,.3)
sector(c, a,Red,.4)
