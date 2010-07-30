#include "arrows.inc"

// perspective (default) camera
camera {
  location  <3.0, 2.0, 5.0>
  look_at   <0.0, 0.0,  0.0>
  right     x*image_width/image_height
}

// general light definition
light_source {
  <10, 10, 10>      // position of the light source
  color rgb 1.0     // color of the light
  // spotlight
  // cylinder
   parallel
  // area_light <AXIS1>, <AXIS2>, SIZE1, SIZE2
  // (---for spotlight/cylinder---)
  // radius FLOAT
  // falloff FLOAT
  // tightness FLOAT
   point_at <0, 0, -1>   // for spotlight/cylinder/parallel
  // (---for area_light---)
  // adaptive FLOAT
  // jitter FLOAT
  // circular
  // orient
  // (---other modifiers---)
  // looks_like { OBJECT }
  // fade_distance FLOAT
  // fade_power FLOAT
  // media_attenuation BOOL
  // media_interaction BOOL
  // shadowless
}

#declare o = <1, 1, 1>;
#declare a = -z;
#declare b = x;
#declare c = y;

object { repere(a, b, c, 1) }
object { repere(a, b, c, 1) translate x+y}