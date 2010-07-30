#include "arrows.inc"

background { color White }

// perspective (default) camera
camera {
  location <2.0, 2.0, -2.0>
  look_at <0.0, 0.0,  0.0>
  //up z
  //right x*image_width/image_height
}


// general light definition
light_source {
  <10, 10, 10>      // position of the light source
  color rgb 1.0     // color of the light
  parallel
  point_at <0, 0, -1>   // for spotlight/cylinder/parallel
}

#declare o = <1, 1, 1>;
#declare a = -x;
#declare b = -z;
#declare c = y;

//object { repere(x, y, z, 1) }
object { repere(a, b, c, 0) }
//object { repere(a, b, c, 1) translate x+y}