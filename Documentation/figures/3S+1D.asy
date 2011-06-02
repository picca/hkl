import diffractometer;

size(6cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-10*X)--(10*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-10*Y)--(10*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-10*Z)--(10*Z), gray, Arrow3());

void diffractometer(real tth, real omega, real chi, real phi, real direction)
{
  real hight = 3*10;
  real detector_arm_support_radius= 2*10;
  real detector_arm_length = 3*10;
  real diffractometer_support_radius = 1.5*10;

  // tpp
  draw(tpp(hight, diffractometer_support_radius), gray);
 
  // detector part
  draw(support(hight, detector_arm_support_radius, 0), blue);
  draw(rotate(-tth, Y) * detector_arm(detector_arm_length, detector_arm_support_radius, direction), red);
  
  // sample part
  draw(support(hight, diffractometer_support_radius, 1), blue);
  draw(rotate(-omega, Y) * chi_circle(diffractometer_support_radius, direction), cyan);
  draw(rotate(-omega, Y) * rotate(chi, X) * rotate(-phi, Y) * sample_holder(diffractometer_support_radius, direction), yellow);
}

diffractometer(60, 30, -45, 0, 1);
