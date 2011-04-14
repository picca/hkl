import diffractometer;

size(6cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-10*X)--(10*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-10*Y)--(10*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-10*Z)--(10*Z), gray, Arrow3());

void diffractometer(real gamma, real delta, real mu, real omega, real chi, real phi, real direction)
{
  real hight = 3*10;
  real detector_arm_support_radius= 2*10;
  real detector_arm_length = 3*10;
  real diffractometer_support_radius = 1.5*10;

  // tpp
  draw(tpp(hight, diffractometer_support_radius), gray);

  // detector part
  draw(rotate(gamma, Z) * support(hight, detector_arm_support_radius, 0), blue);
  draw(rotate(gamma, Z) * rotate(-delta, Y) * detector_arm(detector_arm_length, detector_arm_support_radius, direction), red);

  // sample part
  draw(rotate(mu, Z) * support(hight-5, diffractometer_support_radius, 1), green);
  draw(rotate(mu, Z) * rotate(-omega, Y) * chi_circle(diffractometer_support_radius, direction), cyan);
  draw(rotate(mu, Z) * rotate(-omega, Y) * rotate(chi, X) * rotate(-phi, Y) * sample_holder(diffractometer_support_radius, direction), yellow);
}

diffractometer(30, 0, -30, 0, 0, 0, 0);
