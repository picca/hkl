import diffractometer;

currentprojection=orthographic(-1,1,1);
currentlight=White;

size(12cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-40*X)--(40*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-30*Y)--(30*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-30*Z)--(30*Z), gray, Arrow3());

void diffractometer(real gamma, real delta,
		    real mu, real omega, real chi, real phi)
{
  real hight = 3*10;
  real detector_arm_support_radius= 2*10;
  real detector_arm_length = 3*10;
  real diffractometer_support_radius = 1.5*10;

  // tpp
  draw(tpp(hight, diffractometer_support_radius), gray);

  // detector part
  draw(rotate(gamma, Z) * _support(hight, detector_arm_support_radius, 1, 1, true), blue);
  draw(rotate(gamma, Z) * rotate(-delta, Y) * detector_arm(detector_arm_length, detector_arm_support_radius, 1), red);

  // sample part
  draw(rotate(mu, Z) * _support(hight-5, diffractometer_support_radius, 1, 1, true), green);
  draw(rotate(mu, Z) * rotate(-omega, Y) * chi_circle(diffractometer_support_radius, 1), cyan);
  draw(rotate(mu, Z) * rotate(-omega, Y) * rotate(chi, X) * rotate(-phi, Y) * sample_holder(diffractometer_support_radius, 1), yellow);
}

diffractometer(0, 0, 0, 0, 0, 0);
