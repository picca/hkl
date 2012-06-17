import diffractometer;

currentprojection=orthographic(-1,1,1);
currentlight=White;

size(12cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-40*X)--(40*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-30*Y)--(30*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-30*Z)--(30*Z), gray, Arrow3());

void diffractometer(real tth, real komega, real kappa, real kphi, real kalpha=50)
{
  real hight = 3*10;
  real detector_arm_support_radius= 2*10;
  real detector_arm_length = 3*10;
  real diffractometer_support_radius = 1.5*10;
  triple KAPPA = -cos(radians(kalpha))*Y - sin(radians(kalpha))*Z;

  // tpp
  draw(tpp(hight, diffractometer_support_radius), gray);

  // support
  draw(support_k4cv(hight, detector_arm_support_radius, diffractometer_support_radius), gray);

  // detector part
  draw(rotate(-tth, Y) * detector_arm(detector_arm_length, detector_arm_support_radius, 1), red);

  // sample part
  draw(rotate(-komega, Y) * komega_circle(diffractometer_support_radius, 1, kalpha=kalpha), magenta);
  draw(rotate(-komega, Y) * rotate(kappa, KAPPA) * kappa_circle(diffractometer_support_radius, 1, kalpha=kalpha), cyan);
  draw(rotate(-komega, Y) * rotate(kappa, KAPPA) * rotate(-kphi, Y) * sample_holder(diffractometer_support_radius, 1), yellow);
}

diffractometer(0, 0, 0, 0, kalpha=50);
