import diffractometer;

void eulerians(real omega, real chi, real phi, real kalpha=50, int solution=1)
{
  real hight = 3*10;
  real detector_arm_support_radius= 2*10;
  real detector_arm_length = 3*10;
  real diffractometer_support_radius = 1.5*10;
  triple KAPPA = -cos(radians(kalpha))*Y - sin(radians(kalpha))*Z;
  real komega;
  real kappa;
  real kphi;
  real p = degrees(asin(tan(radians(chi/2.))/tan(radians(kalpha))));

  if (solution == 1){
    komega = omega - p + 90;
    kappa = degrees(2 * asin(sin(radians(chi/2.))/sin(radians(kalpha))));
    kphi = phi - p - 90;
  }else{
    komega = omega + p - 90;
    kappa = degrees(-2 * asin(sin(radians(chi/2.))/sin(radians(kalpha))));
    kphi = phi + p + 90;
  }
  
  // support
  draw(_support(hight, diffractometer_support_radius, 0, 0, true), blue);

  // eulerian part
  draw(rotate(-omega, Y) * chi_circle(diffractometer_support_radius, 1), cyan+opacity(.8));
  //draw(rotate(-omega, Y) * rotate(chi, X) * rotate(-phi, Y) * sample_holder(diffractometer_support_radius, 1), yellow);

  // kappa part
  draw(rotate(-komega, Y) * komega_circle(diffractometer_support_radius, 1, kalpha=kalpha), green);
  draw(rotate(-komega, Y) * rotate(kappa, KAPPA) * kappa_circle(diffractometer_support_radius, 1, kalpha=kalpha), cyan);
  draw(rotate(-komega, Y) * rotate(kappa, KAPPA) * rotate(-kphi, Y) * sample_holder(diffractometer_support_radius, 1), yellow);
}
