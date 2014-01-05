import three;

currentprojection=orthographic(
camera=(3.01553867584536,7.69355163375629,1.55824928263524),
up=(-0.000558993492954675,-0.00231647705177596,0.012518906910417),
target=(2.93710368526323e-16,1.12323345069498e-15,1.74149973955484e-17),
zoom=1);

size(12cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-3*X)--(X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-Y)--(Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-Z)--(2*Z), gray, Arrow3());

surface slits(real size)
{
  surface s;
  surface slits_in = rotate(90, Y) * yscale3(.3) * scale3(size) * unitplane;
  surface slits_out = shift(2 / 3. * size * Y) * slits_in;

  s.append(slits_in);
  s.append(slits_out);
  s = shift(.5 * Z - size / 2. * Y) * s;

  return s;
}

// La construction d'Ewald
void main(triple ki, real gamma, real delta, real eta)
{
  real k = length(ki);
  transform3 M = rotate(gamma, Z) * rotate(-delta, Y) * rotate(eta, X);
  triple kf = M * ki;
  triple Q = kf-ki;

  // incomming and outgoing beam
  draw(Label("$\vec{k_i}$", .5, S), shift(-ki)*(O--ki), Arrow3());
  draw(Label("$\vec{k_f}$"), (O--kf), dashed, Arrow3());

  // project kf on xOy
  triple Pkf_xy = planeproject(Z, O) * kf;
  draw(Pkf_xy--O, grey+dashed);
  draw(Label("$\gamma$"), arc(O, length(Pkf_xy)*X, Pkf_xy), dashed, Arrow3());
  draw(Label("$\delta$"), arc(O, Pkf_xy, kf), dashed, Arrow3());
  
  // draw the slits and the surface of rotation
  triple n_slits = shift(kf) * M * Z;
  path3 Cs = circle(kf, 1, kf);
  draw(shift(kf) * M * slits(1), red);
  draw(Label("$\vec{n_{slits}}$", 1), kf--n_slits, red, Arrow3());
  draw(Cs, red+dashed);
  //draw(surface(Cs), yellow+dashed);
  
  // draw the n and n' surface
  path3 Cn = circle(kf, 1, Y);
  draw(Label("$\vec{n}$"), O--Y, blue, Arrow3());
  draw(Label("$\vec{n'}$"), shift(kf) * (O--Y), blue, Arrow3());
  draw(Y--(Y+kf), dashed);
  draw(Cn, blue);
  draw(surface(Cn), blue+dashed+opacity(.2));

  // draw the ange between eta_p and n
  draw(arc(kf, kf+Y, M * (ki + Z)));

  // draw the expected slits orientation
  triple [] positions;
  positions = intersectionpoints(Cs, Cn);
  for(triple pos : positions){
    dot(pos);
  }

  // angle eta_a
  draw(Label("$\eta_a$"), arc(kf, n_slits, positions[0]), red);
}

//slits(2*X, -30, 40);
main(2*X, 45, 45, -35);
main(2*X, 45, 45, 0);
