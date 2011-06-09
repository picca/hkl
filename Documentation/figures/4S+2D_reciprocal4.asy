import three;

triple a = (1,0,0);
triple b = (.31,.95,0);
triple c = (.4,.5,0.76);

currentprojection=orthographic(2 * (-0.5*X+Y+.5*Z)+ 3*Z + Y, target=3*Z+Y);
currentlight=White;

size(12cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-3*X)--(X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-Y)--(Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-2*Z)--(2*Z), gray, Arrow3());

// La construction d'Ewald
void ewald(triple ki, real gamma, real delta, real alpha, real phi)
{
  real k = length(ki);
  triple kf =  rotate(gamma, Z) * rotate(-delta, Y) * ki;
  triple Q = kf-ki;
  triple n = rotate(phi, Y) * rotate(alpha, X) * scale3(length(Q)) * Y;
  triple np = planeproject(Q, O) * n;
  triple xp = unit(Q);
  triple zp = cross(xp, -X);
  triple yp = length(np) * unit(cross(xp, zp));
  
  // incomming and outgoing beam
  draw(Label("$\vec{k_i}$", .5, S), shift(-ki)*(O--ki), blue, Arrow3());
  draw(Label("$\vec{k_f}$"), shift(-ki)*(O--kf), green, Arrow3());
  draw(surface(plane(Q, ki)), blue+opacity(.3));
  
  // diffraction vector
  draw(Label("$\vec{Q}$", 1), O--Q, red, Arrow3());
  draw(circle(O, length(np), Q), red+dashed);

  // ref vector and its projection
  draw(Label("$\vec{n}$", 1), O--n, orange, Arrow3());
  draw(O--np, gray, Arrow3());
  draw(n--np, gray+dashed);

  // psi angle and its origin
  draw(O--yp, gray, Arrow3());
  draw(Label("$\psi$"), arc(O, yp, np), red, ArcArrow3());
  draw(surface(O--arc(O, yp, np)--cycle), red);
  
  // Ewalds sphere
  surface Ewalds = shift(-ki) * rotate(90, Y) * scale3(k) * unithemisphere;
  draw(Ewalds, green+opacity(0.3), render(compression=Zero,merge=true));
  dot(Label("C"), -ki);
}

ewald(2*X, -30, 40, 60, -60);
