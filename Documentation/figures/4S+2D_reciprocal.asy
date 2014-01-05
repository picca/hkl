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
  triple Qyz = Q.y*Y+Q.z*Z;
  triple Qxy = Q.x*X+Q.y*Y;
  triple C = -ki;
  
  // Vecteurs incident et diffracté
  draw(Label("$\vec{k_i}$", .5, S), shift(-ki)*(O--ki), blue, Arrow3());
  draw(Label("$\vec{k_f}$"), shift(-ki)*(O--kf), green, Arrow3());
  
  // Vecteur de diffraction
  draw(Label("$\vec{Q}$", 1), O--Q, red, Arrow3());

  // angle vartheta
  draw(Label("$\theta$"), arc(O, Qyz, Q), red, Arrow3());
  
  // angle vartheta
  draw(Label("$\vartheta$"), arc(O, Y, Qyz), red, Arrow3());
  
  draw(O--Qyz, gray, Arrow3());
  draw(O--Qxy, gray+dashed);
  draw(C--Qxy, gray, Arrow3());
  draw(Label("$2\theta$", align=W), arc(C, C/2, Q), red, Arrow3());
  draw(Label("$\gamma$", align=SW), arc(C, C/2, Qxy), red, Arrow3());
  draw(Label("$\delta$"), arc(C, C+(Qxy-C)/2, Q), red, Arrow3());

  // projections
  draw(Q--Qyz, gray+dashed);
  draw(Q--Qxy, gray+dashed);
  
  // Ewalds sphere
  surface Ewalds = shift(-ki) * rotate(90, Y) * scale3(k) * unithemisphere;
  draw(Ewalds, green+opacity(0.3), render(compression=Zero,merge=true));
  dot(Label("C"), C);
}

ewald(2*X, -30, 40, 60, -60);
