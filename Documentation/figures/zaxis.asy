import solids;

size(300,0);

real k = 2;
real delta = 30;
real gamma = 10;

// points
triple C = unit((-1, 0, 0)) * k;
triple O = O;
triple P = rotate(-delta, Y) * rotate(gamma, Z) * (-C) + C;

// vectors
triple ki = -C;
triple kf = P - C;


// Planes
pen bg = gray(0.9) + opacity(0.5);
// draw(surface((1.2,0,0)--(1.2,0,1.2)--(0,0,1.2)--(0,0,0)--cycle),bg);
// draw(surface((0,1.2,0)--(0,1.2,1.2)--(0,0,1.2)--(0,0,0)--cycle),bg);
// draw(surface((1.2,0,0)--(1.2,1.2,0)--(0,1.2,0)--(0,0,0)--cycle),bg);

real r = 1.5;
pen p = rgb(0,0.7,0);
draw( Label("$x$",1), O--r*X, p, Arrow3(HookHead3));
draw( Label("$y$",1), O--r*Y, p, Arrow3(HookHead3));
draw( Label("$z$",1), O--r*Z, p, Arrow3(HookHead3));
label("$O$", (0,0,0), W);

// draw the ki vector
path3 ki_p = shift(C) * (O--ki);
draw(Label("$ki$", .5), ki_p, red, Arrow3);

// kf
path3 kf_p = shift(C) * (O--kf);
draw(Label("$kf$", .5), kf_p, red, Arrow3);

// Q
path3 Q_p = O--P;
draw(Label("$\vec{Q}$", .5), Q_p, red, Arrow3);

// draw the dots
dot("C", C, p);
dot("P", P, p);

// plane
path3 P_p = plane(X, Z, P);
draw(surface(P_p), blue+opacity(0.5));

// Ewalds sphere
draw(shift(C) * scale3(k) * unitsphere, green+opacity(0.3), render(compression=Zero,merge=true));

// revolution R=cylinder(-h/2*Z,r,h);
// draw(surface(R),lightgreen+opacity(0.5),render(compression=Low));
// draw((0,0,-h/2)--(0,0,h/2),dashed);
// dot((0,0,-h/2));
// dot((0,0,h/2));
// draw("$L$",(0,r,-h/2)--(0,r,h/2),W,black);
// draw("$r$",(0,0,-h/2)--(0,r,-h/2),red);
// draw(arc(O, 1, 90, 90, 90, 0), red, Arrow3);


