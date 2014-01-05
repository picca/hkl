import solids;

size(300,0);

real k = 2;
real delta = 30;
real gamma = 10;
real mu = 10;

// points
triple C = unit((-1, 0, 0)) * k;
triple O = O;
triple Q = rotate(-delta, Y) * rotate(gamma, Z) * (-C) + C;

// vectors
triple ki = -C;
triple kf = Q - C;

// Planes
pen bg = gray(0.9) + opacity(0.5);

real r = 1.5;
pen p = rgb(0,0.7,0);
draw( Label("$x$",1), O--r*X, p, Arrow3(HookHead3));
draw( Label("$y$",1), O--r*Y, p, Arrow3(HookHead3));
draw( Label("$z$",1), O--r*Z, p, Arrow3(HookHead3));
label("$O$", (0,0,0), W);

// draw the ki vector
path3 ki_p = shift(C) * (O--ki);
//draw(Label("$\vec{k_i}$", .5), ki_p, blue, Arrow3);

// kf
path3 kf_p = shift(C) * (O--kf);
//draw(Label("$\vec{k_f}$", .5), kf_p, blue, Arrow3);

// Q
path3 Q_p = O--Q;
draw(Label("$\vec{Q}$", 1), Q_p, red, Arrow3);

// draw the dots
dot("$C$", C, p);

// rotation plan define by the rotation axis
triple n = rotate(mu, Z) * Y;
draw( Label("$\vec{n}$", 1), O--n, blue, Arrow3(HookHead3));

// project Q on n -> Qper
triple Qper = planeproject(n, Q) * O;
draw( Label("$\vec{Q_{per}}$", 1), O--Qper, blue, Arrow3(HookHead3));
draw(Q--Qper, dashed+red);

// compute Qpar
triple Qpar = Q - Qper;
draw( Label("$\vec{Q_{par}}$", 1), O--Qpar, blue, Arrow3(HookHead3));
draw(Q--Qpar, dashed+red);

// n and x0z plane
path3 plan = plane(Qpar.z * Z, (Qpar-(Qpar.z * Z)));
path3 plan_n = scale(4, 4, 1) * scale3(2) * shift(-Qpar / 2.) * plan;
path3 plan_xOz = scale3(2) * shift(-(C+Qpar.z * Z)/ 2.) * plane(Qpar.z * Z, C);

draw(surface(plan_n), blue+opacity(0.1));
draw(plan_n, blue);
draw(surface(plan_xOz), green+opacity(0.1));
draw(plan_xOz, green);
