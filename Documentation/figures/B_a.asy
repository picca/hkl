import three;

size(6cm);

triple b1 = (1, 0, 0);
triple b2 = (.31, .95, 0);
triple b3 = (.4, .5, 0.76);

triple a1 = cross(b2, b3);
triple a2 = cross(b3, b1);
triple a3 = cross(b1, b2);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), O--(2*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), O--(2*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), O--(2*Z), gray, Arrow3());

// reciprocal space
draw(Label("$\vec{b_1}$", 1), O--b1, blue, Arrow3());
draw(Label("$\vec{b_2}$", 1), O--b2, red, Arrow3());
draw(Label("$\vec{b_3}$", 1), O--b3, green, Arrow3());

// real space
draw(Label("$\vec{a_1}$", 1), O--a1, cyan, Arrow3());
draw(Label("$\vec{a_2}$", 1), O--a2, magenta, Arrow3());
draw(Label("$\vec{a_3}$", 1), O--a3, yellow, Arrow3());
