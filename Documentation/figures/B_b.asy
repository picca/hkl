import three;

size(6cm);

triple b1 = (1, 0, 0);
triple b2 = (.31, .95, 0);
triple b3 = (.4, .5, 0.76);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), O--(2*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), O--(2*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), O--(2*Z), gray, Arrow3());

// plan
draw(plane(2*X, 2*Y));
draw(plane(2*X, 2*Z));
draw(plane(2*Y, 2*Z));

// reciprocal space
draw(Label("$\vec{b_1}$", 1), O--b1, blue, Arrow3());
draw(Label("$\vec{b_2}$", 1), O--b2, red, Arrow3());
draw(Label("$\vec{b_3}$", 1), O--b3, green, Arrow3());

