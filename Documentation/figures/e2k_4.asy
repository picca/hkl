import eulerians;

currentprojection=orthographic(-1,1,1);
currentlight=White;

size(12cm);

// orthonormal coordinates
draw(Label("$\vec{x}$", 1), (-40*X)--(40*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), (-30*Y)--(30*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), (-30*Z)--(30*Z), gray, Arrow3());

eulerians(0, 90, 0, solution=0);
