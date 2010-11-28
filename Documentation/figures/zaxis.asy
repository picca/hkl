import solids;
size(300,0);

real h=3;


// Planes
pen bg=gray(0.9)+opacity(0.5);
// draw(surface((1.2,0,0)--(1.2,0,1.2)--(0,0,1.2)--(0,0,0)--cycle),bg);
// draw(surface((0,1.2,0)--(0,1.2,1.2)--(0,0,1.2)--(0,0,0)--cycle),bg);
// draw(surface((1.2,0,0)--(1.2,1.2,0)--(0,1.2,0)--(0,0,0)--cycle),bg);

real r=1.5;
pen p=rgb(0,0.7,0);
draw(Label("$x$",1),O--r*X,p,Arrow3);
draw(Label("$y$",1),O--r*Y,p,Arrow3);
draw(Label("$z$",1),O--r*Z,p,Arrow3);
label("$\rm O$",(0,0,0),W);

// ki
triple Oewalds = (-1, 0, 0);
triple ki = O;
draw(Label("$ki$", .5), Oewalds--ki, red, Arrow3);

// kf
triple kf = shift(Oewalds)*rotate(-30, Y)*rotate(30, Z)*ki;
draw(Label("$kf$", .5), Oewalds--kf, red, Arrow3);

// Q

// revolution R=cylinder(-h/2*Z,r,h);
// draw(surface(R),lightgreen+opacity(0.5),render(compression=Low));
// draw((0,0,-h/2)--(0,0,h/2),dashed);
// dot((0,0,-h/2));
// dot((0,0,h/2));
// draw("$L$",(0,r,-h/2)--(0,r,h/2),W,black);
// draw("$r$",(0,0,-h/2)--(0,r,-h/2),red);
// draw(arc(O, 1, 90, 90, 90, 0), red, Arrow3);

// Ewalds sphere
draw(shift(Oewalds)*unitsphere, green+opacity(0.3), render(compression=Zero,merge=true));


