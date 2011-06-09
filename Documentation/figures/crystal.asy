import three;

triple a = (1,0,0);
triple b = (.31,.95,0);
triple c = (.4,.5,0.76);

surface atome(triple position, real radius)
{
  return shift(position) * scale3(radius) * unitsphere;
}

surface atomes(triple a, triple b, triple c, real radius)
{
  surface s;
  
  s.append( atome(O, radius) );
  s.append( atome(a, radius) );
  s.append( atome(b, radius) );
  s.append( atome(c, radius) );
  s.append( atome(a+b, radius) );
  s.append( atome(a+c, radius) );
  s.append( atome(b+c, radius) );
  s.append( atome(a+b+c, radius) );

  return s;
}

path3[] frames(triple a, triple b, triple c)
{
  return O--a--a+b--b--O--c--c+a--c+a+b--c+b--c^^
    a--a+c^^
    a+b--a+b+c^^
    b--b+c;
}

currentprojection=orthographic(1, 1, .2);
currentlight=White;

size(6cm);

// orthonormal coordinates without x to not be disturbed for the a vector
draw(Label("$\vec{x}$", 1), O--(2*X), gray, Arrow3());
draw(Label("$\vec{y}$", 1), O--(2*Y), gray, Arrow3());
draw(Label("$\vec{z}$", 1), O--(2*Z), gray, Arrow3());

draw(atomes(a, b, c, .05), blue);
draw(frames(a, b, c), gray);

// real space
draw(Label("$\vec{a}$", 1), O--a, blue, Arrow3());
draw(Label("$\vec{b}$", 1), O--b, red, Arrow3());
draw(Label("$\vec{c}$", 1), O--c, green, Arrow3());
