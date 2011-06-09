import three;
import solids;

restricted surface unitsolidcylinder = surface(unitcylinder,
					       unitdisk,
					       shift(Z)*unitdisk);

surface torus(real R, real a, real theta1, real theta2)
{
  //return surface(revolution(reverse(Circle(R*X,a,Y,32)),Z,90,345));

  triple f(pair t) {
    return ((R+a*cos(t.y))*cos(t.x),(R+a*cos(t.y))*sin(t.x),a*sin(t.y));
  }

  surface s = surface(f, (radians(theta1),0), (radians(theta2),2pi), 8, 8, Spline);

  return s;
}

surface carrow(real radius, real a,
	       real theta1, real theta2, int direction)
{
  surface s;
  
  s.append(torus(radius, a, theta1, theta2));

  if (direction < 0)
    s.append( rotate(theta1, Z) * shift(radius, 0, 0) * rotate(90., X) * scale(2*a, 2*a, 6*a) * unitsolidcone );
  else
    s.append( rotate(theta2, Z) * shift(radius, 0, 0) * rotate(-90., X) * scale(2*a, 2*a, 6*a) * unitsolidcone );

  return s;
}

surface tpp(real height, real radius)
{
  // put the centre of the top surface of the cube at the origin
  surface s=shift(-0.5, -0.5, -1) * unitcube;
  s = shift(0, 0, -height) * scale(3*radius, 3*radius, radius/5) * s;
  return s;
}

surface _support(real hight, real radius, int arrow1, int arrow2, bool draw_cylinder=false)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius * sqrt(alpha * (2-alpha));

  // becarefulle reference of the unitsolidcylinder and the unitcube are not
  // at the same place, so me must take this into account.
  if(draw_cylinder)
    s.append( shift(0, 0, -hight) * scale(radius, radius, radius/5) * unitsolidcylinder );
  s.append( shift(-dz, -radius+dx, -hight) * scale(2*dz, dx, hight) * unitcube );
  s.append( shift(0, -radius+2*dx, 0) * rotate(90, X) * scale(dz, dz, dx) * unitsolidcylinder );

  if (arrow1 != 0)
    s.append( shift(0, -radius+1.5*dx, 0) * rotate(90, X) * carrow(1.3 * dz, 0.5, 0., 90., arrow1) );

  if(arrow2 != 0)
    s.append( shift(0, 0, -hight+radius/5+.5) * carrow(.8*radius, .5, 0, 90, arrow2) );

  return s;
}

surface support_e4cv(real hight, real detector, real sample)
{
  surface s;
  s.append( _support(hight, detector, 0, 0, true) );
  s.append( _support(hight, sample, 1, 0, false) );

  return s;
}

surface detector_arm(real length, real radius, int arrow1)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius*sqrt(alpha*(2-alpha));

  s.append( shift(-dz, -radius+dx+.1, -dz) * scale(length+dz, dx, 2*dz) * unitcube );
  s.append( shift(length, -radius+dx+.2, -dz) * scale(-dx, radius-dx-.2, 2*dz) * unitcube );
  s.append( shift(length-dx, 0, 0) * rotate(90, Y) * scale(dz, dz, dx) * unitsolidcylinder );

  if(arrow1 != 0)
    s.append( shift(0, -radius+1.5dx,0) * rotate(90, X) * carrow(1.3 * dz, 0.5, 0., 90., arrow1) );

  return s;
}

surface chi_circle(real radius, int arrow1)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius*sqrt(alpha*(2-alpha));

  // create a revolution surface
  triple p1 = (radius-dx-.1)*Y-(dz/2)*X;
  triple p2 = p1 + dz*X;
  triple p3 = p2 - (.1*(radius-dx-.1) * Y);
  triple p4 = p3 - dz*X;
  path3 p= p1--p2--p3--p4--cycle;

  s.append( surface(O, p, X) );

  if(arrow1 != 0)
    s.append( rotate(90, Y) * rotate(-90, Z) * carrow(.7 * (radius-dx-.1), .5, -90, 90, arrow1) );

  return s;
}

surface sample_holder(real radius, int arrow1)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius*sqrt(alpha*(2-alpha));

  s.append( shift(0, -.8*(radius-dx), 0) * rotate(90, X) * scale(dz/2, dz/2, 0.15*(radius-dx)) *unitsolidcylinder );
  s.append( shift(0, -.8*(radius-dx), 0) * rotate(90, X) * scale(dz, dz, dx) *shift(-.5, -.5, -1) * unitcube );

  if(arrow1 != 0)
    s.append( shift(0, -.8*(radius-dx)+dx, 0) * rotate(90, X) * carrow(0.15*(radius-dx), .3, 0, 270, arrow1) );

  return s;
}
