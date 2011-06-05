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

surface arrow_circular(real radius, real a,
		       real theta1, real theta2, int direction)
{
  surface s;
  
  s.append(torus(radius, a, theta1, theta2));

  if (direction == 0){
    surface s2 = rotate(theta1, Z) * shift(radius, 0, 0) * rotate(theta1+90, Z) * rotate(-90., Y) * zscale3(2) * unitsolidcone;
    s.append(s2);
  }else{
    surface s2 = rotate(theta2, Z) * shift(radius, 0, 0) * rotate(theta2, Z) * rotate(-90., Y) * zscale3(2) * unitsolidcone;
    s.append(s2);
  }
  /*
  union{
    difference{
      torus{ rayon, rayon_interieur}
      object{Wedge(angle_manquant) rotate angle_debut*y}
    }
    #if (sens > 0)
      cone{vrotate(z*rayon,y*angle_debut), 2*rayon_interieur, vrotate(z*rayon,y*(angle_debut+30)), 0}
    #else
      cone{vrotate(z*rayon,y*(angle_debut+angle_manquant)), 2*rayon_interieur, vrotate(z*rayon,y*(angle_debut+angle_manquant-30)), 0}
    #end
    pigment {color couleur}
    rotate axes_de_rotation*VAngleD(y, vecteur)
  }
  */

  return s;
}

surface tpp(real height, real radius)
{
  // put the centre of the top surface of the cube at the origin
  surface s=shift(-0.5, -0.5, -1) * unitcube;
  s = shift(0, 0, -height) * scale(3*radius, 3*radius, radius/5) * s;
  return s;
}

surface support(real hight, real radius, int direction)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius * sqrt(alpha * (2-alpha));

  // becarefulle reference of the unitsolidcylinder and the unitcube are not
  // at the same place, so me must take this into account.
  surface s1 = shift(0, 0, -hight) * scale(radius, radius, radius/5) * unitsolidcylinder;
  surface s2 = shift(-dz, -radius+dx, -hight) * scale(2*dz, dx, hight) * unitcube;
  surface s3 = shift(0, -radius+2*dx, 0) * rotate(90, X) * scale(dz, dz, dx) * unitsolidcylinder;

  s.append(s1);
  s.append(s2);
  s.append(s3);


  if (direction != 0){
    surface s4 = shift(-radius+dx+.1, 0, 0) * arrow_circular(.8 * dz, 0.5, 0., 270., direction);
    s.append(s4);
  }

  /*
    #if (sens != 0)
      object{
	arrow_circular(x,.8*dz,.05, 0,90,sens,couleur)
	translate (-rayon+dx+.1)*x
      }
    #end
  */
  return s;
}

surface detector_arm(real length, real radius, real direction)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius*sqrt(alpha*(2-alpha));

  surface s1 = shift(-dz, -radius+dx+.1, -dz) * scale(length+dz, dx, 2*dz) * unitcube;
  surface s2 = shift(length, -radius+dx+.2, -dz) * scale(-dx, radius-dx-.2, 2*dz) * unitcube;
  surface s3 = shift(length-dx, 0, 0) * rotate(90, Y) * scale(dz, dz, dx) * unitsolidcylinder;

  s.append(s1);
  s.append(s2);
  s.append(s3);

  /*
  if (direction != 0)
    {
      draw(shift((-radius+dx+.2, 0, 0)) * arc(X, Z, Y), p);
    }
    #if (sens != 0)
      object{
	arrow_circular(x,.9*dz,.05, 0, 90,sens,couleur)
	translate <-rayon+dx+.2,0,0>
      }
    #end
  */
  return s;
}

surface chi_circle(real radius, real direction)
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
  return surface(O, p, X);
  /*
    #if (sens != 0)
      object{
	arrow_circular(z,.7*(rayon-dx-.1),.05, 10, 280,sens,couleur)
      }
    #end
  */
}

surface sample_holder(real radius, real direction)
{
  surface s;
  real alpha = 0.1;
  real dx = alpha * radius;
  real dz = radius*sqrt(alpha*(2-alpha));

  surface s1 = shift(0, -.8*(radius-dx), 0) * rotate(90, X) * scale(dz/2, dz/2, 0.15*(radius-dx)) *unitsolidcylinder;
  surface s2 = shift(-.5, -.5, -1) * unitcube;
  s2 = shift(0, -.8*(radius-dx), 0) * rotate(90, X) * scale(dz, dz, dx) * s2;

  s.append(s1);
  s.append(s2);

    /*
    #if (sens != 0)
      object{
	arrow_circular(x,.3*dz,.05, 0, 90,sens,couleur)
	translate (-.8*(rayon-dx-.1)+.1)*x
      }
    #end
    */
  return s;
}
