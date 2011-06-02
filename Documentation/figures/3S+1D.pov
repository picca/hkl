#include "colors.inc"    // The include files contain
#include "textures.inc"  // pre-defined scene elements
#include "glass.inc"
#include "metals.inc"
#include "skies.inc"
#include "shapes.inc"
#include "arrows.inc"
#include "bravais.inc"

// les vecteurs du cristal
#declare a = <1,0,0>;
#declare b = <.31,.95,0>;
#declare c = <.4,.5,0.76>;

#declare camera_location = 5 * (x+y+z);
#declare camera_look_at = <0,0,0>;

//#declare camera_location = a/2+b/1.5-2*z;
//#declare camera_look_at = a/2+b/1.5;


background { color DarkGreen }

// all setup
camera {
  orthographic
  location camera_location
  look_at camera_look_at
}

light_source {
	<10, 10, 0> 
	color White
        parallel
        point_at <0, 0, 0>
        shadowless
}
light_source {
	<-10, 10, 10> 
	color White
        parallel
        point_at <0, 0, 0>
        shadowless
}
light_source {
	<-10, 10, -10> 
	color White
        parallel
        point_at <0, 0, 0>
        shadowless
}

plane { <0,1,0>, -3 pigment {color LightWood*.9} }
cylinder{<0,0,-10>,<0,0,10>, .01 pigment {color Grey}}
cylinder{<0,-10,0>,<0,10,0>, .01 pigment {color Grey}}
cylinder{<-10,0,0>,<10,0,0>, .01 pigment {color Grey}}

#macro support(hauteur, rayon, sens, couleur)
  #local Alpha = .1;
  #local dx=Alpha*rayon;
  #local dz=rayon*sqrt(Alpha*(2-Alpha));
  union{
    union{
      cylinder{<0,-hauteur,0>,<0,-hauteur+.5,0>, rayon}
      box{<-rayon+dx,-hauteur,-dz>,<-rayon+dx+.1,0,+dz>}
      cylinder{<-rayon+dx,0,0>, <-rayon+dx+.1, 0, 0>, dz}
      pigment{color couleur}
    }
    #if (sens != 0)
      object{
	arrow_circular(x,.8*dz,.05, 0,90,sens,couleur)
	translate (-rayon+dx+.1)*x
      }
    #end
   }
#end

#macro bras_detecteur(longueur,rayon, sens, couleur)
  #local Alpha = .1;
  #local dx=Alpha*rayon;
  #local dz=rayon*sqrt(Alpha*(2-Alpha));
  union{
    box{<-rayon+dx+.1,-dz,dz>,<-rayon+dx+.2,dz,-longueur>}
    box{<-rayon+dx+.2,-dz,-longueur>,<0,dz,-longueur+.1>}
    cylinder{<0,0,-longueur>,<0,0,-longueur+.1>,dz}
    #if (sens != 0)
      object{
	arrow_circular(x,.9*dz,.05, 0, 90,sens,couleur)
	translate <-rayon+dx+.2,0,0>
      }
    #end
    pigment{color couleur}
  }
#end

#macro cercle_chi(rayon, sens, couleur)
  #local Alpha = .1;
  #local dx = Alpha * rayon;
  #local dz = rayon * sqrt(Alpha*(2-Alpha));
  union{
    difference{
      cylinder{<0,0,-dz/2.>, <0,0,dz/2.>, rayon-dx-.1}
      cylinder{<0,0,-10.>, <0,0,10.>, .9*(rayon-dx-.1)}
      pigment{color Cyan}
    }
    #if (sens != 0)
      object{
	arrow_circular(z,.7*(rayon-dx-.1),.05, 10, 280,sens,couleur)
      }
    #end
  }
#end

#macro porte_echantillon(rayon, sens, couleur)
  #local Alpha = .1;
  #local dx = Alpha * rayon;
  #local dz = rayon * sqrt(Alpha*(2-Alpha));
  union{
    cylinder{<-.95*(rayon-dx-.1), 0, 0>, <-.8*(rayon-dx-.1), 0,0>, dz/2.}
    box{<-.8*(rayon-dx-.1), -dz/2., -dz/2.>, <-.8*(rayon-dx-.1)+.1,dz/2., dz/2.>}
    #if (sens != 0)
      object{
	arrow_circular(x,.3*dz,.05, 0, 90,sens,couleur)
	translate (-.8*(rayon-dx-.1)+.1)*x
      }
    #end
    pigment{color couleur}
  }
#end 
  
#macro diffractometre(Two_Theta, Omega, Chi, Phi, sens)
  #local hauteur = 3;
  #local rayon_support_bras_detecteur = 2;
  #local longueur_bras_detecteur = 3;
  #local rayon_support_diffractometre = 1.5;

  // Axes de rotation du detecteur.
  #local vtho_theta = x;
  
  // Axes de rotations du diffractometre.
  #local vomega = x;
  #local vchi = vaxis_rotate(z, vomega, Omega);
  #local vphi = vaxis_rotate(vaxis_rotate(x, vomega, Omega), vchi, Chi);

 
  //Detecteur
  support(hauteur,rayon_support_bras_detecteur, 0, Blue)
  object{bras_detecteur(longueur_bras_detecteur,rayon_support_bras_detecteur, 1*sens, Red) rotate Two_Theta*x}
  
  //Diffractometre
  support(hauteur,rayon_support_diffractometre, 1, Blue)
  object{cercle_chi(rayon_support_diffractometre, 1*sens, Cyan) rotate Omega*x}
  object{porte_echantillon(rayon_support_diffractometre, 1*sens, Yellow) rotate Phi*x rotate Chi*z rotate Omega*x}

  // repère SOLEIL-CRISTAL
  union{
    arrow(-2*z, Blue)
    arrow(2*x, Green)
    arrow(2*y, Red)
    translate 7*z
  }
#end

diffractometre(0,0,0,0, 1)
