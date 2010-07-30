#include "colors.inc"

#macro _arrow(v0, v1, arrow_color)
  #local arrow_length = vlength(v1 - v0);
  #local v2 = v0 + ((v1 - v0) * .9);
  #local body_diameter = arrow_length / 100;
  #local head_diameter = arrow_length / 30;
  union{
    cylinder{v0, v2, body_diameter}
    cone { v1, 0.0, v2, head_diameter }
    pigment {color arrow_color}
  }
#end 

#macro arrow(v0, arrow_color)
  _arrow(<0, 0, 0>, v0, arrow_color)
#end

#macro _sector(v1, v0, v2, sector_color, factor)
  #local vec = vcross(v2-v0, v1-v0);
  #local n1 = vcross(v1-v0, vec);
  #local n2 = vcross(vec, v2-v0);
  #local l1 = vlength(v1-v0);
  #local l2 = vlength(v2-v0);
  #local l = (l1 < l2 ? l1 : l2);
  difference{
    cylinder {-0.0005*vec, 0.0005*vec, factor*l}
    union{
      plane {n1, 0}
      plane {n2, 0}
    }
    translate v0
    pigment {color sector_color transmit 0.7}
    no_reflection
  }
#end

#macro sector(v1, v2, sector_color, factor)
  _sector(v1, <0, 0, 0>, v2, sector_color, factor)
#end

#macro repere(v1,v2,v3, with_sector)
    union{
        arrow(v1, Blue)
        arrow(v2, Red)
        arrow(v3, Green)
        #if (with_sector)
            sector(v1,v2, Green, 0.3)
            sector(v1,v3, Red, 0.4)
            sector(v2,v3, Blue, 0.5)
        #end
    }
#end
            

#declare X_text=
  text{
    ttf "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSerif.ttf" "X"
    0, 1
    pigment {color Blue}
  };
    
#declare Y_text=
  text{
    ttf "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSerif.ttf" "Y"
    0, 1
    pigment {color Blue}
  };

#declare Z_text=
  text{
    ttf "/usr/share/fonts/truetype/ttf-dejavu/DejaVuSerif.ttf" "Z"
    0, 1
    pigment {color Blue}
  };

#declare X=
  union{
    object{ X_text }
    //object{ X_text rotate 90*y}
    object{ X_text rotate -90*x-90*y}
    no_image
  };
  
#declare Y=
  union{
    object{ Y_text }
    object{ Y_text rotate -90*y}
    //object{ Y_text rotate 90*x+90*y}
    no_image
  };

#declare Z=
  union{
    //object{ Z_text }
    object{ Z_text rotate -90*y}
    object{ Z_text rotate -90*x-90*y}
    no_image
  };
  
#macro repere_orthonormal(with_secteur)
    union{
        arrow(x, Blue)
        arrow(y, Red)
        arrow(z, Green)
        #if (with_secteur)
          secteur(x,y, Green, 0.3)
          secteur(x,z, Red, 0.4)
          secteur(y,z, Blue, 0.5)
        #end
        object{ X scale .2 translate x}
        object{ Y scale .2 translate y}
        object{ Z scale .2 translate z}
    }
#end

#macro arrow_circular(vecteur, rayon, rayon_interieur,angle_debut, angle_manquant, sens, couleur)
  #local axes_de_rotation = vcross(y, vnormalize(vecteur));
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
#end

#declare repere_soleil=
  union{
    _arrow(<0, 0, 0>, -z, Blue)
    _arrow(<0, 0, 0>, x, Green)
    _arrow(<0, 0, 0>, y, Red)
  };
