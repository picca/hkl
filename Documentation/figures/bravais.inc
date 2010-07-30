#macro atome(position, taille, couleur)
  sphere{position, taille
    pigment{color couleur}
  }
#end

#macro cristal_atomes(a, b, c, taille, couleur)
  union{
    atome(0, taille, couleur)
    atome(a, taille, couleur)
    atome(b, taille, couleur)
    atome(c, taille, couleur)
    atome(a+b, taille, couleur)
    atome(a+c, taille, couleur)
    atome(b+c, taille, couleur)
    atome(a+b+c, taille, couleur)
  }
#end

#macro cristal_frame(a,b,c,taille, couleur)
  union{
    cylinder{0,a, taille pigment {color couleur}}
    cylinder{0,b, taille pigment {color couleur}}
    cylinder{0,c, taille pigment {color couleur}}
    cylinder{a,a+b, taille pigment {color couleur}}
    cylinder{a,a+c, taille pigment {color couleur}}
    cylinder{b,b+a, taille pigment {color couleur}}
    cylinder{b,b+c, taille pigment {color couleur}}
    cylinder{a+b,a+b+c, taille pigment {color couleur}}
    cylinder{c,c+a, taille pigment {color couleur}}
    cylinder{c,c+b, taille pigment {color couleur}}
    cylinder{a+c,a+b+c, taille pigment {color couleur}}
    cylinder{b+c,a+b+c, taille pigment {color couleur}}
  }
#end