.. _introduction:

Introduction
############

The purpose of the library is to factories diffraction angles computation for
different kind of diffractometers geometries. It is used at the SOLEIL, Desy
and Alba synchrotron with the Tango control system to pilot diffractometers.

Features
********

+ mode computation (aka PseudoAxis)

  + item for different diffractometer geometries.

+ UB matrix computation.

  + busing & Levy with 2 reflections
  + simplex computation with more than 2 reflections using the GSL library.
  + Eulerians angles to pre-orientate your sample.

+ Crystal lattice affinement

  + with more than 2 reflections you can select which parameter must be fitted.

+ Pseudoaxes

  + psi, eulerians, q, ...

Conventions
***********

In all this document the next convention will be used to describe the diffractometers
geometries.

+ right handed convention for all the angles.
+ direct space orthogonal base.
+ description of the diffractometer geometries is done with all axes values set to zero.


La diffraction
##############

Le cristal
**********

Un cristal périodique est l'association d'un réseau et d'un motif
placé en chaque noeud du réseau. Un réseau est un ensemble de points,
appelé noeuds du réseau, dont les positions sont données par:

.. math::
   R_{uvw}=u\cdot\vec{a}+v\cdot\vec{b}+w\cdot\vec{c}

:math:`\vec{a}`, :math:`\vec{b}`, :math:`\vec{c}` sont des vecteurs
formant une base de l'espace et ``u``, ``v``, ``w`` sont des
entiers. Le motif est l'ensemble des atomes associés à chaque noeud du
réseau. L'objet de la diffractométrie est d'étudier la diffraction de
cet ensemble réseau + motif. On peut aisément définir un repère
associé à la maille du cristal, il s'agit du repère cristallin ou
repère directe (fig.~\ref{cap:Le-rep=0000E8re-cristallin.}).

.. figure:: ../../figures/crystal.png
   :align: center
   :width: 8cm

   Le repère cristallin.

Ce repère est défini par les vecteurs :math:`\vec{a}`,
:math:`\vec{b}`, :math:`\vec{c}` ainsi que par les angles
:math:`\alpha`, :math:`\beta`, :math:`\gamma` entre eux.  Dans le cas
général, il n'est pas orthonormé.

Cependant pour des raisons liées à la cristallographie il existe un
repère aux propriétés plus intéressantes: le repère réciproque que
l'on définit par la transformée de Fourier du repère direct. Ses
vecteurs de base sont:

.. math::
   :nowrap:
   :label: vecteurs reciproque

   \begin{eqnarray*}
	\vec{a}^{\star} & = & \tau\frac{\vec{b}\wedge\vec{c}}{\vec{a}\cdot(\vec{b}\wedge\vec{c})}\\
	\vec{b}^{\star} & = & \tau\frac{\vec{c}\wedge\vec{a}}{\vec{b}\cdot(\vec{c}\wedge\vec{a})}\\
	\vec{c}^{\star} & = & \tau\frac{\vec{a}\wedge\vec{b}}{\vec{c}\cdot(\vec{a}\wedge\vec{b})} 
   \end{eqnarray*}

:math:`\tau=2\pi` ou :math:`\tau=1` suivant les conventions.

On en déduit les relations d'orthogonalité suivantes:

.. math::
   :nowrap:

   \begin{eqnarray*}
	\vec{a}^{\star}\cdot\vec{a}=\tau & \vec{b}^{\star}\cdot\vec{a}=0    & \vec{c}^{\star}\cdot\vec{a}=0\\
	\vec{a}^{\star}\cdot\vec{b}=0    & \vec{b}^{\star}\cdot\vec{b}=\tau & \vec{c}^{\star}\cdot\vec{b}=0\\
	\vec{a}^{\star}\cdot\vec{c}=0    & \vec{b}^{\star}\cdot\vec{c}=0    & \vec{c}^{\star}\cdot\vec{c}=\tau 
   \end{eqnarray*}

Le repère réciproque permet d'exprimer simplement les relation entre
faisceau incident et faisceau diffracté suite à une expérience de
diffractométrie. Dans la pratique, pour décrire un cristal on possède
souvent uniquement la norme des vecteurs :math:`\vec{a}`,
:math:`\vec{b}`, :math:`\vec{c}` ainsi que les angles :math:`\alpha`,
:math:`\beta`, :math:`\gamma`. En utilisant les équations
:eq:`reciprocal` on obtient les mêmes information dans l'espace
réciproque. Ainsi:

.. math::
   :label: reciprocal
   :nowrap:

   \begin{eqnarray*}
	a^{\star} & = & \frac{\sin\alpha}{aD}\\
	b^{\star} & = & \frac{\sin\beta}{bD}\\
	c^{\star} & = & \frac{\sin\gamma}{cD}
   \end{eqnarray*}

où

.. math::
   D=\sqrt{1-\cos^{2}\alpha-\cos^{2}\beta-\cos^{2}\gamma+2\cos\alpha\cos\beta\cos\gamma}

pour calculer les angles entre les vecteurs de l'espace réciproque, on
utilise encore une fois les équation~\ref{eq:vecteurs reciproque} mais
cette fois-ci pour calculer les sinus et cosinus des angles
:math:`\alpha^\star`, :math:`\beta^\star` et :math:`\gamma^\star`:

.. math::
   :nowrap:

   \begin{eqnarray*}
	\cos\alpha^{\star}=\frac{\cos\beta\cos\gamma-\cos\alpha}{\sin\beta\sin\gamma} & \, & \sin\alpha^{\star}=\frac{D}{\sin\beta\sin\gamma} \\
	\cos\beta^{\star}=\frac{\cos\gamma\cos\alpha-\cos\beta}{\sin\gamma\sin\alpha} & \, & \sin\beta^{\star}=\frac{D}{\sin\gamma\sin\alpha}\\
	\cos\gamma^{\star}=\frac{\cos\alpha\cos\beta-\cos\gamma}{\sin\alpha\sin\beta} & \, & \sin\gamma^{\star}=\frac{D}{\sin\alpha\sin\beta}
   \end{eqnarray*}


La Diffraction
**************

soit un faisceau de rayon X dont le vecteur d'onde est
:math:`\vec{k_{i}}`, :math:`|k_{i}|=\tau/\lambda` où :math:`\lambda`
est la longueur d'onde du signal.  Soit :math:`\vec{k_{d}}` le vecteur
d'onde du faisceau diffracté. On a diffraction si le vecteur diffusion
:math:`\vec{q}` peut s'exprimer comme suit:

.. math::
   \vec{q}=\vec{k_{d}}-\vec{k_{i}}=h.\vec{a}^{*}+k.\vec{b}^{*}+l.\vec{c}^{*}

où :math:`(h,k,l)\in\mathbb{N}^{3}` et :math:`(h,k,l)\neq(0,0,0)`. Ces
indices :math:`(h,k,l)` sont appelé indices de Miller.

Une autre façon de voir les choses à été donné par Bragg et ça fameuse
relation:

.. math::
   n\lambda=2d\sin\theta

où :math:`d` est la distance inter-réticulaire et
:math:`n \in \mathbb{N}`. La diffraction à lieu pour un angle
:math:`\theta` unique. On a alors :math:`\vec{q}` perpendiculaire au
plan de diffraction.

La construction d'Ewald permet de se représenter facilement la
condition de diffraction à l'aide du réseau réciproque.


Les quaternions
***************

Propriétés
==========

Nous allons utiliser le formalisme des quaternions pour décrire les
diffractomètres. Ces êtres mathématiques permettent de représenter des
rotations dans l'espace à trois dimensions. Il y a plusieurs façons de
les représenter tout comme les nombres complexes.

.. math::
   q=a+bi+cj+dk

ou bien

.. math::
   q=[a,\vec{v}]

La norme d'un quaternion est calculé de la même façon que pour les
nombres complexes

.. math::
   \lvert q \rvert = \sqrt{a{{}^2}+b{{}^2}+c{{}^2}+d{{}^2}}

Son conjugé est :

.. math::
   q^{*}=[a,-\vec{u}]=a-bi-cj-dk

Opérations
==========

La grand différence avec l'algèbre des nombres complexes est sa non
commutativité.

.. math::
   qp \neq pq

.. math::
   \bordermatrix{
	~ & 1 & i  & j  & k \cr
	1 & 1 & i  & j  & k \cr
	i & i & -1 & k  & -j \cr
	j & j & -k & -1 & i \cr
	k & k & j  & -i & -1
   }

Le calcule du produit de deux quaternions s'exprime sous la forme du
produite de Grassman :eq:`produit de Grassman`. Ainsi pour les deux
quaternions :math:`p` et :math:`q`:

.. math::
   :nowrap:

   \begin{align*}
	q &= a+\vec{u} = a+bi+cj+dk\\
	p &= t+\vec{v} = t+xi+yj+zk
   \end{align*}

on obtient

.. math::
   :label: produite de Grassman

   pq=at-\vec{u}\cdot\vec{v}+a\vec{v}+t\vec{u}+\vec{v}\times\vec{u}

ou encore

.. math::
   pq==(at-bx-cy-dz)+(bt+ax+cz-dy)i+(ct+ay+dx-bz)j+(dt+az+by-cx)k

Les rotation de l'espace 3D
===========================

L'ensemble des quaternions unitaires (leur norme est égale à 1) est le
groupe qui représente les rotations dans l'espace 3D. Si on a un
vecteur unitaire :math:`\vec{u}` et un angle de rotation
:math:`\theta` alors le quaternion
:math:`[\cos\frac{\theta}{2},\sin\frac{\theta}{2}\vec{u]}` représente
la rotation de :math:`\theta` autour de l'axe :math:`\vec{u}` dans le
sens trigonométrique. Nous allons donc utiliser ces quaternions
unitaires pour représenter les mouvements du diffractomètre.

Alors que dans le plan 2D une simple multiplication entre un nombre
complex et le nombre :math:`e^{i\theta}` permet de calculer simplement
la rotation d'angle :math:`\theta` autour de l'origine, dans l'espace
3D l'expression équivalente est:

.. math::
   z'=qzq^{-1}

où :math:`q` est le quaternion de norme 1 représentant la rotation dans
l'espace et :math:`z` le quaternion représentant le vecteur qui subit la
rotation (sa partie réelle est nulle).

Dans le cas des quaternions de norme 1, il est très facile de calculer
:math:`q^{-1}`. En effet l'inverse d'une rotation d'angle
:math:`\theta` est la rotation d'angle :math:`-\theta`. On a donc
directement:

.. math::
   q^{-1}=[\cos\frac{-\theta}{2},\sin\frac{-\theta}{2}\vec{u}]=[\cos\frac{\theta}{2},-\sin\frac{\theta}{2}\vec{u}]=q^{*}

Le passage aux matrices de rotation se fait par la formule suivante
:math:`q\rightarrow M`.

.. math::
   \begin{bmatrix}
	a{{}^2}+b{{}^2}-c{{}^2}-d{{}^2} & 2bc-2ad & 2ac+2bd\\
	2ad+2bc & a{{}^2}-b{{}^2}+c{{}^2}-d{{}^2} & 2cd-2ab\\
	2bd-2ac & 2ab+2cd & a{{}^2}-b{{}^2}-c{{}^2}+d{{}^2}
   \end{bmatrix}

La composition de rotation se fait simplement en multipliant les
quaternions entre eux. Si l'on à :math:`q`

Les Diffractomètres
###################

Eulérien 3S+1D
**************

Nous allons nous inspirer du modèle de Busin et Levy pour décrire
notre diffractomètre. Les sens de rotation sont respectés mais le
repère directe est choisi de façon à correspondre au repère de
laboratoire de la ligne CRYSTAL du synchrotron Soleil. Les photons-X
se propagent suivant le vecteur :math:`\vec{x}` et la direction
verticale est suivant le vecteur :math:`\vec{z}`. Ce diffractomètre
est de type verticale (le vecteur de diffusion :math:`\vec{Q}` est
dans le plan xOz). Les angles permettant de décrire la configuration
du diffractomètre sont présentés sur la figure~\ref{cap:3S+1D}.

.. figure:: ../../figures/3S+1D.png
   :align: center
   :width: 8cm

   Dénomination des angles du diffractomètre 3S+1D Eulérien.\label{cap:3S+1D}

Eulérien 4S+2D
**************

Nous allons nous inspirer du modèle de You pour notre diffractomètre
(fig.~\ref{cap:4S+2D}) ici présenté tous les angles mis à zéro.  Les
rayons-X arrivent suivant le vecteur $\vec{x}$ (le repère est
différent de celui de You).

.. figure:: ../../figures/4S+2D.png
   :align: center
   :width: 8cm

   Dénomination des angles du diffractomètre 4S+2D Eulérien.\label{cap:4S+2D}


Le principe des calcules de You est d'exprimer dans le repère du
laboratoire le vecteur diffusion :math:`\vec{Q}` de deux façons
différentes. Une première en utilisant les angles du goniomètre 4S
puis une à partir des angles du détecteur 2D et de la connaissance des
coordonnées du vecteur incident.  En égalant les deux expressions, il
obtient un système d'équation à 6 inconnus mais seulement 3
équations. Pour être à même de résoudre le système il faut fixer des
contraintes supplémentaire. C'est ce que l'on appel les modes de
fonctionnement du diffractomètre. Il est commode de définir d'autres
angles que ceux du diffractomètre relativement à des vecteurs
caractéristiques tel que le vecteur de diffusion :math:`\vec{Q}` ou un
vecteur pointant dans une direction particulière du cristal
:math:`\vec{n}`. Cette direction peut-être soit lié à la
cristallographie du cristal soit à sa forme (une normale à une
face). La figure~\ref{cap:Pseudo-Angles-li=0000E9s} représente les
angles liés au vecteur de diffusion et à ce vecteur de référence. Tout
d'abord :math:`\theta` (angle entre :math:`\vec{Q}` et le plan
:math:`yz`) et qui correspond à l'angle de Bragg. :math:`\vartheta`
qui est l'angle azimutal que fait la projection de :math:`\vec{Q}` sur
le plan :math:`yz` et la direction :math:`+y`
(fig~\ref{cap:Pseudo-Angles-li=0000E9s}a).  Il y a ensuite les angles
:math:`\alpha` et :math:`\varphi` définits comme précédemment mais
pour le vecteur de référence :math:`\vec{n}`
(fig~\ref{cap:Pseudo-Angles-li=0000E9s}b).  Et finalement les angles
$\tau$ (angle entre :math:`\vec{Q}` et :math:`\vec{n}`) et
:math:`\psi` qui correspond à la rotation de :math:`\vec{n}` autour du
vecteur de diffusion :math:`\vec{Q}`
(fig~\ref{cap:Pseudo-Angles-li=0000E9s}c).  L'origine de cet angle
$\psi$ est prise à zéro lorsque le vecteur :math:`\vec{n}` est dans le
plan de diffraction (plan contenant :math:`\vec{Q}` et
:math:`\vec{k_{i}}`) (fig~\ref{cap:Pseudo-Angles-li=0000E9s}d).

.. figure:: ../../figures/4S+2D_reciprocal.png
   :align: center
   :width: 7 cm

   Pseudo angles :math:`\theta` et :math:`\vartheta` liés à :math:`\vec{Q}`

.. figure:: ../../figures/4S+2D_reciprocal2.png
   :align: center
   :width: 7cm

   Pseudo angles :math:`\alpha` et :math:`\phi` liés à :math:`\vec{n}`

.. figure:: ../../figures/4S+2D_reciprocal3.png
   :align: center
   :width: 7cm

   Pseudo angles :math:`\tau` et :math:`\psi` liés à :math:`\vec{n}` relativement à :math:`\vec{Q}` et le plan de diffraction

.. figure:: ../../figures/4S+2D_reciprocal4.png
   :align: center
   :width: 7cm

   Pseudo Angles liés au vecteur de diffusion :math:`\vec{Q}` et à :math:`\vec{n}`

Il est alors possible d'exprimer ces pseudos angles en fonction des
angles physique du diffractomètre.


Modes de fonctionnement
#######################

Equations fondamentales
***********************

Le problème que nous devons résoudre est de calculer pour une famille
de plan :math:`(h,k,l)` donné, les angles de rotation du
diffractomètre qui permettent de le mettre en condition de
diffraction. Il faut donc exprimer les relations mathématiques qui
lient les différents angles entre eux lorsque la condition de Bragg
est vérifiée. L'équation fondamentale est la suivante:

.. math::
   :nowrap:

   \begin{align*}
	\left(\prod_{i}S_{i}\right)\cdot U\cdot B\cdot\vec{h} & =\left(\prod_{j}D_{j}-I\right)\cdot\vec{k_{i}}\\
	R\cdot U\cdot B\cdot\vec{h} & =\vec{Q}
   \end{align*}

ou :math:`\vec{h}` est le vecteur :math:`(h,k,l)`, :math:`\vec{k_{i}}`
est le vecteur incident, :math:`S_{i}` les matrices de rotations des
mouvements liés à l'échantillon, :math:`D_{j}` les matrices de
rotation des mouvements liés au détecteur, :math:`I` la matrice
identité, :math:`U` la matrice d'orientation du cristal par rapport au
repère de l'axe sur lequel ce dernier est monté et :math:`B` la
matrice de passage d'un repère non orthonormé ( celui du crystal
réciproque) à un repère orthonormé.


Calcule de `B`
==============

Si l'on connaît les paramètres cristallins du cristal étudié, il est
très simple de calculer :math:`B`:

.. math::
   B=
   \begin{bmatrix}
	a^{\star} & b^{\star}\cos\gamma^{\star} & c^{\star}\cos\beta^{\star}\\
	0 & b^{\star}\sin\gamma^{\star} & -c^{\star}\sin\beta^{\star}\cos\alpha\\
	0 & 0 & 1/c
   \end{bmatrix}


Calcule de `U`
==============

Il existe plusieurs façons de calculer :math:`U`. Busing et Levy en a
proposé plusieurs. Nous allons présenter celle qui nécessite la mesure
de seulement deux réflections ainsi que la connaissance des paramètres
cristallins. Cette façon de calculer la matrice d'orientation $U$,
peut être généralisée à n'importe quel diffractomètre pour peu que la
description des axes de rotation permette d'obtenir la matrice de
rotation de la machine :math:`R` et le vecteur de diffusion
:math:`\vec{Q}`.

Il est également possible de calculer :math:`U` sans la connaîssance
des paramètres cristallins. il faut alors faire un affinement des
paramètres. Cela revient à minimiser une fonction. Nous allons
utiliser la méthode du simplex pour trouver ce minimum et donc ajuster
l'ensemble des paramètres cristallins ainsi que la matrice
d'orientation.

Algorithme de Busing Levy
=========================

L'idée est de se placer dans le repère de l'axe sur lequel est monté
l'échantillon. On mesure deux réflections
:math:`(\vec{h}_{1},\vec{h}_{2})` ainsi que leurs angles
associés. Cela nous permet de calculer $R$ et :math:`\vec{Q}` pour
chacune de ces reflections. nous avons alors ce système:


.. math::
   :nowrap:

   \begin{eqnarray*}
	U\cdot B\cdot\vec{h}_{1} & = & \tilde{R}_{1}\cdot\vec{Q}_{1}\\
	U\cdot B\cdot\vec{h}_{2} & = & \tilde{R}_{2}\cdot\vec{Q}_{2}
   \end{eqnarray*}

De façon à calculer facilement :math:`U`, il est intéressant de
définir deux trièdres orthonormé :math:`T_{\vec{h}}` et
:math:`T_{\vec{Q}}` à partir des vecteurs
:math:`(B\vec{h}_{1},B\vec{h}_{2})` et
:math:`(\tilde{R}_{1}\vec{Q}_{1},\tilde{R}_{2}\vec{Q}_{2})`. On a
alors très simplement:

.. math::
   U\cdot T_{\vec{h}}=T_{\vec{Q}}

Et donc

.. math::
   U=T_{\vec{Q}}\cdot\tilde{T}_{\vec{h}}

Affinement par la méthode du simplex
====================================

Dans ce cas nous ne connaissons pas la matrice :math:`B`, il faut donc
mesurer plus que deux réflections pour ajuster les 9 paramètres. Six
paramètres pour le crystal et trois pour la matrice d'orientation
:math:`U`. Les trois paramètres qui permennt de representer :math:`U`
sont en fait les angles d'euler. il faut donc être en mesure de passer
d'une représentation eulérien à cette matrice :math::`U` et
réciproquement.

.. math::
   U=X\cdot Y\cdot Z

où :math:`X` est la matrice rotation suivant l'axe Ox et le premier
angle d'Euler, :math:`Y` la matrice de rotation suivant l'axe Oy et le
deuxième angle d'Euler et :math:`Z` la matrice du troisième angle
d'Euler pour l'axe Oz.

.. math::
   :nowrap:

   \begin{tabular}{ccc}
	$X$ & $Y$ & $Z$\tabularnewline
	$\begin{bmatrix}
		1 & 0 & 0\\
		0 & A & -B\\
		0 & B & A
	\end{bmatrix}$
	&
	$\begin{bmatrix}
		C & 0 & D\\
		0 & 1 & 0\\
		-D & 0 & C
	\end{bmatrix}$
	&
	$\begin{bmatrix}
		E & -F & 0\\
		F & E & 0\\
		0 & 0 & 1
	\end{bmatrix}$
   \end{tabular}

et donc:

.. math::
   U=
   \begin{bmatrix}
	CE & -CF & D\\
	BDE+AF & -BDF+AE & -BC\\
	-ADE+BF & ADF+BE & AC
   \end{bmatrix}

Il est donc facile de passer des angles d'Euler à la matrice
d'orientation.

Il faut maintenant faire la transformation inverse de la matrice
:math:`U` vers les angles d'euler.


Diffractomètre 4 Cercle (3S+1D) Eulerien
****************************************

Pour ce diffractomètres, les matrices de rotations des différents axes
sont les suivantes:

.. math::
   :nowrap:

   \begin{tabular}{cccc}
	$\Omega$ & $\chi$ & $\Phi$ & $2\Theta$\tabularnewline
	$\begin{bmatrix}
		\cos\omega & 0 & -\sin\omega\\
		0 & 1 & 0\\
		\sin\omega & 0 & \cos\omega
	\end{bmatrix}$
	&
	$\begin{bmatrix}
		1 & 0 & 0\\
		0 & \cos\chi & -\sin\chi\\
		0 & \sin\chi & \cos\chi
	\end{bmatrix}$
	&
	$\begin{bmatrix}
		\cos\phi & 0 & -\sin\phi\\
		0 & 1 & 0\\
		\sin\phi & 0 & \cos\phi
	\end{bmatrix}$
	&
	$\begin{bmatrix}
		\cos2\theta & 0 & -\sin2\theta\\
		0 & 1 & 0\\
		\sin2\theta & 0 & \cos2\theta
	\end{bmatrix}$
   \end{tabular}

On obtient alors la matrice de rotation de la machine

.. math::
   R=\Omega\chi\Phi

soit

.. math::
   R=
   \begin{bmatrix}
	\cos\omega\cos\phi-\cos\chi\sin\omega\sin\phi & -\sin\chi\sin\omega & -\cos\omega\sin\phi-\cos\chi\sin\omega\cos\phi\\
	-\sin\chi\sin\phi & \cos\chi & -\sin\chi\cos\phi\\
	\sin\omega\cos\phi-\cos\chi\cos\omega\sin\phi & -\sin\chi\cos\omega & -\sin\omega\sin\phi-\cos\chi\cos\omega\cos\phi
   \end{bmatrix}

De la même façon on peut calculer le vecteur diffusion en fonction des
angles du détecteur:

.. math::
   \vec{Q}=\left(2\Theta-I\right)\cdot\vec{k}_{i}

où :math:`I` est la matrice identité. Finalement:

.. math::
   \vec{Q}=k_{i}\left(\begin{array}{c}
   \cos2\theta-1\\
   0\\
   \sin2\theta
   \end{array}\right)

L'équation fondamentale nous permet d'écrire:

.. math::
   U\cdot B\cdot\vec{h}=\tilde{R}\cdot\vec{Q}

Cette équation est de 4 inconnus pour seulement 3 équations. Il faut
donc imposer des contraintes pour résoudre ce système et ainsi
d'orienter le diffractomètre. Ces différentes contraintes définissent
les modes de fonctionnement des diffractomètres. Dans la suite nous
allons nous efforcer de trouver l'ensemble des solutions possibles
pour les différents modes et non pas une seule solution. Ceci afin de
laisser le choix suivant certaines stratégies à l'utilisateur
d'utiliser telle ou telle solution plutôt qu'une autre.

Mode Bisecteur
==============

Dans ce mode on choisit d'avoir:

.. math::
   \omega=\theta

Le système s'écrit alors simplement:

.. math::
   :nowrap:

   \begin{eqnarray*}
	h_{\phi} & = & 2k_{i}\sin\theta\cos\chi\sin\phi\\
	k_{\phi} & = & 2k_{i}\sin\theta\sin\chi\\
	l_{\phi} & = & 2k_{i}\sin\theta\cos\chi\cos\phi
   \end{eqnarray*}

On a:

.. math::
   h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}=4k_{i}\sin^{2}\theta

où :math:`k_{i}=\frac{\tau}{\lambda}`. donc on peut écrire:

.. math::
   \left|\sin\theta\right|=\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}

il faut donc enviseager les deux possibilité selon que :math:`\theta`
est positif ou bien négatif.

.. math::
   \sin\theta<0

On peut alors écrire:

.. math::
   \sin\chi=-\frac{k_{\phi}}{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}

puis en utilisant le relation bien connue :math:`\cos^{2}+\sin^{2}=1`
on a:

.. math::
   \cos^{2}\chi=\frac{h_{\phi}^{2}+l_{\phi}^{2}}{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}

Il faut une fois de plus faire un choix selon que :math:`\cos\chi` est
positif ou négatif.

.. math::
   \cos\chi<0

.. math::
   \cos\chi=-\sqrt{\frac{h_{\phi}^{2}+l_{\phi}^{2}}{h_{\phi}^{2}+k_{\phi+}^{2}l_{\phi}^{2}}}

.. math::
   \cos\chi>0

.. math::
   \cos\chi=\sqrt{\frac{h_{\phi}^{2}+l_{\phi}^{2}}{h_{\phi}^{2}+k_{\phi+}^{2}l_{\phi}^{2}}}

.. math::
   \sin\theta>0

On peut alors écrire:

.. math::
   \sin\chi=\frac{k_{\phi}}{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}

puis en utilisant le relation bien connue :math:`\cos^{2}+\sin^{2}=1` on a:

.. math::
   \cos^{2}\chi=\frac{h_{\phi}^{2}+l_{\phi}^{2}}{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}

Il faut une fois de plus faire un choix selon que :math:`\cos\chi` est
positif ou négatif.

.. math::
   \cos\chi<0

.. math::
   \cos\chi=-\sqrt{\frac{h_{\phi}^{2}+l_{\phi}^{2}}{h_{\phi}^{2}+k_{\phi+}^{2}l_{\phi}^{2}}}

.. math::
   \cos\chi>0

.. math::
   \cos\chi=\sqrt{\frac{h_{\phi}^{2}+l_{\phi}^{2}}{h_{\phi}^{2}+k_{\phi+}^{2}l_{\phi}^{2}}}

La résolution du système donne alors 4 quadruplets de solutions:

.. math::
   :nowrap:

   \begin{tabular}{c|c|c|c}
	$\omega$ & $\chi$ & $\phi$ & $2\theta$\tabularnewline
	\hline
	$-\theta$ & $\arctan2(-k_{\phi},-\sqrt{h_{\phi}^{2}+l_{\phi}^{2}})$ & $\arctan2(h_{\phi},l_{\phi})$ & $2\arcsin-\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	$-\theta$ & $\arctan2(-k_{\phi},\sqrt{h_{\phi}^{2}+l_{\phi}^{2}})$ & $\arctan2(-h_{\phi},-l_{\phi})$ & $2\arcsin-\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	$\theta$ & $\arctan2(k_{\phi},-\sqrt{h_{\phi}^{2}+l_{\phi}^{2}})$ & $\arctan2(-h_{\phi},-l_{\phi})$ & $2\arcsin\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	$\theta$ & $\arctan2(k_{\phi},\sqrt{h_{\phi}^{2}+l_{\phi}^{2}})$ & $\arctan2(h_{\phi},l_{\phi})$ & $2\arcsin\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
   \end{tabular}

Mode Delta Theta
================

Ce mode consiste à décaler :math:`\omega` par rapport à :math:`\theta`
d'une valeur constante :math:`C`:

.. math::
	\omega=\theta+C

Le système s'écrit alors comme suit:

.. math::
   :nowrap:

   \begin{eqnarray*}
	h_{\phi} & = & 2k_{i}\sin\theta\left(\cos C\cos\chi\sin\phi+\sin C\cos\phi\right)\\
	k_{\phi} & = & 2k_{i}\sin\theta\cos C\sin\chi\\
	l_{\phi} & = & 2k_{i}\sin\theta\left(\cos C\cos\chi\cos\phi-\sin C\sin\phi\right)
   \end{eqnarray*}

On a toujours:

.. math::
	h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}=4k_{i}\sin^{2}\theta

La résolution donne 4 quadruplets de solutions:

.. math::
	:nowrap:

	\begin{tabular}{ccc}
		$\omega$ & $\chi$ & $\phi$\tabularnewline
		\hline
		$-\theta+C$ & $\arctan2(\frac{-k_{\phi}}{\cos C},-\sqrt{h_{\phi}^{2}-k_{\phi}^{2}\tan^{2}C+l_{\phi}^{2}})$ & $\arctan2(-h_{\phi}\cos C\cos\chi+l_{\phi}\sin C,-l_{\phi}\cos C\cos\chi-h_{\phi}\sin C)$\tabularnewline
		$-\theta+C$ & $\arctan2(\frac{-k_{\phi}}{\cos C},\sqrt{h_{\phi}^{2}-k_{\phi}^{2}\tan^{2}C+l_{\phi}^{2}})$ & $\arctan2(-h_{\phi}\cos C\cos\chi+l_{\phi}\sin C,-l_{\phi}\cos C\cos\chi-h_{\phi}\sin C)$\tabularnewline
		$\theta+C$ & $\arctan2(\frac{k_{\phi}}{\cos C},-\sqrt{h_{\phi}^{2}-k_{\phi}^{2}\tan^{2}C+l_{\phi}^{2}})$ & $\arctan2(h_{\phi}\cos C\cos\chi-l_{\phi}\sin C,l_{\phi}\cos C\cos\chi+h_{\phi}\sin C)$\tabularnewline
		$\theta+C$ & $\arctan2(\frac{k_{\phi}}{\cos C},\sqrt{h_{\phi}^{2}-k_{\phi}^{2}\tan^{2}C+l_{\phi}^{2}})$ & $\arctan2(h_{\phi}\cos C\cos\chi-l_{\phi}\sin C,l_{\phi}\cos C\cos\chi+h_{\phi}\sin C)$\tabularnewline
	\end{tabular}

.. math::
   :nowrap:

   \begin{tabular}{c}
	$2\theta$\tabularnewline
	\hline
	$2\arcsin-\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	$2\arcsin-\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	$2\arcsin\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	$2\arcsin\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
   \end{tabular}

où

Mode omega constant
===================

Dans ce mode on choisit de garder :math:`\omega` toujours constant:

.. math::
	\omega=C

Le système s'écrit alors comme suit:

.. math::
	:nowrap:

	\begin{eqnarray*}
		h_{\phi} & = & 2k_{i}\sin\theta\left(\cos(C-\theta)\cos\chi\sin\phi+\sin(C-\theta)\cos\phi\right)\\
		k_{\phi} & = & 2k_{i}\sin\theta\cos(C-\theta)\sin\chi\\
		l_{\phi} & = & 2k_{i}\sin\theta\left(\cos(C-\theta)\cos\chi\cos\phi-\sin(C-\theta)\sin\phi\right)
	\end{eqnarray*}

La résolution donne 4 quadruplets de solutions:

.. math::
	:nowrap:

	\begin{tabular}{ccc}
		$\omega$ & $\chi$ & $\phi$\tabularnewline
		\hline
		$-\theta+C$ & $\arctan2\left(-k_{\phi},-\sqrt{(h_{\phi}^{2}+l_{\phi}^{2})\cos^{2}(C-\theta)-k_{\phi}^{2}\sin^{2}(C-\theta)}\right)$ & $\arctan2(-h_{\phi}\cos C\cos\chi+l_{\phi}\sin C,-l_{\phi}\cos C\cos\chi-h_{\phi}\sin C)$\tabularnewline
		$-\theta+C$ & $\arctan2\left(-k_{\phi},\sqrt{(h_{\phi}^{2}+l_{\phi}^{2})\cos^{2}(C-\theta)-k_{\phi}^{2}\sin^{2}(C-\theta)}\right)$ & $\arctan2(-h_{\phi}\cos C\cos\chi+l_{\phi}\sin C,-l_{\phi}\cos C\cos\chi-h_{\phi}\sin C)$\tabularnewline
		$\theta+C$ & $\arctan2\left(k_{\phi},-\sqrt{(h_{\phi}^{2}+l_{\phi}^{2})\cos^{2}(C-\theta)-k_{\phi}^{2}\sin^{2}(C-\theta)}\right)$ & $\arctan2(h_{\phi}\cos C\cos\chi-l_{\phi}\sin C,l_{\phi}\cos C\cos\chi+h_{\phi}\sin C)$\tabularnewline
		$\theta+C$ & $\arctan2\left(k_{\phi},\sqrt{(h_{\phi}^{2}+l_{\phi}^{2})\cos^{2}(C-\theta)-k_{\phi}^{2}\sin^{2}(C-\theta)}\right)$ & $\arctan2(h_{\phi}\cos C\cos\chi-l_{\phi}\sin C,l_{\phi}\cos C\cos\chi+h_{\phi}\sin C)$\tabularnewline
	\end{tabular}

.. math::
	:nowrap:

	\begin{tabular}{c}
		$2\theta$\tabularnewline
		\hline
		$2\arcsin-\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
		$2\arcsin-\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
		$2\arcsin\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
		$2\arcsin\frac{\sqrt{h_{\phi}^{2}+k_{\phi}^{2}+l_{\phi}^{2}}}{2k_{i}}$\tabularnewline
	\end{tabular}
