hkl
====
The hkl library is a framework for diffraction computation and
diffractometer control, heavily used at the SOLEIL synchrotron.
It supports various types of diffractometer geometry: Eulerian
4-circle, Eulerian 6-circle, kappa 4-circle, kappa 6-circle, and
z-axis geometry. For each of these it provides several numerically
computed modes, such as bisector and constant psi.

Installation
============

To build you need the following libraries

| library       | url                                | required/optional  |
|---------------|------------------------------------|--------------------|
|gsl >= 1.12    | http://www.gnu.org/software/gsl/   | required           |
|gtkmm >= 2.18  | http://www.gtkmm.org               | optional           |
|libg3d         | http://automagically.de/g3dviewer/ | optional           |
|libyaml        | http://pyyaml.org/wiki/LibYAML     | optional           |
|gtk-doc >= 1.9 | http://www.gtk.org/gtk-doc/        | optional           |
|povray         | http://www.povray.org/             | optional           |
|asymptote      | http://asymptote.sourceforge.net/  | optional           |
