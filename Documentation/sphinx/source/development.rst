.. _development:

Developpement
#############

Getting hkl
***********

To get hkl, you can download the last stable version from sourceforge or if you
want the latest development version use `git <http://git.or.cz/>`_ or
`msysgit <http://code.google.com/p/msysgit/downloads/list>`_ on windows system and
do::

  $ git clone git://repo.or.cz/hkl.git

or::

  $ git clone http://repo.or.cz/r/hkl.git (slower)

then checkout the next branch like this::

  $ cd hkl
  $ git checkout -b next origin/next

Building hkl
************

To build hkl you need `Python 2.3+ <http://www.python.org>`_ the
`GNU Scientific Library 1.12 <http://www.gnu.org/software/gsl/>`_
and `GLib-2.0 >= 2.3.4 <https://developer.gnome.org/glib/>`_::

  $ ./configure --disable-gui
  $ make
  $ sudo make install

you can also build a GUI interfaces which use `gtk <http://www.gtk.org>`_::

  $ ./configure
  $ make
  $ sudo make install

optionnaly you can build an experimental *libhkl3d* library (no public
API for now) which is used by the GUI to display and compute
diffractometer collisions (only the *K6C* model). To build it you need
also `gtkglext <https://projects.gnome.org/gtkglext/>`_ and
`bullet 2.82 <http://bulletphysics.org/wordpress/>`_::

  $ ./configure --enable-hkl3d
  $ make
  $ sudo make install

if you want to work on the documentation you need the extra

+ `gtk-doc <http://www.gtk.org/gtk-doc/>`_ for the api
+ `sphinx <http://sphinx.pocoo.org/>`_ for the html and latex doc.
+ `asymptote <http://asymptote.sourceforge.net/>`_ for the figures

::

  $ ./configure --enable-gtk-doc
  $ make
  $ make html

Hacking hkl
***********

Bug reporting
=============

You can find the bug tracker here `libhkl <https://bugs.debian.org/cgi-bin/pkgreport.cgi?repeatmerged=no&src=hkl>`_

* Debian/Ubuntu::

    $ reportbug hkl

* Other OS

  You just need to send an email::

    To: submit@bugs.debian.org
    From: xxx@yyy.zzz
    Subject: My problem with hkl...
  
    Package: hkl
    Version: |version|

    I found this problem in hkl...


Providing patchs
================

you can send your patch to `Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>`_ using ``git``

Here a minimalist exemple of the workflow to prepare and send a patch
for hkl. Suppose you wan to add a new feature to hkl create first a
new branch from the next one::

  $ git checkout -b my-next next

hack, hack::

  $ git commit -a

more hacks::

  $ git commit -a

now that your new feature is ready for a review, you can send by
email your work using git format-patch::

  $ git format-patch origin/next

and send generated files `0001_xxx`, `0002_xxx`, ... to the author.

Howto add a diffractometer
**************************

To add a new diffractometer, you just need to copy the
:file:`hkl/hkl-engine-template.c` into
:file:`hkl/hkl-engine-INSTITUT-BEAMLINE-INSTRUMENT.c` where you
replace the upper case with the appropriate values.

The template file is compiled during the build process to ensure that
it is always valid.

Then you just need to follow the instruction found in the template.
If you need some precision about the process, do not hesitate to
contact the main author.

do not forgot also to add this new file into :file:`hkl/Makefile.am`
with other diffractometers in the hkl_c_sources variable (please keep
the alphabetic order).
