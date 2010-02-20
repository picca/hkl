#!/bin/sh

mkdir m4
gtkdocize || exit 1
autoreconf -ivf
