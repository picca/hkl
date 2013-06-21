/* gerror.h - Error reporting system
 *
 *  Copyright 2000 Red Hat, Inc.
 *
 * The Gnome Library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 *
 * The Gnome Library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with the Gnome Library; see the file COPYING.LIB.  If not,
 * write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *   Boston, MA 02111-1307, USA.
 */

/*
 * modified by Picca Frédéric-emmanuel <picca@synchrotron-soleil.fr>
 * for the hkl project
 */
#ifndef __HKL_ERROR_H__
#define __HKL_ERROR_H__

#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklError HklError;

HKLAPI const char *hkl_error_message_get(const HklError *self) HKL_ARG_NONNULL(1);

HKL_END_DECLS

#endif /* __HKL_ERROR_H__ */
