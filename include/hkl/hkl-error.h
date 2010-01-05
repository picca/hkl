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

#include <stdarg.h>
#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

typedef struct _HklError HklError;

struct _HklError
{
	char *message;
};

HklError* hkl_error_new (const char *format, ...) G_GNUC_PRINTF (1, 2);

HklError* hkl_error_new_literal (const char *message);
HklError* hkl_error_new_valist (const char *format, va_list args);

void hkl_error_free (HklError *error);

HklError* hkl_error_new_copy (const HklError *error);

/* if (err) *err = hkl_error_new(domain, code, format, ...), also has
 * some sanity checks.
 */
void hkl_error_set (HklError **err, const char *format, ...) G_GNUC_PRINTF (2, 3);

void hkl_error_set_literal (HklError **err, const char *message);

/* if (dest) *dest = src; also has some sanity checks.
 */
void hkl_error_propagate (HklError **dest, HklError *src);

/* if (err && *err) { hkl_error_free(*err); *err = NULL; } */
void hkl_error_clear (HklError **err);

/* if (err) prefix the formatted string to the ->message */
void hkl_error_prefix (HklError **err, const char *format, ...) G_GNUC_PRINTF (2, 3);

/* hkl_propagate_error then hkl_error_prefix on dest */
void hkl_error_propagate_prefixed (HklError **dest, HklError *src,
				   const char *format, ...) G_GNUC_PRINTF (3, 4);

HKL_END_DECLS

#endif /* __HKL_ERROR_H__ */
