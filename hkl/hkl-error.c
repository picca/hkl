/* GLIB - Library of useful routines for C programming
 * Copyright (C) 1995-1997  Peter Mattis, Spencer Kimball and Josh MacDonald
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */

/*
 * Modified by the GLib Team and others 1997-2000.  See the AUTHORS
 * file for a list of people on the GLib Team.  See the ChangeLog
 * files for a list of changes.  These files are distributed with
 * GLib at ftp://ftp.gtk.org/pub/gtk/.
 */

#define _GNU_SOURCE // need for vasprintf
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <hkl/hkl-error.h>

/**
 * hkl_error_new_valist:
 * @format: printf()-style format for error message
 * @args: #va_list of parameters for the message format
 *
 * Creates a new #HklError with the given message
 * formatted with @format.
 *
 * Returns: a new #HklError
 *
 * Since: 2.22
 */
HklError* hkl_error_new_valist (const char *format, va_list args)
{
	HklError *error;

	error = HKL_MALLOC(HklError);

	vasprintf (&error->message, format, args);

	return error;
}

/**
 * hkl_error_new:
 * @format: printf()-style format for error message
 * @Varargs: parameters for message format
 *
 * Creates a new #HklError with the given,
 * and a message formatted with @format.
 *
 * Return value: a new #HklError
 */
HklError* hkl_error_new (const char *format, ...)
{
	HklError* error;
	va_list args;

	if (!format)
		return NULL;

	va_start (args, format);
	error = hkl_error_new_valist (format, args);
	va_end (args);

	return error;
}

/**
 * hkl_error_new_literal:
 * @message: error message
 *
 * Creates a new #HklError; unlike hkl_error_new(), @message is
 * not a printf()-style format string. Use this function if
 * @message contains text you don't have control over,
 * that could include printf() escape sequences.
 *
 * Return value: a new #HklError
 **/
HklError* hkl_error_new_literal (const char *message)
{
	HklError* err;

	if(!message)
		return NULL;

	err = HKL_MALLOC (HklError);

	err->message = strdup (message);

	return err;
}

/**
 * hkl_error_free:
 * @error: a #HklError
 *
 * Frees a #HklError and associated resources.
 */
void hkl_error_free (HklError *error)
{
	if (!error)
		return;

	free (error->message);
	free (error);
}

/**
 * hkl_error_copy:
 * @error: a #HklError
 *
 * Makes a copy of @error.
 *
 * Return value: a new #HklError
 */
HklError* hkl_error_new_copy (const HklError *error)
{
	HklError *copy;
 
	if(!error)
		return NULL;


	copy = HKL_MALLOC (HklError);

	*copy = *error;

	copy->message = strdup (error->message);

	return copy;
}

#define ERROR_OVERWRITTEN_WARNING "HklError set over the top of a previous HklError or uninitialized memory.\n" \
	"This indicates a bug in someone's code. You must ensure an error is NULL before it's set.\n" \
	"The overwriting error message was: %s"

/**
 * hkl_error_set:
 * @err: a return location for a #HklError, or %NULL
 * @format: printf()-style format
 * @Varargs: args for @format
 *
 * Does nothing if @err is %NULL; if @err is non-%NULL, then *@err
 * must be %NULL. A new #HklError is created and assigned to *@err.
 */
void hkl_error_set (HklError **err, const char  *format, ...)
{
	HklError *new;
	va_list args;

	if (err == NULL)
		return;

	va_start (args, format);
	new = hkl_error_new_valist (format, args);
	va_end (args);

	if (*err == NULL)
		*err = new;
	else
		fprintf (stderr, ERROR_OVERWRITTEN_WARNING, new->message); 
}

/**
 * hkl_error_set_literal:
 * @err: a return location for a #HklError, or %NULL
 * @message: error message
 *
 * Does nothing if @err is %NULL; if @err is non-%NULL, then *@err
 * must be %NULL. A new #HklError is created and assigned to *@err.
 * Unlike hkl_set_error(), @message is not a printf()-style format string.
 * Use this function if @message contains text you don't have control over,
 * that could include printf() escape sequences.
 *
 * Since: 2.18
 */
void hkl_error_set_literal (HklError **err, const char *message)
{
	HklError *new;

	if (err == NULL)
		return;

	new = hkl_error_new_literal (message);
	if (*err == NULL)
		*err = new;
	else
		fprintf (stderr, ERROR_OVERWRITTEN_WARNING, new->message); 
}

/**
 * hkl_propagate_error:
 * @dest: error return location
 * @src: error to move into the return location
 *
 * If @dest is %NULL, free @src; otherwise, moves @src into *@dest.
 * The error variable @dest points to must be %NULL.
 */
void hkl_error_propagate (HklError **dest, HklError  *src)
{
	if(!src)
		return;
 
	if (dest == NULL){
		if (src)
			hkl_error_free (src);
		return;
	}else{
		if (*dest != NULL)
			fprintf (stderr, ERROR_OVERWRITTEN_WARNING, src->message);
		else
			*dest = src;
	}
}

/**
 * hkl_clear_error:
 * @err: a #HklError return location
 *
 * If @err is %NULL, does nothing. If @err is non-%NULL,
 * calls hkl_error_free() on *@err and sets *@err to %NULL.
 */
void hkl_error_clear (HklError **err)
{
	if (err && *err){
		hkl_error_free (*err);
		*err = NULL;
	}
}

static void hkl_error_add_prefix (char **string, const char *format, va_list ap)
{
	char *oldstring;
	char *prefix;
	int len;
	int len_prefix;
	int len_oldstring;

	len_prefix = vasprintf (&prefix, format, ap);
	oldstring = *string;
	len_oldstring = strlen(*string);

	len = len_prefix + len_oldstring;
	*string = malloc (len *sizeof (char) + 1);
	*string = strncpy (*string, prefix, len_prefix + 1);
	*string = strncat (*string, oldstring, len_oldstring);
	free (oldstring);
	free (prefix);
}

/**
 * hkl_prefix_error:
 * @err: a return location for a #HklError, or %NULL
 * @format: printf()-style format string
 * @...: arguments to @format
 *
 * Formats a string according to @format and
 * prefix it to an existing error message.  If
 * @err is %NULL (ie: no error variable) then do
 * nothing.
 *
 * If *@err is %NULL (ie: an error variable is
 * present but there is no error condition) then
 * also do nothing.  Whether or not it makes
 * sense to take advantage of this feature is up
 * to you.
 *
 * Since: 2.16
 */
void hkl_error_prefix (HklError **err, const char *format, ...)
{
	if (err && *err){
		va_list ap;

		va_start (ap, format);
		hkl_error_add_prefix (&(*err)->message, format, ap);
		va_end (ap);
	}
}

/**
 * hkl_propagate_prefixed_error:
 * @dest: error return location
 * @src: error to move into the return location
 * @format: printf()-style format string
 * @...: arguments to @format
 *
 * If @dest is %NULL, free @src; otherwise,
 * moves @src into *@dest. *@dest must be %NULL.
 * After the move, add a prefix as with
 * hkl_prefix_error().
 *
 * Since: 2.16
 **/
void hkl_error_propagate_prefixed (HklError **dest, HklError *src, const char *format, ...)
{
	hkl_error_propagate (dest, src);

	if (dest && *dest){
		va_list ap;

		va_start (ap, format);
		hkl_error_add_prefix (&(*dest)->message, format, ap);
		va_end (ap);
	}
}
