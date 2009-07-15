/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2009 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef __HKL_LIST_H__
#define __HKL_LIST_H__

#include <stdlib.h>
#include <hkl/hkl-macros.h>

HKL_BEGIN_DECLS

#define HKL_LIST_LEN(array) array ## _len

#define HKL_LIST(type, name) type *name; size_t HKL_LIST_LEN(name)

#define HKL_LIST_INIT(array) array = NULL, HKL_LIST_LEN(array) = 0

#define HKL_LIST_ALLOC(array, len) do{			\
		array = malloc((len) * sizeof(*array));	\
		HKL_LIST_LEN(array) = (len);		\
	}while(0)

#define HKL_LIST_COPY(dst, src) memcpy(dst, src, HKL_LIST_LEN(src) * sizeof(*src))

#define HKL_LIST_FREE(array) free(array), HKL_LIST_INIT(array)

#define HKL_LIST_FREE_DESTRUCTOR(array, destructor) do{		\
		if(HKL_LIST_LEN(array)){			\
			size_t i;				\
			for(i=0; i<HKL_LIST_LEN(array); ++i)	\
				destructor(array[i]);		\
		}						\
		HKL_LIST_FREE(array);				\
	}while(0)

#define HKL_LIST_RESIZE(array, len) do{				\
		array = realloc(array, (len) * sizeof(*array));	\
		HKL_LIST_LEN(array) = (len);			\
	}while(0)

#define HKL_LIST_ADD_VALUE(array, value) do{				\
		size_t len = HKL_LIST_LEN(array);			\
		HKL_LIST_RESIZE(array, len + 1);			\
		array[len] = value;					\
	}while(0)

#define HKL_LIST_DEL(array, idx) do{					\
		HKL_LIST_LEN(array) = HKL_LIST_LEN(array) - 1;		\
		if (idx < HKL_LIST_LEN(array))				\
			memmove(&array[idx], &array[idx] + 1, sizeof(*array) * (HKL_LIST_LEN(array) - idx)); \
	}while(0)

#define HKL_LIST_DEL_ITEM_DESTRUCTOR(array, item, destructor) do{	\
		size_t i;						\
		for(i=0; i<HKL_LIST_LEN(array); ++i)			\
			if(array[i] == item){				\
				destructor(array[i]);			\
				HKL_LIST_DEL(array, i);			\
			}						\
	}while(0)

HKL_END_DECLS

#endif
