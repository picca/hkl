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
 * Copyright (C) 2003-2017 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <stdio.h>

//#undef H5_USE_16_API

#include "hkl.h"
#include <hdf5.h>

#define ROOT "/nfs/ruche-sixs/sixs-soleil/com-sixs/2015/Shutdown4-5/XpadAu111/"
#define FILENAME "align_FLY2_omega_00045.nxs"

#define DATASET_IMAGE "com_113934/scan_data/xpad_image"
#define DATASET_MU "com_113934/scan_data/UHV_MU"
#define DATASET_OMEGA "com_113934/scan_data/UHV_OMEGA"
#define DATASET_DELTA "com_113934/scan_data/UHV_DELTA"
#define DATASET_GAMMA "com_113934/scan_data/UHV_GAMMA"
#define DATASET_UB "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/UB"
#define DATASET_WAVELENGTH "com_113934/SIXS/Monochromator/wavelength"
#define DATASET_DIFFRACTOMETER_TYPE "com_113934/SIXS/I14-C-CX2__EX__DIFF-UHV__#1/type"

/* Display all the node informations */

static herr_t attribute_info(hid_t location_id, const char *attr_name, const H5A_info_t *ainfo, void *op_data)
{
	printf("    Attribute: %d %s\n", location_id, attr_name);

	return 0;
}

static herr_t file_info(hid_t loc_id, const char *name, const H5L_info_t *info, void *opdata)
{
	H5O_info_t statbuf;
	hsize_t n = 0;

	/*
	 * Get type of the object and display its name and type.
	 * The name of the object is passed to this function by
	 * the Library. Some magic :-)
	 */
	H5Oget_info_by_name(loc_id, name, &statbuf, H5P_DEFAULT);
	switch (statbuf.type) {
	case H5O_TYPE_UNKNOWN:
		printf(" Object with name %s is an unknown type\n", name);
		break;
	case H5O_TYPE_GROUP:
		printf(" Object with name %s is a group\n", name);
		break;
	case H5O_TYPE_DATASET:
		printf(" Object with name %s is a dataset\n", name);
		break;
	case H5O_TYPE_NAMED_DATATYPE:
		printf(" Object with name %s is a named datatype\n", name);
		break;
	default:
		printf(" Unable to identify an object ");
	}

	H5Aiterate_by_name(loc_id,  name, H5_INDEX_NAME, H5_ITER_NATIVE, &n, attribute_info, NULL, H5P_DEFAULT);

	return 0;
}

typedef struct _HklDataframeSixsUhv HklDataframeSixsUhv;

struct _HklDataframeSixsUhv {
	hid_t file;
	hid_t mu;
	hid_t omega;
	hid_t delta;
	hid_t gamma;
	hid_t ub;
	hid_t wavelength;
	hid_t dtype;
};


static HklDataframeSixsUhv hkl_h5_open(const char *filename)
{
	/* herr_t status; */
	hid_t h5file_id;
	HklDataframeSixsUhv dataframe = {0};

	/* is it an hdf5 file */
	if (H5Fis_hdf5(filename) == 0)
		return dataframe;

	h5file_id = H5Fopen (filename, H5F_ACC_RDONLY, H5P_DEFAULT);
	if (h5file_id < 0)
		return dataframe;

	/* display all the node informations */
	/* status = H5Lvisit(h5file_id, H5_INDEX_CRT_ORDER, H5_ITER_NATIVE, file_info, NULL); */
	/* if(status < 0) */
	/* 	return dataframe; */

	dataframe.file = h5file_id;
	dataframe.mu = H5Dopen (h5file_id, DATASET_MU, H5P_DEFAULT);
	dataframe.omega = H5Dopen (h5file_id, DATASET_OMEGA, H5P_DEFAULT);
	dataframe.delta = H5Dopen (h5file_id, DATASET_DELTA, H5P_DEFAULT);
	dataframe.gamma = H5Dopen (h5file_id, DATASET_GAMMA, H5P_DEFAULT);

	dataframe.ub = H5Dopen (h5file_id, DATASET_UB, H5P_DEFAULT);
	dataframe.wavelength = H5Dopen (h5file_id, DATASET_WAVELENGTH, H5P_DEFAULT);
	dataframe.dtype = H5Dopen (h5file_id, DATASET_DIFFRACTOMETER_TYPE, H5P_DEFAULT);

	return dataframe;
}

static hssize_t check_ndims(hid_t dataset_id, int expected, hid_t *space_id)
{
	*space_id = H5Dget_space (dataset_id);
	return H5Sget_simple_extent_ndims (*space_id) == expected;
}

static int hkl_h5_len(const HklDataframeSixsUhv *dataframe)
{
	hid_t space_id = H5Dget_space (dataframe->mu);
	return H5Sget_simple_extent_npoints(space_id);
}


static int hkl_h5_is_valid(const HklDataframeSixsUhv *dataframe)
{
	int res = TRUE;
	hid_t space_id;
	hssize_t n;

	/* check the dimensionnality of the axes */
	res &= check_ndims(dataframe->mu, 1, &space_id);
	n = H5Sget_simple_extent_npoints(space_id);

	/* check the dimensionnality of all the axes */
	res &= check_ndims(dataframe->omega, 1, &space_id);
	res &= n == H5Sget_simple_extent_npoints(space_id);

	res &= check_ndims(dataframe->delta, 1, &space_id);
	res &= n == H5Sget_simple_extent_npoints(space_id);

	res &= check_ndims(dataframe->gamma, 1, &space_id);
	res &= n == H5Sget_simple_extent_npoints(space_id);

	return res;
}

static herr_t hkl_h5_close(const HklDataframeSixsUhv *dataframe)
{
	H5Dclose(dataframe->dtype);
	H5Dclose(dataframe->wavelength);
	H5Dclose(dataframe->ub);
	H5Dclose(dataframe->gamma);
	H5Dclose(dataframe->delta);
	H5Dclose(dataframe->omega);
	H5Dclose(dataframe->mu);

	return  H5Fclose (dataframe->file);
}

typedef struct _HklDataframe HklDataframe;

struct _HklDataframe {
	int i;
	int len;
	const HklDataframeSixsUhv *_dataframe;
};

static const HklDataframe hkl_dataframe_first(const HklDataframeSixsUhv *dataframe)
{
	HklDataframe frame;

	frame.i = 0;
	frame.len = hkl_h5_len(dataframe);
	frame._dataframe = dataframe;

	return frame;
}

static int hkl_dataframe_done(const HklDataframe dataframe)
{
	return dataframe.i < dataframe.len;
}

static const HklDataframe hkl_dataframe_next(const HklDataframe dataframe)
{
	HklDataframe frame = dataframe;

	frame.i++;

	return frame;
}


static herr_t get_position(hid_t dataset_id, int idx, double *position)
{
	hid_t space_id;
	hid_t mem_type_id;
	hid_t mem_space_id;
	hsize_t count[1];
	hsize_t offset[1];

	mem_type_id = H5Dget_type(dataset_id);

	/* Get the dataspace handle */
	if ( (space_id = H5Dget_space( dataset_id )) < 0 )
		goto out;

	/* Define a hyperslab in the dataset of the size of the records */
	offset[0] = idx;
	count[0]  = 1;
	if ( H5Sselect_hyperslab(space_id, H5S_SELECT_SET, offset, NULL, count, NULL) < 0 )
		goto out;

	/* Create a memory dataspace handle */
	if ( (mem_space_id = H5Screate_simple( 1, count, NULL )) < 0 )
		goto out;

	if ( H5Dread(dataset_id, mem_type_id, mem_space_id, space_id, H5P_DEFAULT, position ) < 0 )
		goto out;

	/* Terminate access to the memory dataspace */
	if ( H5Sclose( mem_space_id ) < 0 )
		goto out;

	/* Terminate access to the dataspace */
	if ( H5Sclose( space_id ) < 0 )
		goto out;

	return 0;
out:
	H5Tclose(mem_type_id);
	return -1;
}

static herr_t hkl_dataframe_geometry_get(const HklDataframe dataframe, HklGeometry **geometry)
{
	herr_t status = 0;
	hid_t datatype;
	double wavelength;
	double axes[4];

	/* create the HklGeometry */
	if((*geometry) == NULL){
		char *name;
		size_t n;
		HklFactory *factory;

		/* read the diffractometer type from the hdf5 file */
		datatype = H5Dget_type(dataframe._dataframe->dtype);
		n = H5Tget_size(datatype);
		name = malloc(n+1);
		status = H5Dread(dataframe._dataframe->dtype,
				 datatype,
				 H5S_ALL, H5S_ALL,
				 H5P_DEFAULT, name);
		if(status >= 0){
			/* remove the last "\n" char */
			name[n-1] = 0;

			factory = hkl_factory_get_by_name(name, NULL);
			*geometry = hkl_factory_create_new_geometry(factory);
		}
		free(name);
		H5Tclose(datatype);
	}

	/* read the wavelength double */
	/* TODO check the right size */
	/* TODO how to obtain the unit of the  wavelength */
	datatype = H5Dget_type(dataframe._dataframe->wavelength);
	status = H5Dread(dataframe._dataframe->wavelength,
			 datatype,
			 H5S_ALL, H5S_ALL,
			 H5P_DEFAULT, &wavelength);
	if(status >= 0)
		hkl_geometry_wavelength_set(*geometry, wavelength, HKL_UNIT_USER, NULL);
	H5Tclose(datatype);

	/* read the axis positions of the ith dataframe */
	/* check how to decide about the dataset connection and the hkl axes connection */
	/* TODO check the right size */
	/* TODO how to obtain the unit of the axes position */
	if (get_position(dataframe._dataframe->mu,
			 dataframe.i, &axes[0]) < 0)
		goto out;
	if (get_position(dataframe._dataframe->omega,
			 dataframe.i, &axes[1]) < 0)
		goto out;
	if (get_position(dataframe._dataframe->gamma,
			 dataframe.i, &axes[2]) < 0)
		goto out;
	if (get_position(dataframe._dataframe->delta,
			 dataframe.i, &axes[3]) < 0)
		goto out;

	hkl_geometry_axis_values_set(*geometry, axes, 4, HKL_UNIT_USER, NULL);
	/* hkl_geometry_fprintf(stdout, *geometry); */
	/* fprintf(stdout, "\n"); */

	return 0;
out:
	return -1;

}

int main (int argc, char ** argv)
{
	const char *filename =  ROOT FILENAME;
	HklDataframeSixsUhv dataframe_h5 = hkl_h5_open(filename);
	int res = hkl_h5_is_valid(&dataframe_h5);
	/* fprintf(stdout, "h5file is valid : %d\n", res); */
	HklGeometry *geometry = NULL;

	for(HklDataframe dataframe =  hkl_dataframe_first(&dataframe_h5);
	    hkl_dataframe_done(dataframe);
	    dataframe = hkl_dataframe_next(dataframe))
	{
		hkl_dataframe_geometry_get(dataframe, &geometry);
		/* fprintf(stdout, " %d", dataframe.i); */
	}

	hkl_geometry_free(geometry);
	hkl_h5_close(&dataframe_h5);

	return res;
}
