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
 * Copyright (C) 2003-2010 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#ifndef GTKMM_MODEL_COLUMNS_H
#define GTKMM_MODEL_COLUMNS_H

#include <gtkmm/treemodelcolumn.h>
#include "hkl/hkl-geometry.h"

class ReflectionModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<unsigned int> index;
	Gtk::TreeModelColumn<double> h;
	Gtk::TreeModelColumn<double> k;
	Gtk::TreeModelColumn<double> l;
	Gtk::TreeModelColumn<bool> flag;

	ReflectionModelColumns()
	{
		this->add(index);
		this->add(h);
		this->add(k);
		this->add(l);
		this->add(flag);
	}
};

class CrystalModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<double> a;
	Gtk::TreeModelColumn<double> b;
	Gtk::TreeModelColumn<double> c;
	Gtk::TreeModelColumn<double> alpha;
	Gtk::TreeModelColumn<double> beta;
	Gtk::TreeModelColumn<double> gamma;

	CrystalModelColumns()
	{
		this->add(name);
		this->add(a);
		this->add(b);
		this->add(c);
		this->add(alpha);
		this->add(beta);
		this->add(gamma);
	}
};

class AxeModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<HklAxis *> axis;
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<double> read;
	Gtk::TreeModelColumn<double> write;
	Gtk::TreeModelColumn<double> min;
	Gtk::TreeModelColumn<double> max;

	AxeModelColumns()
	{
		add(axis);
		add(name);
		add(read);
		add(write);
		add(min);
		add(max);
	}
};

class PseudoAxeModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<HklPseudoAxis *> pseudoAxis;
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<double> read;
	Gtk::TreeModelColumn<double> write;
	Gtk::TreeModelColumn<double> min;
	Gtk::TreeModelColumn<double> max;
	Gtk::TreeModelColumn<bool> is_initialized;

	PseudoAxeModelColumns()
	{
		this->add(pseudoAxis);
		this->add(name);
		this->add(read);
		this->add(write);
		this->add(min);
		this->add(max);
		this->add(is_initialized);
	}
};

class ParameterModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<HklParameter *> parameter;
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<double> value;

	ParameterModelColumns()
	{
		this->add(parameter);
		this->add(name);
		this->add(value);
	}
};

class SolutionModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<gint> index;
	std::vector<Gtk::TreeModelColumn<gdouble> > axes;

	SolutionModelColumns(HklGeometry *geometry)
	{
		size_t i;

		this->add(this->index);

		for(i=0; i<HKL_LIST_LEN(geometry->axes); ++i){
			this->axes.push_back(Gtk::TreeModelColumn<gdouble>());
			this->add(this->axes[i]);
		}
	}
};

class DiffractometerModelColumns : public Gtk::TreeModel::ColumnRecord
{
public:
	Gtk::TreeModelColumn<Glib::ustring> name;
	Gtk::TreeModelColumn<gint> type;

	DiffractometerModelColumns(void)
	{
		this->add(name);
		this->add(type);
	}
};

#endif // GTKMM_MODEL_COLUMNS_H
