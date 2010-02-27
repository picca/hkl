#ifndef GTKMM_MODEL_COLUMNS_H
#define GTKMM_MODEL_COLUMNS_H

#include <gtkmm/treemodelcolumn.h>

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
	Gtk::TreeModelColumn<double> fitness;

	CrystalModelColumns()
	{
		this->add(name);
		this->add(a);
		this->add(b);
		this->add(c);
		this->add(alpha);
		this->add(beta);
		this->add(gamma);
		this->add(fitness);
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
	Gtk::TreeModelColumn<bool> is_readable;
	Gtk::TreeModelColumn<bool> is_writable;

	PseudoAxeModelColumns()
	{
		this->add(pseudoAxis);
		this->add(name);
		this->add(read);
		this->add(write);
		this->add(min);
		this->add(max);
		this->add(is_initialized);
		this->add(is_readable);
		this->add(is_writable);
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

#endif // GTKMM_MODEL_COLUMNS_H
