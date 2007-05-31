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
      add(index);
      add(h);
      add(k);
      add(l);
      add(flag);
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
      add(name);
      add(a);
      add(b);
      add(c);
      add(alpha);
      add(beta);
      add(gamma);
      add(fitness);
    }
  };

class AxeModelColumns : public Gtk::TreeModel::ColumnRecord
  {
  public:
    Gtk::TreeModelColumn<hkl::Axe *> axe;
    Gtk::TreeModelColumn<Glib::ustring> name;
    Gtk::TreeModelColumn<double> read;
    Gtk::TreeModelColumn<double> write;
    Gtk::TreeModelColumn<double> min;
    Gtk::TreeModelColumn<double> max;

    AxeModelColumns()
    {
      add(axe);
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
    Gtk::TreeModelColumn<hkl::PseudoAxe *> pseudoAxe;
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
      add(pseudoAxe);
      add(name);
      add(read);
      add(write);
      add(min);
      add(max);
      add(is_initialized);
      add(is_readable);
      add(is_writable);
    }
  };
#endif // GTKMM_MODEL_COLUMNS_H
