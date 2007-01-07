#include "affinementlist.h"

using namespace std;

namespace hkl
  {

  AffinementList::~AffinementList(void)
  {
    clear();
  }

  void
  AffinementList::set_current(MyString const & name) throw (HKLException)
  {
    _current = operator[](name);
  }

  void
  AffinementList::add(Affinement * affinement) throw (HKLException)
    {
      //! @todo if the affinement already exist throw the exception.
      vector<Affinement *>::push_back(affinement);
    }

  void
  AffinementList::clear(void)
  {
    vector<Affinement *>::iterator iter = vector<Affinement *>::begin();
    vector<Affinement *>::iterator end = vector<Affinement *>::end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    vector<Affinement *>::clear();
  }

  Affinement *
  AffinementList::operator[](MyString const & name) throw (HKLException)
  {
    vector<Affinement *>::iterator iter = vector<Affinement *>::begin();
    vector<Affinement *>::iterator end = vector<Affinement *>::end();
    while(iter != end)
      {
        if ( (*iter)->get_name() == name )
          return *iter;
        ++iter;
      }
    ostringstream reason;
    ostringstream description;
    reason << "The Affinement named \"" << name << "\" does not exist.";

    if (vector<Affinement *>::size())
      {
        description << "Available affinements are:";

        iter = vector<Affinement *>::begin();
        while (iter != end)
          {
            description << "\"" << (*iter)->get_name() << "\" ";
            ++iter;
          }
      }
    else
      description << "No affinement available.";
    HKLEXCEPTION(reason.str(),
                 description.str());
  }

  bool
  AffinementList::operator ==(AffinementList const & affinementList) const
    {
      if (size() != affinementList.size())
        return false;
      else
        {
          vector<Affinement *>::const_iterator iter = vector<Affinement *>::begin();
          vector<Affinement *>::const_iterator end = vector<Affinement *>::end();
          vector<Affinement *>::const_iterator iter2 = affinementList.begin();
          while(iter != end)
            {
              if (!(**iter == **iter2))
                return false;
              ++iter;
              ++iter2;
            }
          return true;
        }
    }

  ostream &
  AffinementList::printToStream(ostream & flux) const
    {
      flux << " AffinementList : " << vector<Affinement *>::size() << endl;
      vector<Affinement *>::const_iterator iter = vector<Affinement *>::begin();
      vector<Affinement *>::const_iterator end = vector<Affinement *>::end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  AffinementList::toStream(ostream & flux) const
    {
      flux << " " << vector<Affinement *>::size();
      vector<Affinement *>::const_iterator iter = vector<Affinement *>::begin();
      vector<Affinement *>::const_iterator end = vector<Affinement *>::end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  AffinementList::fromStream(istream & flux)
  {
    unsigned int size;
    int type;
    flux >> size;
    vector<Affinement *>::iterator iter = vector<Affinement *>::begin();
    for(unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

} // namespace hkl
