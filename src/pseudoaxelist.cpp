#include "pseudoaxelist.h"

using namespace std;

namespace hkl
  {

  PseudoAxeList::~PseudoAxeList(void)
  {
    //clear();
  }

  void
  PseudoAxeList::add(PseudoAxe * pseudoAxe) throw (HKLException)
    {
      vector<PseudoAxe *>::push_back(pseudoAxe);
    }

  void
  PseudoAxeList::erase(vector<PseudoAxe *>::iterator pos) throw (HKLException)
  {
    delete *pos;
    vector<PseudoAxe *>::erase(pos);
  }

  void
  PseudoAxeList::clear(void)
  {
    vector<PseudoAxe *>::iterator iter = vector<PseudoAxe *>::begin();
    vector<PseudoAxe *>::iterator end = vector<PseudoAxe *>::end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    vector<PseudoAxe *>::clear();
  }

  PseudoAxe *
  PseudoAxeList::operator[](unsigned int index) throw (HKLException)
  {
    return vector<PseudoAxe *>::operator[](index);
  }

  PseudoAxe *
  PseudoAxeList::operator[](MyString const & name) throw (HKLException)
  {
    vector<PseudoAxe *>::iterator iter = vector<PseudoAxe *>::begin();
    vector<PseudoAxe *>::iterator end = vector<PseudoAxe *>::end();
    while(iter != end)
      {
        if ( (*iter)->get_name() == name )
          return *iter;
        ++iter;
      }
    ostringstream reason;
    ostringstream description;
    reason << "The PseudoAxe named \"" << name << "\" does not exist.";

    if (vector<PseudoAxe *>::size())
      {
        description << "Available pseudoAxes are:";

        iter = vector<PseudoAxe *>::begin();
        while (iter != end)
          {
            description << "\"" << (*iter)->get_name() << "\" ";
            ++iter;
          }
      }
    else
      description << "No pseudoAxe available.";
    HKLEXCEPTION(reason.str(),
                 description.str());
  }

  bool
  PseudoAxeList::operator ==(PseudoAxeList const & pseudoAxeList) const
    {
      if (size() != pseudoAxeList.size())
        return false;
      else
        {
          vector<PseudoAxe *>::const_iterator iter = vector<PseudoAxe *>::begin();
          vector<PseudoAxe *>::const_iterator end = vector<PseudoAxe *>::end();
          vector<PseudoAxe *>::const_iterator iter2 = pseudoAxeList.begin();
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
  PseudoAxeList::printToStream(ostream & flux) const
    {
      flux << " PseudoAxeList : " << vector<PseudoAxe *>::size() << endl;
      vector<PseudoAxe *>::const_iterator iter = vector<PseudoAxe *>::begin();
      vector<PseudoAxe *>::const_iterator end = vector<PseudoAxe *>::end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  PseudoAxeList::toStream(ostream & flux) const
    {
      flux << " " << vector<PseudoAxe *>::size();
      vector<PseudoAxe *>::const_iterator iter = vector<PseudoAxe *>::begin();
      vector<PseudoAxe *>::const_iterator end = vector<PseudoAxe *>::end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  PseudoAxeList::fromStream(istream & flux)
  {
    unsigned int size;
    int type;
    flux >> size;
    vector<PseudoAxe *>::iterator iter = vector<PseudoAxe *>::begin();
    for(unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

} // namespace hkl
