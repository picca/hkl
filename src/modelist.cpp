#include "modelist.h"

using namespace std;

namespace hkl
  {

  ModeList::~ModeList(void)
  {
    clear();
  }

  void
  ModeList::set_current(string const & name) throw (HKLException)
  {
    _current = operator[](name);
  }

  void
  ModeList::add(Mode * mode) throw (HKLException)
    {
      //! @todo if the mode already exist throw the exception.
      vector<Mode *>::push_back(mode);
    }

  void
  ModeList::clear(void)
  {
    vector<Mode *>::iterator iter = vector<Mode *>::begin();
    vector<Mode *>::iterator end = vector<Mode *>::end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    vector<Mode *>::clear();
  }

  Mode *
  ModeList::operator[](string const & name) throw (HKLException)
  {
    vector<Mode *>::iterator iter = vector<Mode *>::begin();
    vector<Mode *>::iterator end = vector<Mode *>::end();
    while(iter != end)
      {
        if ( (*iter)->get_name() == name )
          return *iter;
        ++iter;
      }
    ostringstream reason;
    ostringstream description;
    reason << "The Mode named \"" << name << "\" does not exist.";

    if (vector<Mode *>::size())
      {
        description << "Available modes are:";

        iter = vector<Mode *>::begin();
        while (iter != end)
          {
            description << "\"" << (*iter)->get_name() << "\" ";
            ++iter;
          }
      }
    else
      description << "No mode available.";
    HKLEXCEPTION(reason.str(),
                 description.str());
  }

  bool
  ModeList::operator ==(ModeList const & modeList) const
    {
      if (size() != modeList.size())
        return false;
      else
        {
          vector<Mode *>::const_iterator iter = vector<Mode *>::begin();
          vector<Mode *>::const_iterator end = vector<Mode *>::end();
          vector<Mode *>::const_iterator iter2 = modeList.begin();
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
  ModeList::printToStream(ostream & flux) const
    {
      flux << " ModeList : " << vector<Mode *>::size() << endl;
      vector<Mode *>::const_iterator iter = vector<Mode *>::begin();
      vector<Mode *>::const_iterator end = vector<Mode *>::end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  ModeList::toStream(ostream & flux) const
    {
      flux << " " << vector<Mode *>::size();
      vector<Mode *>::const_iterator iter = vector<Mode *>::begin();
      vector<Mode *>::const_iterator end = vector<Mode *>::end();
      while(iter != end)
        {
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  ModeList::fromStream(istream & flux)
  {
    unsigned int size;
    int type;
    flux >> size;
    vector<Mode *>::iterator iter = vector<Mode *>::begin();
    for(unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

} // namespace hkl
