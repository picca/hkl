#include "modelist.h"

using namespace std;

namespace hkl
  {

  ModeList::~ModeList(void)
  {
    _modes.clear();
  }

  void
  ModeList::add(Mode * mode) throw (HKLException)
    {
      _modes.push_back(mode);
    }

  vector<Mode *>::iterator
  ModeList::begin(void)
  {
    return _modes.begin();
  }

  vector<Mode *>::iterator
  ModeList::end(void)
  {
    return _modes.end();
  }

  void
  ModeList::erase(vector<Mode *>::iterator pos) throw (HKLException)
  {
    delete *pos;
    _modes.erase(pos);
  }

  void
  ModeList::clear(void)
  {
    vector<Mode *>::iterator iter = _modes.begin();
    vector<Mode *>::iterator end = _modes.end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    _modes.clear();
  }

  unsigned int
  ModeList::size(void) const
    {
      return _modes.size();
    }

  Mode *
  ModeList::operator[](MyString const & name) throw (HKLException)
  {
    vector<Mode *>::iterator iter = _modes.begin();
    vector<Mode *>::iterator end = _modes.end();
    while(iter != end)
      {
        if ( (*iter)->get_name() == name )
          return *iter;
        ++iter;
      }
    ostringstream reason;
    ostringstream description;
    reason << "The Mode named \"" << name << "\" does not exist.";

    if (_modes.size())
      {
        description << "Available modes are:";

        iter = _modes.begin();
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
          vector<Mode *>::const_iterator iter = _modes.begin();
          vector<Mode *>::const_iterator end = _modes.end();
          vector<Mode *>::const_iterator iter2 = modeList._modes.begin();
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
      flux << " ModeList : " << _modes.size() << endl;
      vector<Mode *>::const_iterator iter = _modes.begin();
      vector<Mode *>::const_iterator end = _modes.end();
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
      flux << " " << _modes.size();
      vector<Mode *>::const_iterator iter = _modes.begin();
      vector<Mode *>::const_iterator end = _modes.end();
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
    vector<Mode *>::iterator iter = _modes.begin();
    for(unsigned int i=0;i<size; i++)
      {
        (*iter)->fromStream(flux);
        ++iter;
      }
    return flux;
  }

} // namespace hkl
