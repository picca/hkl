#include "samplelist.h"

using namespace std;

namespace hkl
  {

  SampleList::SampleList(Geometry & geometry) :
      _geometry(geometry),
      _current(NULL)
  {
    _samplefactory = new SampleFactory(geometry);
  }

//!< @todo gerer lors de la copie le _current.
  SampleList::SampleList(SampleList const & sampleList) :
      _geometry(sampleList._geometry)
  {
    _samplefactory = new SampleFactory(_geometry);

    vector<Sample *>::const_iterator iter = sampleList.begin();
    vector<Sample *>::const_iterator end = sampleList.end();
    while(iter != end)
      {
        vector<Sample *>::push_back((*iter)->clone());
        ++iter;
      }
  }

  SampleList::~SampleList(void)
  {
    vector<Sample *>::iterator iter = vector<Sample *>::begin();
    vector<Sample *>::iterator end = vector<Sample *>::end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    vector<Sample *>::clear();

    delete _samplefactory;
  }

  vector<SampleType>
  SampleList::types(void) const
    {
      return _samplefactory->types();
    }

  void
  SampleList::add(MyString const & name, SampleType type) throw (HKLException)
    {
      vector<Sample *>::push_back(_samplefactory->create(name, type));
    }

  void
  SampleList::add_copy(vector<Sample *>::iterator pos)
  {
    Sample * sample_to_copy = *pos;
    Sample * copy = sample_to_copy->clone();
    MyString name = sample_to_copy->get_name();
    name += "_copy";
    copy->set_name(name);
    vector<Sample *>::push_back(copy);
  }

  void
  SampleList::erase(vector<Sample *>::iterator pos) throw (HKLException)
  {
    delete *pos;
    vector<Sample *>::erase(pos);
  }

  void
  SampleList::clear(void)
  {
    vector<Sample *>::iterator iter = vector<Sample *>::begin();
    vector<Sample *>::iterator end = vector<Sample *>::end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    vector<Sample *>::clear();
  }

  void
  SampleList::set_current(unsigned int index) throw (HKLException)
  {
    if (index < size())
      _current = vector<Sample *>::operator[](index);
    else
      HKLEXCEPTION("index out of bounds", "set a correct index");
  }


  void
  SampleList::set_current(MyString const & name) throw (HKLException)
  {
    //! @todo maybe remove this method.
    vector<Sample *>::iterator iter = vector<Sample *>::begin();
    vector<Sample *>::iterator end = vector<Sample *>::end();
    while(iter != end)
      {
        if ((*iter)->get_name() == name)
          {
            _current = *iter;
            return;
          }
        ++iter;
      }
    HKLEXCEPTION("Cannot find the sample", "set a correct name");
  }

  Sample *
  SampleList::current(void) throw (HKLException)
  {
    if (_current)
      return _current;
    else
      HKLEXCEPTION("current sample not yet set", "please use set_current to select a sample");
  }

  bool
  SampleList::operator ==(SampleList const & sampleList) const
    {
      if (size() != sampleList.size())
        return false;
      else
        {
          vector<Sample *>::const_iterator iter = vector<Sample *>::begin();
          vector<Sample *>::const_iterator end = vector<Sample *>::end();
          vector<Sample *>::const_iterator iter2 = sampleList.begin();
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
  SampleList::printToStream(ostream & flux) const
    {
      flux << " SampleList : " << vector<Sample *>::size() << endl;
      vector<Sample *>::const_iterator iter = vector<Sample *>::begin();
      vector<Sample *>::const_iterator end = vector<Sample *>::end();
      while(iter != end)
        {
          (*iter)->printToStream(flux);
          ++iter;
        }
      return flux;
    }

  ostream &
  SampleList::toStream(ostream & flux) const
    {
      flux << " " << vector<Sample *>::size();
      vector<Sample *>::const_iterator iter = vector<Sample *>::begin();
      vector<Sample *>::const_iterator end = vector<Sample *>::end();
      while(iter != end)
        {
          flux << " " << (*iter)->type();
          (*iter)->toStream(flux);
          ++iter;
        }
      return flux;
    }

  istream &
  SampleList::fromStream(istream & flux)
  {
    unsigned int size;
    int type;
    flux >> size;
    for(unsigned int i=0;i<size; i++)
      {
        flux >> type;
        Sample * sample = _samplefactory->create("fromstream", (SampleType)type);
        sample->fromStream(flux);
        vector<Sample *>::push_back(sample);
      }
    return flux;
  }

} // namespace hkl
