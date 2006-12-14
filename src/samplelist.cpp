#include "samplelist.h"

using namespace std;

namespace hkl
  {

  SampleList::SampleList(Geometry & geometry) :
      _geometry(geometry)
  {
    _samplefactory = new SampleFactory(geometry);
  }

  SampleList::SampleList(SampleList const & sampleList) :
      _geometry(sampleList._geometry)
  {
    _samplefactory = new SampleFactory(_geometry);

    vector<Sample *>::const_iterator iter = sampleList._samples.begin();
    vector<Sample *>::const_iterator end = sampleList._samples.end();
    while(iter != end)
      {
        _samples.push_back((*iter)->clone());
        ++iter;
      }
  }

  SampleList::~SampleList(void)
  {
    vector<Sample *>::iterator iter = _samples.begin();
    vector<Sample *>::iterator end = _samples.end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    _samples.clear();

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
      _samples.push_back(_samplefactory->create(name, type));
    }

  vector<Sample *>::iterator
  SampleList::begin(void)
  {
    return _samples.begin();
  }

  vector<Sample *>::iterator
  SampleList::end(void)
  {
    return _samples.end();
  }

  void
  SampleList::erase(vector<Sample *>::iterator pos) throw (HKLException)
  {
    delete *pos;
    _samples.erase(pos);
  }

  void
  SampleList::clear(void)
  {
    vector<Sample *>::iterator iter = _samples.begin();
    vector<Sample *>::iterator end = _samples.end();
    while(iter != end)
      {
        delete *iter;
        ++iter;
      }
    _samples.clear();
  }

  unsigned int
  SampleList::size(void) const
    {
      return _samples.size();
    }

  Sample *
  SampleList::operator[](unsigned int index) throw (HKLException)
  {
    if (index < size())
      return _samples[index];
    else
      HKLEXCEPTION("index out of bounds", "set a correct index");
  }

  bool
  SampleList::operator ==(SampleList const & sampleList) const
    {
      if (size() != sampleList.size())
        return false;
      else
        {
          vector<Sample *>::const_iterator iter = _samples.begin();
          vector<Sample *>::const_iterator end = _samples.end();
          vector<Sample *>::const_iterator iter2 = sampleList._samples.begin();
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
      flux << " SampleList : " << _samples.size() << endl;
      vector<Sample *>::const_iterator iter = _samples.begin();
      vector<Sample *>::const_iterator end = _samples.end();
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
      flux << " " << _samples.size();
      vector<Sample *>::const_iterator iter = _samples.begin();
      vector<Sample *>::const_iterator end = _samples.end();
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
        _samples.push_back(sample);
      }
    return flux;
  }

} // namespace hkl
