#include "reflectionlist.h"

using namespace std;

namespace hkl {

    ReflectionList::ReflectionList(Geometry & geometry, ReflectionType const & type) :
      _geometry(geometry)
    {
      _reflectionFactory = new ReflectionFactory(_geometry, type);
    }

    ReflectionList::ReflectionList(ReflectionList const & factory) :
      _geometry(factory._geometry),
      _reflectionFactory(factory._reflectionFactory)
    {
      _reflectionFactory = new ReflectionFactory(*(factory._reflectionFactory));

      vector<Reflection *>::const_iterator iter = factory._reflections.begin();
      vector<Reflection *>::const_iterator end = factory._reflections.end();
      while(iter != end)
        {
          _reflections.push_back((*iter)->clone());
          ++iter;
        }
    }

    ReflectionList::~ReflectionList(void)
      {
        delete _reflectionFactory;

        vector<Reflection *>::iterator iter = _reflections.begin();
        vector<Reflection *>::iterator end = _reflections.end();
        while(iter != end)
          {
            delete *iter;
            ++iter;
          }
      }

    ReflectionList *
    ReflectionList::clone(void)
      {
        return new ReflectionList(*this);
      }

    Reflection &
    ReflectionList::add(Value const & h, Value const & k, Value const & l)
      {
        Reflection * reflection = _reflectionFactory->create();
        reflection->h() = h;
        reflection->k() = k;
        reflection->l() = l;

        // When trying to add an active reflection, check that the reflection is not already in.
        // if already in change the flag to false.
        if (reflection->flag())
          {
            vector<Reflection *>::iterator iter = _reflections.begin();
            vector<Reflection *>::iterator end = _reflections.end();
            while(iter != end)
              {
                if (fabs(h - (*iter)->h()) < constant::math::epsilon_0
                    && fabs(k - (*iter)->k()) < constant::math::epsilon_0
                    && fabs(l - (*iter)->l()) < constant::math::epsilon_0)
                  {
                    reflection->flag() = false;
                  }
                ++iter;
              }
          } 

        // add the reflection
        _reflections.push_back(reflection);

        return *reflection;
      }

    void
    ReflectionList::del(unsigned int index) throw (HKLException)
      {
        unsigned int nb_reflection = _reflections.size();

        if (index >= nb_reflection)
          {
            ostringstream reason;
            ostringstream description;

            reason << "Can not delete the reflection : " << index;
            if (nb_reflection)
                description << "Index out of range, the maximum index is : " << nb_reflection-1;
            else
                description << "There is no reflection to delete.";

            HKLEXCEPTION(reason.str(), description.str());
          }
        else
          {
            vector<Reflection *>::iterator iter = _reflections.begin();
            for(unsigned int i=0;i<index;i++)
                ++iter;
            delete *iter;
            _reflections.erase(iter);
          }
      }

    unsigned int
    ReflectionList::size(void) const
      {
        return _reflections.size();
      }

    unsigned int
    ReflectionList::size_indep(void) const
      {
        unsigned int nb_usable_reflections = 0;
        vector<Reflection *>::const_iterator iter = _reflections.begin();
        vector<Reflection *>::const_iterator iter2 = _reflections.begin();
        vector<Reflection *>::const_iterator end = _reflections.end();

        while(iter < end)
          {
            if ((*iter)->flag())
              {
                if (nb_usable_reflections == 0)
                    nb_usable_reflections = 1;
                iter2 = iter;
                ++iter2;
                while(iter2 < end)
                  {
                    if ((*iter2)->flag() && !(*iter)->isColinear(**iter2))
                        nb_usable_reflections++;
                    ++iter2;
                  }
              }
            ++iter;
          }
        return nb_usable_reflections;
      }

    Reflection &
    ReflectionList::operator[](unsigned int index) throw (HKLException)
      {
        unsigned int nb_reflection = _reflections.size();

        if (index >= nb_reflection)
          {
            ostringstream reason;
            ostringstream description;

            reason << "Index of the reflection is out of range : " << index;
            if (nb_reflection > 1)
              description << "The maximum index is : " << nb_reflection-1;
            else
              description << "No reflection in the ReflectionList";

            HKLEXCEPTION(reason.str(), description.str());
          }
        else
            return *_reflections[index];
      }

    bool
    ReflectionList::operator ==(ReflectionList const & reflectionListFactory) const
      {
        if (!(_geometry == reflectionListFactory._geometry))
            return false;

        if (_reflections.size() != reflectionListFactory._reflections.size())
            return false;
        else
          {
            vector<Reflection *>::const_iterator iter = _reflections.begin();
            vector<Reflection *>::const_iterator end = _reflections.end();
            vector<Reflection *>::const_iterator iter2 = reflectionListFactory._reflections.begin();
            while(iter != end)
              {
                if (!(**iter == **iter2))
                    return false;
                ++iter;
                ++iter2;
              }
          }
        return true;
      }

    ostream &
    ReflectionList::printToStream(ostream & flux) const
      {
        _geometry.printToStream(flux);

        flux << _reflections.size() << " reflection(s)" << endl;
        vector<Reflection *>::const_iterator iter = _reflections.begin();
        vector<Reflection *>::const_iterator end = _reflections.end();
        while(iter != end)
          {
            (*iter)->printToStream(flux);
            flux << endl;
            ++iter;
          }
        return flux;
      }

    ostream &
    ReflectionList::toStream(ostream & flux) const
      {
        unsigned int nb_reflections = _reflections.size();

        flux << nb_reflections << endl;
        for(unsigned int i=0;i<nb_reflections;i++)
            _reflections[i]->toStream(flux);
        return flux;
      }

    istream &
    ReflectionList::fromStream(istream & flux)
      {
        unsigned int nb_reflections = _reflections.size();
        if ( nb_reflections )
          {
            vector<Reflection *>::iterator iter = _reflections.begin();
            vector<Reflection *>::iterator end = _reflections.end();
            while(iter != end)
              {
                delete *iter;
              }
            _reflections.clear();
          }

        flux >> nb_reflections;
        for(unsigned int i=0; i< nb_reflections; i++)
          {
            Reflection * reflection = _reflectionFactory->create();
            reflection->fromStream(flux);
            _reflections.push_back(reflection);
          }
        return flux;
      }

} // namespace hkl
