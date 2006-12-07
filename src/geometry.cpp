#include "geometry.h"
namespace hkl
  {

  Geometry::Geometry(MyString const & name, MyString const & description)
      : HKLObject(name, description)
  {}

  Geometry::Geometry(Geometry const & geometry) :
      HKLObject(geometry),
      _source(geometry._source),
      _axes(geometry._axes)
  {
    AxeMap::const_iterator AxeMap_iter = geometry._axes.begin();
    AxeMap::const_iterator AxeMap_end = geometry._axes.end();

    // update the _sample and _detector menmbers
    _sample.clear();
    _detector.clear();

    vector<Axe const *>::const_iterator AxeVector_iter = geometry._sample.begin();
    vector<Axe const *>::const_iterator AxeVector_end = geometry._sample.end();
    while(AxeVector_iter != AxeVector_end)
      {
        MyString const & name = (*AxeVector_iter)->get_name();
        AxeMap_iter = _axes.find(name);
        if (AxeMap_iter != AxeMap_end)
          _sample.push_back(&(AxeMap_iter->second));
        ++AxeVector_iter;
      }

    AxeVector_iter = geometry._detector.begin();
    AxeVector_end = geometry._detector.end();
    while(AxeVector_iter != AxeVector_end)
      {
        MyString const & name = (*AxeVector_iter)->get_name();
        AxeMap_iter = _axes.find(name);
        if (AxeMap_iter != AxeMap_end)
          _detector.push_back(&(AxeMap_iter->second));
        ++AxeVector_iter;
      }
  }

  Geometry::~Geometry(void)
{}

  Geometry &
  Geometry::operator=(Geometry const & geometry)
  {
    HKLObject::operator=(geometry);
    _source = geometry._source;

    // now make a deep copy of _samples and _detector
    _axes = geometry._axes;

    // update the _sample and _detector AxeVector
    AxeMap::const_iterator AxeMap_iter = geometry._axes.begin();
    AxeMap::const_iterator AxeMap_end = geometry._axes.end();

    _sample.clear();
    _detector.clear();

    vector<Axe const *>::const_iterator AxeVector_iter = geometry._sample.begin();
    vector<Axe const *>::const_iterator AxeVector_end = geometry._sample.end();
    while(AxeVector_iter != AxeVector_end)
      {
        MyString const & name = (*AxeVector_iter)->get_name();
        AxeMap_iter = _axes.find(name);
        if (AxeMap_iter != AxeMap_end)
          _sample.push_back(&(AxeMap_iter->second));
        ++AxeVector_iter;
      }

    AxeVector_iter = geometry._detector.begin();
    AxeVector_end = geometry._detector.end();
    while(AxeVector_iter != AxeVector_end)
      {
        MyString const & name = (*AxeVector_iter)->get_name();
        AxeMap_iter = _axes.find(name);
        if (AxeMap_iter != AxeMap_end)
          _detector.push_back(&(AxeMap_iter->second));
        ++AxeVector_iter;
      }
    return *this;
  }

  bool
  Geometry::operator==(Geometry const & geometry) const
    {
      return HKLObject::operator==(geometry)
             && _source == geometry._source
             && _axes == geometry._axes;
    }

  ostream &
  Geometry::printToStream(ostream & flux) const
    {
      int nb_axes = _sample.size();
      int i;

      flux.precision(3);
      flux << "  Source: " << _source.get_waveLength()
      << ", " << _source.get_direction() << endl;
      //samples
      flux << "  Samples: (" << nb_axes << ")" << endl;
      vector<Axe const *>::const_iterator it = _sample.begin();
      vector<Axe const *>::const_iterator end = _sample.end();
      while(it != end)
        {
          Axe const & axe = **it;
          flux.width(12);
          flux << axe.get_name();
          flux << ": " << axe.get_axe();
          flux << "(" << showpos << axe.get_direction() << ")";
          flux.unsetf(ios_base::showpos);
          flux << "  " << axe.get_current().get_value()*constant::math::radToDeg;
          flux << endl;
          ++it;
        }

      //detector
      nb_axes = _detector.size();
      flux << "  Detectors: (" << nb_axes << ")" << endl;
      it = _detector.begin();
      end = _detector.end();
      while(it != end)
        {
          Axe const & axe = **it;
          flux.width(12);
          flux << axe.get_name();
          flux << ": " << axe.get_axe();
          flux << "(" << showpos << axe.get_direction() << ")";
          flux.unsetf(ios_base::showpos);
          flux << "  " << axe.get_current().get_value()*constant::math::radToDeg;
          flux << endl;
          ++it;
        }

      return flux;
    }

  vector<MyString> const
  Geometry::getAxesNames(void) const
    {
      vector<MyString> nameList;

      // sample part
      vector<Axe const *>::const_iterator it = _sample.begin();
      vector<Axe const *>::const_iterator end = _sample.end();
      while(it != end)
        {
          nameList.push_back((*it)->get_name());
          ++it;
        }

      // detector part
      it = _detector.begin();
      end = _detector.end();
      while(it != end)
        {
          nameList.push_back((*it)->get_name());
          ++it;
        }

      return nameList;
    }

  Axe &
  Geometry::get_axe(MyString const & name) throw (HKLException)
  {
    return _axes[name];
  }

  Axe const &
  Geometry::get_axe(MyString const & name) const throw (HKLException)
  {
    return _axes[name];
  }

  Axe *
  Geometry::addSampleAxe(Axe const & axe) throw (HKLException)
  {
    MyString const & name = axe.get_name();

    //Est-ce que cet axe est déjà présent dans la liste?
    vector<Axe const *>::iterator sample_iter = _sample.begin();
    vector<Axe const *>::iterator sample_end = _sample.end();
    while(sample_iter != sample_end)
      {
        if ((*sample_iter)->get_name() == name)
          {
            ostringstream description;
            description << "The axe \"" << name << "\" is already present in the sample axe list";
            HKLEXCEPTION("Can not add two times the same axe",
                         description.str());
          }
        else
          ++sample_iter;
      }

    AxeMap::iterator iter = _axes.find(name);
    AxeMap::iterator end = _axes.end();
    if (iter == end)
      {
        pair<AxeMap::iterator, bool> res = _axes.insert(AxeMap::value_type(name, axe));
        Axe & stored_axe = res.first->second;
        _sample.push_back(&stored_axe);
        return &stored_axe;
      }
    else
      {
        if (iter->second == axe)
          {
            _sample.push_back(&iter->second);
            return &iter->second;
          }
        else
          {
            ostringstream description;
            description << "Same name but different axe." << endl
            << "Axe1 : ";
            iter->second.printToStream(description);
            description << "Axe2 : ";
            axe.printToStream(description);
            HKLEXCEPTION("Can not add this axe \"Axe2\" to the sample axe list",
                         description.str());
          }
      }
  }

  Axe *
  Geometry::addDetectorAxe(Axe const & axe) throw (HKLException)
  {
    MyString const & name = axe.get_name();

    //Est-ce que cet axe est deja present dans la liste?
    vector<Axe const *>::iterator detector_iter = _detector.begin();
    vector<Axe const *>::iterator detector_end = _detector.end();
    while(detector_iter != detector_end)
      {
        if ((*detector_iter)->get_name() == name)
          {
            ostringstream description;
            description << "The axe \"" << name << "\" is already present in the detector axe list";
            HKLEXCEPTION("Can not add two times the same axe",
                         description.str());
          }
        detector_iter++;
      }

    AxeMap::iterator iter = _axes.find(name);
    AxeMap::iterator end = _axes.end();
    if (iter == end)
      {
        pair<AxeMap::iterator, bool> res = _axes.insert(AxeMap::value_type(name, axe));
        Axe & stored_axe = res.first->second;
        _detector.push_back(&stored_axe);
        return &stored_axe;
      }
    else
      {
        if (iter->second == axe)
          {
            _detector.push_back(&iter->second);
            return &iter->second;
          }
        else
          {
            ostringstream description;
            description << "Same name but different axe." << endl
            << "Axe1 : ";
            iter->second.printToStream(description);
            description << "Axe2 : ";
            axe.printToStream(description);
            HKLEXCEPTION("Can not add this axe \"Axe2\" to the detector axe list",
                         description.str());
          }
      }
  }

  Quaternion
  Geometry::getSampleQuaternion(void) const
    {
      Quaternion q;

      vector<Axe const *>::const_iterator iter = _sample.begin();
      vector<Axe const *>::const_iterator end = _sample.end();
      while (iter != end)
        {
          q *= (*iter)->asQuaternion();
          ++iter;
        }

      return q;
    }

  smatrix
  Geometry::getSampleRotationMatrix(void) const
    {
      return getSampleQuaternion().asMatrix();
    }

  svector
  Geometry::getQ(void) const
    {
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();

      vector<Axe const *>::const_iterator iter = _detector.begin();
      vector<Axe const *>::const_iterator end = _detector.end();
      while (iter != end)
        {
          qr *= (*iter)->asQuaternion();
          ++iter;
        }

      Quaternion q(qr);
      q *= qi;
      q *= qr.conjugate();
      q -= qi;

      return svector(q[1], q[2], q[3]);
    }

  svector
  Geometry::getKf(void) const
    {
      // Attention pour l'instant qf est obtenu a partir de qi
      // il faudrait prendre 1, 0, 0 comme référence.
      Quaternion qr;
      Quaternion const & qi = _source.get_qi();

      vector<Axe const *>::const_iterator iter = _detector.begin();
      vector<Axe const *>::const_iterator end = _detector.end();
      while (iter != end)
        {
          qr *= (*iter)->asQuaternion();
          ++iter;
        }

      Quaternion q(qr);
      q *= qi;
      q *= (qr.conjugate());

      return svector(q[1], q[2], q[3]);
    }

  double
  Geometry::getDistance(Geometry const & geometry) throw (HKLException)
  {
    double distance = 0;
    AxeMap::const_iterator iter1 = _axes.begin();
    AxeMap::const_iterator end = _axes.end();
    AxeMap::const_iterator iter2 = geometry._axes.begin();
    while(iter1 != end)
      {
        distance += iter1->second.getDistance(iter2->second);
        ++iter1;
        ++iter2;
      }
    return distance;
  }

  void
  Geometry::computeHKL(double & h, double & k, double & l, smatrix const & UB) throw (HKLException)
  {
    smatrix R = getSampleRotationMatrix() * UB;

    double det;

    det  =  R.get(0,0)*(R.get(1,1)*R.get(2,2)-R.get(2,1)*R.get(1,2));
    det += -R.get(0,1)*(R.get(1,0)*R.get(2,2)-R.get(2,0)*R.get(1,2));
    det +=  R.get(0,2)*(R.get(1,0)*R.get(2,1)-R.get(2,0)*R.get(1,1));

    if (fabs(det) < constant::math::epsilon_1)
      HKLEXCEPTION("det(R) is null",
                   "La matrice rotation de la machine n'est pas valide");
    else
      {

        svector q = getQ();

        double sum;

        sum =   q.x() * (R.get(1,1)*R.get(2,2)-R.get(1,2)*R.get(2,1));
        sum += -q.y() * (R.get(0,1)*R.get(2,2)-R.get(0,2)*R.get(2,1));
        sum +=  q.z() * (R.get(0,1)*R.get(1,2)-R.get(0,2)*R.get(1,1));
        h = sum / det;

        sum =  -q.x() * (R.get(1,0)*R.get(2,2)-R.get(1,2)*R.get(2,0));
        sum +=  q.y() * (R.get(0,0)*R.get(2,2)-R.get(0,2)*R.get(2,0));
        sum += -q.z() * (R.get(0,0)*R.get(1,2)-R.get(0,2)*R.get(1,0));
        k = sum / det;

        sum =   q.x() * (R.get(1,0)*R.get(2,1)-R.get(1,1)*R.get(2,0));
        sum += -q.y() * (R.get(0,0)*R.get(2,1)-R.get(0,1)*R.get(2,0));
        sum +=  q.z() * (R.get(0,0)*R.get(1,1)-R.get(0,1)*R.get(1,0));
        l = sum / det;
      }

  }

  //!< @todo Geometry must be an abstract class
  void
  Geometry::setFromGeometry(Geometry const & geometry, bool const & strict) throw (HKLException)
  {}

  ostream &
  Geometry::toStream(ostream & flux) const
    {
      HKLObject::toStream(flux);
      _source.toStream(flux);
      _axes.toStream(flux);
      return flux;
    }

  istream &
  Geometry::fromStream(istream & flux)
  {
    HKLObject::fromStream(flux);
    _source.fromStream(flux);
    _axes.fromStream(flux);
    return flux;
  }

} // namespace hkl

std::ostream & operator<< (std::ostream & flux, hkl::Geometry const & geometry)
{
  return geometry.printToStream(flux);
}
