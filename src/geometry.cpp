#include "geometry.h"
namespace hkl {

    Geometry::Geometry(void)
    : ObjectWithParameters()
      {}

    Geometry::Geometry(Geometry const & geometry) :
      ObjectWithParameters(geometry),
      m_source(geometry.m_source)
    {}

    Geometry::~Geometry(void)
      {}

    Geometry &
    Geometry::operator=(Geometry const & geometry)
      {
        ObjectWithParameters::operator=(geometry);
        m_source = geometry.m_source;
        return *this;
      }

    bool
    Geometry::operator==(Geometry const & geometry) const
      {
        return ObjectWithParameters::operator==(geometry)
        && m_source == geometry.m_source
        && m_axes == geometry.m_axes;
      }

    bool
    Geometry::isValid(void) const throw (HKLException)
      {
        if (m_source.isValid())
            return true;
        else
            HKLEXCEPTION("The geometry is not valid", "Please set a correct source.");
      }

    ostream &
    Geometry::printToStream(ostream & flux) const
      {
        int nb_axes = m_samples.size();
        int i;

        flux.precision(3);
        flux << "  Source: " << m_source.get_waveLength() 
        << ", " << m_source.get_direction() << endl;
        //samples
        flux << "  Samples: (" << nb_axes << ")" << endl;
        vector<Axe *>::const_iterator it = m_samples.begin();
        vector<Axe *>::const_iterator end = m_samples.end();
        while(it != end)
          {
            Axe const & axe = **it;
            flux.width(12); flux << axe.get_name();     
            flux << ": " << axe.get_axe();
            flux << "(" << showpos << axe.get_direction() << ")";
            flux.unsetf(ios_base::showpos); 
            flux << "  " << axe.get_value()*constant::math::radToDeg;      
            flux << endl;
            ++it;
          }

        //detector
        nb_axes = m_detectors.size();
        flux << "  Detectors: (" << nb_axes << ")" << endl;
        it = m_detectors.begin();
        end = m_detectors.end();
        while(it != end)
          {
            Axe const & axe = **it;
            flux.width(12); flux << axe.get_name();     
            flux << ": " << axe.get_axe();
            flux << "(" << showpos << axe.get_direction() << ")";
            flux.unsetf(ios_base::showpos);
            flux << "  " << axe.get_value()*constant::math::radToDeg;      
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
        vector<Axe *>::const_iterator it = m_samples.begin();
        vector<Axe *>::const_iterator end = m_samples.end();
        while(it != end)
          {
            nameList.push_back((*it)->get_name());
            ++it;
          }

        // detector part
        it = m_detectors.begin();
        end = m_detectors.end();
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
        return *m_axes[name];
      }

    Axe const &
    Geometry::get_axe(MyString const & name) const throw (HKLException)
      {
        return *m_axes[name];
      }

    void
    Geometry::addSampleAxe(Axe & axe) throw (HKLException)
      {
        MyString const & name = axe.get_name();

        //Est-ce que cet axe est deja present dans la liste?
        vector<Axe *>::iterator sample_iter = m_samples.begin();
        vector<Axe *>::iterator sample_end = m_samples.end();
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

        map<MyString, Axe*>::iterator iter = m_axes.find(name);
        map<MyString, Axe*>::iterator end = m_axes.end();
        if (iter == end)
          {
            m_axes.insert(map<MyString, Axe*>::value_type(name, &axe));
            m_samples.push_back(&axe);
          }
        else
          {
            if (*(iter->second) == axe)
              {
                m_samples.push_back(&axe);
              }
            else
              {
                ostringstream description;
                description << "Same name but different axe." << endl
                << "Axe1 : ";
                iter->second->printToStream(description);
                description << "Axe2 : ";
                axe.printToStream(description);
                HKLEXCEPTION("Can not add this axe \"Axe2\" to the sample axe list",
                             description.str());
              }
          }
      }

    void
    Geometry::addDetectorAxe(Axe & axe) throw (HKLException)
      {
        MyString const & name = axe.get_name();

        //Est-ce que cet axe est deja present dans la liste?
        vector<Axe *>::iterator detector_iter = m_detectors.begin();
        vector<Axe *>::iterator detector_end = m_detectors.end();
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

        map<MyString, Axe*>::iterator iter = m_axes.find(name);
        map<MyString, Axe*>::iterator end = m_axes.end();
        if (iter == end)
          {
            m_axes.insert(map<MyString, Axe*>::value_type(name, &axe));
            m_detectors.push_back(&axe);
          }
        else
          {
            if (*(iter->second) == axe)
              {
                m_detectors.push_back(&axe);
              }
            else
              {
                ostringstream description;
                description << "Same name but different axe." << endl
                << "Axe1 : ";
                iter->second->printToStream(description);
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

        AxeVector::const_iterator iter = m_samples.begin();
        AxeVector::const_iterator end = m_samples.end();
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
        Quaternion const & qi = m_source.get_qi();

        AxeVector::const_iterator iter = m_detectors.begin();
        AxeVector::const_iterator end = m_detectors.end();
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
        Quaternion const & qi = m_source.get_qi();

        AxeVector::const_iterator iter = m_detectors.begin();
        AxeVector::const_iterator end = m_detectors.end();
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
        map<MyString, Axe*>::const_iterator iter1 = m_axes.begin();
        map<MyString, Axe*>::const_iterator end = m_axes.end();
        map<MyString, Axe*>::const_iterator iter2 = geometry.m_axes.begin();
        while(iter1 != end)
          {
            distance += iter1->second->getDistance(*(iter2->second));
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

            sum =   q[0] * (R.get(1,1)*R.get(2,2)-R.get(1,2)*R.get(2,1));
            sum += -q[1] * (R.get(0,1)*R.get(2,2)-R.get(0,2)*R.get(2,1));
            sum +=  q[2] * (R.get(0,1)*R.get(1,2)-R.get(0,2)*R.get(1,1));
            h = sum / det;

            sum =  -q[0] * (R.get(1,0)*R.get(2,2)-R.get(1,2)*R.get(2,0));
            sum +=  q[1] * (R.get(0,0)*R.get(2,2)-R.get(0,2)*R.get(2,0));
            sum += -q[2] * (R.get(0,0)*R.get(1,2)-R.get(0,2)*R.get(1,0));
            k = sum / det;

            sum =   q[0] * (R.get(1,0)*R.get(2,1)-R.get(1,1)*R.get(2,0));
            sum += -q[1] * (R.get(0,0)*R.get(2,1)-R.get(0,1)*R.get(2,0));
            sum +=  q[2] * (R.get(0,0)*R.get(1,1)-R.get(0,1)*R.get(1,0));
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
        ObjectWithParameters::toStream(flux);
        m_source.toStream(flux);
        return flux;    
      }

    istream &
    Geometry::fromStream(istream & flux)
      {
        ObjectWithParameters::fromStream(flux);
        m_source.fromStream(flux);
        return flux;
      }

} // namespace hkl

std::ostream & operator<< (std::ostream & flux, hkl::Geometry const & geometry)
{
    return geometry.printToStream(flux);
}
