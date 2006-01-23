#include "geometry.h"
namespace hkl {
   
  Geometry::Geometry(void)
    : ObjectWithParameters()
  {}

  Geometry::Geometry(Geometry const & geometry)
    : ObjectWithParameters(geometry),
      m_source(geometry.m_source),
      m_axeMap(geometry.m_axeMap),
      m_samples(geometry.m_samples),
      m_detectors(geometry.m_detectors)
    
  {}

  Geometry::~Geometry(void)
  {}

  bool
  Geometry::operator==(Geometry const & geometry) const
  {
    return ObjectWithParameters::operator==(geometry)
            && m_source == geometry.m_source
            && m_axeMap == geometry.m_axeMap
            && m_samples == geometry.m_samples
            && m_detectors == geometry.m_detectors;
  }

  std::ostream &
  Geometry::printToStream(std::ostream & flux) const
  {
    int nb_axes = m_samples.size();
    int i;
    
    flux.precision(3);
    flux << "  Source: " << m_source.get_waveLength() 
         << ", " << m_source.get_direction() << endl;
    //samples
    flux << "  Samples: (" << nb_axes << ")" << endl;
    for(i=0; i<nb_axes; i++)
    {
      Axe const & axe = m_axeMap[m_samples[i]];
      flux.width(12); flux << axe.get_name();     
      flux << ": " << axe.get_axe();
      flux << "(" << showpos << axe.get_direction() << ")";
      flux.unsetf(ios_base::showpos); 
      flux << "  " << axe.get_value()*constant::math::radToDeg;      
      flux << endl;
    }
    
    //detector
    nb_axes = m_detectors.size();
    flux << "  Detectors: (" << nb_axes << ")" << endl;
    for(i=0; i<nb_axes; i++)
    {
      Axe const & axe = m_axeMap[m_detectors[i]];
      flux.width(12); flux << axe.get_name();     
      flux << ": " << axe.get_axe();
      flux << "(" << showpos << axe.get_direction() << ")";
      flux.unsetf(ios_base::showpos);
      flux << "  " << axe.get_value()*constant::math::radToDeg;      
      flux << endl;
    }

    return flux;
  }

  vector<string> const
  Geometry::getAxesNames(void) const
  {
    vector<string> nameList(m_samples);
    
    vector<string>::const_iterator iter = m_detectors.begin();
    vector<string>::const_iterator end = m_detectors.end();
    while(iter != end)
    {
      nameList.push_back(*iter);
      iter++;
    }
    
    return nameList;
  }

  Axe &
  Geometry::get_axe(string const & name) throw (HKLException)
  {
    return m_axeMap[name];
  }

  Axe const &
  Geometry::get_axe(string const & name) const throw (HKLException)
  {
    return m_axeMap[name];
  }

  void
  Geometry::addSampleAxe(Axe const & axe) throw (HKLException)
  {
    string const & name = axe.get_name();
    
    //Est-ce que cet axe est deja present dans la liste?
    vector<string>::iterator sample_iter = m_samples.begin();
    vector<string>::iterator sample_end = m_samples.end();
    while(sample_iter != sample_end)
    {
      if (*sample_iter == name)
        throw HKLException("Can not add two times the same axe",
                           "change the name of the axe",
                           "AngleConfiguration::addSampleAxe");
      sample_iter++;
    }
    
    AxeMap::iterator iter = m_axeMap.find(name);
    AxeMap::iterator end = m_axeMap.end();

    if (iter == end)
    {
      m_axeMap.insert(AxeMap::value_type(name, axe));
      m_samples.push_back(name);
    } else {
      if (iter->second == axe)
      {
        m_samples.push_back(name);
      } else {
        throw HKLException("Can not add this axe",
                           "Same name but different axe",
                           "AngleConfiguration::addSampleAxe");
      }
    }
  }

  void
  Geometry::addDetectorAxe(Axe const & axe) throw (HKLException)
  {
    string const & name = axe.get_name();

    //Est-ce que cet axe est deja present dans la liste?
    vector<string>::iterator detector_iter = m_detectors.begin();
    vector<string>::iterator detector_end = m_detectors.end();
    while(detector_iter != detector_end)
    {
      if (*detector_iter == name)
        throw HKLException("Can not add two times the same axe",
                           "change the name of the axe",
                           "AngleConfiguration::addDetectorAxe");
      detector_iter++;
    }

    AxeMap::iterator iter = m_axeMap.find(name);
    AxeMap::iterator end = m_axeMap.end();

    if (iter == end){
      m_axeMap.insert(AxeMap::value_type(name, axe));
      m_detectors.push_back(name);
    } else {
      if (iter->second == axe){
        m_detectors.push_back(name);
      } else {
        throw HKLException("Can not add this axe",
                           "Same name but different axe",
                           "AngleConfiguration::addDetectorAxe");
      }
    }
  }

  Quaternion
  Geometry::getSampleQuaternion(void) const
  {
    unsigned int nb_axes = m_samples.size();
    Quaternion q;
    
    for(unsigned int i=0;i<nb_axes;i++)
      q *= m_axeMap[m_samples[i]].asQuaternion();
    
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
    int nb_axes = m_detectors.size();
    Quaternion qr;
    Quaternion const & qi = m_source.get_qi();

    for(int i=0;i<nb_axes;i++)
      qr *= m_axeMap[m_detectors[i]].asQuaternion();
    
    Quaternion q(qr);
    q *= qi;
    q *= qr.conjugate();
    q -= qi;

    return svector(q[1], q[2], q[3]);
  }

  ostream &
  Geometry::toStream(ostream & flux) const
  {
    ObjectWithParameters::toStream(flux);
    m_source.toStream(flux);
    m_axeMap.toStream(flux);
    
    flux << m_samples.size() << " ";
    vector<string>::const_iterator iter = m_samples.begin();
    vector<string>::const_iterator end = m_samples.end();
    while (iter != end)
    {
      flux << *iter << char(30);
      ++iter;
    }
    
    flux << m_detectors.size() << " ";
    iter = m_detectors.begin();
    end = m_detectors.end();
    while (iter != end)
    {
      flux << *iter << char(30);
      ++iter;
    }
    
    return flux;    
  }

  istream &
  Geometry::fromStream(istream & flux)
  {
    ObjectWithParameters::fromStream(flux);
    m_source.fromStream(flux);
    m_axeMap.fromStream(flux);
    
    unsigned int i;
    unsigned int size;
    string s;
    
    flux >> size;
    char c;
    flux >> c;
    if (c != '\n')
      flux.putback(c);    
    m_samples.clear();
    for(i=0; i<size; i++)
    {
      getline(flux, s, char(30));
      m_samples.push_back(s);
    }
    
    flux >> size;
    flux >> c;
    if (c != ' ')
      flux.putback(c);
    m_detectors.clear();
    for(i=0; i<size; i++)
    {
      getline(flux, s, char(30));
      m_detectors.push_back(s);
    }
    
    return flux;
  }
  
} // namespace hkl

std::ostream & operator<< (std::ostream & flux, hkl::Geometry const & geometry)
{
  return geometry.printToStream(flux);
}
