#include "geometry.h"
namespace hkl {
   
  Geometry::Geometry(void)
    : ObjectWithParameters()
  {}

  Geometry::Geometry(Geometry const & geometry)
    : ObjectWithParameters(geometry)
  {
    m_axeMap = geometry.m_axeMap;
    m_samples = geometry.m_samples;
    m_detectors = geometry.m_detectors;
  }

  Geometry::~Geometry(void)
  {}

  bool
  Geometry::operator==(Geometry const & geometry) const
  {
    return ObjectWithParameters::operator==(geometry)
            && m_axeMap == geometry.m_axeMap
            && m_samples == geometry.m_samples
            && m_detectors == geometry.m_detectors;
  }

  std::ostream &
  Geometry::printToStream(std::ostream & flux) const
  {
    int nb_axes = m_samples.size();
    int i;
    flux << endl << "GEOMETRY" << endl;
    flux << "Samples: (" << nb_axes << ")" << endl;
    for(i=0; i<nb_axes; i++)
      flux << "  " << m_axeMap[m_samples[i]];

    nb_axes = m_detectors.size();
    flux << "Detectors: (" << nb_axes << ")" << endl;
    for(i=0; i<nb_axes; i++)
      flux << "  " << m_axeMap[m_detectors[i]];
    flux << endl;

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
    try{
      return m_axeMap[name];
    } catch (HKLException const &){
      throw;
    }
  }

  Axe const &
  Geometry::get_axe(string const & name) const throw (HKLException)
  {
    try{
      return m_axeMap[name];
    } catch (HKLException const &){
      throw;
    }
  }

  void
  Geometry::addSampleAxe(Axe const & axe) throw (HKLException)
  {
    string const & name = axe.get_name();
    
    //Est-ce que cet axe est deja present dans la liste?
    vector<string>::iterator sample_iter = m_samples.begin();
    vector<string>::iterator sample_end = m_samples.end();
    while(sample_iter != sample_end){
      if (*sample_iter == name)
        throw HKLException("Can not add two times the same axe",
                           "change the name of the axe",
                           "AngleConfiguration::addSampleAxe");
      sample_iter++;
    }
    
    AxeMap::iterator iter = m_axeMap.find(name);
    AxeMap::iterator end = m_axeMap.end();

    if (iter == end){
      m_axeMap.insert(AxeMap::value_type(name, axe));
      m_samples.push_back(name);
    } else {
      if (iter->second == axe){
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
    while(detector_iter != detector_end){
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

} // namespace hkl

std::ostream & operator<< (std::ostream & flux, hkl::Geometry const & geometry)
{
  return geometry.printToStream(flux);
}
