#include "diffractometer.h"

namespace hkl {

    Diffractometer::Diffractometer(void) : ObjectWithParameters()
    {
      m_mode = NULL;
      setCurrentCrystal(DEFAULT_CRYSTAL_NAME);

      // On s'occupe de remplir avec les bons affinements la liste.
      m_affinementList.add(new affinement::Simplex);
    }

    Diffractometer::~Diffractometer(void)
      {
        m_affinementList.free();
      }

    //m_source
    double
    Diffractometer::getWaveLength(void) const
      {
        return m_geometry->get_source().get_waveLength();
      }

    void
    //!< @todo gérer HKLException
    Diffractometer::setWaveLength(double wl)
      {
        m_geometry->get_source().setWaveLength(wl);
      }

    //m_geometry
    vector<string> const
    Diffractometer::getAxesNames(void) const
      {
        vector<MyString> myNames = m_geometry->getAxesNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    vector<string> const
    Diffractometer::getSampleAxesNames(void) const
      {
        vector<MyString> myNames = m_geometry->get_samples();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    vector<string> const
    Diffractometer::getDetectorAxesNames(void) const
      {
        vector<MyString> myNames = m_geometry->get_detectors();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    void
    Diffractometer::setAxeValue(string const & name,
                                double value) throw (HKLException)
      {
        m_geometry->get_axe(name).set_value(value);
      }

    double const
    Diffractometer::getAxeValue(string const & name) const throw (HKLException)
      {
        return m_geometry->get_axe(name).get_value();
      }

    //m_pseudoAxeList
    vector<string> const
    Diffractometer::getPseudoAxesNames(void) const
      {
        vector<MyString> myNames = m_pseudoAxeList.getNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    string const &
    Diffractometer::getPseudoAxeDescription(string const & name) const throw (HKLException)
      {
        return m_pseudoAxeList[name]->get_description();
      }

    vector<string> const
    Diffractometer::getPseudoAxeParametersNames(string const & name) const throw(HKLException)
      {
        vector<MyString> myNames = m_pseudoAxeList[name]->getParametersNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    double
    Diffractometer::getPseudoAxeParameterValue(string const & pseudoAxe_name,
                                               string const & parameter_name) const throw (HKLException)
      {
        return m_pseudoAxeList[pseudoAxe_name]->getParameterValue(parameter_name); 
      }

    void
    Diffractometer::setPseudoAxeParameterValue(string const & pseudoAxe_name,
                                               string const & parameter_name,
                                               double value) throw (HKLException)
      {
        m_pseudoAxeList[pseudoAxe_name]->setParameterValue(parameter_name, value);
      }

    void
    Diffractometer::initializePseudoAxe(string const & name) throw (HKLException)
      {
        m_pseudoAxeList[name]->initialize(*m_geometry);
      }

    bool
    Diffractometer::getPseudoAxeIsValid(string const & name) const throw (HKLException)
      {
        return m_pseudoAxeList[name]->get_isValid(*m_geometry);
      }

    double
    Diffractometer::getPseudoAxeValue(string const & name) const throw (HKLException)
      {
        return m_pseudoAxeList[name]->get_value(*m_geometry);
      }

    void
    Diffractometer::setPseudoAxeValue(string const & name, double value) throw (HKLException)
      {
        m_pseudoAxeList[name]->set_value(*m_geometry, value);
      }

    //m_crystalList

    vector<string> const
    Diffractometer::getCrystalNames(void) const
      {
        vector<MyString> myNames = m_crystalList.getNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    string const &
    Diffractometer::getCurrentCrystalName(void) const throw (HKLException)
      {
        if (!m_crystal)
            HKLEXCEPTION("No crystal selected",
                         "Please select a crystal");

        return m_crystal->get_name();
      }

    void
    Diffractometer::setCurrentCrystal(string const & name) throw (HKLException)
      {
        m_crystal = &m_crystalList[name];
      }

    void
    Diffractometer::addNewCrystal(string const & name) throw (HKLException)
      {
        m_crystalList.add(Crystal(name));
      }

    void
    Diffractometer::setCrystalLattice(string const & name,
                                      double a, double b, double c,
                                      double alpha, double beta, double gamma) throw (HKLException)
      {
        m_crystalList[name].setLattice(a, b, c, alpha, beta, gamma);
      }

    void
    Diffractometer::getCrystalLattice(string const & name,
                                      double * a, double * b, double * c,
                                      double * alpha, double * beta, double * gamma) const throw (HKLException)
      {
        m_crystalList[name].getLattice(a, b, c, alpha, beta, gamma);  
      }

    void
    Diffractometer::getCrystalReciprocalLattice(string const & name,
                                                double * a, double * b, double * c,
                                                double * alpha, double * beta, double * gamma) const throw (HKLException)
      {
        m_crystalList[name].getReciprocalLattice(a, b, c, alpha, beta, gamma);   
      }

    vector<string>
    Diffractometer::getCrystalParametersNames(string const & name) const throw (HKLException)
      {
        vector<MyString> myNames = m_crystalList[name].getNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    void
    Diffractometer::getCrystalParameterValues(string const & crystal_name,
                                              string const & parameter_name,
                                              double * value,
                                              double * min,
                                              double * max,
                                              bool * flagFit) const throw (HKLException)
      {
        FitParameter const & fitparameter = m_crystalList[crystal_name][parameter_name];
        *value = fitparameter.get_value(); 
        *min = fitparameter.get_min(); 
        *max = fitparameter.get_max(); 
        *flagFit = fitparameter.get_flagFit(); 
      }

    void
    Diffractometer::setCrystalParameterValues(string const & crystal_name,
                                              string const & parameter_name,
                                              double value,
                                              double min,
                                              double max,
                                              bool flagFit) throw (HKLException)
      {
        FitParameter & fitparameter = m_crystalList[crystal_name][parameter_name];
        fitparameter.set_value(value); 
        fitparameter.set_min(min); 
        fitparameter.set_max(max); 
        fitparameter.set_flagFit(flagFit); 
      }

    smatrix
    Diffractometer::getCrystal_UB(string const & name) const throw (HKLException)
      {
        Crystal const & crystal = m_crystalList[name];
        return crystal.get_U() * crystal.get_B();
      }

    double
    Diffractometer::getCrystalFitness(string const & name) throw (HKLException)
      {
        return m_crystalList[name].fitness();
      }

    void
    Diffractometer::delCrystal(string const & name) throw (HKLException)
      {
        if (name == getCurrentCrystalName())
          {
            switch (m_crystalList.size())
              {
              case 1: m_crystalList.remove(name);
                      setCurrentCrystal(DEFAULT_CRYSTAL_NAME);
                      break;
              case 2: m_crystalList.remove(name);
                      setCurrentCrystal(m_crystalList.begin()->first);
                      break;
              default:
                      m_crystalList.remove(name);
                      m_crystal = NULL;
              }
          } 
        else
          {
            m_crystalList.remove(name);
          }
      }

    void
    Diffractometer::delAllCrystals(void)
      {
        m_crystalList.clear();
        setCurrentCrystal(DEFAULT_CRYSTAL_NAME);
      }

    void
    Diffractometer::copyCrystalAsNew(string const & from,
                                     string const & to) throw (HKLException)
      {
        Crystal crystal(m_crystalList[from]);
        crystal.set_name(to);
        m_crystalList.add(crystal);
      }

    void
    Diffractometer::renameCrystal(string const & from,
                                  string const & to) throw (HKLException)
      {
        Crystal crystal(m_crystalList[from]);
        crystal.set_name(to);

        m_crystalList.add(crystal);  
        m_crystalList.remove(from);
        setCurrentCrystal(to);
      }

    // reflections

    unsigned int
    Diffractometer::getCrystalNumberOfReflection(string const & name) const throw (HKLException)
      {
        return m_crystalList[name].get_reflectionList().size();
      }

    unsigned int
    Diffractometer::addCrystalReflection(string const & name,
                                         double h, double k, double l,
                                         int relevance, bool flag) throw (HKLException)
      {
        return m_crystalList[name].addReflection(Reflection(*m_geometry, h, k, l, relevance, flag));
      }

    double
    Diffractometer::getCrystalReflectionAxeAngle(string const & crystalName, 
                                                 unsigned int index,
                                                 string const & axeName) const throw (HKLException)
      {
        return m_crystalList[crystalName].getReflection(index).get_geometry().get_axe(axeName).get_value();
      }

    void
    Diffractometer::delCrystalReflection(string const & name,
                                         unsigned int index) throw (HKLException)
      {
        m_crystalList[name].delReflection(index);
      }

    unsigned int
    Diffractometer::copyCrystalReflectionFromTo(string const & from,
                                                unsigned int ifrom,
                                                string const & to) throw (HKLException)
      {
        Crystal & from_crystal = m_crystalList[from];
        Crystal & to_crystal = m_crystalList[to];

        return to_crystal.addReflection(from_crystal.getReflection(ifrom));
      }

    void
    Diffractometer::getCrystalReflectionParameters(string const & name,
                                                   unsigned int index,
                                                   double * h, double * k, double *l,
                                                   int * relevance, bool * flag) const throw (HKLException)
      {
        Reflection const & R = m_crystalList[name].getReflection(index);
        *h = R.get_h();
        *k = R.get_k();
        *l = R.get_l();
        *relevance = R.get_relevance();
        *flag = R.get_flag();
      }

    void
    Diffractometer::setCrystalReflectionParameters(string const & name,
                                                   unsigned int index,
                                                   double h, double k, double l,
                                                   int relevance, bool flag) throw (HKLException)
      {
        Reflection & r = m_crystalList[name].getReflection(index);
        r.set_h(h);
        r.set_k(k);
        r.set_l(l);
        r.set_relevance(relevance);
        r.set_flag(flag);
      }

    // Modes

    vector<string>
    Diffractometer::getModeNames(void) const
      {
        vector<MyString> myNames = m_modeList.getNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    string const &
    Diffractometer::getCurrentModeName(void) const throw (HKLException)
      {
        if (!m_mode)
            HKLEXCEPTION("no mode set",
                         "please set a mode");
        else
            return m_mode->get_name();
      }

    string const &
    Diffractometer::getModeDescription(string const & name) const throw (HKLException)
      {
        return m_modeList[name]->get_description();
      }

    vector<string>
    Diffractometer::getModeParametersNames(string const & name) const throw (HKLException)
      {
        vector<MyString> myNames = m_modeList[name]->getParametersNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    double
    Diffractometer::getModeParameterValue(string const & mode_name,
                                          string const & parameter_name) const throw (HKLException)
      {
        return m_modeList[mode_name]->getParameterValue(parameter_name);
      }

    void
    Diffractometer::setModeParameterValue(string const & mode_name,
                                          string const & parameter_name,
                                          double value) throw (HKLException)
      {
        m_modeList[mode_name]->setParameterValue(parameter_name, value);
      }

    void
    Diffractometer::setCurrentMode(string const & name) throw (HKLException)
      {
        m_mode = m_modeList[name];
      }

    // Affinement functions


    vector<string>
    Diffractometer::getAffinementNames(void) const
      {
        vector<MyString> myNames = m_affinementList.getNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator end = myNames.end();
        while(iter != end)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    unsigned int
    Diffractometer::getAffinementMaxIteration(string const & name) const throw (HKLException)
      {
        return m_affinementList[name]->get_nb_max_iteration();
      }

    void
    Diffractometer::setAffinementMaxIteration(string const & name, unsigned int max) throw (HKLException)
      {
        m_affinementList[name]->set_nb_max_iteration(max);
      }

    unsigned int
    Diffractometer::getAffinementIterations(string const & name) const throw (HKLException)
      {
        return m_affinementList[name]->get_nb_iteration();
      }

    double
    Diffractometer::affineCrystal(string const & crystal_name, string const & method_name) throw (HKLException)
      {
        Crystal & crystal = m_crystalList[crystal_name];
        Affinement * affinement = m_affinementList[method_name];

        unsigned int nb_parameters = crystal.getNumberOfParameterToFit();
        unsigned int nb_reflections = (unsigned int)ceil(nb_parameters / 3.);
        if (crystal.isEnoughReflections(nb_reflections))
          {
            // Ugly patch... must use another affinement method instead of the simplex.
            // Or maybe improve the speed.
            unsigned int const & tmp = affinement->get_nb_max_iteration();

            affinement->set_nb_max_iteration(800);
            affinement->fit(crystal);
            affinement->fit(crystal);
            affinement->fit(crystal);
            affinement->set_nb_max_iteration(tmp);
            affinement->fit(crystal);
            return affinement->get_fitness();
          } 
        else
            HKLEXCEPTION("","");
      }

    //Calculation functions

    void
    Diffractometer::computeU(void) throw (HKLException)
      {
        if (!m_crystal)
            HKLEXCEPTION("No crystal selected.",
                         "Please select a crystal before.");
        else
            m_crystal->computeU();
      }

    void
    Diffractometer::computeHKL(double * h, double * k, double * l) throw (HKLException)
      {
        if (!m_crystal)
            HKLEXCEPTION("No crystal selected.",
                         "Please select a crystal before.");
        else
          {

            smatrix UB = m_crystal->get_U() * m_crystal->get_B();
            smatrix R = m_geometry->getSampleRotationMatrix() * UB;
            double det;

            det  =  R.get(0,0)*(R.get(1,1)*R.get(2,2)-R.get(2,1)*R.get(1,2));
            det += -R.get(0,1)*(R.get(1,0)*R.get(2,2)-R.get(2,0)*R.get(1,2));
            det +=  R.get(0,2)*(R.get(1,0)*R.get(2,1)-R.get(2,0)*R.get(1,1));

            if (fabs(det) < constant::math::epsilon_1)
                HKLEXCEPTION("det(R) is null",
                             "La matrice rotation de la machine n'est pas valide");
            else
              {

                svector q = m_geometry->getQ();

                double sum;

                sum =   q[0] * (R.get(1,1)*R.get(2,2)-R.get(1,2)*R.get(2,1));
                sum += -q[1] * (R.get(0,1)*R.get(2,2)-R.get(0,2)*R.get(2,1));
                sum +=  q[2] * (R.get(0,1)*R.get(1,2)-R.get(0,2)*R.get(1,1));
                *h = sum / det;

                sum =  -q[0] * (R.get(1,0)*R.get(2,2)-R.get(1,2)*R.get(2,0));
                sum +=  q[1] * (R.get(0,0)*R.get(2,2)-R.get(0,2)*R.get(2,0));
                sum += -q[2] * (R.get(0,0)*R.get(1,2)-R.get(0,2)*R.get(1,0));
                *k = sum / det;

                sum =   q[0] * (R.get(1,0)*R.get(2,1)-R.get(1,1)*R.get(2,0));
                sum += -q[1] * (R.get(0,0)*R.get(2,1)-R.get(0,1)*R.get(2,0));
                sum +=  q[2] * (R.get(0,0)*R.get(1,1)-R.get(0,1)*R.get(1,0));
                *l = sum / det;
              }
          }
      }

    void
    Diffractometer::computeAngles(double h, double k, double l) throw (HKLException)
      {
        if (!m_mode)
            HKLEXCEPTION("m_currentMode is null",
                         "The mode has not been set");

        if (m_geometry->get_source().get_waveLength() < constant::math::epsilon_1)
            HKLEXCEPTION("lamdba is null",
                         "The wave length has not been set");

        if ((fabs(h) < constant::math::epsilon_1) 
            && (fabs(k) < constant::math::epsilon_1)
            && (fabs(l) < constant::math::epsilon_1))
            HKLEXCEPTION("(h,k,l) is null",
                         "check your parameters");

        if (!m_crystal)
            HKLEXCEPTION("No crystal selected.",
                         "Please select a crystal before.");

        if (m_crystal->get_B() == smatrix())
            HKLEXCEPTION("null B matrix",
                         "Please set the crystal parameters correctly");

        if (m_crystal->get_U() == smatrix())
            HKLEXCEPTION("null U matrix",
                         "Please compute the orientation matrix first");

        try
          {
            smatrix UB = m_crystal->get_U() * m_crystal->get_B();
            m_mode->computeAngles(h, k, l, UB, *m_geometry);
          }
        catch (const HKLException &)
          {
            throw;
          }
      }

    ostream &
    Diffractometer::printToStream(ostream & flux) const
      {
        flux << showpoint << fixed;
        flux << endl;
        flux << "Diffractometer: \"" << get_name() << "\"" << endl;

        //Parameters
        ObjectWithParameters::printToStream(flux);

        //geometry
        flux << "Geometry:" << endl;
        flux << *m_geometry << endl;

        //mode
        flux << "Modes:" << endl;
        vector<string> modeNames = getModeNames();
        vector<string>::const_iterator m_iter = modeNames.begin();
        vector<string>::const_iterator m_end = modeNames.end();
        while(m_iter != m_end)
          {
            flux << "\"" << *m_iter << "\"";
            if (m_mode && *m_iter == m_mode->get_name())
                flux << "(*)  ";
            else
                flux << "  ";
            ++m_iter;
          }
        flux << endl << endl;

        //crystals   
        CrystalList::const_iterator c_iter = m_crystalList.begin();
        while(c_iter != m_crystalList.end())
          {
            flux << "Crystal";
            if (m_crystal && c_iter->first == m_crystal->get_name())
                flux << "(*)";
            flux << ":";
            flux << c_iter->second << endl;
            ++c_iter;
          }
        return flux;
      }

    bool
    Diffractometer::operator ==(Diffractometer const & diffractometer) const
      {
        return Object::operator==(diffractometer)
        && m_crystalList == diffractometer.m_crystalList
        && m_modeList == diffractometer.m_modeList
        && m_affinementList == diffractometer.m_affinementList
        && m_pseudoAxeList == diffractometer.m_pseudoAxeList;
      }

    ostream &
    Diffractometer::toStream(ostream & flux) const
      {
        flux << " " << HKL_VERSION;
        ObjectWithParameters::toStream(flux);
        m_geometry->toStream(flux);
        if (m_crystal)
            flux << char(30) << m_crystal->get_name() << char(30);
        else
            flux << char(30) << "NULL" << char(30);
        m_crystalList.toStream(flux);
        if (m_mode)
            flux << char(30) << m_mode->get_name() << char(30);
        else
            flux << char(30) << "NULL" << char(30);
        m_modeList.toStream(flux);
        m_affinementList.toStream(flux);
        m_pseudoAxeList.toStream(flux);

        return flux;
      }

    istream &
    Diffractometer::fromStream(istream & flux)
      {
        unsigned int version;
        string crystal_name;
        string mode_name;
        string junk;

        flux >> version;
        if (version == HKL_VERSION)
          {
            ObjectWithParameters::fromStream(flux);
            m_geometry->fromStream(flux);
            getline(flux, junk, char(30)); 		
            getline(flux, crystal_name, char(30));
            m_crystalList.fromStream(flux);
            getline(flux, junk, char(30));
            getline(flux, mode_name, char(30));
            m_modeList.fromStream(flux);
            m_affinementList.fromStream(flux);
            m_pseudoAxeList.fromStream(flux);

            // Set the current crystal and the current mode
            if (crystal_name == "NULL")
                m_crystal = NULL;
            else
                setCurrentCrystal(crystal_name);
            if (mode_name == "NULL")
                m_mode = NULL;
            else
                setCurrentMode(mode_name);
          }
        return flux;
      }

} // namespace hkl

ostream &
operator <<(ostream& flux, hkl::Diffractometer const & diffractometer)
{ 
    return diffractometer.printToStream(flux);
}
