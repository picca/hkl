#ifndef _DIFFRACTOMETER_H_
#define _DIFFRACTOMETER_H_

using namespace std;

/**
 *
 * \mainpage
 *
 * L'objectif de cette librairie est de mêtre à disposition l'ensemble des outils permettant de piloter
 * un diffractomètre. L'ensemble des calcules présents dans cette librairie sont basés sur une équation
 * fondamentale.
 *
 * @ref Diffractometer
 * 
 * @ref Diffractometer_eulerian_4C
 *
 * @ref Diffractometer_eulerian_6C
 *
 * @ref Diffractometer_kappa_4C
 */

/**
 * @page Diffractometer Généralité.
 *
 * \section Equation_fondamentales Equations fondamentales
 * 
 * Le problème que nous devons résoudre est de calculer pour une famille de plan (h,k,l) donné,
 * les angles de rotation du diffractomètre qui permettent de le mettre en condition de diffraction.
 * Il faut donc exprimer les relations mathématiques qui lient les différents angles entre eux lorsque
 * la condition de Bragg est vérifiée. L'équation fondamentale est la suivante:
 * \f[
 *    \left( \prod_i S_i \right) \cdot U \cdot B \cdot \vec{h} = \left( \prod_i D_i - I \right)
 * \f]
 * \f[
 *    R \cdot U \cdot B \cdot \vec{h} = \vec{Q}
 * \f]
 * où \f$ \vec{h} \f$ est le vecteur (h,k,l) , \f$ \vec{k_i} \f$ est le vecteur incident,  \f$ S_i \f$ les
 * matrices de rotations des mouvements liés à l'échantillon, \f$ D_j \f$  les matrices de rotation
 * des mouvements liés au détecteur,
 * \a I  la matrice identité, \a U  la matrice d'orientation du cristal par rapport au repère de l'axe
 * sur lequel ce dernier est monté et \a B  la matrice de passage d'un repère non orthonormé
 * (celui du crystal réciproque) à un repère orthonormé. 
 * 
 * L'équation fondamentale nous permet d'écrire:
 * \f[
 *    U \cdot B \cdot \vec{h} = \tilde{R} \cdot \vec{Q}.
 * \f]
 * 
 * Cette équation est de 4 ou 6 inconnues pour seulement 3 équations.
 * Il faut donc imposer des contraintes pour résoudre ce système et ainsi orienter le diffractomètre.
 * Ces différentes contraintes définissent les modes de fonctionnement des diffractomètres.
 * 
 * \section Calcule_de_B Calcule de B.
 *
 * Si l'on connaît les paramètres cristallins du cristal étudié, il est très simple de calculer
 * cette matrice \a B :
 * \f[ 
 *    B=\left(
 *        \begin{matrix}
 *          a^{*} & b^{*}\cos\gamma^{*} & c^{*}\cos\beta^{*} \\
 *              0 & b^{*}\sin\gamma^{*} & -c^{*} \sin\beta^{*} \cos\alpha \\
 *              0 &                   0 & 1/c
 *        \end{matrix}
 *      \right)
 * \f]
 * 
 * Le calcule de \f$ a^\star \f$, \f$ b^\star \f$ et \f$ c^\star \f$
 * est obtenu de la façon suivante:
 * \f{eqnarray*}
 *    a^\star & = & \tau \frac{\sin\alpha}{aD} \\
 *    b^\star & = & \tau \frac{\sin\beta}{bD} \\
 *    c^\star & = & \tau \frac{\sin\gamma}{cD}
 * \f}
 * 
 * ou
 *
 * \f[
 *    D = \sqrt{1 - \cos^2\alpha - \cos^2\beta - \cos^2\gamma + 2\cos\alpha \cos\beta \cos\gamma}
 * \f]
 * 
 * pour obtenir les angles \f$ \alpha^\star \f$, \f$ \beta^\star \f$ et \f$ \gamma^\star \f$,
 * on passe par le calcule des sinus et cosinus.
 * \f[
 *  \begin{array}{cc}
 *    \cos\alpha^\star = \frac{\cos\beta \cos\gamma - \cos\alpha}{\sin\beta \sin\gamma} 
 *      & \sin\alpha^\star = \frac{D}{\sin\beta \sin\gamma} \\
 *    \cos\beta^\star = \frac{\cos\gamma \cos\alpha - \cos\beta}{\sin\gamma \sin\alpha} 
 *      & \sin\beta^\star = \frac{D}{\sin\gamma \sin\alpha} \\
 *    \cos\gamma^\star = \frac{\cos\alpha \cos\beta - \cos\gamma}{\sin\alpha \sin\beta} 
 *      & \sin\gamma^\star = \frac{D}{\sin\alpha \sin\beta} \\
 *  \end{array}
 * \f]
*
* \section Calcule_de_U Calcule de U.
*
* Il existe plusieurs façons de calculer \a U. Busing et Levy en a proposé plusieurs.
* Nous allons présenter celle qui nécessite la mesure de seulement deux réflections ainsi que la
* connaissance des paramètres cristallins.
* Cette façon de calculer la matrice d'orientation \a U, peut être généralisée à n'importe quel
* diffractomètre pour peu que la description des axes de rotation permette d'obtenir la matrice
* de rotation de la machine \a R et le vecteur de diffusion \f$ \vec{Q} \f$.
* Il est également possible de calculer \a U sans la connaîssance des paramètres cristallins.
* il faut alors faire un affinement des paramètres. Cela revient à minimiser une fonction.
* Nous allons utiliser la méthode du simplex pour trouver ce minimum et ainsi ajuster l'ensemble
* des paramètres cristallins ainsi que la matrice d'orientation.
* 
* \subsection Algorithme_de_Busing_Levy Algorithme de Busing Levy.
*
* L'idée est de se placer dans le repère de l'axe sur lequel est monté l'échantillon.
* On mesure deux réflections \f$ (\vec{h}_1, \vec{h}_2) \f$ ainsi que leurs angles associés.
* Cela nous permet de calculer \a R et \f$ \vec{Q} \f$ pour chacune de ces reflections.
* Nous avons alors ce système:
* \f[
*    U \cdot B \cdot \vec{h}_1
* \f]
* De façon à calculer facilement \a U, il est intéressant de définir deux trièdres orthonormé
* \f$ T_{\vec{h}} \f$ et \f$ T_{\vec{Q}} \f$ à partir des vecteurs \f$ (B \cdot \vec{h}_1, B \cdot \vec{h}_2) \f$
* et \f$ (\tilde{R}_1 \cdot \vec{Q}_1, \tilde{R}_2 \cdot \vec{Q}_2) \f$.
* On a alors très simplement:
* \f[
*    U \cdot T_{\vec{h}} = T_{\vec{Q}}
* \f]
* Et donc:
* \f[
*    U = T_{\vec{Q}} \cdot \tilde{T}_{\vec{h}}
* \f]
*
* \subsection Affinement_par_la_methode_du_simplex Affinement par la méthode du simplex
*
* Dans ce cas nous ne connaissons pas la matrice \a B, il faut alors mesurer plus de
* deux réflections afin d'ajuster les 9 paramètres.
* Six paramètres pour le crystal et trois pour la matrice d'orientation \a U.
* Les trois paramètres qui permennt de representer \a U sont en fait les angles d'euler.
* Il est donc nécessaire de connaitre la représentation Eulérien de la matrice \a U et réciproquement.
* \f[
*    U = X \cdot Y \cdot Z
* \f]
* où \a X est la matrice rotation suivant l'axe Ox et le premier angle d'Euler,
* \a Y la matrice de rotation suivant l'axe Oy et le deuxième angle d'Euler et \a Z la matrice du troisième
* angle d'Euler pour l'axe Oz.
* \f[
*      \left(
             *        \begin{matrix}
             *          1 & 0 & 0\\
             *          0 & A & -B\\
             *          0 & B & A
             *        \end{matrix}
             *      \right)
*      \left(
             *        \begin{matrix}
             *          C & 0 & D\\
             *          0 & 1 & 0\\
             *         -D & 0 & C
             *        \end{matrix}
             *      \right)
*      \left(
             *        \begin{matrix}
             *          E & -F & 0\\
             *          F & E & 0\\
             *          0 & 0 & 1
             *        \end{matrix}
             *      \right)
* \f]
* 
* et donc:
* 
* \f[ 
*    U = \left(
               *          \begin{matrix}
               *                CE &     -CF & D \\
               *            BDE+AF & -BDF+AE & -BC \\
               *           -ADE+BF &  ADF+BE & AC
               *          \end{matrix}
               *        \right)
*  \f]
*/

namespace hkl {

    class Diffractometer
      {
      public:

        virtual ostream & printToStream(ostream & flux) const = 0;
        virtual ostream & toStream(ostream & flux) const = 0;
        virtual istream & fromStream(istream & flux) = 0;

        // source
        virtual void setWaveLength(double wl) = 0;
        virtual double getWaveLength(void) const = 0;

        // geometry
        virtual Axe & getAxe(string const & name) = 0;
        virtual vector<string> const getAxesNames(void) const = 0;
        virtual vector<string> const getSampleAxesNames(void) const = 0;
        virtual vector<string> const getDetectorAxesNames(void) const = 0;
        //virtual void setAxeValue(string const & name, double value) throw (HKLException) = 0;
        //virtual double const getAxeValue(string const & name) const throw (HKLException) = 0;
        virtual void setAxesFromCrystalReflection(string const & name, unsigned int index) throw (HKLException) = 0; 


        // pseudoAxes
        virtual vector<string> const getPseudoAxesNames(void) const = 0;
        virtual PseudoAxeInterface & getPseudoAxe(string const & name) = 0;
        //virtual string const & getPseudoAxeDescription(string const & name) const throw (HKLException) = 0;
        //virtual vector<string> const getPseudoAxeParametersNames(string const & name) const throw (HKLException) = 0;
        //virtual double getPseudoAxeParameterValue(string const & pseudoAxe_name,
        //                                  string const & parameter_name) const throw (HKLException) = 0;
        //virtual void setPseudoAxeParameterValue(string const & pseudoAxe_name,
        //                                string const & parameter_name,
        //                                double value) throw (HKLException) = 0;
        //virtual void initializePseudoAxe(string const & name) throw (HKLException) = 0;
        //virtual bool getPseudoAxeIsValid(string const & name) const throw (HKLException) = 0;
        //virtual double getPseudoAxeValue(string const & name) const throw (HKLException) = 0;
        //virtual void setPseudoAxeValue(string const & name, double value) throw (HKLException) = 0;

        // Crystals
        virtual vector<string> const getCrystalNames(void) const = 0;
        virtual string const & getCurrentCrystalName(void) const throw (HKLException) = 0;
        virtual void setCurrentCrystal(string const & name) throw (HKLException) = 0;
        virtual void addNewCrystal(string const & name) throw (HKLException) = 0;
        virtual void setCrystalLattice(string const & name,
                                       double a, double b, double c,
                                       double alpha, double beta, double gamma) throw (HKLException) = 0;
        virtual void getCrystalLattice(string const & name,
                                       double * a, double * b, double * c,
                                       double * alpha, double * beta, double * gamma) const throw (HKLException) = 0;
        virtual void getCrystalReciprocalLattice(string const & name,
                                                 double * a, double * b, double * c,
                                                 double * alpha, double * beta, double * gamma) const throw (HKLException) = 0;
        virtual vector<string> getCrystalParametersNames(string const & name) const throw (HKLException) = 0;
        virtual void getCrystalParameterValues(string const & crystal_name,
                                               string const & parameter_name,
                                               double * value,
                                               double * min,
                                               double *max,
                                               bool * to_fit) const throw (HKLException) = 0;
        virtual void setCrystalParameterValues(string const & crystal_name,
                                               string const & parameter_name,
                                               double value,
                                               double min,
                                               double max,
                                               bool to_fit) throw (HKLException) = 0;
        virtual smatrix getCrystal_UB(string const & name) const throw (HKLException) = 0;
        virtual double getCrystalFitness(string const & name) throw (HKLException) = 0;
        virtual void delCrystal(string const & name) throw (HKLException) = 0;
        virtual void delAllCrystals(void) = 0;
        virtual void copyCrystalAsNew(string const & from,
                                      string const & to) throw (HKLException) = 0;
        virtual void renameCrystal(string const & from, string const & to) throw (HKLException) = 0;

        // reflections
        virtual unsigned int getCrystalNumberOfReflection(string const & name) const throw (HKLException) = 0;
        virtual unsigned int addCrystalReflection(string const & name, 
                                                  double h, double k, double l,
                                                  int relevance, bool flag) throw (HKLException) = 0;
        virtual double getCrystalReflectionAxeAngle(string const & crystalName,
                                                    unsigned int index,
                                                    string const & axeName) const throw (HKLException) = 0;
        virtual void setCrystalReflectionParameters(string const & name,
                                                    unsigned int index,
                                                    double h, double k, double l,
                                                    int relevance, bool flag) throw (HKLException) = 0;
        virtual void getCrystalReflectionParameters(string const & name,
                                                    unsigned int index,
                                                    double * h, double * k, double *l,
                                                    int * relevance, bool * flag) const throw (HKLException) = 0;
        virtual void delCrystalReflection(string const & name,
                                          unsigned int index) throw (HKLException) = 0;
        virtual unsigned int copyCrystalReflectionFromTo(string const & from, 
                                                         unsigned int ifrom,
                                                         string const & to) throw (HKLException) = 0;

        // Modes
        virtual vector<string> getModeNames(void) const = 0;
        virtual string const & getCurrentModeName(void) const throw (HKLException) = 0;
        virtual string const & getModeDescription(string const & name) const throw (HKLException) = 0;
        virtual vector<string> getModeParametersNames(string const & name) const throw (HKLException) = 0;
        virtual double getModeParameterValue(string const & mode_name,
                                             string const & parameter_name) const throw (HKLException) = 0;
        virtual void setModeParameterValue(string const & mode_name,
                                           string const & parameter_name,
                                           double value) throw (HKLException) = 0;
        virtual void setCurrentMode(string const & name) throw (HKLException) = 0;

        // Affinement functions
        virtual vector<string> getAffinementNames(void) const = 0;
        virtual unsigned int getAffinementMaxIteration(string const & name) const throw (HKLException) = 0;
        virtual void setAffinementMaxIteration(string const & name, unsigned int max) throw (HKLException) = 0;
        virtual unsigned int getAffinementIterations(string const & name) const throw (HKLException) = 0;
        virtual double affineCrystal(string const & crystal_name, string const & method_name) throw (HKLException) = 0;

        // Calculation functions
        virtual void computeU(void) throw (HKLException) = 0;
        virtual void computeHKL(double & h, double & k, double & l) throw (HKLException) = 0;
        virtual void computeAngles(double h, double k, double l) throw (HKLException) = 0;

      private:

        Geometry * _geometry; //!< The current diffractometer Geometry.
        Sample * _sample; //!< The Sample we are working with.
        SampleList _samples; //!< The SampleList of the diffractometers.
        Mode * _mode; //!< The current Mode.
        ModeList _modes; //!< The available modes.
        PseudoAxeList _pseudoAxes; //!< The available PseudoAxes.
        AffinementList _affinements; //!< The available Affinement methode.

        Diffractometer(void);
      };


    /**
     * @brief The abstract base class to define all different kinds of diffractometers and drive experiments.
     */
    template<typename T>
    class Diffractometer : public DiffractometerInterface, public ObjectWithParameters
      {
      public:

        virtual ~Diffractometer(void);
        bool operator ==(Diffractometer const & diffractometer) const;
        Diffractometer<T> & operator =(Diffractometer<T> const & diffractometer);
        ostream & printToStream(ostream & flux) const;
        ostream & toStream(ostream & flux) const;
        istream & fromStream(istream & flux);

        // source
        void setWaveLength(double wl);
        double getWaveLength(void) const;

        // geometry
        Axe & getAxe(std::string const & name);
        vector<string> const getAxesNames(void) const;
        vector<string> const getSampleAxesNames(void) const;
        vector<string> const getDetectorAxesNames(void) const;
        /*
           void setAxeValue(string const & name,
           double value) throw (HKLException);
           double const getAxeValue(string const & name) const throw (HKLException);
           */
        void setAxesFromCrystalReflection(string const & name, unsigned int index) throw (HKLException); 

        // pseudoAxes
        vector<string> const getPseudoAxesNames(void) const;
        virtual PseudoAxeInterface & getPseudoAxe(string const & name);
        /*
           string const & getPseudoAxeDescription(string const & name) const throw (HKLException);
           vector<string> const getPseudoAxeParametersNames(string const & name) const throw (HKLException);
           double getPseudoAxeParameterValue(string const & pseudoAxe_name,
           string const & parameter_name) const throw (HKLException);
           void setPseudoAxeParameterValue(string const & pseudoAxe_name,
           string const & parameter_name,
           double value) throw (HKLException);
           void initializePseudoAxe(string const & name) throw (HKLException);
           bool getPseudoAxeIsValid(string const & name) const throw (HKLException);
           double getPseudoAxeValue(string const & name) const throw (HKLException);
           void setPseudoAxeValue(string const & name, double value) throw (HKLException);
           */
        // Crystals
        vector<string> const getCrystalNames(void) const;
        string const & getCurrentCrystalName(void) const throw (HKLException);
        void setCurrentCrystal(string const & name) throw (HKLException);
        void addNewCrystal(string const & name) throw (HKLException);
        void setCrystalLattice(string const & name,
                               double a, double b, double c,
                               double alpha, double beta, double gamma) throw (HKLException);
        void getCrystalLattice(string const & name,
                               double * a, double * b, double * c,
                               double * alpha, double * beta, double * gamma) const throw (HKLException);
        void getCrystalReciprocalLattice(string const & name,
                                         double * a, double * b, double * c,
                                         double * alpha, double * beta, double * gamma) const throw (HKLException);
        vector<string> getCrystalParametersNames(string const & name) const throw (HKLException);
        void getCrystalParameterValues(string const & crystal_name,
                                       string const & parameter_name,
                                       double * value,
                                       double * min,
                                       double *max,
                                       bool * to_fit) const throw (HKLException);
        void setCrystalParameterValues(string const & crystal_name,
                                       string const & parameter_name,
                                       double value,
                                       double min,
                                       double max,
                                       bool to_fit) throw (HKLException);
        smatrix getCrystal_UB(string const & name) const throw (HKLException);
        double getCrystalFitness(string const & name) throw (HKLException);
        void delCrystal(string const & name) throw (HKLException);
        void delAllCrystals(void);
        void copyCrystalAsNew(string const & from,
                              string const & to) throw (HKLException);
        void renameCrystal(string const & from, string const & to) throw (HKLException);

        // reflections
        unsigned int getCrystalNumberOfReflection(string const & name) const throw (HKLException);
        unsigned int addCrystalReflection(string const & name, 
                                          double h, double k, double l,
                                          int relevance, bool flag) throw (HKLException);
        double getCrystalReflectionAxeAngle(string const & crystalName,
                                            unsigned int index,
                                            string const & axeName) const throw (HKLException);
        void setCrystalReflectionParameters(string const & name,
                                            unsigned int index,
                                            double h, double k, double l,
                                            int relevance, bool flag) throw (HKLException);
        void getCrystalReflectionParameters(string const & name,
                                            unsigned int index,
                                            double * h, double * k, double *l,
                                            int * relevance, bool * flag) const throw (HKLException);
        void delCrystalReflection(string const & name,
                                  unsigned int index) throw (HKLException);
        unsigned int copyCrystalReflectionFromTo(string const & from, 
                                                 unsigned int ifrom,
                                                 string const & to) throw (HKLException);

        // Modes
        vector<string> getModeNames(void) const;
        string const & getCurrentModeName(void) const throw (HKLException);
        string const & getModeDescription(string const & name) const throw (HKLException);
        vector<string> getModeParametersNames(string const & name) const throw (HKLException);
        double getModeParameterValue(string const & mode_name,
                                     string const & parameter_name) const throw (HKLException);
        void setModeParameterValue(string const & mode_name,
                                   string const & parameter_name,
                                   double value) throw (HKLException);
        void setCurrentMode(string const & name) throw (HKLException);

        // Affinement functions
        vector<string> getAffinementNames(void) const;
        unsigned int getAffinementMaxIteration(string const & name) const throw (HKLException);
        void setAffinementMaxIteration(string const & name, unsigned int max) throw (HKLException);
        unsigned int getAffinementIterations(string const & name) const throw (HKLException);
        double affineCrystal(string const & crystal_name, string const & method_name) throw (HKLException);

        // Calculation functions
        void computeU(void) throw (HKLException);
        void computeHKL(double & h, double & k, double & l) throw (HKLException);
        void computeAngles(double h, double k, double l) throw (HKLException);

      protected:
        T m_geometry; //!< The current diffractometer Geometry.
        Crystal<T> * m_crystal; //!< The Crystal we are working with.
        CrystalList<T> m_crystalList; //!< The CrystalList of the diffractometer.
        Mode<T> * m_mode; //!< The Mode describes the way we use the diffractometer.
#ifdef MSVC6
        MyStarMap<Mode<T> *> m_modeList; //!< the available modes.
        MyStarMap<PseudoAxeInterface *> m_pseudoAxeList; //!< the available modes.
#else
        MyMap<Mode<T> *> m_modeList; //!< the available modes.
        MyMap<PseudoAxe<T> *> m_pseudoAxeList; //!< The map containing the pseudo axes
#endif
        AffinementList m_affinementList; //!< The available Affinement methode.

        Diffractometer(void);
      };

    /**
     * @brief Default constructor
     *
     * - protected to make sure this class is abstract.
     * - by default a diffractometer contain alvays a default crystal.
     */
    template<typename T>
    Diffractometer<T>::Diffractometer(void) : ObjectWithParameters()
      {
        m_mode = NULL;
        setCurrentCrystal(DEFAULT_CRYSTAL_NAME);

        // On s'occupe de remplir avec les bons affinements la liste.
        m_affinementList.add(new affinement::Simplex);
      }

    /**
     * @brief Destructor
     */
    template<typename T>
    Diffractometer<T>::~Diffractometer(void)
      {
        m_affinementList.free();
      }

    /**
     * @brief Are two Diffractometer equals ?
     * @param diffractometer the Diffractomter to compare with
     * @return The comparison of the two Diffractometer.
     */
    template<typename T>
    bool
    Diffractometer<T>::operator ==(Diffractometer const & diffractometer) const
      {
        return Object::operator==(diffractometer)
        && m_crystalList == diffractometer.m_crystalList
        && m_modeList == diffractometer.m_modeList
        && m_affinementList == diffractometer.m_affinementList
        && m_pseudoAxeList == diffractometer.m_pseudoAxeList;
      }

    template<typename T>
    Diffractometer<T> &
    Diffractometer<T>::operator =(Diffractometer<T> const & diffractometer)
      {
        ObjectWithParameters::operator=(diffractometer);
        m_geometry = diffractometer.m_geometry;
        m_crystalList = diffractometer.m_crystalList;
        m_modeList = diffractometer.m_modeList;
        m_pseudoAxeList = diffractometer.m_pseudoAxeList;
        m_affinementList = diffractometer.m_affinementList;
        if (diffractometer.m_mode)
            setCurrentMode(diffractometer.getCurrentModeName());
        else
            m_mode = NULL;
        if (diffractometer.m_crystal)
            setCurrentCrystal(diffractometer.getCurrentCrystalName());
        else
            m_crystal = NULL;
        return *this;
      }

    /**
     * @brief Print the state of the current diffractometer on a ostream.
     * @param flux The ostrema to write into.
     * @return the flux modified.
     */
    template<typename T>
    ostream &
    Diffractometer<T>::printToStream(ostream & flux) const
      {
        flux << showpoint << fixed;
        flux << endl;
        flux << "Diffractometer: \"" << get_name() << "\"" << endl;

        //Parameters
        ObjectWithParameters::printToStream(flux);

        //geometry
        flux << "Geometry : " << endl;
        flux << m_geometry << endl;

        //mode
        flux << "Modes : " << endl;
        vector<string> modeNames = getModeNames();
        vector<string>::const_iterator m_iter = modeNames.begin();
        vector<string>::const_iterator m_end = modeNames.end();
        while(m_iter != m_end)
          {
            flux << " \"" << *m_iter << "\"";
            if (m_mode && *m_iter == m_mode->get_name())
                flux << "(*)  ";
            else
                flux << "  ";
            flux << endl;
            ++m_iter;
          }
        flux << endl;

        //pseudoAxes
        flux << "PseudoAxe : " << endl;
#ifdef MSVC6
        typename MyStarMap<PseudoAxe<T> *>::const_iterator p_iter = m_pseudoAxeList.begin();
        typename MyStarMap<PseudoAxe<T> *>::const_iterator p_end = m_pseudoAxeList.end();
#else
        typename MyMap<PseudoAxe<T> *>::const_iterator p_iter = m_pseudoAxeList.begin();
        typename MyMap<PseudoAxe<T> *>::const_iterator p_end = m_pseudoAxeList.end();
#endif
        p_iter = m_pseudoAxeList.begin();
        p_end = m_pseudoAxeList.end();
        while(p_iter != p_end)
          {
            flux << " \"" << p_iter->first << "\" : ";
            if (p_iter->second->get_initialized())
                flux << p_iter->second->get_value();
            else
                flux << "not yet initialize";
            flux << endl;
            ++p_iter;
          }
        flux << endl;


        //crystals   
        typename CrystalList<T>::const_iterator c_iter = m_crystalList.begin();
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

    /**
     * \brief Save the Diffractometer into a stream.
     * \param flux the stream to save the Diffractometer into.
     * \return The stream with the Diffractometer.
     */
    template<typename T>
    ostream &
    Diffractometer<T>::toStream(ostream & flux) const
      {
        flux << " " << HKL_VERSION;
        ObjectWithParameters::toStream(flux);
        m_geometry.toStream(flux);
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

    /**
     * \brief Restore a Diffractometer from a stream.
     * \param flux The stream containing the Diffractometer.
     */
    template<typename T>
    istream &
    Diffractometer<T>::fromStream(istream & flux)
      {
        unsigned int version;
        string crystal_name;
        string mode_name;
        string junk;

        flux >> version;
        if (version == HKL_VERSION)
          {
            ObjectWithParameters::fromStream(flux);
            m_geometry.fromStream(flux);
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

    /******************************/
    /* Modification of the Source */
    /******************************/

    /**
     * @brief Get the X-Ray wave length use by the diffractometer
     */
    template<typename T>
    double
    Diffractometer<T>::getWaveLength(void) const
      {
        return m_geometry.get_source().get_waveLength();
      }

    /**
     * @brief Set the X-Ray wave length use by the diffractometer
     * @param wl the new wave length
     */
    template<typename T>
    void
    //!< @todo gérer HKLException
    Diffractometer<T>::setWaveLength(double wl)
      {
        m_geometry.get_source().setWaveLength(wl);
      }

    /********************************/
    /* Modification of the geometry */
    /********************************/

    /**
     * @brief Get a reference on the axe named:
     * @param name The name of the axe
     * @return The reference on the axe.
     */
    template<typename T>
    Axe &
    Diffractometer<T>::getAxe(string const & name)
      {
        return m_geometry.get_axe(name);
      }

    /**
     * @brief Get a list of the axes names
     * @return the list of all the axes names.
     */
    template<typename T>
    vector<string> const
    Diffractometer<T>::getAxesNames(void) const
      {
        vector<MyString> myNames = m_geometry.getAxesNames();
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

    /**
     * @brief Get a list of the sample axes names
     * @return The list of all the sample axes names.
     */
    template<typename T>
    vector<string> const
    Diffractometer<T>::getSampleAxesNames(void) const
      {
        vector<string> names;
        vector<Axe *> const & axes = m_geometry.get_samples();
        vector<Axe *>::const_iterator iter = axes.begin();
        vector<Axe *>::const_iterator end = axes.end();
        while(iter != end)
          {
            names.push_back((*iter)->get_name());
            ++iter;
          }
        return names;
      }

    /**
     * @brief Get a list of the detector  axes names
     * @return The list of all the detector axes names.
     */
    template<typename T>
    vector<string> const
    Diffractometer<T>::getDetectorAxesNames(void) const
      {
        vector<string> names;
        vector<Axe *> const & axes = m_geometry.get_detectors();
        vector<Axe *>::const_iterator iter = axes.begin();
        vector<Axe *>::const_iterator end = axes.end();
        while(iter != end)
          {
            names.push_back((*iter)->get_name());
            ++iter;
          }
        return names;
      }

    /**
     * @brief Set the Axe current value.
     * @param name The Axe name.
     * @param value The value to set.
     */
    /*
       template<typename T>
       void
       Diffractometer<T>::setAxeValue(string const & name,
       double value) throw (HKLException)
       {
       m_geometry.get_axe(name).set_value(value);
       }
       */

    /**
     * @brief Get the Axe current value.
     * @param name The Axe name.
     * @return The current Value.
     */
    /*
       template<typename T>
       double const
       Diffractometer<T>::getAxeValue(string const & name) const throw (HKLException)
       {
       return m_geometry.get_axe(name).get_value();
       }
       */

    template<typename T>
    void
    Diffractometer<T>::setAxesFromCrystalReflection(string const & name, unsigned int index) throw (HKLException)
      {
        m_geometry = m_crystalList[name].getReflection(index).get_geometry();
      }

    /**********************************/
    /* Modification of the pseudoAxes */
    /**********************************/

    /**
     * @brief Get a reference on the PseudoAxe named:
     * @param name The name of the pseudoaxe
     * @return The reference on the pseudoaxe.
     */
    template<typename T>
    PseudoAxeInterface &
    Diffractometer<T>::getPseudoAxe(string const & name)
      {
        return *m_pseudoAxeList[name];
      }

    /**
     * @brief Get a list of the PseudoAxe names
     * @return The list of all the PseudoAxe.
     */
    template<typename T>
    vector<string> const
    Diffractometer<T>::getPseudoAxesNames(void) const
      {
        vector<MyString> myNames = m_pseudoAxeList.getNames();
        vector<string> names;
        vector<MyString>::iterator iter = myNames.begin();
        vector<MyString>::iterator last = myNames.end();
        while(iter != last)
          {
            names.push_back(*iter);
            ++iter;
          }
        return names;
      }

    /** 
     * @brief Get the description of the PseudoAxe.
     * @param name Name of the PseudoAxe.
     * @throw HKLException when the name is not a valid PseudoAxe.
     * @return The description of the PseudoAxe. 
     */
    /*
       template<typename T>
       string const &
       Diffractometer<T>::getPseudoAxeDescription(string const & name) const throw (HKLException)
       {
       return m_pseudoAxeList[name]->get_description();
       }
       */
    /**
     * @brief Get a list of all the parameters of a PseudoAxe.
     * @param name The name of the PseudoAxe.
     * @throw HKLException when the name is not a valid PseudoAxe.
     * @return The list of all the parameters of this PseudoAxe.
     */
    /*
       template<typename T>
       vector<string> const
       Diffractometer<T>::getPseudoAxeParametersNames(string const & name) const throw(HKLException)
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
       */
    /**
     * \brief Get the value of a parameter of a PseudoAxe
     * \param pseudoAxe_name The name of the PseudoAxe.
     * \param parameter_name the name of the parameter.
     * \throw HKLException when the pseudoAxe_name or the parameter_name are wrong.
     * \return The value of the parameter.
     */
    /*
       template<typename T>
       double
       Diffractometer<T>::getPseudoAxeParameterValue(string const & pseudoAxe_name,
       string const & parameter_name) const throw (HKLException)
       {
       return m_pseudoAxeList[pseudoAxe_name]->getParameterValue(parameter_name); 
       }
       */

    /**
     * \brief Set the value of a parameter of a PseudoAxe.
     * \param pseudoAxe_name The name of the PseudoAxe
     * \param parameter_name the name of the parameter.
     * \param value the value we want set.
     * \throw HKLException when the pseudoAxe_name or the parameter_name are wrong.
     */
    /*
       template<typename T>
       void
       Diffractometer<T>::setPseudoAxeParameterValue(string const & pseudoAxe_name,
       string const & parameter_name,
       double value) throw (HKLException)
       {
       m_pseudoAxeList[pseudoAxe_name]->setParameterValue(parameter_name, value);
       }
       */

    /** 
     * @brief Initialize a PseudoAxe
     * @param name The name of the PseudoAxe.
     * @throw HKLException when the name is not a valid PseudoAxe.
     */
    /*
       template<typename T>
       void
       Diffractometer<T>::initializePseudoAxe(string const & name) throw (HKLException)
       {
       m_pseudoAxeList[name]->initialize();
       }
       */

    /** 
     * @brief Is a pseudoAxe valid
     * @param name The name of the PseudoAxe
     * @throw HKLException when the name is not a valid PseudoAxe.
     * @return The validity of the PseudoAxe.
     *
     * A PseudoAxe is valid if its computation have a meaning in the actual context.
     * for exemple the pseudoAxe::Psi is valid if the Q vector is the same than the
     * initialization one.
     */
    /*
       template<typename T>
       bool
       Diffractometer<T>::getPseudoAxeIsValid(string const & name) const throw (HKLException)
       {
       return m_pseudoAxeList[name]->isValid();
       }
       */

    /*!
     * \brief Get the value of a PseudoAxe.
     * \param name The name of the PseudoAxe.
     * \return The value of the PseudoAxe.
     * \throw HKLException The pseudoaxe name is wrong.
     */
    /*
       template<typename T>
       double
       Diffractometer<T>::getPseudoAxeValue(string const & name) const throw (HKLException)
       {
       return m_pseudoAxeList[name]->get_value();
       }
       */

    /*!
     * \brief Set the value of a PseudoAxe.
     * \param name The name of the PseudoAxe.
     * \param value The value we want set.
     * \throw HKLException The pseudoAxe name is wrong.
     */
    /*
       template<typename T>
       void
       Diffractometer<T>::setPseudoAxeValue(string const & name, double value) throw (HKLException)
       {
       m_pseudoAxeList[name]->set_value(value);
       }
       */

    /*****************************/
    /* Modifications of crystals */
    /*****************************/

    /**
     * @brief Get a vector of string fill with the crystal names.
     */
    template<typename T>
    vector<string> const
    Diffractometer<T>::getCrystalNames(void) const
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

    /**
     * @brief Get the name of the currentCrystal as a string
     */
    template<typename T>
    string const &
    Diffractometer<T>::getCurrentCrystalName(void) const throw (HKLException)
      {
        if (!m_crystal)
            HKLEXCEPTION("No current crystal set.",
                         "Please select a crystal.");

        return m_crystal->get_name();
      }

    /**
     * @brief Choose the crystal to work with.
     * @param name A string containing the name of the %Crystal to use with the diffractometer.
     */
    template<typename T>
    void
    Diffractometer<T>::setCurrentCrystal(string const & name) throw (HKLException)
      {
        m_crystal = &m_crystalList[name];
      }

    /**
     * @brief Add a new crystal into the crystal list.
     * @param name A string containing the name of the %Crystal to add.
     */
    template<typename T>
    void
    Diffractometer<T>::addNewCrystal(string const & name) throw (HKLException)
      {
        m_crystalList.add(Crystal<T>(name));
      }

    /**
     * @brief Set the crystal Parameters
     * @param name
     * @param a
     * @param b
     * @param c
     * @param alpha
     * @param beta
     * @param gamma
     */
    template<typename T>
    void
    Diffractometer<T>::setCrystalLattice(string const & name,
                                         double a, double b, double c,
                                         double alpha, double beta, double gamma) throw (HKLException)
      {
        m_crystalList[name].setLattice(a, b, c, alpha, beta, gamma);
      }

    /**
     * @brief Get the crystal Parameters
     * @param name
     * @param a
     * @param b
     * @param c
     * @param alpha
     * @param beta
     * @param gamma
     */
    template<typename T>
    void
    Diffractometer<T>::getCrystalLattice(string const & name,
                                         double * a, double * b, double * c,
                                         double * alpha, double * beta, double * gamma) const throw (HKLException)
      {
        m_crystalList[name].getLattice(a, b, c, alpha, beta, gamma);  
      }

    /**
     * @brief Get the crystal Parameters
     * @param name
     * @param a
     * @param b
     * @param c
     * @param alpha
     * @param beta
     * @param gamma
     */
    template<typename T>
    void
    Diffractometer<T>::getCrystalReciprocalLattice(string const & name,
                                                   double * a, double * b, double * c,
                                                   double * alpha, double * beta, double * gamma) const throw (HKLException)
      {
        m_crystalList[name].getReciprocalLattice(a, b, c, alpha, beta, gamma);   
      }

    /**
     * \brief Get the names of the parameters of a crystal.
     * \param name The name of the crystal.
     * \return A vector with the parameters names.
     */
    template<typename T>
    vector<string>
    Diffractometer<T>::getCrystalParametersNames(string const & name) const throw (HKLException)
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

    /**
     * @brief Get the values store in the %FitParameters of a %Crystal.
     * @param[in] crystal_name The name of the crystal.
     * @param[in] parameter_name The name of the parameter.
     * @param[out] value The value of the parameter.
     * @param[out] min The allow minimum value.
     * @param[out] max The allow maximum value.
     * @param[out] to_fit The flag saying if the parameter must be fit.
     */
    template<typename T>
    void
    Diffractometer<T>::getCrystalParameterValues(string const & crystal_name,
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

    /**
     * @brief Set the values store in the %FitParameters of a %Crystal.
     * @param[in] crystal_name The name of the crystal.
     * @param[in] parameter_name The name of the parameter.
     * @param[in] value The value of the parameter.
     * @param[in] min The allow minimum value.
     * @param[in] max The allow maximum value.
     * @param[in] to_fit The flag saying if the parameter must be fit.
     */
    template<typename T>
    void
    Diffractometer<T>::setCrystalParameterValues(string const & crystal_name,
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

    /**
     * @brief get the UB matrix of a %Crystal.
     */
    template<typename T>
    smatrix
    Diffractometer<T>::getCrystal_UB(string const & name) const throw (HKLException)
      {
        Crystal<T> const & crystal = m_crystalList[name];
        return crystal.get_U() * crystal.get_B();
      }

    /**
     * @brief get the fitness of a %Crystal.
     * @param name The name of the %Crystal.
     * @return the fitness of the %Crystal.
     */
    template<typename T>
    double
    Diffractometer<T>::getCrystalFitness(string const & name) throw (HKLException)
      {
        return m_crystalList[name].fitness();
      }

    /**
     * @brief Delete a crystal from the crystal list.
     * @param name
     * 
     * if the crystal deleted was the currentCrystal, unset the m_crystal pointer.
     */
    template<typename T>
    void
    Diffractometer<T>::delCrystal(string const & name) throw (HKLException)
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

    /**
     * @brief Delete all crystals from the crystal list.
     *
     * set the default crystal as current crystal.
     */
    template<typename T>
    void
    Diffractometer<T>::delAllCrystals(void)
      {
        m_crystalList.clear();
        setCurrentCrystal(DEFAULT_CRYSTAL_NAME);
      }

    /**
     * @brief Copy a crystal to an other one.
     * @param from Name of the copied crystal.
     * @param to Name of the new crystal.
     */
    template<typename T>
    void
    Diffractometer<T>::copyCrystalAsNew(string const & from,
                                        string const & to) throw (HKLException)
      {
        Crystal<T> crystal(m_crystalList[from]);
        crystal.set_name(to);
        m_crystalList.add(crystal);
      }

    /**
     * \brief Rename a crystal
     * \param from The name of the crystal to rename.
     * \param to The name of the renames crystal.
     */
    template<typename T>
    void
    Diffractometer<T>::renameCrystal(string const & from,
                                     string const & to) throw (HKLException)
      {

        if (from != to)
          {
            Crystal<T> crystal(m_crystalList[from]);
            crystal.set_name(to);

            m_crystalList.add(crystal);  
            if (from == m_crystal->get_name())
                setCurrentCrystal(to);
            m_crystalList.remove(from);
          }
      }

    /********************************/
    /* Modifications of reflections */
    /********************************/

    /**
     * @brief return the number of reflections of a crystal.
     * @param name the crystal name.
     * @return the number of reflections.
     */
    template<typename T>
    unsigned int
    Diffractometer<T>::getCrystalNumberOfReflection(string const & name) const throw (HKLException)
      {
        return m_crystalList[name].get_reflectionList().size();
      }

    /**
     * @brief add a reflection to the current crystal.
     * @param name The name of the crystal which containe the new reflection.
     * @param h
     * @param k
     * @param l
     * @param relevance
     * @param flag (is the reflection use for calculation).
     * @return the index of the reflection we have just added.
     */
    template<typename T>
    unsigned int
    Diffractometer<T>::addCrystalReflection(string const & name,
                                            double h, double k, double l,
                                            int relevance, bool flag) throw (HKLException)
      {
        return m_crystalList[name].addReflection(Reflection<T>(m_geometry, h, k, l, relevance, flag));
      }

    /**
     * @brief get the angle values of a refelction of a crystal.
     * @param crystalName of the crystal
     * @param index of the reflection
     * @param axeName of the axe.
     */
    template<typename T>
    double
    Diffractometer<T>::getCrystalReflectionAxeAngle(string const & crystalName, 
                                                    unsigned int index,
                                                    string const & axeName) const throw (HKLException)
      {
        return m_crystalList[crystalName].getReflection(index).get_geometry().get_axe(axeName).get_value();
      }

    /**
     * @brief delete the reflection from thr currentcrystal
     * @param name The name of the crystal wich containe the reflection list.
     * @param index The reflection to delete.
     */
    template<typename T>
    void
    Diffractometer<T>::delCrystalReflection(string const & name,
                                            unsigned int index) throw (HKLException)
      {
        m_crystalList[name].delReflection(index);
      }

    /**
     * @brief Copy a reflection from a crytal to an other one.
     * @param from The first crystal.
     * @param ifrom The index of the reflection.
     * @param to The second crystal.
     * \return The index of the added reflection.
     */
    template<typename T>
    unsigned int
    Diffractometer<T>::copyCrystalReflectionFromTo(string const & from,
                                                   unsigned int ifrom,
                                                   string const & to) throw (HKLException)
      {
        Crystal<T> & from_crystal = m_crystalList[from];
        Crystal<T> & to_crystal = m_crystalList[to];

        return to_crystal.addReflection(from_crystal.getReflection(ifrom));
      }

    /**
     * @brief Get the parameters related to a reflection of a crystal.
     * @param name
     * @param index
     * @param h
     * @param k
     * @param l
     * @param relevance
     * @param flag
     */
    template<typename T>
    void
    Diffractometer<T>::getCrystalReflectionParameters(string const & name,
                                                      unsigned int index,
                                                      double * h, double * k, double *l,
                                                      int * relevance, bool * flag) const throw (HKLException)
      {
        Reflection<T> const & R = m_crystalList[name].getReflection(index);
        *h = R.get_h();
        *k = R.get_k();
        *l = R.get_l();
        *relevance = R.get_relevance();
        *flag = R.get_flag();
      }

    /**
     * @brief Modify a reflection of a crystal.
     * @param name the name of the crystal
     * @param index the index of the reflection.
     * @param h
     * @param k
     * @param l
     * @param relevance
     * @param flag
     */
    template<typename T>
    void
    Diffractometer<T>::setCrystalReflectionParameters(string const & name,
                                                      unsigned int index,
                                                      double h, double k, double l,
                                                      int relevance, bool flag) throw (HKLException)
      {
        Reflection<T> & r = m_crystalList[name].getReflection(index);
        r.set_h(h);
        r.set_k(k);
        r.set_l(l);
        r.set_relevance(relevance);
        r.set_flag(flag);
      }

    /********************************/
    /* Gestion des modes de calcule */
    /********************************/

    /**
     * @brief Get the available modes.
     * @return An array of MyString with all modes.
     */
    template<typename T>
    vector<string>
    Diffractometer<T>::getModeNames(void) const
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

    /**
     * @brief Get the name of the current Mode.
     * @return The name of the current Mode.
     */
    template<typename T>
    string const &
    Diffractometer<T>::getCurrentModeName(void) const throw (HKLException)
      {
        if (!m_mode)
            HKLEXCEPTION("No current mode set.",
                         "please select a mode.");
        else
            return m_mode->get_name();
      }

    /**
     * @brief Get a Mode description.
     * @param name The name of the mode.
     * @return The description of a Mode.
     */
    template<typename T>
    string const &
    Diffractometer<T>::getModeDescription(string const & name) const throw (HKLException)
      {
        return m_modeList[name]->get_description();
      }

    /**
     * @brief Get the parametres names use by a mode
     * @param name the name of the mode.
     * @return An array of MyString will all the parameters names.
     */
    template<typename T>
    vector<string>
    Diffractometer<T>::getModeParametersNames(string const & name) const throw (HKLException)
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

    /**
     * @brief get the parameter value of a Mode
     * @param mode_name The name of the mode.
     * @param parameter_name The name of the parameter.
     * @return The value of the parameter.
     */
    template<typename T>
    double
    Diffractometer<T>::getModeParameterValue(string const & mode_name,
                                             string const & parameter_name) const throw (HKLException)
      {
        return m_modeList[mode_name]->getParameterValue(parameter_name);
      }

    /**
     * @brief Set the parameter value of a Mode
     * @param mode_name The name of the mode.
     * @param parameter_name The name of the parameter.
     * @param value The value to set.
     */
    template<typename T>
    void
    Diffractometer<T>::setModeParameterValue(string const & mode_name,
                                             string const & parameter_name,
                                             double value) throw (HKLException)
      {
        m_modeList[mode_name]->setParameterValue(parameter_name, value);
      }

    /**
     * @brief Change the current computational mode.
     * @param name The name of the mode you want to use.
     */
    template<typename T>
    void
    Diffractometer<T>::setCurrentMode(string const & name) throw (HKLException)
      {
        m_mode = m_modeList[name];
      }

    /**************/
    /* Affinement */
    /**************/

    /**
     * @brief Get the available Affinement.
     * @return An array of MyString with all the affinement names.
     */
    template<typename T>
    vector<string>
    Diffractometer<T>::getAffinementNames(void) const
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

    /**
     * @brief Get the maximum number of iteration for a fit methode.
     * @param name The name of the fit methode.
     * @return the maximum number of iterations.
     */
    template<typename T>
    unsigned int
    Diffractometer<T>::getAffinementMaxIteration(string const & name) const throw (HKLException)
      {
        return m_affinementList[name]->get_nb_max_iteration();
      }

    /**
     * @brief Set the maximum number of iteration for a fit methode.
     * @param name The name of the fit methode.
     * @param max the value to set.
     */
    template<typename T>
    void
    Diffractometer<T>::setAffinementMaxIteration(string const & name, unsigned int max) throw (HKLException)
      {
        m_affinementList[name]->set_nb_max_iteration(max);
      }

    /**
     * @brief Get the number of iteration ran by a fit methode.
     * @param name The name of the fit methode.
     * @return the number of iterations.
     */
    template<typename T>
    unsigned int
    Diffractometer<T>::getAffinementIterations(string const & name) const throw (HKLException)
      {
        return m_affinementList[name]->get_nb_iteration();
      }

    /**
     * @brief fit the crystal Parameters of a %Crystal
     * @param crystal_name The %Crystal name to fit.
     * @param method_name The %Affinement name methode to use.
     * @return The fitness of the fitted crystal.
     */
    template<typename T>
    double
    Diffractometer<T>::affineCrystal(string const & crystal_name, string const & method_name) throw (HKLException)
      {
        Crystal<T> & crystal = m_crystalList[crystal_name];
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

    /************/
    /* Calcules */
    /************/

    /**
     * @brief Compute the orientation matrix from two basic non-parallel reflections.
     *
     * Compute the orientation matrix from two basic non-parallel reflections.
     */
    template<typename T>
    void
    Diffractometer<T>::computeU(void) throw (HKLException)
      {
        if (!m_crystal)
            HKLEXCEPTION("Cannot compute U with no current crystal set.",
                         "Please select a crystal.");
        else
            m_crystal->computeU();
      }

    /**
     * @brief Compute (h,k,l) from a sample of angles.
     * @param[out] h The scaterring vector first element.
     * @param[out] k The scaterring vector second element.
     * @param[out] l The scaterring vector third element.
     * @exception det(A)=0
     * 
     * Solve a linear system Ax = b where A is the product of the rotation matrices 
     * OMEGA, CHI, PHI by the orientation matrix U and the crystal matrix B. b is the
     * scattering vector (q,0,0) and x = (h,k,l). Raise an exception when det(A)=0.
     */
    template<typename T>
    void
    Diffractometer<T>::computeHKL(double & h, double & k, double & l) throw (HKLException)
      {
        if (!m_crystal)
            HKLEXCEPTION("Cannot compute HKL with no current crystal selected.",
                         "Please select a crystal.");
        else
          {
            smatrix UB = m_crystal->get_U() * m_crystal->get_B();
            m_geometry.computeHKL(h, k, l, UB);
          }
      }

    /**
     * @brief The main function to get a sample of angles from (h,k,l).
     * @param h The scaterring vector first element.
     * @param k The scaterring vector second element.
     * @param l The scaterring vector third element.
     *
     *  The main function to get a sample of angles from (h,k,l).
     */
    template<typename T>
    void
    Diffractometer<T>::computeAngles(double h, double k, double l) throw (HKLException)
      {
        if (!m_mode)
            HKLEXCEPTION("Cannot compute the angles with no current mode set.",
                         "Please select a mode.");

        if (!m_crystal)
            HKLEXCEPTION("Cannot compute the angles with no current crystal selected.",
                         "Please select a crystal.");

        smatrix UB = m_crystal->get_U() * m_crystal->get_B();
        m_mode->computeAngles(h, k, l, UB, m_geometry);
      }

} // namespace hkl

/*!
 * @brief Surcharge de l'operateur << pour la class %Diffractometer
 * @param flux 
 * @param diffractometer 
 * @return 
 */
template<typename T>
ostream &
operator << (ostream & flux, hkl::Diffractometer<T> const & diffractometer)
{
    return diffractometer.printToStream(flux);
}

#endif // _DIFFRACTOMETER_H_
