#ifndef _DIFFRACTOMETER_H_
#define _DIFFRACTOMETER_H_

#include "config.h" // just for the pragma of VC++6

#include <string>
#include <vector>
#include <iostream>

#include "axe.h"
#include "mode.h"
#include "crystal.h"
#include "svecmat.h"
#include "mystring.h"
#include "geometry.h"
#include "pseudoaxe.h"
#include "affinement.h"
#include "reflection.h"
#include "crystallist.h"
#include "HKLException.h"
#include "objectwithparameters.h"

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

    /**
     * @brief The abstract base class to define all different kinds of diffractometers and drive experiments.
     */
    class Diffractometer : public ObjectWithParameters
    {
    public:

      /**
       * @brief Destructor
       */
      virtual ~Diffractometer(void);

      /**
       * @brief Are two Diffractometer equals ?
       * @param diffractometer the Diffractomter to compare with
       * @return The comparison of the two Diffractometer.
       */
      bool operator ==(Diffractometer const & diffractometer) const;

      /**
       * @brief Print the state of the current diffractometer on a ostream.
       * @param flux The ostrema to write into.
       * @return the flux modified.
       */
      ostream & printToStream(ostream & flux) const;

      /**
       * \brief Save the Diffractometer into a stream.
       * \param flux the stream to save the Diffractometer into.
       * \return The stream with the Diffractometer.
       */
      ostream & toStream(ostream & flux) const;

      /**
       * \brief Restore a Diffractometer from a stream.
       * \param flux The stream containing the Diffractometer.
       */
      istream & fromStream(istream & flux);

      /******************************/
      /* Modification of the Source */
      /******************************/

      /**
       * @brief Set the X-Ray wave length use by the diffractometer
       * @param wl the new wave length
       */
      void setWaveLength(double wl);

      /**
       * @brief Get the X-Ray wave length use by the diffractometer
       */
      double getWaveLength(void) const;


      /********************************/
      /* Modification of the geometry */
      /********************************/

      /**
       * @brief Get a list of the axes names
       * @return the list of all the axes names.
       */
      vector<string> const getAxesNames(void) const;

      /**
       * @brief Get a list of the sample axes names
       * @return The list of all the sample axes names.
       */
      vector<string> const getSampleAxesNames(void) const;

      /**
       * @brief Get a list of the detector  axes names
       * @return The list of all the detector axes names.
       */
      vector<string> const getDetectorAxesNames(void) const;

      /**
       * @brief Set the Axe current value.
       * @param name The Axe name.
       * @param value The value to set.
       */
      void setAxeValue(string const & name,
                       double value) throw (HKLException);

      /**
       * @brief Get the Axe current value.
       * @param name The Axe name.
       * @return The current Value.
       */
      double const getAxeValue(string const & name) const throw (HKLException);

      /**********************************/
      /* Modification of the pseudoAxes */
      /**********************************/

      /**
       * @brief Get a list of the PseudoAxe names
       * @return The list of all the PseudoAxe.
       */
      vector<string> const getPseudoAxesNames(void) const;

      /** 
       * @brief Get the description of the PseudoAxe.
       * @param name Name of the PseudoAxe.
       * @throw HKLException when the name is not a valid PseudoAxe.
       * @return The description of the PseudoAxe. 
       */
      string const & getPseudoAxeDescription(string const & name) const throw (HKLException);

      /**
       * @brief Get a list of all the parameters of a PseudoAxe.
       * @param name The name of the PseudoAxe.
       * @throw HKLException when the name is not a valid PseudoAxe.
       * @return The list of all the parameters of this PseudoAxe.
       */
      vector<string> const getPseudoAxeParametersNames(string const & name) const throw (HKLException);

      /**
       * \brief Get the value of a parameter of a PseudoAxe
       * \param pseudoAxe_name The name of the PseudoAxe.
       * \param parameter_name the name of the parameter.
       * \throw HKLException when the pseudoAxe_name or the parameter_name are wrong.
       * \return The value of the parameter.
       */
      double getPseudoAxeParameterValue(string const & pseudoAxe_name,
                                        string const & parameter_name) const throw (HKLException);

      /**
       * \brief Set the value of a parameter of a PseudoAxe.
       * \param pseudoAxe_name The name of the PseudoAxe
       * \param parameter_name the name of the parameter.
       * \param value the value we want set.
       * \throw HKLException when the pseudoAxe_name or the parameter_name are wrong.
       */
      void setPseudoAxeParameterValue(string const & pseudoAxe_name,
                                      string const & parameter_name,
                                      double value) throw (HKLException);

      /** 
       * @brief Initialize a PseudoAxe
       * @param name The name of the PseudoAxe.
       * @throw HKLException when the name is not a valid PseudoAxe.
       */
      void initializePseudoAxe(string const & name) throw (HKLException);

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
      bool getPseudoAxeIsValid(string const & name) const throw (HKLException);

      /*!
       * \brief Get the value of a PseudoAxe.
       * \param name The name of the PseudoAxe.
       * \return The value of the PseudoAxe.
       * \throw HKLException The pseudoaxe name is wrong.
       */
      double getPseudoAxeValue(string const & name) const throw (HKLException);

      /*!
       * \brief Set the value of a PseudoAxe.
       * \param name The name of the PseudoAxe.
       * \param value The value we want set.
       * \throw HKLException The pseudoAxe name is wrong.
       */
      void setPseudoAxeValue(string const & name, double value) throw (HKLException);


      /*****************************/
      /* Modifications of crystals */
      /*****************************/

      /**
       * @brief Get a vector of string fill with the crystal names.
       */
      vector<string> const getCrystalNames(void) const;

      /**
       * @brief Get the name of the currentCrystal as a string
       */
      string const & getCurrentCrystalName(void) const throw (HKLException);

      /**
       * @brief Choose the crystal to work with.
       * @param name A string containing the name of the %Crystal to use with the diffractometer.
       */
      void setCurrentCrystal(string const & name) throw (HKLException);

      /**
       * @brief Add a new crystal into the crystal list.
       * @param name A string containing the name of the %Crystal to add.
       */
      void addNewCrystal(string const & name) throw (HKLException);

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
      void setCrystalLattice(string const & name,
                             double a, double b, double c,
                             double alpha, double beta, double gamma) throw (HKLException);

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
      void getCrystalLattice(string const & name,
                             double * a, double * b, double * c,
                             double * alpha, double * beta, double * gamma) const throw (HKLException);

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
      void getCrystalReciprocalLattice(string const & name,
                                       double * a, double * b, double * c,
                                       double * alpha, double * beta, double * gamma) const throw (HKLException);

      /**
       * \brief Get the names of the parameters of a crystal.
       * \param name The name of the crystal.
       * \return A vector with the parameters names.
       */
      vector<string> getCrystalParametersNames(string const & name) const throw (HKLException);

      /**
       * @brief Get the values store in the %FitParameters of a %Crystal.
       * @param[in] crystal_name The name of the crystal.
       * @param[in] parameter_name The name of the parameter.
       * @param[out] value The value of the parameter.
       * @param[out] min The allow minimum value.
       * @param[out] max The allow maximum value.
       * @param[out] to_fit The flag saying if the parameter must be fit.
       */
      void
      getCrystalParameterValues(string const & crystal_name,
                                string const & parameter_name,
                                double * value,
                                double * min,
                                double *max,
                                bool * to_fit) const throw (HKLException);
      /**
       * @brief Set the values store in the %FitParameters of a %Crystal.
       * @param[in] crystal_name The name of the crystal.
       * @param[in] parameter_name The name of the parameter.
       * @param[in] value The value of the parameter.
       * @param[in] min The allow minimum value.
       * @param[in] max The allow maximum value.
       * @param[in] to_fit The flag saying if the parameter must be fit.
       */
      void
      setCrystalParameterValues(string const & crystal_name,
                                string const & parameter_name,
                                double value,
                                double min,
                                double max,
                                bool to_fit) throw (HKLException);

      /**
       * @brief get the UB matrix of a %Crystal.
       */
      smatrix getCrystal_UB(string const & name) const throw (HKLException);

      /**
       * @brief get the fitness of a %Crystal.
       * @param name The name of the %Crystal.
       * @return the fitness of the %Crystal.
       */
      double getCrystalFitness(string const & name) throw (HKLException);

      /**
       * @brief Delete a crystal from the crystal list.
       * @param name
       * 
       * if the crystal deleted was the currentCrystal, unset the m_crystal pointer.
       */
      void delCrystal(string const & name) throw (HKLException);

      /**
       * @brief Delete all crystals from the crystal list.
       *
       * set the default crystal as current crystal.
       */
      void delAllCrystals(void);

      /**
       * @brief Copy a crystal to an other one.
       * @param from Name of the copied crystal.
       * @param to Name of the new crystal.
       */
      void copyCrystalAsNew(string const & from,
                            string const & to) throw (HKLException);

      /**
       * \brief Rename a crystal
       * \param from The name of the crystal to rename.
       * \param to The name of the renames crystal.
       */
      void renameCrystal(string const & from, string const & to) throw (HKLException);

      /********************************/
      /* Modifications of reflections */
      /********************************/

      /**
       * @brief return the number of reflections of a crystal.
       * @param name the crystal name.
       * @return the number of reflections.
       */
      unsigned int getCrystalNumberOfReflection(string const & name) const throw (HKLException);

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
      unsigned int addCrystalReflection(string const & name, 
                                        double h, double k, double l,
                                        int relevance, bool flag) throw (HKLException);

      /**
       * @brief get the angle values of a refelction of a crystal.
       * @param crystalName of the crystal
       * @param index of the reflection
       * @param axeName of the axe.
       */
      double getCrystalReflectionAxeAngle(string const & crystalName,
                                          unsigned int index,
                                          string const & axeName) const throw (HKLException);

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
      void setCrystalReflectionParameters(string const & name,
                                          unsigned int index,
                                          double h, double k, double l,
                                          int relevance, bool flag) throw (HKLException);

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
      void getCrystalReflectionParameters(string const & name,
                                          unsigned int index,
                                          double * h, double * k, double *l,
                                          int * relevance, bool * flag) const throw (HKLException);

      /**
       * @brief delete the reflection from thr currentcrystal
       * @param name The name of the crystal wich containe the reflection list.
       * @param index The reflection to delete.
       */
      void delCrystalReflection(string const & name,
                                unsigned int index) throw (HKLException);

      /**
       * @brief Copy a reflection from a crytal to an other one.
       * @param from The first crystal.
       * @param ifrom The index of the reflection.
       * @param to The second crystal.
       * \return The index of the added reflection.
       */
      unsigned int copyCrystalReflectionFromTo(string const & from, 
                                               unsigned int ifrom,
                                               string const & to) throw (HKLException);


      /********************************/
      /* Gestion des modes de calcule */
      /********************************/

      /**
       * @brief Get the available modes.
       * @return An array of MyString with all modes.
       */
      vector<string> getModeNames(void) const;

      /**
       * @brief Get the name of the current Mode.
       * @return The name of the current Mode.
       */
      string const & getCurrentModeName(void) const throw (HKLException);

      /**
       * @brief Get a Mode description.
       * @param name The name of the mode.
       * @return The description of a Mode.
       */
      string const & getModeDescription(string const & name) const throw (HKLException);

      /**
       * @brief Get the parametres names use by a mode
       * @param name the name of the mode.
       * @return An array of MyString will all the parameters names.
       */
      vector<string> getModeParametersNames(string const & name) const throw (HKLException);

      /**
       * @brief get the parameter value of a Mode
       * @param mode_name The name of the mode.
       * @param parameter_name The name of the parameter.
       * @return The value of the parameter.
       */
      double getModeParameterValue(string const & mode_name,
                                   string const & parameter_name) const throw (HKLException);

      /**
       * @brief Set the parameter value of a Mode
       * @param mode_name The name of the mode.
       * @param parameter_name The name of the parameter.
       * @param value The value to set.
       */
      void setModeParameterValue(string const & mode_name,
                                 string const & parameter_name,
                                 double value) throw (HKLException);

      /**
       * @brief Change the current computational mode.
       * @param name The name of the mode you want to use.
       */
      void setCurrentMode(string const & name) throw (HKLException);

      /**************/
      /* Affinement */
      /**************/

      /**
       * @brief Get the available Affinement.
       * @return An array of MyString with all the affinement names.
       */
      vector<string> getAffinementNames(void) const;

      /**
       * @brief Get the maximum number of iteration for a fit methode.
       * @param name The name of the fit methode.
       * @return the maximum number of iterations.
       */
      unsigned int getAffinementMaxIteration(string const & name) const throw (HKLException);

      /**
       * @brief Set the maximum number of iteration for a fit methode.
       * @param name The name of the fit methode.
       * @param max the value to set.
       */
      void setAffinementMaxIteration(string const & name, unsigned int max) throw (HKLException);

      /**
       * @brief Get the number of iteration ran by a fit methode.
       * @param name The name of the fit methode.
       * @return the number of iterations.
       */
      unsigned int getAffinementIterations(string const & name) const throw (HKLException);

      /************/
      /* Calcules */
      /************/

      /**
       * @brief Compute the orientation matrix from two basic non-parallel reflections.
       *
       * Compute the orientation matrix from two basic non-parallel reflections.
       */
      void computeU(void) throw (HKLException);

      /**
       * @brief fit the crystal Parameters of a %Crystal
       * @param crystal_name The %Crystal name to fit.
       * @param method_name The %Affinement name methode to use.
       * @return The fitness of the fitted crystal.
       */
      double affineCrystal(string const & crystal_name, string const & method_name) throw (HKLException);

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
      void computeHKL(double & h, double & k, double & l) throw (HKLException);

      /**
       * @brief The main function to get a sample of angles from (h,k,l).
       * @param h The scaterring vector first element.
       * @param k The scaterring vector second element.
       * @param l The scaterring vector third element.
       *
       *  The main function to get a sample of angles from (h,k,l).
       */
      void computeAngles(double h, double k, double l) throw (HKLException);

    protected:
      Geometry * m_geometry; //!< The current diffractometer Geometry.
      Crystal * m_crystal; //!< The Crystal we are working with.
      CrystalList m_crystalList; //!< The CrystalList of the diffractometer.
      Mode * m_mode; //!< The Mode describes the way we use the diffractometer.
      ModeList m_modeList; //!< the available modes.
      PseudoAxeList m_pseudoAxeList; //!< The map containing the pseudo axes
      AffinementList m_affinementList; //!< The available Affinement methode.

      /**
       * @brief Default constructor
       *
       * - protected to make sure this class is abstract.
       * - by default a diffractometer contain alvays a default crystal.
       */
      Diffractometer(void);
    };

} // namespace hkl

/*!
 * @brief Surcharge de l'operateur << pour la class %Diffractometer
 * @param flux 
 * @param diffractometer 
 * @return 
 */
ostream & operator << (ostream & flux, hkl::Diffractometer const & diffractometer);

#endif // _DIFFRACTOMETER_H_
