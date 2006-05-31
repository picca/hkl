#include "config.h"

#include "affinement.h"

#ifdef MSVC6
# include "float.h"
# define isnan _isnan
#endif

namespace hkl{
    namespace affinement {

        Simplex::Simplex() :
          Affinement("simplex")
        {}

        Simplex::~Simplex()
          {}

        void
        Simplex::fit(FitParameterList & fitParameterList)
          {
            unsigned int i;
            unsigned int j;

            unsigned int nb_parameters = fitParameterList.size();
            unsigned int nb_vertex = fitParameterList.getNumberOfParameterToFit() + 1;

            // On initialise ensuite les vertex ainsi que la fitness
            valarray<valarray<double> > vertexList(nb_vertex);
            valarray<double> fitnessList(nb_vertex);

            // En ajoutant le crystal initial
            vertexList[0].resize(nb_parameters);
            _updateParameterListFromVertex(fitParameterList, vertexList[0]);
            fitnessList[0] = fitParameterList.fitness();

            // Puis le nombre de crystaux nécessaire à la résolution du problème.
            for(i=1;i<nb_vertex;i++)
              {
                fitParameterList.randomize();
                vertexList[i].resize(nb_parameters);
                _updateParameterListFromVertex(fitParameterList, vertexList[i]);
                fitnessList[i] = fitParameterList.fitness();
              }

            double fitness_lower;
            double fitness_highest;
            double fitness_second_highest;
            double fitness_reflected;
            double fitness_expanded;
            double fitness_contracted;
            valarray<double> meanParameterList(nb_parameters);
            valarray<double> reflectedParameterList(nb_parameters);
            valarray<double> expandedParameterList(nb_parameters);
            valarray<double> contractedParameterList(nb_parameters);

            unsigned int n = 0;
            unsigned int n_max = get_nb_max_iteration();

            unsigned int i_lower = 0;
            unsigned int i_highest;
            unsigned int i_second_highest;

            double f0, f1;

            while (n < n_max)
              {
#ifdef DEBUG_HKL
                cout << n << endl;
                for(i=0;i<nb_vertex;i++)
                  {
                    cout << i << " vertexList " << fitnessList[i] << " : ";
                    for(j=0;j<nb_parameters;j++)
                        cout << vertexList[i][j] << " ";
                    cout << endl;
                  }
                cout << endl;
#endif
                // On recherche les indice des vertex qui nous intéressent
                // Ici on initialize la recherche en comparant les deux premiers vertex.
                f0 = fitnessList[0];
                f1 = fitnessList[1];
                if (f0 <= f1)
                  {
                    i_lower = 0;
                    fitness_lower = f0;
                    i_second_highest = 0;
                    fitness_second_highest = f1;
                    i_highest = 1;
                    fitness_highest = f1;
                  }
                else
                  {
                    i_lower = 1;
                    fitness_lower = f1;
                    i_second_highest = 1;
                    fitness_second_highest = f1;
                    i_highest = 0;
                    fitness_highest = f0;
                  }
                //puis on compare les autres vertex;
                for(i=2;i<nb_vertex;i++)
                  {
#ifdef MSVC6
                    // VC++6.0 n'est pas compliant IEEE 754.
                    // if (NaN < number) retourne True au lieu de False.
                    // Il faut donc vérifier explicitement si fitnessList[i] est NaN
                    if (!_isnan(fitnessList[i]))
                      {
#endif
                        if (fitnessList[i] <= fitness_lower)
                          {
                            i_lower = i;
                            fitness_lower = fitnessList[i];
                          }
                        if (fitnessList[i] > fitness_highest)
                          {
                            i_second_highest = i_highest;
                            fitness_second_highest = fitness_highest;
                            i_highest = i;
                            fitness_highest = fitnessList[i];
                          }
                        else if (fitnessList[i] > fitness_second_highest && i != i_highest)
                          {
                            i_second_highest = i;
                            fitness_second_highest = fitnessList[i];
                          }
#ifdef MSVC6
                      }
#endif
                  }
                // On vérifie la condition d'arrêt.
                if (fabs((fitness_highest - fitness_lower)/fitness_highest) < constant::math::tiny)
                    break;

                // On calcule le vertex moyen
                meanParameterList = 0.;
                for(i=0;i<nb_vertex;i++)
                    if (i != i_highest)
                        meanParameterList += vertexList[i];
                meanParameterList /= nb_vertex - 1.;

                // On calcule le reflected vertex à partir du vertex moyen et du pire des vertex
                double factor = 2.;
                do
                  {
                    reflectedParameterList = meanParameterList;
                    reflectedParameterList *= factor;
                    factor /= 2;
                    reflectedParameterList -= vertexList[i_highest];
                    _updateVertexFromParameterList(fitParameterList, reflectedParameterList);
                    fitness_reflected = fitParameterList.fitness();
                  }
                while(isnan(fitness_reflected));
#ifdef DEBUG_HKL
                if (isnan(fitness_reflected))
                    cout << "fitness_reflected is nan" << endl;
#endif
#ifdef MSVC6
                if (fitness_reflected < fitness_lower && !_isnan(fitness_reflected))
#else
                    if (fitness_reflected < fitness_lower)
#endif
                      {
                        //On continue dans la même direction que le reflected vertex et on crée the expanded vertex.
                        expandedParameterList = reflectedParameterList;
                        expandedParameterList *= 2.;
                        expandedParameterList -= meanParameterList;
                        _updateVertexFromParameterList(fitParameterList, expandedParameterList);
                        fitness_expanded = fitParameterList.fitness();

#ifdef MSVC6
                        if (fitness_expanded < fitness_reflected && !_isnan(fitness_expanded))
                          {
#else
                            if (fitness_expanded < fitness_reflected)
                              {
#endif
                                // Le resultat est meilleur donc on garde l'expanded
                                vertexList[i_highest] = expandedParameterList;
                                fitnessList[i_highest] = fitness_expanded;
                              }
                            else
                              {
                                // Sinon on garde le reflected.
                                vertexList[i_highest] = reflectedParameterList;
                                fitnessList[i_highest] = fitness_reflected;
                              }

                          }
                        else if (fitness_reflected > fitness_second_highest)
                          {
                            // On contract le vertex dans la direction oposée au plus mauvais vertex. 
                            contractedParameterList = meanParameterList;
                            contractedParameterList += vertexList[i_highest];
                            contractedParameterList /= 2.;
                            _updateVertexFromParameterList(fitParameterList, contractedParameterList);
                            fitness_contracted = fitParameterList.fitness();
                            if (fitness_contracted > fitness_highest)
                              {
                                // Si c'est pire qu'avant, on contract autour du meilleur Vertex.
                                for(j=0; j<nb_vertex; j++)
                                  {
                                    if (j != i_lower)
                                      {
                                        vertexList[j] += vertexList[i_lower];
                                        vertexList[j] /= 2.;
                                        _updateVertexFromParameterList(fitParameterList, vertexList[j]);
                                        fitnessList[j] = fitParameterList.fitness();
                                      }
                                  }
                              }
                            else
                              {
                                // sinon on garde le vertex contracté.
                                vertexList[i_highest] = contractedParameterList;
                                fitnessList[i_highest] = fitness_contracted;
                              }
                          }
                        else
                          {
                            vertexList[i_highest] = reflectedParameterList;
                            fitnessList[i_highest] = fitness_reflected;
                          }
                        n++;
#ifndef MSVC6
                      }
#else
              }
#endif
            set_nb_iteration(n);
            _updateVertexFromParameterList(fitParameterList, vertexList[i_lower]);
            // On utilise un appèle à la fonction plutôt que fitnessList[i_lower]
            // pour mettre à jour la matrice B dans le cristal.
            set_fitness(fitParameterList.fitness());
          }

        void
        Simplex::_updateParameterListFromVertex(FitParameterList const & fitParameterList,
                                                valarray<double> & parameterList)
          {
            unsigned int i = 0;
            FitParameterList::const_iterator iter = fitParameterList.begin();
            FitParameterList::const_iterator end = fitParameterList.end();

            while(iter != end)
              {
                parameterList[i] = iter->get_value();
                ++iter;
                ++i;
              }
          }

        void
        Simplex::_updateVertexFromParameterList(FitParameterList & fitParameterList,
                                                valarray<double> const & parameterList)
          {
            unsigned int i = 0;
            FitParameterList::iterator iter = fitParameterList.begin();
            FitParameterList::iterator end = fitParameterList.end();

            while(iter != end)
              {
                iter->set_value(parameterList[i]);
                ++iter;
                ++i;
              }
          }

    } // namespace affinement
} // namespace hkl
