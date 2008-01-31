/* This file is part of the hkl library.
 * 
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 * 
 * Copyright (C) 2003-2008 Synchrotron SOLEIL 
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include "affinement_simplex.h"
#include "fitparameterlist.h"

#include "constant.h"
#include "fitparameter.h"
namespace hkl
  {

  namespace affinement
    {

    Simplex::Simplex() :
        Affinement("Simplex", "Simplex method")
    {
    }

    Simplex::~Simplex()
    {
    }

    /**
     * @brief fit the data using the simplex method.
     * @param fitParameterList the hkl::FitParameterList to fit.
     *
     * This function modify the vertex.
     */

    void Simplex::fit(hkl::FitParameterList & fitParameterList) throw(hkl::HKLException)
    {
#ifdef DEBUG_HKL
      unsigned int idebug;
      unsinged int jdebug;
#endif
      if (fitParameterList.ready_to_fit())
        {
          bool ok;
          unsigned int i;
          unsigned int j;

          unsigned int nb_parameters = fitParameterList.size();
          unsigned int nb_vertex = fitParameterList.size_to_fit() + 1;

          // On initialise ensuite les vertex ainsi que la fitness
          valarray<valarray<double> > vertexList(nb_vertex);
          valarray<double> fitnessList(nb_vertex);

          // En ajoutant le crystal initial
          vertexList[0].resize(nb_parameters);
          _updateParameterListFromVertex(fitParameterList, vertexList[0]);
          fitParameterList.fitness(fitnessList[0]);

          // Puis le nombre de crystaux nécessaire à la résolution du problème.
          for (i=1;i<nb_vertex;i++)
            {
              fitParameterList.randomize();
              vertexList[i].resize(nb_parameters);
              _updateParameterListFromVertex(fitParameterList, vertexList[i]);
              fitParameterList.fitness(fitnessList[i]);
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
          unsigned int n_max = _nb_max_iterations;

          unsigned int i_lower = 0;
          unsigned int i_highest;
          unsigned int i_second_highest;

          double f0, f1;

          while (n < n_max)
            {
#ifdef DEBUG_HKL
              cout << n << endl;
              for (idebug=0;idebug<nb_vertex;idebug++)
                {
                  cout << idebug << " vertexList " << fitnessList[idebug] << " : ";
                  for (jdebug=0;jdebug<nb_parameters;jdebug++)
                    cout << vertexList[idebug][jdebug] << " ";
                  cout << endl;
                }
              cout << endl;
#endif
              // On recherche les indice des vertex qui nous intéressent
              // à savoir le meilleur, le pire et le deuxième pire.
              // Ici on initialize la recherche en comparant les deux premiers vertex.
              f0 = fitnessList[0];
              f1 = fitnessList[1];
              if (f0 <= f1)
                {
                  i_lower = 0;
                  fitness_lower = f0;
                  i_second_highest = 0;
                  fitness_second_highest = f0;
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
              for (i=2;i<nb_vertex;i++)
                {
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
                }
#ifdef DEBUG_HKL
              cout << " lower : " << i_lower << " 2nd highest : " << i_second_highest << " highest : " << i_highest << endl;
#endif
              // On vérifie la condition d'arrêt.
              if (fabs((fitness_highest - fitness_lower)/fitness_highest) < constant::math::tiny)
                break;

              // On calcule le vertex moyen
              meanParameterList = 0.;
              for (i=0;i<nb_vertex;i++)
                if (i != i_highest)
                  meanParameterList += vertexList[i];
              meanParameterList /= nb_vertex - 1.;

#ifdef DEBUG_HKL
              cout << " mean vertex :";
              for (idebug=0;idebug<nb_parameters;idebug++)
                cout << " " << meanParameterList[idebug];
              cout << endl;
              cout << " reflected :";
#endif
              // On calcule le reflected vertex à partir du vertex moyen et du pire des vertex
              double factor = 2.;
              do
                {
                  reflectedParameterList = meanParameterList;
                  reflectedParameterList *= factor;
                  reflectedParameterList -= vertexList[i_highest] * (factor - 1.);
                  factor /= 2.;
                  _updateVertexFromParameterList(fitParameterList, reflectedParameterList);
                }
              while (!fitParameterList.fitness(fitness_reflected));
#ifdef DEBUG_HKL
              for (idebug=0;idebug<nb_parameters;idebug++)
                cout << " " << reflectedParameterList[idebug];
              cout << " : " << fitness_reflected << " with a factor of " << factor << endl;
#endif
              if (fitness_reflected < fitness_lower)
                {
#ifdef DEBUG_HKL
                  cout << " reflected < lower -> expand : ";
#endif
                  //On continue dans la même direction que le reflected vertex et on crée the expanded vertex.
                  expandedParameterList = reflectedParameterList;
                  expandedParameterList *= 2.;
                  expandedParameterList -= meanParameterList;
                  _updateVertexFromParameterList(fitParameterList, expandedParameterList);
                  ok = fitParameterList.fitness(fitness_expanded);
#ifdef DEBUG_HKL
                  for (idebug=0;idebug<nb_parameters;idebug++)
                    cout << " " << expandedParameterList[idebug];
                  cout << " : " << fitness_expanded << endl;
#endif

                  if (ok && fitness_expanded < fitness_reflected)
                    {
                      // Le resultat est meilleur donc on garde l'expanded
                      vertexList[i_highest] = expandedParameterList;
                      fitnessList[i_highest] = fitness_expanded;
#ifdef DEBUG_HKL
                      cout << " expanded < reflected -> keep the expanded" << endl;
#endif
                    }
                  else
                    {
                      // Sinon on garde le reflected.
                      vertexList[i_highest] = reflectedParameterList;
                      fitnessList[i_highest] = fitness_reflected;
#ifdef DEBUG_HKL
                      cout << " expanded > reflected -> keep the reflected" << endl;
#endif
                    }

                }
              else if (fitness_reflected > fitness_second_highest)
                {
#ifdef DEBUG_HKL
                  cout << " reflected > 2nd highest -> contract : ";
#endif
                  // On contract le vertex dans la direction oposée au plus mauvais vertex.
                  contractedParameterList = meanParameterList;
                  contractedParameterList += vertexList[i_highest];
                  contractedParameterList /= 2.;
                  _updateVertexFromParameterList(fitParameterList, contractedParameterList);
                  ok = fitParameterList.fitness(fitness_contracted);
#ifdef DEBUG_HKL
                  for (idebug=0;idebug<nb_parameters;idebug++)
                    cout << " " << contractedParameterList[idebug];
                  cout << " : " << fitness_contracted << endl;
#endif

                  if (!ok || fitness_contracted > fitness_highest)
                    {
#ifdef DEBUG_HKL
                      cout << " contracted > highest -> contract everything" << endl << " new vertexes:" << endl;
#endif
                      // Si c'est pire qu'avant, on contract autour du meilleur Vertex.
                      for (j=0; j<nb_vertex; j++)
                        {
                          if (j != i_lower)
                            {
                              factor = .5;
                              do
                                {
                                  valarray<double> tmp(vertexList[j]);
                                  tmp *= factor;
                                  tmp += (1. - factor) * vertexList[i_lower];
                                  _updateVertexFromParameterList(fitParameterList, tmp);
                                  ok = fitParameterList.fitness(fitnessList[j]);
                                  if (ok)
                                    vertexList[j] = tmp;
                                  else
                                    factor /= 2.;
#ifdef DEBUG_HKL
                                  for (idebug=0;idebug<nb_parameters;idebug++)
                                    cout << " " << tmp[idebug];
                                  cout << endl;
#endif
                                }
                              while (!ok);
                            }
                        }
                    }
                  else
                    {
                      // sinon on garde le vertex contracté.
                      vertexList[i_highest] = contractedParameterList;
                      fitnessList[i_highest] = fitness_contracted;
#ifdef DEBUG_HKL
                      cout << " contracted < highest -> keep contracted" << endl;
#endif
                    }
                }
              else
                {
                  vertexList[i_highest] = reflectedParameterList;
                  fitnessList[i_highest] = fitness_reflected;
#ifdef DEBUG_HKL
                  cout << " lower < reflected < 2nd highest -> keep the reflected" << endl;
#endif
                }
              n++;
            }
          _nb_iterations = n;
          _updateVertexFromParameterList(fitParameterList, vertexList[i_lower]);
          // On utilise un appèle à la fonction plutôt que fitnessList[i_lower]
          // pour mettre à jour la matrice B dans le cristal.
          //set_fitness(fitParameterList.fitness());
        }
    }

    void Simplex::_updateParameterListFromVertex(const hkl::FitParameterList & fitParameterList, valarray<double> & parameterList)
    {
      unsigned int i = 0;
      FitParameterList::const_iterator iter = fitParameterList.begin();
      FitParameterList::const_iterator end = fitParameterList.end();

      while (iter != end)
        {
          parameterList[i] = (*iter)->get_current().get_value();
          ++iter;
          ++i;
        }
    }

    void Simplex::_updateVertexFromParameterList(hkl::FitParameterList & fitParameterList, const valarray<double> & parameterList)
    {
      unsigned int i = 0;
      FitParameterList::iterator iter = fitParameterList.begin();
      FitParameterList::iterator end = fitParameterList.end();

      while (iter != end)
        {
          (*iter)->set_current(parameterList[i]);
          ++iter;
          ++i;
        }
      fitParameterList.update();
    }


  } // namespace hkl::affinement

} // namespace hkl
