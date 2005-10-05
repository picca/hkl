#include "affinement.h"

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
        unsigned int i, j;

        unsigned int nb_parameters = fitParameterList.getNumberOfParameterToFit();
        unsigned int nb_vertex = nb_parameters + 1;

        // On initialise ensuite les vertex ainsi que la fitness
        // En ajoutant le crystal initial
        std::valarray<double> parameterList(nb_parameters);
        FitParameterList::iterator iter = fitParameterList.begin();
        for(i=0;i<nb_parameters;i++){
          parameterList[i] = iter->get_value();
          ++iter;
        }

        std::valarray<std::valarray<double> > vertexList(nb_vertex);
        vertexList[0].resize(nb_parameters);
        vertexList[0] = parameterList;
        std::valarray<double> fitnessList(nb_vertex);
        fitnessList[0] = fitParameterList.fitness();

        // Puis le nombre de crystaux nécessaire à la résolution du problème.
        for(i=1;i<nb_vertex;i++){
          fitParameterList.randomize();
          iter = fitParameterList.begin();
          for(j=0;j<nb_parameters;j++){
            parameterList[j] = iter->get_value();
            ++iter;
          }
          vertexList[i].resize(nb_parameters);
          vertexList[i] = parameterList;
          fitnessList[i] = fitParameterList.fitness();
        }

        double fitness_lower;
        double fitness_highest;
        double fitness_second_highest;
        double fitness_reflected;
        double fitness_expanded;
        double fitness_contracted;
        std::valarray<double> meanParameterList(nb_parameters);
        std::valarray<double> reflectedParameterList(nb_parameters);
        std::valarray<double> expandedParameterList(nb_parameters);
        std::valarray<double> contractedParameterList(nb_parameters);

        unsigned int n = 0;
        unsigned int n_max = get_nb_max_iteration();

        unsigned int i_lower = 0;
        unsigned int i_highest;
        unsigned int i_second_highest;

        double f0, f1;

        while (n < n_max){
          // On recherche les indice des vertex qui nous intéressent
          // Ici on initialize la recherche en comparant les deux premiers vertex.
          f0 = fitnessList[0];
          f1 = fitnessList[1];
          if (f0 <= f1){
            i_lower = 0;
            fitness_lower = f0;
            i_second_highest = 0;
            fitness_second_highest = f1;
            i_highest = 1;
            fitness_highest = f1;
          } else {
            i_lower = 1;
            fitness_lower = f1;
            i_second_highest = 1;
            fitness_second_highest = f1;
            i_highest = 0;
            fitness_highest = f0;
          }
          //puis on compare les autres vertex;
          for(i=2;i<nb_vertex;i++){
#ifdef VCPP6
            // VC++6.0 n'est pas compliant IEEE 754.
            // if (NaN < number) retourne True au lieu de False.
            // Il faut donc vérifier explicitement si fitnessList[i] est NaN
            if (!_isnan(fitnessList[i])){
#endif
            if (fitnessList[i] <= fitness_lower){
              i_lower = i;
              fitness_lower = fitnessList[i];
            }
            if (fitnessList[i] > fitness_highest){
              i_second_highest = i_highest;
              fitness_second_highest = fitness_highest;
              i_highest = i;
              fitness_highest = fitnessList[i];
            } else if (fitnessList[i] > fitness_second_highest && i != i_highest){
              i_second_highest = i;
              fitness_second_highest = fitnessList[i];
            }
#ifdef VCPP6
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
          reflectedParameterList = meanParameterList;
          reflectedParameterList *= 2.;
          reflectedParameterList -= vertexList[i_highest];
          _updateVertexFromParameterList(fitParameterList, reflectedParameterList);
          fitness_reflected = fitParameterList.fitness();

#ifdef VCPP6
          if (fitness_reflected < fitness_lower && !_isnan(fitness_reflected)){
#else
          if (fitness_reflected < fitness_lower){
#endif
            //On continue dans la même direction que le reflected vertex et on crée the expanded vertex.
            expandedParameterList = reflectedParameterList;
            expandedParameterList *= 2.;
            expandedParameterList -= meanParameterList;
            _updateVertexFromParameterList(fitParameterList, expandedParameterList);
            fitness_expanded = fitParameterList.fitness();

#ifdef VCPP6
            if (fitness_expanded < fitness_reflected && !_isnan(fitness_expanded)){
#else
            if (fitness_expanded < fitness_reflected){
#endif
              // Le resultat est meilleur donc on garde l'expanded
              vertexList[i_highest] = expandedParameterList;
              fitnessList[i_highest] = fitness_expanded;
            } else {
              // Sinon on garde le reflected.
              vertexList[i_highest] = reflectedParameterList;
              fitnessList[i_highest] = fitness_reflected;
            }

          } else if (fitness_reflected > fitness_second_highest) {
            // On contract le vertex dans une direction 
            contractedParameterList = meanParameterList;
            contractedParameterList += vertexList[i_highest];
            contractedParameterList /= 2.;
            _updateVertexFromParameterList(fitParameterList, contractedParameterList);
            fitness_contracted = fitParameterList.fitness();
            if (fitness_contracted > fitness_highest){
              // Si c'est pire qu'avant, on contract autour du meilleur Vertex.
              for(j=0; j<nb_vertex; j++){
                if (j != i_lower){
                  vertexList[j] += vertexList[i_lower];
                  vertexList[j] /= 2.;
                  _updateVertexFromParameterList(fitParameterList, vertexList[j]);
                  fitnessList[j] = fitParameterList.fitness();
                }
              }
            } else {
              // sinon on garde le vertex contracté.
              vertexList[i_highest] = contractedParameterList;
              fitnessList[i_highest] = fitness_contracted;
            }
          } else {
            vertexList[i_highest] = reflectedParameterList;
            fitnessList[i_highest] = fitness_reflected;
          }
          n++;
        }
        set_nb_iteration(n);
        set_fitness(fitnessList[i_lower]);
        _updateVertexFromParameterList(fitParameterList, vertexList[i_lower]);

      }

    void
      Simplex::_updateVertexFromParameterList(FitParameterList & fitParameterList, std::valarray<double> const & parameterList)
      {
        unsigned int i;
        unsigned int nb_parameters = fitParameterList.size();
        FitParameterList::iterator iter = fitParameterList.begin();

        for(i=0;i<nb_parameters;i++){
          iter->set_value(parameterList[i]);
          ++iter;
        }
      }
  } // namespace affinement
} // namespace hkl
