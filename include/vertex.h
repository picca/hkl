#ifndef _VERTEX_H
#define _VERTEX_H

#include <math.h>
#include <string>
#include <map>
#include <iostream>

#include "fitparameter.h"
#include "HKLException.h"

/**
 * @brief A class design to describe a Vertex for the simplex methode
 */
class Vertex
{
  public:
    /**
     * @brief The default constructor
     */
    Vertex(void);

    /**
     * @brief The Copy constructor
     * @param vertex a %Vertex to copy from
     */
    Vertex(Vertex const & vertex);
    
    /**
     * @brief The default destructor
     */
    virtual ~Vertex(void);

  
    /**
     * @brief Get the liste of parameters
     * @return the list of parameters
     */
    FitParameterList const & get_fitParameterList(void) const {return m_fitParameterList;}

    /**
     * @brief getset the fitParameterList parameter.
     */
    FitParameterList & get_fitParameterList(void) {return m_fitParameterList;}
    
    /**
     * @brief Set the liste of parameters
     * @param fitParameterList the FitParameterList to set.
     */
    void set_fitParameterList(FitParameterList const & fitParameterList) {m_fitParameterList = fitParameterList;}
    
    /**
     * @brief Are two %Vertex equals ?
     * @param vertex the %Vertex to compare with
     * @return The comparison with the %Vertex vetex.
     */
    bool operator ==(Vertex const & vertex) const;
     
    /**
     * @brief Add a %Vertex to an other one
     * @param vertex The %Vertex to add.
     * @return The modified %Vertex.
     */
    Vertex & operator +=(Vertex const & vertex);
   
    /**
     * @brief Substract a Crystal to an other one
     * @param crystal The %Vertex to substract.
     * @return The modified %Vertex.
     */
    Vertex & operator -=(Vertex const & vertex);
   
    /**
     * @brief Multiply a Crystal by a number.
     * @param d The number
     * @return The modified %Vertex.
     */
    Vertex & operator *=(double const & d);
    
    /**
     * @brief Divide a Crystal by a number.
     * @param d The number
     * @return The modified %Vertex.
     */
    Vertex & operator /=(double const & d);
      
    /**
     * @brief Add a %Vertex to an other one
     * @param crystal The %Vertex to add.
     * @return The modified %Vertex.
     */
    Vertex operator +(Vertex const & vertex) const;
   
    /**
     * @brief Substract a %Vertex to an other one
     * @param crystal The %Vertex to substract.
     * @return The modified %Vertex.
     */
    Vertex operator -(Vertex const & vertex) const;
   
    /**
     * @brief Multiply a %Vertex by a number.
     * @param d The number
     * @return The modified %Vertex.
     */
    Vertex operator *(double const & d) const;
  
    /**
     * @brief print the %Vertex into a flux
     * @param flux The stream to print into.
     * @return The modified flux.
     */
    std::ostream & printToStream(std::ostream & flux) const;

    /**
     * @brief Add a FitParameter to the vertex.
     * @param fitParameter the #FitParameter to add.
     */
    void addFitParameter(FitParameter const & fitParameter);
    
    /**
     * @brief get the number of parameter to fit of the vertex
     * @return the number of parameter
     */
    unsigned int Vertex::getNumberOfParameterToFit(void) const;

    /**
     * @brief set all fitParameter of the %Vertex randomly.
     */
    void randomize(void);
  
   /**
    * @brief Calculation of the crystal fitness
    * @return the fitness
    */
    virtual double fitness(void) throw (HKLException) = 0;

  private:
    FitParameterList m_fitParameterList; //!< the list of parameters in the vertex
};

/**
 * @brief Overload of the << operator for the %Vertex class
 * @param flux
 * @param vertex
 * @return the modified flux.
 */
std::ostream & operator<<(std::ostream & flux, Vertex const & vertex); 

#endif // _VERTEX_H
