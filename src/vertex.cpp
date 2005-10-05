#include "vertex.h"

Vertex::Vertex()
{}

Vertex::Vertex(Vertex const & vertex)
{
  set_fitParameterList(vertex.get_fitParameterList());
}

Vertex::~Vertex()
{}

bool
Vertex::operator ==(Vertex const & vertex) const
{
  return get_fitParameterList() == vertex.get_fitParameterList();
}

Vertex &
Vertex::operator +=(Vertex const & vertex)
{
  FitParameterList & fitParameterList = get_fitParameterList();
  FitParameterList::iterator iter = fitParameterList.begin();
  FitParameterList::iterator end = fitParameterList.end();
  
  FitParameterList::const_iterator iter2 = vertex.get_fitParameterList().begin();
  
  while(iter != end){
    iter->second += iter2->second;
    ++iter;
    ++iter2;
  }
  return *this;
}

Vertex &
Vertex::operator -=(Vertex const & vertex)
{
  FitParameterList & fitParameterList = get_fitParameterList();
  FitParameterList::iterator iter = fitParameterList.begin();
  FitParameterList::iterator end = fitParameterList.end();
  
  FitParameterList::const_iterator iter2 = vertex.get_fitParameterList().begin();
  
  while(iter != end){
    iter->second -= iter2->second;
    ++iter;
    ++iter2;
  }
  return *this;
}

Vertex &
Vertex::operator *=(double const & d)
{
  FitParameterList & fitParameterList = get_fitParameterList();
  FitParameterList::iterator iter = fitParameterList.begin();
  FitParameterList::iterator end = fitParameterList.end();
  
  while(iter != end){
    iter->second *= d;
    ++iter;
  }
  return *this;
}

Vertex &
Vertex::operator /=(double const & d)
{
  FitParameterList & fitParameterList = get_fitParameterList();
  FitParameterList::iterator iter = fitParameterList.begin();
  FitParameterList::iterator end = fitParameterList.end();
  
  while(iter != end){
    iter->second /= d;
    ++iter;
  }
  return *this;
}

std::ostream & 
Vertex::printToStream(std::ostream & flux) const
{
  FitParameterList::const_iterator iter = m_fitParameterList.begin();
  FitParameterList::const_iterator end = m_fitParameterList.end();
  while(iter != end){
    flux << iter->second;
    ++iter;
  }

  return flux;
}

void
Vertex::addFitParameter(FitParameter const & fitParameter)
{
  m_fitParameterList.insert(FitParameterList::value_type(fitParameter.get_name(),fitParameter));
}

unsigned int
Vertex::getNumberOfParameterToFit() const
{ 
  FitParameterList::const_iterator iter = m_fitParameterList.begin();
  FitParameterList::const_iterator end = m_fitParameterList.end();
  
  unsigned int n = 0;
  while(iter != end){
    if (iter->second.get_flagFit())
      n++;
    ++iter;
  }
  return n;
}

void
Vertex::randomize(void)
{
  FitParameterList::iterator iter = m_fitParameterList.begin();
  FitParameterList::iterator end = m_fitParameterList.end();
  
  while(iter != end){
    iter->second.randomize();
    ++iter;
  }
}

std::ostream &
operator <<(std::ostream & flux, Vertex const & vertex)
{
  return vertex.printToStream(flux);
}
