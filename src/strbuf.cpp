#include "strbuf.h"

namespace hkl
  {

  std::ostream & strbuf_to_stream(std::string const & s, std::ostream & flux)
  {
    flux << " " << s.size() << " " << s.c_str();
    return flux;
  }

  std::istream & strbuf_from_stream(std::string & s, std::istream & flux)
  {
    unsigned int size;

    flux >> size;
    //remove the first space
    flux.get();
    char * chaine = (char *)malloc(size+1);
    flux.read(chaine, size);
    s.assign(chaine, size);
    free(chaine);
    return flux;
  }


} // namespace hkl
