#ifndef _STRBUF_H
#define _STRBUF_H

#include <string>
#include <ostream>
#include <istream>

namespace hkl
  {

  /* save a string to a std::ostream */
  std::ostream & strbuf_to_stream(const std::string &, std::ostream & flux);

  /* restore a string from a std::istream */
  std::istream & strbuf_from_stream(std::string &, std::istream & flux);

} // namespace hkl

#endif
