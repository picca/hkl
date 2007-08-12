#ifndef _CONSTANT_H
#define _CONSTANT_H


namespace hkl {

class constant {
  public:
    class math {
      public:
        static double epsilon;

        static double tiny;

        static int precision;

        static double pi;

        static double degToRad;

        static double radToDeg;

    };
    
    class physic {
      public:
        static double tau;

    };
    
};

} // namespace hkl
#endif
