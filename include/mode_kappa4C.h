#ifndef _MODE_KAPPA4C_H_
#define _MODE_KAPPA4C_H_

#include "mode.h"

namespace hkl {
  namespace mode {

    /**
     * This class defines the mode for all the 4 circles Kappa diffractometers.
     */

    class Kappa4C : public Mode
    {
      public:

        virtual ~Kappa4C(void); //<! The destructor.

        /**
         * @brief The main function to get a sample of angles from (h,k,l).
         * @param h The scaterring vector first element.
         * @param k The scaterring vector second element.
         * @param l The scaterring vector third element.
         * @param UB The product of the orientation matrix U by the crystal matrix B.
         * @param lambda The wave length.
         * @param[out] aC The %AngleConfiguration to calculate.
         *
         * The main function to get a sample of angles from (h,k,l).
         */
        virtual void computeAngles(double h, double k, double l,
            const smatrix & UB, double lambda,
            AngleConfiguration * aC) const = 0;

      protected:

        double m_alpha; //!< The incident angle, its typical value is around 50°.

        /**
         * @brief Default constructor.
         * @return a new kappa_mode
         *
         * Default constructor - protected to make sure this class is abstract.
         */
        Kappa4C(void);
    };

    namespace kappa4C {

      /*
         class kappa_bissectorMode4C : public kappa_mode
         {
         public:
         kappa_bissectorMode4C();

         angleConfiguration computeAngles(
         int h, int k, int l) const;

         angleConfiguration_Kappa4C convertFromEulerian(
         angleConfiguration_Eulerian4C eul) const;

         angleConfiguration_Eulerian4C convertFromKappa(
         angleConfiguration_Kappa4C kap) const;
         };
         */

    } // namesapce kappa4C
  } // namespace mode
} // namespace hkl

#endif // _MODE_KAPPA4C_H_
