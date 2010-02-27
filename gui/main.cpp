#include <gtkmm/main.h>

#include "hklwindow.h"

int main(int argc, char *argv[])
{
	Gtk::Main kit(argc, argv);
	HKLWindow window(HKL_GEOMETRY_TYPE_KAPPA6C);
	kit.run(window);

	return 0;
}
