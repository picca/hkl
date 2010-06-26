/*
 * compilation:
 * valac --pkg gtk+-2.0 --pkg gmodule-2.0 sample.vala
 */
using Gtk;

public static void on_button1_clicked (Button source) {
    source.label = "Thank you!";
}

public static void on_button2_clicked (Button source) {
    source.label = "Thanks!";
}

static int main (string[] args) {     
    Gtk.init (ref args);

    try {
        var builder = new Builder ();
        builder.add_from_file ("ghkl.ui");
        builder.connect_signals (null);
        var window = builder.get_object ("window") as Window;
        window.show_all ();
        window.destroy.connect (Gtk.main_quit);
        Gtk.main ();
    } catch (Error e) {
        stderr.printf ("Could not load UI: %s\n", e.message);
        return 1;
    } 

    return 0;
}
