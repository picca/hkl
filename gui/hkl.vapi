namespace Hkl
{

	public const double DEGTORAD;

	[CCode (cheader_filename="hkl.h", destroy_function="")]
	public struct Matrix
	{
		[CCode (array_length=false)]
		public double[][] data;
	}

	[CCode (cheader_filename="hkl.h", destroy_function="")]
	public struct Vector
	{
		public double[] data;
	}

	[CCode (cheader_filename="hkl.h", destroy_function="")]
	public struct Parameter
	{
		public string name;
		public bool fit;

		public double get_value_unit();
		public void set_value_unit(double value);
		public void get_range_unit(out double min, out double max);
		public void set_range_unit(double min, double max);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Error
	{
		public string message;
	}

	[CCode (cheader_filename="hkl.h")]
	public enum DetectorType
	{
		0D
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Detector
	{
		public size_t idx;

		[CCode (cname="hkl_detector_factory_new")]
		public Detector.Factory(DetectorType type);
	}

	[CCode (cheader_filename="hkl.h")]
	public enum SampleType
	{
		MONOCRYSTAL
	}

	[Compact]
	[CCode (cheader_filename="hkl.h", ref_function="", unref_function="")]
	public class Sample
	{
		public string name;
		public Lattice lattice;
		public Parameter? ux;
		public Parameter? uy;
		public Parameter? uz;
		[CCode (array_length_cname="reflections_len")]
		public SampleReflection[] reflections;

		public Sample(string name, SampleType type);
		public Sample.copy([Immutable] Sample sample);
		public void set_name(string name);
		public void set_lattice(double a, double b, double c,
								double alpha, double beta, double gamma);
		public void set_U_from_euler(double ux, double uy, double uz);
		public void get_UB(out Matrix UB);
		public double set_UB([Immutable] ref Matrix UB);
		public double affine();

		public SampleReflection add_reflection(Geometry geometry,
											   [Immutable] Detector detector,
											   double h, double k, double l);
		public int del_reflection(size_t idx);
		public int compute_UB_busing_levy(size_t idx1, size_t idx2);

		[CCode (instance_pos=-1)]
		public void fprintf(GLib.FileStream f);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h", ref_function="", unref_function="")]
	public class SampleReflection
	{
		public Geometry geometry;
		public Detector detector;
		public Vector hkl;
		public Vector _hkl;
		public int flag;

		public void set_hkl(double h, double k, double l);
		public void set_flag(int flag);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class SampleList
	{
		public unowned Sample? current;
		[CCode (array_length_cname="len")]
		public Sample[] samples;

		public SampleList();
		public unowned Sample append(owned Sample sample);
		public int select_current(string name);
		public void del(Sample sample);
		public unowned Sample? get_by_name(string name);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Lattice
	{
		public Parameter *a;
		public Parameter *b;
		public Parameter *c;
		public Parameter *alpha;
		public Parameter *beta;
		public Parameter *gamma;

		public Lattice.default();
		public void reciprocal(Lattice reciprocal);
	}

	[CCode (cheader_filename="hkl.h")]
	public struct Source
	{
		public double wave_length;

		public void compute_ki(out Vector ki);
		public double get_wavelength();
	}

	[CCode (cheader_filename="hkl.h")]
	public enum GeometryType
	{
		E4CV,
			K4CV,
			E6C,
			K6C,
			ZAXIS,
			E4CH
	}

	[SimpleType]
	[CCode (cname="hkl_geometry_factory_configs", array_null_terminated=true, array_length=false)]
	public const GeometryConfig[] geometry_factory_configs;


	[CCode (cheader_filename="hkl.h")]
	public struct GeometryConfig
	{
		public unowned string name;
	}

	static Geometry geometry_factory_new([Immutable] GeometryConfig config, double alpha);
	static PseudoAxisEngineList pseudo_axis_engine_list_factory([Immutable] GeometryConfig config);

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Geometry
	{
		public Source source;
		[CCode (array_length_cname="len")]
		public Axis[] axes;
		public void init_geometry(Geometry goemetry);
		public unowned Axis? get_axis_by_name(string name);
		public void update();
	}

	[Simple]
	[CCode (cheader_filename="hkl.h")]
	public struct GeometryListItem
	{
		public Geometry geometry;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class GeometryList
	{
		[CCode (array_length_cname="len")]
		public GeometryListItem[] items;
	}

	[CCode (cheader_filename="hkl.h", destroy_function="")]
	public struct Axis
	{
		/* members */
		public Parameter parent_instance;

		/* methods */
		public double get_value_unit();
		public void set_value_unit(double value);
		public void get_range_unit(out double min, out double max);
		public void set_range_unit(double min, double max);		
	}


	[Compact]
	[CCode (cheader_filename="hkl.h", ref_function="", unref_function="")]
	public class PseudoAxis
	{
		public Parameter parent;
		public unowned PseudoAxisEngine engine;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class PseudoAxisMode
	{
		public string name;

		[SimpleType]
		[CCode (array_length_cname="parameters_len")]
		public Parameter[] parameters;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h", ref_function="", unref_function="")]
	public class PseudoAxisEngine
	{
		/*  members */
		public string name;
		public PseudoAxisEngineList engines;
		[CCode (array_length_cname="pseudoAxes_len")]
		public PseudoAxis pseudoAxes[];
		[CCode (array_length_cname="modes_len")]
		public PseudoAxisMode[] modes;
		public PseudoAxisMode? mode;

		/* methods */
		public void select_mode(size_t idx);
		public bool initialize(void *error);
		public bool set(void *error);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class PseudoAxisEngineList
	{
		/* members */
		public Geometry geometry;
		public GeometryList geometries;
  		[CCode (array_length_cname="len")]
		public PseudoAxisEngine[] engines;

		/* method */
		[CCode (cname="hkl_pseudo_axis_engine_list_factory")]
		public PseudoAxisEngineList.Factory(GeometryConfig config);
		public void get();
		public void init(Geometry geometry, Detector detector, Sample sample);
	}
}