namespace Hkl
{

	public const double DEGTORAD;

	[SimpleType]
	[CCode (cheader_filename="hkl.h", copy_function="hkl_parameter_new_copy")]
	public struct Matrix
	{
		public double[,] data;
	}

	[SimpleType]
	[CCode (cheader_filename="hkl.h", copy_function="hkl_parameter_new_copy")]
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
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Detector
	{
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Sample
	{
		public string name;
		public Lattice lattice;
		public Parameter? ux;
		public Parameter? uy;
		public Parameter? uz;

		public void set_lattice(double a, double b, double c,
								double alpha, double beta, double gamma);
		public void set_U_from_euler(double ux, double uy, double uz);
		public void get_UB(out Matrix UB);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class SampleReflection
	{
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class SampleList
	{
		public Sample *current;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Lattice
	{
		public Parameter a;
		public Parameter b;
		public Parameter c;
		public Parameter alpha;
		public Parameter beta;
		public Parameter gamma;

		public void reciprocal(out Lattice reciprocal);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Source
	{
		public double wave_length;
	}

	[CCode (cname="hkl_geometry_factory_configs")]
	static GeometryConfig hkl_geometry_factory_configs[];

	[SimpleType]
	[CCode (cheader_filename="hkl.h")]
	public class GeometryConfig
	{
		public unowned string name;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Geometry
	{
		public Source source;
		public void init_geometry(Geometry goemetry);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class GeometryListItem
	{
		public Geometry geometry;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class GeometryList
	{
		[CCode (array_length_cname="items_len")]
		public GeometryListItem[] items;
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Axis
	{
		public Parameter parent;
	}


	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class PseudoAxis
	{
		public Parameter parent;
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
	[CCode (cheader_filename="hkl.h")]
	public class PseudoAxisEngine
	{
		public string name;
		public PseudoAxisEngineList engines;

		[CCode (array_length_cname="pseudoAxes_len")]
		public PseudoAxis* pseudoAxes[];

		[CCode (array_length_cname="modes_len")]
		public PseudoAxisMode[] modes;

		public PseudoAxisMode? mode;

		public void select_mode(size_t idx);
		public bool initialize(void *error);
		public bool set(void *error);
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class PseudoAxisEngineList
	{
		public Geometry geometry;
		public GeometryList geometries;
		public void get();
	}
}