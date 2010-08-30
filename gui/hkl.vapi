namespace Hkl
{
	[SimpleType]
	[CCode (cheader_filename="hkl.h", copy_function="hkl_parameter_new_copy")]
	public struct Matrix
	{
	}

	[SimpleType]
	[CCode (cheader_filename="hkl.h", copy_function="hkl_parameter_new_copy")]
	public struct Parameter
	{
		public string name;
		public double get_value_unit();
		public void set_value_unit(double value);
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
	}

	[Compact]
	[CCode (cheader_filename="hkl.h")]
	public class Lattice
	{
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
	}
}