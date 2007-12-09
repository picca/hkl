#ifndef _REFLECTION_H_
#define _REFLECTION_H_

struct hkl_reflection {
	struct hkl_geometry geometry;
	struct hkl_svector hkl;
	int flag;
	struct hkl_svector _hkl_angles;
}

extern void hkl_reflection_init(struct hkl_reflection * reflection, struct hkl_vector const * hkl, int flag);

extern int hkl_reflection_angle(struct hkl_reflection const *reflection1, struct hkl_reflection const *reflection2);

#endif /* _REFLECTION_H_ */
