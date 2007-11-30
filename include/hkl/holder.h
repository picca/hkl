#ifndef _NEW_HOLDER_H
#define _NEW_HOLDER_H

/* Allow the use in C++ code.  */
#ifdef __cplusplus
extern "C"
{
#endif

	/* forward declaration begin */
	struct hkl_axes;
	struct hkl_axis;
	struct hkl_svector;
	/* forward declaration end */

	struct hkl_holder {
		struct hkl_axes * axes;
		unsigned int len;
		unsigned int alloc;
		struct hkl_axis **private_axes;
	};

	extern void hkl_holder_init(struct hkl_holder *holder, struct hkl_axes *axes);

	extern void hkl_holder_release(struct hkl_holder *holder);

	extern struct hkl_axis* hkl_holder_add_rotation_axis(struct hkl_holder *holder, char const *name, double x, double y, double z);

#ifdef __cplusplus
}
#endif  /* C++ */

#endif /* _NEW_HOLDER_H */
