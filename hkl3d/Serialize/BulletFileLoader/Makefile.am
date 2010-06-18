# Process this file with automake to produce Makefile.in

AM_CXXFLAGS = \
	-I$(top_srcdir)/hkl3d/bullet/src

noinst_LTLIBRARIES = libbulletFileLoader.la

libbulletFileLoader_la_SOURCES = \
	bChunk.cpp\
	bChunk.h\
	bCommon.h\
	bDefines.h\
	bDNA.cpp\
	bDNA.h\
	bFile.cpp\
	bFile.h\
	btBulletFile.cpp\
	btBulletFile.h\
	bullet_btBvhSubtreeInfoData.h\
	bullet_btCapsuleShapeData.h\
	bullet_btCollisionObjectDoubleData.h\
	bullet_btCollisionObjectFloatData.h\
	bullet_btCollisionShapeData.h\
	bullet_btCompoundShapeChildData.h\
	bullet_btCompoundShapeData.h\
	bullet_btConeTwistConstraintData.h\
	bullet_btConstraintInfo1.h\
	bullet_btConvexHullShapeData.h\
	bullet_btConvexInternalShapeData.h\
	bullet_btCylinderShapeData.h\
	bullet_btGeneric6DofConstraintData.h\
	bullet_btGImpactMeshShapeData.h\
	bullet_btHingeConstraintDoubleData.h\
	bullet_btHingeConstraintFloatData.h\
	bullet_btIntIndexData.h\
	bullet_btMatrix3x3DoubleData.h\
	bullet_btMatrix3x3FloatData.h\
	bullet_btMeshPartData.h\
	bullet_btMultiSphereShapeData.h\
	bullet_btOptimizedBvhNodeDoubleData.h\
	bullet_btOptimizedBvhNodeFloatData.h\
	bullet_btPhysicsSystem.h\
	bullet_btPoint2PointConstraintDoubleData.h\
	bullet_btPoint2PointConstraintFloatData.h\
	bullet_btPositionAndRadius.h\
	bullet_btQuantizedBvhDoubleData.h\
	bullet_btQuantizedBvhFloatData.h\
	bullet_btQuantizedBvhNodeData.h\
	bullet_btRigidBodyDoubleData.h\
	bullet_btRigidBodyFloatData.h\
	bullet_btShortIntIndexData.h\
	bullet_btShortIntIndexTripletData.h\
	bullet_btSliderConstraintData.h\
	bullet_btStaticPlaneShapeData.h\
	bullet_btStridingMeshInterfaceData.h\
	bullet_btTransformDoubleData.h\
	bullet_btTransformFloatData.h\
	bullet_btTriangleInfoData.h\
	bullet_btTriangleInfoMapData.h\
	bullet_btTriangleMeshShapeData.h\
	bullet_btTypedConstraintData.h\
	bullet_btVector3DoubleData.h\
	bullet_btVector3FloatData.h\
	bullet_Common.h\
	bullet.h\
	bullet_ListBase.h\
	bullet_PointerArray.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
