unit NewtonHeaders;

{$include opts.inc}
{-$DEFINE DOUBLE_PRECISION}

interface

uses
	ctypes, USystem, UMath, DynamicLoader;

type
	Newton = class
	const
		MAJOR_VERSION                            =  2;
		MINOR_VERSION                            = 33;

		PROFILER_WORLD_UPDATE                    =  0;
		PROFILER_COLLISION_UPDATE                =  1;
		PROFILER_FORCE_CALLBACK_UPDATE           =  2;
		PROFILER_COLLISION_UPDATE_BROAD_PHASE    =  3;
		PROFILER_COLLISION_UPDATE_NARROW_PHASE   =  4;
		PROFILER_DYNAMICS_UPDATE                 =  5;
		PROFILER_DYNAMICS_CONSTRAINT_GRAPH       =  6;
		PROFILER_DYNAMICS_SOLVE_CONSTRAINT_GRAPH =  7;

	type
		SERIALIZE_ID = class
		const
			BOX                                =  0;
			CONE                               =  1;
			SPHERE                             =  2;
			CAPSULE                            =  3;
			CYLINDER                           =  4;
			COMPOUND                           =  5;
			CONVEXHULL                         =  6;
			CONVEXMODIFIER                     =  7;
			CHAMFERCYLINDER                    =  8;
			TREE                               =  9;
			NULL                               = 10;
			HEIGHTFIELD                        = 11;
			USERMESH                           = 12;
			SCENE                              = 13;
			COMPOUND_BREAKABLE                 = 14;
		end;

	{$push} {$hints off} // C arrays are passed by reference
	type
		float  = type {$IFDEF DOUBLE_PRECISION} float64 {$ELSE} float32 {$ENDIF};
		pFloat = ^Float;

		Mat4 = array[0..15] of Float; pMat4 = ^Mat4;
		Vec3 = array[0..2] of Float;  pVec3 = ^Vec3;
		Vec4 = array[0..3] of Float;  pVec4 = ^Vec4;
		Plane = type Vec4; pPlane = ^Plane;

		// pointer types
		pMesh = ^pointer;
		pBody = ^pointer;
		pWorld = ^pointer;
		pJoint = ^pointer;
		pContact = ^pointer;
		pMaterial = ^pointer;
		pCollision = ^pointer; ppCollision = ^pCollision;
		pSceneProxy = ^pointer;
		pBreakableComponentMesh = ^pointer;
		// JointLibrary
		pUserJoint = ^pointer;

		// CollisionInfoRecord
		BoxParam = record
			m_x, m_y, m_z: Float;
		end;

		SphereParam = record
			m_r0, m_r1, m_r2: Float;
		end;

		CylinderParam = record
			m_r0, m_r1, m_height: Float;
		end;

		CapsuleParam = record
			m_r0, m_r1, m_height: Float;
		end;

		ConeParam = record
			m_r, m_height: Float;
		end;

		ChamferCylinderParam = record
			m_r, m_height: Float;
		end;

		ConvexHullParam = record
			m_vertexCount, m_vertexStrideInBytes, m_faceCount: cint;
			m_vertex  : pFloat;
		end;

		ConvexHullModifierParam = record
			m_chidren: pCollision;
		end;

		CompoundCollisionParam = record
			m_childrenCount: cint;
			m_children: ppCollision; // pointer to array of pcollisions
		end;

		CollisionTreeParam = record
			m_vertexCount, m_indexCount: cint;
		end;

		HeightFieldCollisionParam = record
			m_width, m_height, m_gridsDiagonals: cint;
			m_horizontalScale, m_verticalScale: Float;
			m_elevation: pointer; //unsigned short *m_elevation;
			m_atributes: pchar;
		end;

		SceneCollisionParam = record
			m_childrenProxyCount: cint;
		end;

		CollisionNullParam = record
			// nothing.
		end;

		CollisionInfoRecord = record
			m_offsetMatrix: Mat4;
			m_collisionType,                 // tag id to identify the collision primitive
			m_referenceCount: cint;       // the current reference count for this collision
			m_collisionUserID: cint;
			Case cint of
				SERIALIZE_ID.BOX: (asbox: BoxParam);
				SERIALIZE_ID.CONE: (ascone: ConeParam);
				SERIALIZE_ID.SPHERE: (assphere: SphereParam);
				SERIALIZE_ID.CAPSULE: (ascapsule: CapsuleParam);
				SERIALIZE_ID.CYLINDER: (ascylinder: CylinderParam);
				SERIALIZE_ID.COMPOUND: (ascompound: CompoundCollisionParam);
				SERIALIZE_ID.CONVEXHULL: (asconvexhull: ConvexHullParam);
				SERIALIZE_ID.CONVEXMODIFIER: (asxonvexhull: ConvexHullModifierParam);
				SERIALIZE_ID.CHAMFERCYLINDER: (aschamfercylinder: ChamferCylinderParam);
				SERIALIZE_ID.TREE: (astree: CollisionTreeParam);
				SERIALIZE_ID.NULL: (asnull: CollisionNullParam);
				SERIALIZE_ID.HEIGHTFIELD: (asheightfield: HeightFieldCollisionParam);
				SERIALIZE_ID.USERMESH: (m_paramArray: array[0..63] of Float);
				SERIALIZE_ID.SCENE: (asscenecollision: SceneCollisionParam);
		end;

		pCollisionInfoRecord = ^CollisionInfoRecord;

		JointRecord = record
			m_attachmenMatrix_0: Mat4;
			m_attachmenMatrix_1: Mat4;
			m_minLinearDof    : Vec3;
			m_maxLinearDof    : Vec3;
			m_minAngularDof   : Vec3;
			m_maxAngularDof   : Vec3;
			m_attachBody_0    : pBody;
			m_attachBody_1    : pBody;
			m_extraParameters : array[0..15] of Float;
			m_bodiesCollisionOn: cint;
			m_descriptionType : array[0..31] of uint16;
		end;

		pUserMeshCollisionCollideDesc = ^UserMeshCollisionCollideDesc;
		UserMeshCollisionCollideDesc = record
			m_boxP0             : Vec4;   // lower bounding box of intersection query in local space
			m_boxP1             : Vec4;   // upper bounding box of intersection query in local space
			m_threadNumber      : cint;         // current thread executing this query
			m_faceCount         : cint;         // the application should set here how many polygons intersect the query box
			m_vertexStrideInBytes: cint;        // the application should set here the size of each vertex
			m_userData          : pointer;      // user data passed to the collision geometry at creation time
			m_vertex            : ^Float; // the application should the pointer to the vertex array.
			m_userAttribute     : pcint;        // the application should set here the pointer to the user data, one for each face
			m_faceIndexCount    : pcint;        // the application should set here the pointer to the vertex count of each face.
			m_faceVertexIndex   : pcint;        // the application should set here the pointer index array for each vertex on a face.
			m_objBody           : pBody;  // pointer to the colliding body
			m_polySoupBody      : pBody;  // pointer to the rigid body owner of this collision tree
		end;

		pWorldConvexCastReturnInfo = ^WorldConvexCastReturnInfo;
		WorldConvexCastReturnInfo = record
			m_point          : Vec4; // collision point in global space
			m_normal         : Vec4; // surface normal at collision point in global space
			m_normalOnHitPoint: Vec4; // surface normal at the surface of the hit body,
			                                // is the same as the normal calculated by a ray cast hitting the body at the hit poi
			m_penetration    : Float; // contact penetration at collision point
			m_contactID      : cint;        // collision ID at contact point
			m_hitBody        : pBody; // body hit at contact point
		end;

		pUserMeshCollisionRayHitDesc = ^UserMeshCollisionRayHitDesc;
		UserMeshCollisionRayHitDesc = record
			m_p0      : Vec4; // ray origin in collision local space
			m_p1      : Vec4; // ray destination in collision local space
			m_normalOut: Vec4; // copy here the normal at the ray intersection
			m_userIdOut: cint;      // copy here a user defined id for further feedback
			m_userData: pointer;    // user data passed to the collision geometry at creation time
		end;

		AllocMemory                         = function(sizeInBytes: cint): pointer; cdecl;
		FreeMemory                          = procedure(ptr: pointer; sizeInBytes: cint); cdecl;
		DestroyWorld                        = procedure(World: pWorld); cdecl;
		GetTicksCountCallback               = function(): cuint; cdecl;
		Serialize                           = procedure(serializeHandle: pointer; buffer: pointer; size: csize_t); cdecl;
		Deserialize                         = procedure(serializeHandle: pointer; buffer: pointer; size: csize_t); cdecl;
		UserMeshCollisionDestroyCallback    = procedure(descData: pointer); cdecl;
		UserMeshCollisionCollideCallback    = procedure(UserMeshCollisionCollideDesc: pUserMeshCollisionCollideDesc); cdecl;
		UserMeshCollisionRayHitCallback     = function(UserMeshCollisionRayHitDesc: pUserMeshCollisionRayHitDesc): cint; cdecl;
		UserMeshCollisionGetCollisionInfo   = procedure(userData: pointer; infoRecord: pCollisionInfoRecord); cdecl;
		UserMeshCollisionGetFacesInAABB     = function(userData: pointer; constref p0, p1: Vec3; vertexArray: pVec3; vertexCount: pcint;
		                                                    vertexStrideInBytes: pcint; indexList: pcint; maxIndexCount: cint; userDataList: pcint): cint; cdecl;
		CollisionTreeRayCastCallback        = function(Body: pBody; TreeCollision: pCollision; interception: Float; constref normal: Vec3; faceId: cint; usedData: pointer): Float; cdecl;
		HeightFieldRayCastCallback          = function(Body: pBody; HeightFieldCollision: pCollision; Interception: Float; Row, Col: cint; constref Normal: Vec3; FaceID: cint; UsedData: pointer): Float; cdecl;
		TreeCollisionCallback               = procedure(bodyWithTreeCollision: pBody; body: pBody; faceID: cint;
		                                                     vertex: pVec3; vertexstrideInBytes: cint); cdecl;
		BodyDestructor                      = procedure(body: pBody); cdecl;
		ApplyForceAndTorque                 = procedure(body: pBody; timestep: Float; threadIndex: cint); cdecl;
		SetTransform                        = procedure(body: pBody; constref matrix: Mat4; threadIndex: cint); cdecl;
		IslandUpdate                        = function(World: pWorld; islandHandle: pointer; bodyCount: cint): cint; cdecl;
		BodyLeaveWorld                      = procedure(body: pBody; threadIndex: cint); cdecl;
		DestroyBodyByExeciveForce           = procedure(body: pBody; contact: pJoint); cdecl;
		CollisionDestructor                 = procedure(World: pWorld; collision: pCollision); cdecl;
		CollisionCompoundBreakableCallback  = function(Mesh: pMesh; userData: pointer; out planeMatrixOut: Plane): cint; cdecl;
		GetBuoyancyPlane                    = function(collisionID: cint; context: pointer; constref globalSpaceMatrix: Mat4; out globalSpacePlane: Plane): cint; cdecl;
		WorldRayPrefilterCallback           = function(body: pBody; collision: pCollision; userData: pointer): cuint; cdecl;
		WorldRayFilterCallback              = function(body: pBody; constref hitNormal: Vec3; collisionID: cint; userData: pointer; intersetParam: Float): Float; cdecl;
		OnAABBOverlap                       = function(material: pMaterial; body0, body1: pBody; threadIndex: cint): cint; cdecl;
		ContactsProcess                     = procedure(contact: pJoint; timestep: Float; threadIndex: cint); cdecl;
		BodyIterator                        = procedure(body: pBody; userData: pointer); cdecl;
		JointIterator                       = procedure(joint: pJoint; userData: pointer); cdecl;
		CollisionIterator                   = procedure(userData: pointer; vertexCount: cint; FaceArray: pVec3; faceId: cint); cdecl;
		BallCallBack                        = procedure(ball: pJoint; timestep: Float); cdecl;
		ConstraintDestructor                = procedure(me: pJoint); cdecl;

	class var
		// world control functions
		WorldGetVersion: function(World: pWorld): cint; cdecl;

		WorldCreate: function: pWorld; cdecl;
		WorldDestroy: procedure(World: pWorld); cdecl;
		//DestroyAllBodies: procedure(World: pWorld); cdecl;

		GetMemoryUsed: function: cint; cdecl;
		SetMemorySystem: procedure(malloc: AllocMemory; mfree: FreeMemory); cdecl;

		Update: procedure(World: pWorld; timestep: Float); cdecl;

		//InvalidateCache: procedure(World: pWorld); cdecl;
		//CollisionUpdate: procedure(World: pWorld); cdecl;
		SetSolverModel: procedure(World: pWorld; Model: cint); cdecl;
		SetPlatformArchitecture: procedure(World: pWorld; mode: cint); cdecl;
		//GetPlatformArchitecture: function(World: pWorld; description: PChar): cint; cdecl;
		//SetMultiThreadSolverOnSingleIsland: procedure(World: pWorld; mode: cint); cdecl;
		//GetMultiThreadSolverOnSingleIsland: function(World: pWorld): cint; cdecl;

		//SetPerformanceClock: procedure(World: pWorld; GetTicksCountCallback: pGetTicksCountCallback); cdecl;

		//ReadPerformanceTicks: function(World: pWorld; performanceEntry: cuint): cuint; cdecl;
		//ReadThreadPerformanceTicks: function(World: pWorld; ThreadIndex: cuint): cuint; cdecl;

		WorldCriticalSectionLock: procedure(World: pWorld); cdecl;
		WorldCriticalSectionUnlock: procedure(World: pWorld); cdecl;

		SetThreadsCount: procedure(World: pWorld; threads: cint); cdecl;
		GetThreadsCount: function(World: pWorld): cint; cdecl;
		GetMaxThreadsCount: function(World: pWorld): cint; cdecl;

		SetFrictionModel: procedure(World: pWorld; Model: cint); cdecl;
		//SetMinimumFrameRate: procedure(World: pWorld; frameRate: Float); cdecl;
		//SetBodyLeaveWorldEvent: procedure(World: pWorld; callback: pBodyLeaveWorld); cdecl;
		SetWorldSize: procedure(World: pWorld; constref minPoint, maxPoint: Vec3); cdecl;

		//SetIslandUpdateEvent: procedure(World: pWorld; IslandUpdate: pIslandUpdate); cdecl;
		//SetCollisionDestructor: procedure(World: pWorld; callback: CollisionDestructor); cdecl;
		//SetDestroyBodyByExeciveForce: procedure(World: pWorld; callback: pDestroyBodyByExeciveForce); cdecl;
		//WorldForEachJointDo: procedure(World: pWorld; callback: pJointIterator; userData: pointer); cdecl;

		WorldForEachBodyInAABBDo: procedure(World: pWorld; constref p0, p1: Vec3; callback: BodyIterator; userData: pointer); cdecl;

		//WorldSetUserData: procedure(World: pWorld; userData: pointer); cdecl;
		//WorldGetUserData: function(World: pWorld): pointer; cdecl;

		//WorldSetDestructorCallBack: procedure(World: pWorld; DestroyWorld: pDestroyWorld); cdecl;
		//WorldGetDestructorCallBack: function(World: pWorld): pDestroyWorld; cdecl;

		WorldRayCast: procedure(World: pWorld; constref p0, p1: Vec3;
			filter: WorldRayFilterCallback; userData: pointer; prefilter: WorldRayPrefilterCallback); cdecl;

		{WorldConvexCast: function(World: pWorld; constref matrix: Mat4; constref target: Vec3;
		                              shape: pCollision; hitParam: pFloat; userData: pointer;
		                              prefilter: WorldRayPrefilterCallback; info: pWorldConvexCastReturnInfo;
		                              maxContactsCount: cint; threadIndex: cint): cint; cdecl;}

		//WorldGetBodyCount: function(World: pWorld): cint; cdecl;
		//WorldGetConstraintCount: function(World: pWorld): cint; cdecl;

		// Simulation islands
		//IslandGetBody: function(island: pointer; bodyIndex: cint): pBody; cdecl;
		//IslandGetBodyAABB: procedure(island: pointer; bodyIndex: cint; var p0, p1: Vec3); cdecl;

		//  Physics Material Section
		MaterialCreateGroupID: function(World: pWorld): cint; cdecl;
		MaterialGetDefaultGroupID: function(World: pWorld): cint; cdecl;
		//MaterialDestroyAllGroupID: procedure(World: pWorld); cdecl;

		MaterialGetUserData: function(World: pWorld; id0, id1: cint): pointer; cdecl;

		//MaterialSetSurfaceThickness: procedure(World: pWorld; id0: cint; id1: cint; thickness: Float); cdecl;
		//MaterialSetContinuousCollisionMode: procedure(World: pWOrld; id0, id1, state: cint); cdecl;

		MaterialSetCollisionCallback: procedure(World: pWorld; id0, id1: cint;
			userData: pointer; AABBOverlap: OnAABBOverlap; process: ContactsProcess); cdecl;

		// MaterialSetDefaultSoftness: procedure(World: pWorld; id0: cint; id1: cint; value: Float); cdecl;
		// MaterialSetDefaultElasticity: procedure(World: pWorld; id0: cint; id1: cint; elasticCoef: Float); cdecl;
		MaterialSetDefaultCollidable: procedure(World: pWorld; id0: cint; id1: cint; state: cint); cdecl;
		MaterialSetDefaultFriction: procedure(World: pWorld; id0: cint; id1: cint;
		                                          staticFriction: Float; kineticFriction: Float); cdecl;

		{WorldGetFirstMaterial: function(World: pWorld): pMaterial; cdecl;
		WorldGetNextMaterial: function(World: pWorld; material: pMaterial): pMaterial; cdecl;
		WorldGetFirstBody: function(World: pWorld): pBody; cdecl;
		WorldGetNextBody: function(World: pWorld; curBody: pBody): pBody; cdecl;}

		// Physics Contact control functions
		MaterialGetMaterialPairUserData: function(material: pMaterial): pointer; cdecl;
		MaterialGetContactFaceAttribute: function(material: pMaterial): cuint; cdecl;
		// MaterialGetBodyCollisionID: function(material: pMaterial; body: pBody): cuint; cdecl;

		MaterialGetContactNormalSpeed: function(material: pMaterial): Float; cdecl;
		MaterialGetContactForce: procedure(Material: pMaterial; body: pBody; out Force: Vec3); cdecl;
		MaterialGetContactPositionAndNormal: procedure(Material: pMaterial; body: pBody; out Posit, Normal: Vec3); cdecl;
		MaterialGetContactTangentDirections: procedure(Material: pMaterial; body: pBody; out Dir0, Dir1: Vec3); cdecl;

		MaterialGetContactTangentSpeed: function(material: pMaterial; index: cint): Float; cdecl;

		{MaterialSetContactSoftness: procedure(material: pMaterial; softness: Float); cdecl;
		MaterialSetContactElasticity: procedure(material: pMaterial; restitution: Float); cdecl;
		MaterialSetContactFrictionState: procedure(material: pMaterial; state: cint; index: cint); cdecl;
		MaterialSetContactFrictionCoef: procedure(material: pMaterial; staticFrictionCoef,kineticFrictionCoef: Float; index: cint); cdecl;

		MaterialSetContactNormalAcceleration: procedure(material: pMaterial; accel: Float); cdecl;
		MaterialSetContactNormalDirection: procedure(material: pMaterial; constref directionVector: Vec3); cdecl;

		MaterialSetContactTangentAcceleration: procedure(material: pMaterial; accel: Float; index: cint); cdecl;
		MaterialContactRotateTangentDirections: procedure(material: pMaterial; constref directionVector: Vec3); cdecl;}

	// convex collision primitives creation functions
		CreateNull: function(World: pWorld): pCollision; cdecl;
		CreateSphere: function(World: pWorld; radiusX, radiusY, radiusZ: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		CreateBox: function(World: pWorld; dx: Float; dy: Float; dz: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		CreateCone: function(World: pWorld; radius: Float; height: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		CreateCapsule: function(World: pWorld; radius: Float; height: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		CreateCylinder: function(World: pWorld; radius: Float; height: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		CreateChamferCylinder: function(World: pWorld; raduis: Float; height: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		CreateConvexHull: function(World: pWorld; count: cint; vertexCloud: pFloat; strideInBytes: cint; tolerance: Float; shapeID: cint; constref offsetMatrix: Mat4): pCollision; cdecl;
		//CreateConvexHullFromMesh: function(World: pWorld; mesh: pMesh; tolerance: Float; shapeID: cint): pCollision; cdecl;

		{CreateConvexHullModifier: function(World: pWorld; convexHullCollision: pCollision; shapeID: cint): pCollision; cdecl;
		ConvexHullModifierGetMatrix: procedure(convexHullCollision: pCollision; var matrix: Mat4); cdecl;
		ConvexHullModifierSetMatrix: procedure(convexHullCollision: pCollision; constref matrix: Mat4); cdecl;

		CollisionIsTriggerVolume: function(convexCollision: pCollision): cint; cdecl;
		CollisionSetAsTriggerVolume: procedure(convexCollision: pCollision; trigger: cint); cdecl;}

		{CollisionSetMaxBreakImpactImpulse: procedure(convexHullCollision: pCollision; maxImpactImpulse: Float); cdecl;
		CollisionGetMaxBreakImpactImpulse: function(convexHullCollision: pCollision): Float; cdecl;}

		{CollisionSetUserID: procedure(convexCollision: pCollision; id: cuint); cdecl;
		CollisionGetUserID: function(convexCollision: pCollision): cuint; cdecl;

		ConvexHullGetFaceIndices: function(convexHullCollision: pCollision; face: cint; faceIndices: pcint): cint; cdecl;}
		ConvexCollisionCalculateVolume: function(convexCollision: pCollision): Float; cdecl;
		ConvexCollisionCalculateInertialMatrix: procedure(convexCollision: pCollision; out inertia, origin: Vec3); cdecl;

		CollisionMakeUnique: procedure(World: pWorld; collision: pCollision); cdecl;
		ReleaseCollision: procedure(World: pWorld; collision: pCollision); cdecl;
		AddCollisionReference: function(Collision: pCollision): cint; cdecl;

		// complex collision primitives creation functions
		// note: can only be used with static bodies(bodies with infinite mass)
		CreateCompoundCollision: function(World: pWorld; count: cint; collisionPrimitiveArray: ppCollision; shapeID: cint): pCollision; cdecl;
		{CreateCompoundCollisionFromMesh: function(
			World: pWorld; mesh: pMesh; maxSubshapes, shapeID, subShapeID: cint): pCollision; cdecl;}
		{CreateUserMeshCollision: function(World: pWorld; constref minBox, maxBox: Vec3;
			userData: pointer; collideCallback: UserMeshCollisionCollideCallback;
			rayHitCallback: UserMeshCollisionRayHitCallback;
			destroyCallback: UserMeshCollisionDestroyCallback;
			getInfoCallback: UserMeshCollisionGetCollisionInfo;
			facesInAABBCallback: UserMeshCollisionGetFacesInAABB; shapeID: cint): pCollision; cdecl;}

		{CreateSceneCollision: function(World: pWorld; shapeID: cint): pCollision; cdecl;
		SceneCollisionCreateProxy: function(scene: pCollision; collision: pCollision): pSceneProxy; cdecl;
		SceneCollisionDestroyProxy: procedure(scene: pCollision; Proxy: pSceneProxy); cdecl;
		SceneProxySetMatrix: procedure(Proxy: pSceneProxy; constref Matrix: Mat4); cdecl;
		SceneProxyGetMatrix: procedure(Proxy: pSceneProxy; out Matrix: Mat4); cdecl;
		SceneGetFirstProxy: function(Scene: pCollision): pSceneProxy; cdecl;
		SceneGetNextProxy: function(Scene: pCollision; Proxy: pSceneProxy): pSceneProxy; cdecl;

		SceneCollisionOptimize: procedure(scene: pCollision); cdecl;}

	// complex breakable collision primitives interface
		{CreateCompoundBreakable: function(World: pWorld; meshCount: cint;
			SolidsArray: pMesh; ShapeIDArray: pcint; Densities: pFloat;
			internalFaceMaterial: pcint; ShapeID: cint;
			debrisID: cint; DebrisSeparationGap: Float): pCollision; cdecl;

		CompoundBreakableResetAnchoredPieces: procedure(compoundBreakable: pCollision); cdecl;
		CompoundBreakableSetAnchoredPieces: procedure(compoundBreakable: pCollision; fixshapesCount: cint; matrixPallete: pFloat; fixedShapesArray: pCollision); cdecl;

		CompoundBreakableGetVertexCount: function(compoundBreakable: pCollision): cint; cdecl;
		CompoundBreakableGetVertexStreams: procedure(compoundBreakable: pCollision;
			vertexStrideInByte: cint; Vertex: pFloat; normalStrideInByte: cint;
			normal: pFloat; uvStrideInByte: cint; uv: pFloat); cdecl;

		BreakableGetMainMesh: function(compoundBreakable: pCollision): pBreakableComponentMesh; cdecl;
		BreakableGetFirstComponent: function(compoundBreakable: pCollision): pBreakableComponentMesh; cdecl;
		BreakableGetNextComponent: function(component: pBreakableComponentMesh): pBreakableComponentMesh; cdecl;

		BreakableBeginDelete: procedure(compoundBreakable: pCollision); cdecl;
		BreakableCreateDebrieBody: function(compoundBreakable: pCollision; component: pBreakableComponentMesh): pBody; cdecl;
		BreakableDeleteComponent: procedure(compoundBreakable: pCollision; component: pBreakableComponentMesh); cdecl;
		BreakableEndDelete: procedure(compoundBreakable: pCollision); cdecl;

		BreakableGetComponentsInRadius: function(compoundBreakable: pCollision; position: pFloat; radius: Float; Segments: pBreakableComponentMesh; maxCount: cint): cint; cdecl;

		BreakableGetFirstSegment: function(BreakableComponent: pBreakableComponentMesh): pointer; cdecl;
		BreakableGetNextSegment: function(Segment: pointer): pointer; cdecl;

		BreakableSegmentGetMaterial: function(Segment: pointer): cint; cdecl;
		BreakableSegmentGetIndexCount: function(Segment: pointer): cint; cdecl;
		BreakableSegmentGetIndexStream: function(CompoundBreakable: pCollision; MeshOwner: pBreakableComponentMesh; Segment: pointer; Index: pcint): cint; cdecl;
		BreakableSegmentGetIndexStreamShort: function(CompoundBreakable: pCollision; MeshOwner: pBreakableComponentMesh; Segment: pointer; Index: pcshort): cint; cdecl;}

		// Collision serialization functions
		CreateCollisionFromSerialization: function(World: pWorld; deserializeFunction: Deserialize; serializeHandle: pointer): pCollision; cdecl;
		CollisionSerialize: procedure(World: pWorld; collision: pCollision; serializeFunction: Serialize; serializeHandle: pointer); cdecl;
		CollisionGetInfo: procedure(collision: pCollision; out collisionInfo: CollisionInfoRecord); cdecl;

		// Static collision shapes functions
		CreateHeightFieldCollision: function(World: pWorld; width, height, gridDiagonals: cint;
		                                           elevationMap: pcushort; attributeMap: pcuchar; horizontalScale, verticalScale: Float;
		                                           shapeID: cint): pCollision; cdecl;
		//HeightFieldSetUserRayCastCallback: procedure(TreeCollision: pCollision; RayHitCallBack: HeightFieldRayCastCallback); cdecl;
		CreateTreeCollision: function(World: pWorld; shapeID: cint): pCollision; cdecl;
		//TreeCollisionSetUserRayCastCallback: procedure(treeCollision: pCollision; rayHitCallback: pCollisionTreeRayCastCallback); cdecl;

		TreeCollisionBeginBuild: procedure(treeCollision: pCollision); cdecl;
		TreeCollisionAddFace: procedure(treeCollision: pCollision; vertexCount: cint; vertexPtr: pVec3; strideInBytes: cint; faceAttribute: cint); cdecl;
		TreeCollisionEndBuild: procedure(treeCollision: pCollision; optimize: cint); cdecl;

		{TreeCollisionGetFaceAtribute: function(treeCollision: pCollision; faceIndexArray: pcint): cint; cdecl;
		TreeCollisionSetFaceAtribute: procedure(treeCollision: pCollision; faceIndexArray: pcint; attribute: cint); cdecl;}

		{TreeCollisionGetVertexListIndexListInAABB: function(treeCollision: pCollision; constref p0, p1: Vec3;
			vertexArray: pFloat; vertexCount, vertexStrideInBytes: pcint; indexList: pcint; maxIndexCount: cint; faceAttribute: pcint): cint; cdecl;}

		//StaticCollisionSetDebugCallback: procedure(staticCollision: pCollision; userCallback: pTreeCollisionCallback); cdecl;

		// General purpose collision library functions
		CollisionPointDistance: function(World: pWorld; constref point: Vec3; collision: pCollision; constref matrix: Mat4;
			out contact: Vec3; out normal: Vec3; threadIndex: cint): cint; cdecl;

		{CollisionClosestPoint: function(World: pWorld; collsionA: pCollision;
			matrixA: pFloat; collisionB: pCollision; matrixB: pFloat;
			contactA, contactB, normalAB: pFloat; threadIndex: cint): cint; cdecl;}

		{CollisionCollide: function(World: pWorld; maxSize: cint; collsionA: pCollision;
			matrixA: pFloat; collisionB: pCollision; matrixB: pFloat;
			contacts, normals, penetration: pFloat; threadIndex: cint): cint; cdecl;}

		{CollisionCollideContinue: function(World: pWorld; maxSize: cint; timestep: Float;
			collsionA: pCollision; matrixA: pFloat; velocA: pFloat; omegaA: Float;
			collsionB: pCollision; matrixB: pFloat; velocB: pFloat; omegaB: Float;
			timeOfImpact: pFloat; contacts: pFloat; normals: pFloat;
			penetration: pFloat; threadIndex: cint): cint; cdecl;}

		//CollisionSupportVertex: procedure(collision: pCollision; constref dir: Vec3; out vertex: Vec3); cdecl;
		CollisionRayCast: function(collision: pCollision; constref p0, p1: Vec3; out normal: Vec3; attribute: pcint): Float; cdecl;
		//CollisionCalculateAABB: procedure(collision: pCollision; constref matrix: Mat4; out p0, p1: Vec3); cdecl;
		{CollisionForEachPolygonDo: procedure(collision: pCollision; constref matrix: Mat4; callback: CollisionIterator; UserData: pointer); cdecl;}

		// transforms utility functions
		//GetEulerAngle: procedure(constref matrix: Mat4; out eulersAngles: Vec3); cdecl;
		//SetEulerAngle: procedure(constref eulersAngles: Vec3; out matrix: Mat4); cdecl;
		//CalculateSpringDamperAcceleration: function(dt, ks, x, kd, s: Float): Float; cdecl;

		// body manipulation functions
		CreateBody: function(World: pWorld; collision: pCollision; constref Matrix: Mat4): pBody; cdecl;
		DestroyBody: procedure(World: pWorld; body: pBody); cdecl;

		BodyAddForce: procedure(body: pBody; constref force: Vec3); cdecl;
		BodyAddTorque: procedure(body: pBody; constref torque: Vec3); cdecl;

		//BodyCalculateInverseDynamicsForce: procedure(body: pBody; timestep: Float; constref desiredVeloc: Vec3; out forceOut: Vec3); cdecl;

		BodySetMatrix: procedure(body: pBody; constref matrix: Mat4); cdecl;
		// BodySetMatrixRecursive: procedure(body: pBody; constref matrix: Mat4); cdecl;
		BodySetMassMatrix: procedure(body: pBody; mass: Float; Ixx: Float; Iyy: Float; Izz: Float); cdecl;
		BodySetMaterialGroupID: procedure(body: pBody; id: cint); cdecl;
		//BodySetContinuousCollisionMode: procedure(body: pbody; state: cint); cdecl;
		//BodySetJointRecursiveCollision: procedure(body: pBody; state: cuint); cdecl;
		BodySetOmega: procedure(body: pBody; constref omega: Vec3); cdecl;
		BodySetVelocity: procedure(body: pBody; constref velocity: Vec3); cdecl;
		// BodySetForce: procedure(body: pBody; constref force: Vec3); cdecl;
		// BodySetTorque: procedure(body: pBody; constref torque: Vec3); cdecl;

		BodySetCentreOfMass: procedure(body: pBody; constref com: Vec3); cdecl;
		BodySetLinearDamping: procedure(body: pBody; linearDamp: Float); cdecl;
		BodySetAngularDamping: procedure(body: pBody; constref angularDamp: Vec3); cdecl;
		BodySetUserData: procedure(body: pBody; userData: pointer); cdecl;

		BodySetCollision: procedure(body: pBody; collision: pCollision); cdecl;

		{BodyGetSleepState: function(body: pBody): cint; cdecl;
		BodyGetAutoSleep: function(body: pBody): cint; cdecl;}
		BodySetAutoSleep: procedure(body: pBody; state: cint); cdecl;

		{BodyGetFreezeState: function(body: pBody): cint; cdecl;
		BodySetFreezeState: procedure(body: pBody; state: cint); cdecl;}

		//BodySetDestructorCallback: procedure(body: pBody; callback: BodyDestructor); cdecl;

		BodySetTransformCallback: procedure(body: pBody; callback: SetTransform); cdecl;
		//BodyGetTransformCallback: function(body: pBody): SetTransform; cdecl;

		BodySetForceAndTorqueCallback: procedure(body: pBody; callback: ApplyForceAndTorque); cdecl;
		//BodyGetForceAndTorqueCallback: function(body: pBody): ApplyForceAndTorque; cdecl;
		BodyGetUserData: function(body: pBody): pointer; cdecl;
		//BodyGetWorld: function(body: pBody): pWorld; cdecl;
		//BodyGetCollision: function(body: pBody): pCollision; cdecl;
		//BodyGetMaterialGroupID: function(body: pBody): cint; cdecl;

		//BodyGetContinuousCollisionMode: function(body: pBody): cint; cdecl;
		//BodyGetJointRecursiveCollision: function(body: pBody): cint; cdecl;

		//BodyGetMatrix: procedure(body: pBody; out matrix: Mat4); cdecl;
		// BodyGetRotation: procedure(body: pBody; out rotation: Quaternion); cdecl;
		// BodyGetMassMatrix: procedure(body: pBody; out mass: Float; out Ixx, Iyy, Izz: Float); cdecl;
		// BodyGetInvMass: procedure(body: pBody; out invMass: Float; out invIxx, invIyy, invIzz: Float); cdecl;
		BodyGetOmega: procedure(body: pBody; out vector: Vec3); cdecl;
		BodyGetVelocity: procedure(body: pBody; out vector: Vec3); cdecl;
		// BodyGetForce: procedure(body: pBody; out vector: Vec3); cdecl;
		// BodyGetTorque: procedure(body: pBody; out vector: Vec3); cdecl;
		//BodyGetForceAcc: procedure(body: pBody; out vector: Vec3); cdecl;
		//BodyGetTorqueAcc: procedure(body: pBody; out vector: Vec3); cdecl;
		BodyGetCentreOfMass: procedure(body: pBody; out com: Vec3); cdecl;

		//BodyGetLinearDamping: function(body: pBody): Float; cdecl;
		//BodyGetAngularDamping: procedure(body: pBody; out vector: Vec3); cdecl;
		BodyGetAABB: procedure(body: pBody; out p0, p1: Vec3); cdecl;

		//BodyGetFirstJoint: function(body: pBody): pJoint; cdecl;
		//BodyGetNextJoint: function(body: pBody; joint: pJoint): pJoint; cdecl;
		//BodyGetFirstContactJoint: function(body: pBody): pJoint; cdecl;
		//BodyGetNextContactJoint: function(body: pBody; contactJoint: pJoint): pJoint; cdecl;

		ContactJointGetFirstContact: function(contactJoint: pJoint): pointer; cdecl;
		ContactJointGetNextContact: function(contactJoint: pJoint; contact: pointer): pointer; cdecl;
		//ContactJointGetContactCount: function(contactJoint: pJoint): cint; cdecl;
		ContactJointRemoveContact: procedure(contactJoint: pJoint; contact: pointer); cdecl;
		ContactGetMaterial: function(contact: pointer): pMaterial; cdecl;

		BodyAddBuoyancyForce: procedure(body: pBody; fluidDensity: Float; fluidLinearViscosity, fluidAngularViscosity: Float;
			constref gravityVector: Vec3; buoyancyPlane: GetBuoyancyPlane; context: pointer); cdecl;

		// BodyAddImpulse: procedure(body: pBody; constref pointDeltaVeloc, pointPosit: Vec3); cdecl;

		// Common joint funtions
		JointGetUserData: function(joint: pJoint): pointer; cdecl;
		JointSetUserData: procedure(joint: pJoint; userData: pointer); cdecl;

		JointGetBody0: function(joint: pJoint): pBody; cdecl;
		JointGetBody1: function(joint: pJoint): pBody; cdecl;

		//JointGetInfo: procedure(joint: pJoint; out info: JointRecord); cdecl;
		//JointGetCollisionState: function(joint: pJoint): cint; cdecl;
		//JointSetCollisionState: procedure(joint: pJoint; state: cint); cdecl;

		//JointGetStiffness: function(joint: pJoint): Float; cdecl;
		// JointSetStiffness: procedure(joint: pJoint; state: Float); cdecl;

		DestroyJoint: procedure(World: pWorld; joint: pJoint); cdecl;
		//JointSetDestructor: procedure(joint: pJoint; _destructor: ConstraintDestructor); cdecl;

		// Ball and Socket joint functions
		ConstraintCreateBall: function(World: pWorld; constref pivotPoint: Vec3; childBody: pBody; parentBody: pBody): pJoint; cdecl;
		// BallSetUserCallback: procedure(ball: pJoint; callback: BallCallBack); cdecl;
		// BallGetJointAngle: procedure(ball: pJoint; out angle: Float); cdecl;
		// BallGetJointOmega: procedure(ball: pJoint; out omega: Vec3); cdecl;
		// BallGetJointForce: procedure(ball: pJoint; out force: Vec3); cdecl;
		BallSetConeLimits: procedure(ball: pJoint; constref pin: Vec3; maxConeAngle, maxTwistAngle: Float); cdecl;
	{$pop}

	const
		IdentityMat4: Mat4 =
		(
			1.0, 0.0, 0.0, 0.0,
			0.0, 1.0, 0.0, 0.0,
			0.0, 0.0, 1.0, 0.0,
			0.0, 0.0, 0.0, 1.0
		);

		class function From(const v: Vec3): UMath.Vec3;
		class function From(const m: Mat4): UMath.Matrix4;

	class var
		loader: DLLoader;
	end;

	operator :=(const v: Vec3): Newton.Vec3;
	operator :=(const m: Matrix4): Newton.Mat4;

implementation

	class function Newton.From(const v: Vec3): UMath.Vec3;
	type
		TestSize = UMath.Vec3;
	begin
	{$if sizeof(v) = sizeof(TestSize)}
		result := UMath.Vec3(v);
	{$else} {$note NewtonVec3 size mismatch}
		result := UMath.Vec3.Make(v[0], v[1], v[2]);
	{$endif}
	end;

	class function Newton.From(const m: Mat4): UMath.Matrix4;
	type
		TestSize = UMath.Matrix4;
	{$if sizeof(m) <> sizeof(TestSize)} var i: sint; {$endif}
	begin
	{$if sizeof(m) = sizeof(TestSize)} result.data := UMath.Matrix4.DataUnion(m);
	{$else} {$note NewtonMat4 size mismatch}
		for i := 0 to 15 do result.data.l[i] := m[i];
	{$endif}
	end;

	operator :=(const v: Vec3): Newton.Vec3;
	type
		TestSize = Newton.Vec3;
	begin
	{$if sizeof(v) = sizeof(TestSize)} result := Newton.Vec3(v.data);
	{$else} {$note NewtonVec3 size mismatch}
		result[0] := v.data[0];
		result[1] := v.data[1];
		result[2] := v.data[2];
	{$endif}
	end;

	operator :=(const m: Matrix4): Newton.Mat4;
	type
		TestSize = Newton.Mat4;
	{$if sizeof(m) <> sizeof(TestSize)} var i: sint; {$endif}
	begin
	{$if sizeof(m) = sizeof(TestSize)} result := Newton.Mat4(m.data);
	{$else} {$note NewtonMat4 size mismatch}
		for i := 0 to 15 do result[i] := m.data.l[i];
	{$endif}
	end;

	procedure DescribeNewtonFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@Newton.WorldGetVersion,                     'WorldGetVersion')^
		.Func(@Newton.WorldCreate,                         'Create')^
		.Func(@Newton.WorldDestroy,                        'Destroy')^
		.Func(@Newton.GetMemoryUsed,                       'GetMemoryUsed')^
		.Func(@Newton.SetMemorySystem,                     'SetMemorySystem')^
		.Func(@Newton.Update,                              'Update')^
		.Func(@Newton.SetSolverModel,                      'SetSolverModel')^
		.Func(@Newton.SetPlatformArchitecture,             'SetPlatformArchitecture')^
		.Func(@Newton.WorldCriticalSectionLock,            'WorldCriticalSectionLock')^
		.Func(@Newton.WorldCriticalSectionUnlock,          'WorldCriticalSectionUnlock')^
		.Func(@Newton.SetThreadsCount,                     'SetThreadsCount')^
		.Func(@Newton.GetThreadsCount,                     'GetThreadsCount')^
		.Func(@Newton.GetMaxThreadsCount,                  'GetMaxThreadsCount')^
		.Func(@Newton.SetFrictionModel,                    'SetFrictionModel')^
		.Func(@Newton.SetWorldSize,                        'SetWorldSize')^
		.Func(@Newton.WorldForEachBodyInAABBDo,            'WorldForEachBodyInAABBDo')^
		.Func(@Newton.WorldRayCast,                        'WorldRayCast')^
		.Func(@Newton.MaterialCreateGroupID,               'MaterialCreateGroupID')^
		.Func(@Newton.MaterialGetDefaultGroupID,           'MaterialGetDefaultGroupID')^
		.Func(@Newton.MaterialGetUserData,                 'MaterialGetUserData')^
		.Func(@Newton.MaterialSetCollisionCallback,        'MaterialSetCollisionCallback')^
		.Func(@Newton.MaterialSetDefaultCollidable,        'MaterialSetDefaultCollidable')^
		.Func(@Newton.MaterialSetDefaultFriction,          'MaterialSetDefaultFriction')^
		.Func(@Newton.MaterialGetMaterialPairUserData,     'MaterialGetMaterialPairUserData')^
		.Func(@Newton.MaterialGetContactFaceAttribute,     'MaterialGetContactFaceAttribute')^
		.Func(@Newton.MaterialGetContactNormalSpeed,       'MaterialGetContactNormalSpeed')^
		.Func(@Newton.MaterialGetContactForce,             'MaterialGetContactForce')^
		.Func(@Newton.MaterialGetContactPositionAndNormal, 'MaterialGetContactPositionAndNormal')^
		.Func(@Newton.MaterialGetContactTangentDirections, 'MaterialGetContactTangentDirections')^
		.Func(@Newton.MaterialGetContactTangentSpeed,      'MaterialGetContactTangentSpeed')^
		.Func(@Newton.CreateNull,                          'CreateNull')^
		.Func(@Newton.CreateSphere,                        'CreateSphere')^
		.Func(@Newton.CreateBox,                           'CreateBox')^
		.Func(@Newton.CreateCone,                          'CreateCone')^
		.Func(@Newton.CreateCapsule,                       'CreateCapsule')^
		.Func(@Newton.CreateCylinder,                      'CreateCylinder')^
		.Func(@Newton.CreateChamferCylinder,               'CreateChamferCylinder')^
		.Func(@Newton.CreateConvexHull,                    'CreateConvexHull')^
		.Func(@Newton.ConvexCollisionCalculateVolume,      'ConvexCollisionCalculateVolume')^
		.Func(@Newton.ConvexCollisionCalculateInertialMatrix, 'ConvexCollisionCalculateInertialMatrix')^
		.Func(@Newton.CollisionMakeUnique,                 'CollisionMakeUnique')^
		.Func(@Newton.ReleaseCollision,                    'ReleaseCollision')^
		.Func(@Newton.AddCollisionReference,               'AddCollisionReference')^
		.Func(@Newton.CreateCompoundCollision,             'CreateCompoundCollision')^
		.Func(@Newton.CreateCollisionFromSerialization,    'CreateCollisionFromSerialization')^
		.Func(@Newton.CollisionSerialize,                  'CollisionSerialize')^
		.Func(@Newton.CollisionGetInfo,                    'CollisionGetInfo')^
		.Func(@Newton.CreateHeightFieldCollision,          'CreateHeightFieldCollision')^
		.Func(@Newton.CreateTreeCollision,                 'CreateTreeCollision')^
		.Func(@Newton.TreeCollisionBeginBuild,             'TreeCollisionBeginBuild')^
		.Func(@Newton.TreeCollisionAddFace,                'TreeCollisionAddFace')^
		.Func(@Newton.TreeCollisionEndBuild,               'TreeCollisionEndBuild')^
		.Func(@Newton.CollisionPointDistance,              'CollisionPointDistance')^
		.Func(@Newton.CollisionRayCast,                    'CollisionRayCast')^
		.Func(@Newton.CreateBody,                          'CreateBody')^
		.Func(@Newton.DestroyBody,                         'DestroyBody')^
		.Func(@Newton.BodyAddForce,                        'BodyAddForce')^
		.Func(@Newton.BodyAddTorque,                       'BodyAddTorque')^
		.Func(@Newton.BodySetMatrix,                       'BodySetMatrix')^
		.Func(@Newton.BodySetMassMatrix,                   'BodySetMassMatrix')^
		.Func(@Newton.BodySetMaterialGroupID,              'BodySetMaterialGroupID')^
		.Func(@Newton.BodySetOmega,                        'BodySetOmega')^
		.Func(@Newton.BodySetVelocity,                     'BodySetVelocity')^
		.Func(@Newton.BodySetCentreOfMass,                 'BodySetCentreOfMass')^
		.Func(@Newton.BodySetLinearDamping,                'BodySetLinearDamping')^
		.Func(@Newton.BodySetAngularDamping,               'BodySetAngularDamping')^
		.Func(@Newton.BodySetUserData,                     'BodySetUserData')^
		.Func(@Newton.BodySetCollision,                    'BodySetCollision')^
		.Func(@Newton.BodySetAutoSleep,                    'BodySetAutoSleep')^
		.Func(@Newton.BodySetTransformCallback,            'BodySetTransformCallback')^
		.Func(@Newton.BodySetForceAndTorqueCallback,       'BodySetForceAndTorqueCallback')^
		.Func(@Newton.BodyGetUserData,                     'BodyGetUserData')^
		.Func(@Newton.BodyGetOmega,                        'BodyGetOmega')^
		.Func(@Newton.BodyGetVelocity,                     'BodyGetVelocity')^
		.Func(@Newton.BodyGetCentreOfMass,                 'BodyGetCentreOfMass')^
		.Func(@Newton.BodyGetAABB,                         'BodyGetAABB')^
		.Func(@Newton.ContactJointGetFirstContact,         'ContactJointGetFirstContact')^
		.Func(@Newton.ContactJointGetNextContact,          'ContactJointGetNextContact')^
		.Func(@Newton.ContactJointRemoveContact,           'ContactJointRemoveContact')^
		.Func(@Newton.ContactGetMaterial,                  'ContactGetMaterial')^
		.Func(@Newton.BodyAddBuoyancyForce,                'BodyAddBuoyancyForce')^
		.Func(@Newton.JointGetUserData,                    'JointGetUserData')^
		.Func(@Newton.JointSetUserData,                    'JointSetUserData')^
		.Func(@Newton.JointGetBody0,                       'JointGetBody0')^
		.Func(@Newton.JointGetBody1,                       'JointGetBody1')^
		.Func(@Newton.DestroyJoint,                        'DestroyJoint')^
		.Func(@Newton.ConstraintCreateBall,                'ConstraintCreateBall')^
		.Func(@Newton.BallSetConeLimits,                   'BallSetConeLimits');
	end;

	procedure Init;
	begin
		Newton.loader.Init('newton(prefix = Newton, lock)', @DescribeNewtonFunctions);
	end;

	procedure Done;
	begin
		Newton.loader.Done;
	end;

initialization
	&Unit('Newton').Initialize(@Init, @Done);
end.
