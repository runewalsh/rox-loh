unit Cameras;

{$include opts.inc}

interface

uses
	USystem, UMath, UClasses, GLBase, Utils, GLClasses {$ifdef use_serialization}, Streams {$endif}, SceneGraph;

const
	GL_CAMERA_FLAG_UNDERWATER = UserShaderFlags.PRIVATE_BUILTIN + 0;

type
	CameraProjectionMode =
	(
		proj_Perspective,
		proj_Ortho
	);

	CameraOrieMode =
	(
		orie_Quat,
		orie_Target
	);

	CameraFlag =
	(
		camera_Cinematic,
		camera_Main,
		camera_DontUpdateProjectionOnFly,
		camera_ClampToScene,
		_camera_First,
		_camera_ProjDirty
	);
	CameraFlags = set of CameraFlag;

	pCamera = ^Camera;
	Camera = object(&Object)
	private
		_rt: pRenderTarget;
		_postprocess: pPostprocess;
		_owner: pSceneNode;

		_projMode: CameraProjectionMode;
		_projectionMatrix, _invProjectionMatrix: Matrix4;
		_zNear, _zFar, _zFarLimit: float;
		_fov: float;
		_orthoRect: Rect;
		_splits: pFloat;

		_mode: CameraOrieMode;
		_flags: CameraFlags;
		_pos, _target, _tPos, _tTarget, _tTargetCastShift: Vec3;
		_qrot: Quaternion;
		_zrot, _tZRot: float;
		_accK: float;

		procedure _Initialize(dese: boolean);
		procedure _SetProjMode(newMode: CameraProjectionMode);
		procedure _SetZNear(const newZNear: float);
		procedure _SetZFar(const newZFar: float);
		procedure _SetFOV(const newFov: float);
		procedure _SetOrthoRect(const newRect: Rect);
		function _GetClampToScene: boolean;
		procedure _SetClampToScene(newClamp: boolean);

		function _GetTPos: Vec3;
		procedure _SetTPos(const newPos: Vec3);
		procedure _SetQRot(const newQrot: Quaternion);
		function _GetTTarget: Vec3;
		procedure _SetTTarget(const newTarget: Vec3);
		function _GetTZRot: float;
		procedure _SetTZRot(newZRot: float);
		function _GetCinematic: boolean;
		procedure _SetCinematic(newCinematic: boolean);
		procedure _SetPostprocess(newPP: pPostprocess);
		procedure _UpdateProjection(force: boolean);
	public
		gl: GLEntityParams;
		viewTransform: Transform;
		forceAspect: float;
		constructor Init(newFlags: CameraFlags; newRT: pRenderTarget = nil);
		destructor Done; virtual;
		procedure Update(const dt: float);
		procedure UpdateStatic;
		function DirectTransform: Transform;
		function DirectRotation: Quaternion;
		function UnProject(const v: Vec3): Vec3;
		procedure AdjustZToAABB(const bb: AABB);
		function GetCSMSplitZ(id, total: sint; lambda: float): float;
		function Main: boolean;
		function BeginDraw: pRenderTarget;
		function EndDraw: pRenderTarget;
		procedure DenyProjectionUpdateOnFly;
		procedure ChangeRT(newRT: pRenderTarget);

		property ProjectionMode: CameraProjectionMode read _projMode write _SetProjMode;
		property ZNear: float read _zNear write _SetZNear;
		property ZFar: float read _zFar write _SetZFar;
		property FOV: float read _fov write _SetFOV;
		property OrthoRect: Rect read _orthoRect write _SetOrthoRect;
		property ClampToScene: boolean read _GetClampToScene write _SetClampToScene;
		function NSplits: sint;
		function Split(id: sint): float;
		function ProjectionMatrix: Matrix4;
		function InversedProjectionMatrix: Matrix4;
		function ViewDirection: Vec3;

		property OrieMode: CameraOrieMode read _mode;
		property Pos: Vec3 read _pos write _pos;
		property TPos: Vec3 read _GetTPos write _SetTPos;
		property QRot: Quaternion read _qrot write _SetQRot;
		property Target: Vec3 read _target write _target;
		property TTarget: Vec3 read _GetTTarget write _SetTTarget;
		property TTargetCastShift: Vec3 read _tTargetCastShift write _tTargetCastShift;
		property ZRot: float read _zRot write _zRot;
		property TZRot: float read _GetTZRot write _SetTZRot;

		property Cinematic: boolean read _GetCinematic write _SetCinematic;
		property AccK: float read _accK write _accK;

		property RT: pRenderTarget read _rt;
		property Postprocess: pPostprocess read _postprocess write _SetPostprocess;
		property Owner: pSceneNode read _owner write _owner;
		property Flags: CameraFlags read _flags;
	private
		procedure UpdateCinematic(const dt: float);
		procedure TrustedUpdateStatic;
	end;

implementation

uses
	MMSystem, Scene
{$ifdef use_serialization}, Serialization {$endif}
{$ifdef Profile}, Profile {$endif};

	procedure Camera._Initialize(dese: boolean);
	begin
		_flags += [_camera_First, _camera_ProjDirty];
		viewTransform := Transform.Identity;
		if not Assigned(_rt) then _rt := @MainRT;
		_zFarLimit := GLBase.Config.maxZFar;

		if camera_Main in _flags then
			_splits := GetMem(MAX_CSM_SPLITS * sizeof(float))
		else
			_splits := nil;
		if dese then _UpdateProjection(yes);
	end;

	constructor Camera.Init(newFlags: CameraFlags; newRT: pRenderTarget = nil);
	begin
		inherited Init;
		_flags := newFlags + [camera_ClampToScene];
		_rt := newRT;
		_owner := nil;
		_Initialize(no);
		forceAspect := 0.0;

		_pos := Vec3.Zero;
		gl.Init;
		_projMode := proj_Perspective;
		_zNear := GLBase.Config.zNear;
		_zFar := _zFarLimit;
		_fov := 50.0;
		_orthoRect := Rect.OrthoIdentity;

		_mode := orie_Quat;
		_qrot := Quaternion.Identity;
		_target := Vec3.Zero;
		_zrot := 0.0;
		_accK := 0.93;
		_postprocess := nil;
		_tTargetCastShift := Vec3.Zero;
	end;

	destructor Camera.Done;
	begin
		FreeMem(_splits);
		Release(_postprocess);
		gl.Done;
		inherited Done;
	end;

	procedure Camera._SetProjMode(newMode: CameraProjectionMode);
	begin
		if _projMode <> newMode then
		begin
			_projMode := newMode;
			Include(_flags, _camera_ProjDirty);
		end;
	end;

	procedure Camera._SetZNear(const newZNear: float);
	begin
		if not Equals(_zNear, newZNear) then
		begin
			_zNear := newZNear;
			Include(_flags, _camera_ProjDirty);
		end;
	end;

	procedure Camera._SetZFar(const newZFar: float);
	begin
		if not Equals(_zFar, newZFar) then
		begin
			_zFar := newZFar;
			Include(_flags, _camera_ProjDirty);
		end;
	end;

	procedure Camera._SetFOV(const newFov: float);
	begin
		if not Equals(_fov, newFov) then
		begin
			_fov := newFov;
			Include(_flags, _camera_ProjDirty);
		end;
	end;

	procedure Camera._SetOrthoRect(const newRect: Rect);
	begin
		if not Equals(_orthoRect, newRect) then
		begin
			_orthoRect := newRect;
			Include(_flags, _camera_ProjDirty);
		end;
	end;

	function Camera._GetClampToScene: boolean;
	begin
		result := camera_ClampToScene in _flags;
	end;

	procedure Camera._SetClampToScene(newClamp: boolean);
	begin
		if newClamp then Include(_flags, camera_ClampToScene) else Exclude(_flags, camera_ClampToScene);
	end;

	function Camera.NSplits: sint;
	begin
		if Assigned(_splits) then
			result := GLBase.Config.nCsmSplits
		else
			result := 0;
	end;

	function Camera.Split(id: sint): float;
	begin
		Assert((id >= 0) and (id < NSplits));
		_UpdateProjection(no);
		result := _splits[id];
	end;

	function Camera.ProjectionMatrix: Matrix4;
	begin
		_UpdateProjection(no);
		result := _projectionMatrix;
	end;

	function Camera.InversedProjectionMatrix: Matrix4;
	begin
		_UpdateProjection(no);
		result := _invProjectionMatrix;
	end;

	function Camera.ViewDirection: Vec3;
	begin
		result := DirectRotation * Vec3.NegativeZ;
	end;

	function Camera._GetTPos: Vec3;
	begin
		if camera_Cinematic in _flags then
			result := _tPos
		else
			result := _pos;
	end;

	procedure Camera._SetTPos(const newPos: Vec3);
	begin
		if camera_Cinematic in _flags then
			_tPos := newPos
		else
			_pos := newPos;
	end;

	procedure Camera._SetQRot(const newQrot: Quaternion);
	begin
		_mode := orie_Quat;
		_qrot := newQRot;
	end;

	function Camera._GetTTarget: Vec3;
	begin
		if camera_Cinematic in _flags then
			result := _tTarget
		else
			result := _target;
	end;

	procedure Camera._SetTTarget(const newTarget: Vec3);
	begin
		_mode := orie_Target;
		if camera_Cinematic in _flags then
			_tTarget := newTarget
		else
			_target := newTarget;
	end;

	function Camera._GetTZRot: float;
	begin
		if camera_Cinematic in _flags then
			result := _tZRot
		else
			result := _zrot;
	end;

	procedure Camera._SetTZRot(newZRot: float);
	begin
		_mode := orie_Target;
		if camera_Cinematic in _flags then
			_tZRot := newZRot
		else
			_zrot := newZRot;
	end;

	function Camera._GetCinematic: boolean;
	begin
		result := camera_Cinematic in _flags;
	end;

	procedure Camera._SetCinematic(newCinematic: boolean);
	begin
		if (camera_Cinematic in _flags) = newCinematic then exit;
		if newCinematic then Include(_flags, camera_Cinematic) else Exclude(_flags, camera_Cinematic);
	end;

	procedure Camera._SetPostprocess(newPP: pPostprocess);
	begin
		SetRef(_postprocess, newPP);
	end;

	procedure Camera.Update(const dt: float);
	begin
		if camera_Cinematic in _flags then UpdateCinematic(dt);
		TrustedUpdateStatic;
	end;

	procedure Camera.UpdateStatic;
	begin
		Assert(not (camera_Cinematic in _flags), 'use Camera.Update(dt)');
		TrustedUpdateStatic;
	end;

	procedure Camera.UpdateCinematic(const dt: float);
		procedure ApplyMagic(var apos, atarget: Vec3);
		var
			v, vel: Vec3;
		begin
			v := atarget - apos;
			vel := _accK * v * dt;
			v := apos + vel;
			if (atarget - v) ** (atarget - apos) < 0.0 then
				apos := atarget
			else
				apos := v;
		end;
		procedure ApplyMagic(var apos, atarget: float);
		var
			v, vel: float;
		begin
			v := atarget - apos;
			vel := _accK * v * dt;
			v := apos + vel;
			if IntSign(atarget - v) <> IntSign(atarget - apos) then
				apos := atarget
			else
				apos := v;
		end;
	var
		tpos2: Vec3;
		x: float;
	begin
		Assert(camera_Cinematic in _flags);
		if Assigned(owner) then
		begin
			Assert(Assigned(owner^.Root));
			pScene(owner^.Root)^.RayCast(cast_ForCamera, _tTarget + _tTargetCastShift, _tPos, @x);
			tpos2 := lerp(_tTarget, _tPos, x);
		end else
			tpos2 := _tPos;
		if (_camera_First in _flags) then
		begin
			Exclude(_flags, _camera_First);
			_pos := tpos2;
			_target := _tTarget;
		end;
		ApplyMagic(_target, _tTarget);
		ApplyMagic(_pos, tpos2);
		_zrot := NormalizeAngle(_zrot);
		_tZRot := NormalizeAngle(_tZRot);
		if abs(_zrot - _tZRot) > Pi then _tZRot := _tZRot - FloatSign(_tZRot - _zRot) * TwoPi;
		ApplyMagic(_zrot, _tZRot);
		_zrot := NormalizeAngle(_zrot);
		_tZRot := NormalizeAngle(_tZRot);
	end;

	procedure Camera.TrustedUpdateStatic;
	var
		qr: Quaternion;
	begin
		case _mode of
			orie_Quat: qr := _qrot.Inversed;
			orie_Target: qr := Quaternion.Rotation(_zrot, Vec3.PositiveZ) * Quaternion.RotationThroughX0Z((_target - _pos).Normalized, Vec3.NegativeZ);
		end;
		viewTransform := Rotate(qr) * Translate(-_pos);
		if _mode <> orie_Quat then _qrot := qr.Inversed;
		_UpdateProjection(camera_DontUpdateProjectionOnFly in _flags);
	end;

	function Camera.DirectTransform: Transform;
	begin
		result := viewTransform.Inversed;
	end;

	function Camera.DirectRotation: Quaternion;
	begin
		result := viewTransform.rot.Inversed;
	end;

	procedure Camera._UpdateProjection(force: boolean);
	var
		aspect: float;
		i, n: sint;
	begin
		if
			(
				(camera_DontUpdateProjectionOnFly in _flags) or
				((not (camera_DontUpdateProjectionOnFly in _flags)) and not (_camera_ProjDirty in _flags))
			) and not force
		then
			exit;

		case _projMode of
			proj_Perspective:
				begin
					if forceAspect <> 0.0 then aspect := forceAspect else
						if _rt^.size.Y <> 0 then aspect := _rt^.size.Aspect else aspect := 1.0;
					if _fov < 0 then aspect := -aspect;
					_projectionMatrix := Matrix4.PerspectiveProjection(_fov, aspect, _zNear, _zFar);
				end;
			proj_Ortho:
				_projectionMatrix := Matrix4.OrthographicProjection(_orthoRect.A.x, _orthoRect.B.x, _orthoRect.A.y, _orthoRect.B.y, _zFar, _zNear);
			else Assert(no);
		end;
		_invProjectionMatrix := _projectionMatrix.Inversed;
		if Assigned(_splits) then
		begin
			n := NSplits;
			for i := 0 to n - 1 do
				_splits[i] := GetCSMSplitZ(i, n, GLBase.Config.csmLogK);
		end;
		Exclude(_flags, _camera_ProjDirty);
	end;

	function Camera.UnProject(const v: Vec3): Vec3;
	const
		Scale: Vec3 = (data: (2, -2, 2));
		Shift: Vec3 = (data: (-1, +1, -1));
	begin
		result := DirectTransform * (InversedProjectionMatrix * Vec4.Make(Scale * v + Shift, 1.0)).Homo3;
	end;

	procedure Camera.AdjustZToAABB(const bb: AABB);
	var
		d: float;
	begin
		d := Distance(_pos, bb.SupportVertex(ViewDirection));
		ZFar := clamp(d, ZNear * 10.0, _zFarLimit);
	end;

	// Эмпирические формулы эмпиричны.
	function Camera.GetCSMSplitZ(id, total: sint; lambda: float): float;
	var
		zUni, zLog, wz: float;
	begin
		if total < 2 then exit(ZFar);
		lambda := pow(lambda, 0.7 + 0.7*id/total);
		wz := ZFar - ZNear;
		zUni := ZNear + wz * ((id+1) / total);
		zLog := ZNear + wz * pow(6.0 - 4.0 * id/total, -(total - id - 1));
		result := zLog * lambda + zUni * (1.0 - lambda);
	end;

	function Camera.Main: boolean;
	begin
		result := camera_Main in _flags;
	end;

	function Camera.BeginDraw: pRenderTarget;
	begin
	trace_call('Camera.BeginDraw');
		if Assigned(_postprocess) then
			result := _postprocess^.PreparePass(_rt^.Size)
		else
			result := _rt;
	leave_call
	end;

	function Camera.EndDraw: pRenderTarget;
	begin
	trace_call('Camera.EndDraw');
		if Assigned(_postprocess) then
			_postprocess^.Draw(_rt^.inGL, @gl);
		result := _rt;
	leave_call
	end;

	procedure Camera.DenyProjectionUpdateOnFly;
	begin
		Assert(not (camera_DontUpdateProjectionOnFly in _flags));
		Include(_flags, camera_DontUpdateProjectionOnFly);
	end;

	procedure Camera.ChangeRT(newRT: pRenderTarget);
	begin
		_rt := newRT;
	end;

{$ifdef use_serialization}
const
	CAMERA_CINEMATIC_BIT       = 1 shl 0;
	CAMERA_MAIN_BIT            = 1 shl 1;
	CAMERA_DONT_UPDATE_PROJECTION_ON_FLY_BIT = 1 shl 2;
	CAMERA_CLAMP_TO_SCENE_BIT  = 1 shl 3;
	CAMERA_HAS_POSTPROCESS_BIT = 1 shl 4;
	CAMERA_HAS_OWNER_BIT       = 1 shl 5;

	procedure SerializeCamera(se: pSerializer; obj: pointer);
	var
		cam: pCamera absolute obj;
		flags: uint;
	begin
		Assert(cam^._rt = @MainRT, SizeToString(cam^._rt^.size));
		with se^ do
		begin
			flags := 0;
			if camera_Cinematic                 in cam^.flags then flags := flags or CAMERA_CINEMATIC_BIT;
			if camera_Main                      in cam^.flags then flags := flags or CAMERA_MAIN_BIT;
			if camera_DontUpdateProjectionOnFly in cam^.flags then flags := flags or CAMERA_DONT_UPDATE_PROJECTION_ON_FLY_BIT;
			if camera_ClampToScene              in cam^.flags then flags := flags or CAMERA_CLAMP_TO_SCENE_BIT;
			if Assigned(cam^.Postprocess) then flags := flags or CAMERA_HAS_POSTPROCESS_BIT;
			if Assigned(cam^.Owner)       then flags := flags or CAMERA_HAS_OWNER_BIT;
			Serialize_ui8(stream, flags);

			if Assigned(cam^.postprocess) then SeObject(cam^.postprocess);
			if (flags and CAMERA_HAS_OWNER_BIT) <> 0 then SeObject(cam^.owner);
			Serialize_ui8(stream, ord(cam^.ProjectionMode));
			Serialize_f32(stream, cam^.ZNear);
			Serialize_f32(stream, cam^.ZFar);
			case cam^.ProjectionMode of
				proj_Perspective:
					begin
						Serialize_f16(stream, cam^.FOV);
					end;
				proj_Ortho:
					begin
						Serialize_vec2f32(stream, cam^._orthoRect.A);
						Serialize_vec2f32(stream, cam^._orthoRect.B);
					end;
			end;

			Serialize_ui8(stream, ord(cam^.OrieMode));
			case cam^.OrieMode of
				orie_Quat:
					begin
						Serialize_vec3f32(stream, cam^.pos);
						Serialize_vec3f32(stream, cam^.target);
						Serialize_IQuat8(stream, cam^.QRot);
					end;
				orie_Target:
					begin
						Serialize_vec3f32(stream, cam^.TPos);
						Serialize_vec3f32(stream, cam^.TTarget);
						Serialize_f32(stream, cam^.TZRot);
						Serialize_f32(stream, cam^.AccK);
						if camera_Cinematic in cam^.flags then Serialize_vec3f16(stream, cam^._tTargetCastShift);
					end;
			end;
		end;
	end;

	procedure DeserializeCamera(de: pDeserializer; obj: pointer);
	var
		cam: pCamera absolute obj;
		flags: uint;
	begin
		with de^ do
		begin
			flags := Deserialize_ui8(stream);
			if (flags and CAMERA_CINEMATIC_BIT) <> 0 then Include(cam^._flags, camera_Cinematic);
			if (flags and CAMERA_MAIN_BIT) <> 0 then Include(cam^._flags, camera_Main);
			if (flags and CAMERA_DONT_UPDATE_PROJECTION_ON_FLY_BIT) <> 0 then Include(cam^._flags, camera_DontUpdateProjectionOnFly);
			if (flags and CAMERA_CLAMP_TO_SCENE_BIT) <> 0 then Include(cam^._flags, camera_ClampToScene);

			if (flags and CAMERA_HAS_POSTPROCESS_BIT) <> 0 then DeObjectR(cam^._postprocess);
			if (flags and CAMERA_HAS_OWNER_BIT) <> 0 then DeWeakR(cam^._owner);
			cam^._projMode := CameraProjectionMode(Deserialize_ui8(stream));
			cam^._zNear := Deserialize_f32(stream);
			cam^._zFar := Deserialize_f32(stream);
			case cam^.ProjectionMode of
				proj_Perspective:
					begin
						cam^._fov := Deserialize_f16(stream);
					end;
				proj_Ortho:
					begin
						cam^._orthoRect.A := Deserialize_vec2f32(stream);
						cam^._orthoRect.B := Deserialize_vec2f32(stream);
					end;
			end;

			cam^._mode := CameraOrieMode(Deserialize_ui8(stream));
			case cam^.OrieMode of
				orie_Quat:
					begin
						cam^._pos    := Deserialize_vec3f32(stream);
						cam^._target := Deserialize_vec3f32(stream);
						cam^._qrot   := Deserialize_IQuat8(stream);
					end;
				orie_Target:
					begin
						cam^._tPos    := Deserialize_vec3f32(stream); cam^._pos    := cam^._tPos;
						cam^._tTarget := Deserialize_vec3f32(stream); cam^._target := cam^._tTarget;
						cam^._tZRot   := Deserialize_f32(stream);     cam^._zRot   := cam^._tZRot;
						cam^._accK    := Deserialize_f32(stream);
						if camera_Cinematic in cam^._flags then cam^._tTargetCastShift := Deserialize_vec3f16(stream) else cam^._tTargetCastShift := Vec3.Zero;
					end;
			end;
			cam^._Initialize(yes);
		end;
	end;

	procedure CameraDeSpecial(de: pDeserializer; what: DeSpecial; var obj: pointer);
	var
		cam: pCamera absolute obj;
	begin
		Assert(@de = @de);
		case what of
			de_Initialize: cam^.DeseInit;
		end;
	end;
{$endif}

	procedure Init;
	begin
	{$ifdef use_serialization}
		SerializationDB.Shared
		^.RegisterType('Camera', TypeOf(Camera), nil, sizeof(Camera), yes,
		               @SerializeCamera, @DeserializeCamera, nil, @CameraDeSpecial);
	{$endif}
		ShaderDefines^.AddFlag('UNDERWATER_CAMERA', GL_CAMERA_FLAG_UNDERWATER);
	end;

initialization
	&Unit('Camera').Initialize(@Init);
end.
