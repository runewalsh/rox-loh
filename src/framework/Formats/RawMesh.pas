unit RawMesh;

{$include opts.inc}

interface

uses
	USystem, Streams, UMath, U_GL, GLBase, SpatialIndex, UClasses;

	procedure Load(var mesh: Mesh; stream: pStream);

implementation

type
	Internals = Mesh.RawMesh;

	procedure Load(var mesh: Mesh; stream: pStream);
	var
		flags: uint;
		i, j, lv: sint;
		batchesCount, vatCount, nLods: sint;
		batch: pBatch;
		vaName: string;
		vaType: GLType;
		aabb: UMath.AABB;
		sphere: UMath.Sphere;
	begin
		Deserialize_signature(stream, Internals.Signature, no);
		flags := Deserialize_ui8(stream);
		batchesCount := VarInt.Read(stream);
		for i := 0 to batchesCount - 1 do
		begin
			batch := mesh.AddBatch(Deserialize_string(stream));
			batch^.VerticesCount := VarInt.Read(stream);
			vatCount := VarInt.Read(stream);
			for j := 0 to vatCount - 1 do
			begin
				vaName := Deserialize_string(stream);
				vaType := GLType(Deserialize_ui8(stream));
				batch^.AddVA(vaName, vaType);
				stream^.Read(batch^.FindVA(vaName)^.Ptr, GLTypeInfo[vaType].sizeof * size_t(batch^.VerticesCount));
			end;

			batch^.inds.Deserialize(stream);
		end;

		if flags and Internals.HAS_LODS <> 0 then
		begin
			nLods := VarInt.Read(stream);
			for i := 0 to nLods - 1 do
			begin
				if i = 0 then
				begin
					lv := 0;
					mesh.levels[lv].minLod := Deserialize_f16(stream);
				end else
					lv := mesh.AddLevel(Deserialize_f16(stream));
				for j := 0 to High(mesh.batches) do
					mesh.batches[j].IndicesCount[lv] := VarInt.Read(stream);
			end;
		end;

		if flags and Internals.HAS_BND <> 0 then
		begin
			aabb.A := Deserialize_vec3f32(stream);
			aabb.B := Deserialize_vec3f32(stream);
			sphere.center := Deserialize_vec3f32(stream);
			sphere.radius := Deserialize_f32(stream);
			mesh.Bounding := Bounding.BySphereAndAABB(sphere, aabb);
		end;
	end;

	procedure SuiteLoad(obj: pointer; s: pStream); begin Load(pMesh(obj)^, s); end;
	procedure Init;
	begin
		Mesh.Loaders.Register('mesh', @SuiteLoad);
	end;

initialization
	&Unit('RawMesh').Initialize(@Init);
end.
