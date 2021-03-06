{$include opts.inc}
unit rox_mv_flight;

interface

uses
	USystem, UMath, Audio, GLUtils,
	rox_state, rox_world, rox_gl, rox_gfx, rox_paths;

type
	pMv_Flight = ^Mv_Flight;
	Mv_Flight = object(State)
		world: pWorld;
		snd: pSound;
		time, marsTime: float;
		camera: Transform2;
		mars, mars_closer, space, radial, ship, shipFire: pTexture;
		draw: boolean;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
	private
		procedure DrawFlightScene;
	const
		StateID = 'mv_flight';
		CloserTime = 7.5;
		SongLength = 15;
	end;

implementation

uses
	rox_ep_mars;

	constructor Mv_Flight.Init(world: pWorld);
	begin
		inherited Init(StateID);
		self.world := MakeRef(world);
		space := Texture.Load(Environment('space.crn'));
		mars := Texture.Load(Environment('mars.png'));
		mars_closer := Texture.Load(Environment('mars_closer[c].crn'));
		snd := new(pSound, Init(Music('mars.mp3'), [{Sound.StartPaused}]));
		radial := Texture.Load(Fx('radial [z-tile].dds'));
		ship := Texture.Load(Environment('ship-space.png'));
		shipFire := Texture.Load(Environment('ship-space-fire.png'));
	end;

	destructor Mv_Flight.Done;
	begin
		if Assigned(snd) then snd^.Stop;
		Release(shipFire); Release(ship);
		Release(radial);
		Release(snd);
		Release(mars); Release(mars_closer);
		Release(space);
		Release(world);
		inherited Done;
	end;

	procedure Mv_Flight.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		time += dt;
		marsTime += dt;
		camera :=
			Translate(lerp(Vec2.Make(0, -3*mgr^.nvp.y), Vec2.Zero, sin(HalfPi * clamp(time/3, 0, 1)))) *
			Rotate(-1.2 * smoothstep(SongLength - 0.5, SongLength, time)) *
			Scale2(1 - 0.99 * smoothstep(SongLength - 0.5, SongLength, time));
		if time > SongLength + 0.5 then mgr^.Switch(new(pEp_Mars, Init(world, no)));
	end;

	procedure Mv_Flight.HandleDraw;
	var
		q: Quad;
		x: float;
	begin
		inherited HandleDraw;
		if time < SongLength then DrawFlightScene;

		if time > SongLength - 0.5 then
		begin
			q.fields := [q.Field.Color];
			x := smoothstep(SongLength - 0.5, SongLength, time);
			q.color := Vec4.Make(1, x, x, x * smoothstep(SongLength + 0.5, SongLength, time));
			q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
		end;
	end;

	procedure Mv_Flight.DrawFlightScene;
	const
		MarsSize = 0.8;
	var
		q: Quad;
		spaceDelta, marsDelta, size: Vec2;
		spaceAlpha: float;
	begin
		// gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
		spaceAlpha := smoothstep(0, 1, time);
		q.fields := [q.Field.Transform, q.Field.Color];
		q.transform := -camera;
		q.color := Vec4.Make(1, 1, 1, spaceAlpha);
		spaceDelta := Vec2.Make(0.2*sin(0.1*marsTime), -0.2*cos(0.08*marsTime));
		q.Draw(space, space^.ap.Aspect2(asp2_x1, -mgr^.nvp.x), space^.ap.Aspect2(asp2_x1, 2*mgr^.nvp.x), spaceDelta,
			Vec2.Make(1.5, 1));

		q.fields := [q.Field.Transform, q.Field.ColorAB];
		q.transform := -camera;
		q.colorA := Vec4.Make(1, 1, 1, spaceAlpha);
		q.colorB := Vec4.Make(1, 1, 1, 0);
		q.Draw(space, space^.ap.Aspect2(asp2_x1, -mgr^.nvp.x) - Vec2.Make(0, space^.ap.Aspect2Item(asp2_x1, 1, 2*mgr^.nvp.x)),
			space^.ap.Aspect2(asp2_x1, 2*mgr^.nvp.x), spaceDelta, Vec2.Make(1.5, 1));
		// gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

		marsDelta := Vec2.Make(0.1*sin(marsTime) - 1.0 * smoothstep(CloserTime, 0, time), 0.08*cos(0.8*marsTime));
		marsDelta := lerp(marsDelta, Vec2.Zero, smoothstep(CloserTime - 0.5, CloserTime + 2.5, time));
		q.transform := -camera * Rotate(0.2*sin(0.3*marsTime)) * Translate(Vec2.Make(-1.3, 0) + marsDelta);
		q.transform := Mix(q.transform, -camera * Scale2(10) * Translate(-0.36, -0.73), smoothstep(CloserTime, CloserTime + 3.5, time));
		if time < CloserTime + 2.2 + (1/0.7) + 1.5 then
		begin
			q.fields := [q.Field.Transform, q.Field.Color];
			q.color := Vec4.Make(1, 1, 1, clamp((1/1.5) * ((CloserTime + 2.2 + (1/0.7) + 1.5) - time), 0, 1));
			q.Draw(mars, Vec2.Zero, Vec2.Make(MarsSize), Vec2.Zero, Vec2.Ones);
		end;

		if time > CloserTime + 2.2 then
		begin
			q.transform *= Translate(MarsSize*50/243, MarsSize*(1-56/243));
			q.fields := [q.Field.Transform, q.Field.Color];
			q.color := Vec4.Make(1, 1, 1, min(1, 0.7*(time - (CloserTime + 2.2))));
			q.Draw(mars_closer, Vec2.Zero, Vec2.Make(0.13/(243/918)*MarsSize, 0.13/(243/918)*MarsSize*(451/918)), Vec2.Zero, Vec2.Ones);
		end;

		if (time > CloserTime) and (time < SongLength) then
		begin
			q.fields := [q.Field.Color, q.Field.TexZ];
			q.texZ := time;
			q.color := Vec4.Make(1, 1, 1, min(1, 0.6 * (time - CloserTime)));
			gl.BlendFunc(gl.SRC_ALPHA, gl.ONE);
			q.Draw(radial, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
			gl.BlendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);
		end;

		q.fields := [q.Field.Transform];
		size := ship^.ap.Aspect2(asp2_x1, 0.9);
		q.transform := -camera * Translate(Vec2.Make(0.4, -0.6) +
			Vec2.Make(0.2*sin(0.38*marsTime), -0.2*cos(0.29*marsTime)) * smoothstep(0, 3, time) +
			Vec2.Make(-0.4 * smoothstep(CloserTime, CloserTime-3, time) + (mgr^.nvp.x-size.x) * smoothstep(4, 1, time), mgr^.nvp.y * smoothstep(5, 1, time)));
		q.Draw(ship, Vec2.Zero, size, Vec2.Zero, Vec2.Ones);
		q.transform *= Translate(Vec2.Make(-16/261) * size);
		q.Draw(shipFire, Vec2.Zero, shipFire^.ap.Fixed(1/3).Aspect2(asp2_x1, size.x * (299/261)), Vec2.Make(1/3 * floor(modf(10 * marsTime, 3)), 0), Vec2.Make(1/3, 1));
	end;

end.

