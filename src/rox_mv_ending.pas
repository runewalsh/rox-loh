{$include opts.inc}
unit rox_mv_ending;

interface

uses
	USystem, Errors, UMath, Utils, GLUtils,
	rox_state_adventure, rox_world, rox_location, rox_decoration, rox_paths, rox_actor, rox_ep_bar, rox_gfx;

type
	pMv_Ending = ^Mv_Ending;
	Mv_Ending = object(Adventure)
		land: pDecoration;
		state: (Setup, Idle);
		fadeMode: (NoFade, FadeIn, FadeOut);
		fade: float;
		valera, twinkle, kazah: pActor;
		sunset, space: pTexture;
		sunsetTime: float;
		constructor Init(world: pWorld);
		destructor Done; virtual;
		procedure HandleUpdate(const dt: float); virtual;
		procedure HandleDraw; virtual;
	private
		function PutOnLand(actor: pActor; const x, y: float): pActor;
	const
		StateID = 'mv_ending';
	end;

implementation

	constructor Mv_Ending.Init(world: pWorld);
	begin
		inherited Init(StateID, world);
		sunset := Texture.Load(Environment('sunset.png'));
		space := Texture.Load(Environment('space.crn'));
		playerControlMode := PlayerControlDisabled;
		cameraMode := LookPredefined;
		fadeMode := FadeIn;
	end;

	destructor Mv_Ending.Done;
	begin
		Release(valera); Release(twinkle); Release(kazah);
		Release(sunset);
		inherited Done;
	end;

	procedure Mv_Ending.HandleUpdate(const dt: float);
	begin
		inherited HandleUpdate(dt);
		sunsetTime += dt;

		case state of
			Setup:
				begin
					pNode(land) :=
						(new(pDecoration, Init(Environment('ed-land.png'), Transform2.Identity, Vec2.Make(1.0, Deduce))))^.SetLayer(-2)^.AddTo(location);
					land^.local := Translate(-0.5 * land^.size.x, -mgr^.nvp.y);

					kazah := PutOnLand(CreateKolobok('kazah'), 0.2, 0.14);
					twinkle := PutOnLand(CreateKolobok('twinkle'), 0.4, 0.2);
					PutOnLand(player, 0.6, 0.15);
					valera := PutOnLand(CreateKolobok('valera'), 0.8, 0.16);

					state := Idle;
				end;
		end;

		case fadeMode of
			NoFade: ;
			FadeIn: begin fade := min(fade + dt, 1); if fade = 1 then fadeMode := NoFade; end;
			FadeOut: begin fade := min(fade + dt, 1); if fade = 1 then fadeMode := NoFade; end;
			else raise ExhaustiveCase(ord(fadeMode), 'FadeMode');
		end;
	end;

	procedure Mv_Ending.HandleDraw;
	const
		Start = 1.0;
		SunsetEnd = 6.5;
		NightSkyAppearanceStart = 4.0;
		NightSkyAppearanceEnd = 10.0;
		NightSkyShift = 0.4;
		ExtraNightShift = 0.01;
	var
		q: Quad;
		sign, k: float;
		isign: sint;
		size: Vec2;
		i: uint;
	begin
		if sunsetTime < NightSkyAppearanceEnd then
		begin
			k := smoothstep(Start, SunsetEnd, sunsetTime);
			q.fields := [q.Field.Transform, q.Field.ColorAB];
			q.transform := Translate(0, (2 * mgr^.nvp.y - sunset^.ap.Aspect2Item(asp2_x1, 1, land^.size.x)) * k);
			q.colorA := lerp(Vec4.Make(1, 1, 1, 1), Vec4.Make(0.1, 0.2, 0.3, 1), k);
			q.colorB := Vec4.Make(1, 1, 1, 1);
			q.Draw(sunset, Vec2.Make(-0.5 * land^.size.x, -mgr^.nvp.y), sunset^.ap.Aspect2(asp2_x1, land^.size.x), Vec2.Zero, Vec2.Ones);
		end;

		if sunsetTime >= NightSkyAppearanceStart then
		begin
			q.fields := [q.Field.Transform, q.Field.Color];
			q.transform := Translate(0, -NightSkyShift * smoothstep(Start, NightSkyAppearanceEnd, sunsetTime)) *
				Translate(0.5 * land^.size.x, -mgr^.nvp.y) * Rotate(Pi/2);
			q.color := Vec4.Make(1, 1, 1, smoothstep(NightSkyAppearanceStart, NightSkyAppearanceEnd, sunsetTime));

			size := Vec2.Make(2 * mgr^.nvp.y + NightSkyShift + 2*ExtraNightShift, land^.size.x + 2*ExtraNightShift);
			for i := 0 to 1 do
			begin
				q.Draw(space, Vec2.Make(-ExtraNightShift), size, Vec2.Zero, AspectPair.Make(size).ReverseCombined(space^.ap).Aspect2(asp2_y1, 1));
				if i = 0 then
				begin
					k := 0.5 + 0.5 * sin(sunsetTime);
					q.transform := Translate((1 - k) * ExtraNightShift * Vec2.Make(cos(0.88*sunsetTime), sin(0.97*sunsetTime))) * q.transform;
					q.color.data[3] *= k;
				end;
			end;
		end;

		inherited HandleDraw;
		// градиентные рамки, т. к. локация не на весь экран
		q.fields := [q.Field.Transform, q.Field.ColorAB];
		k := smoothstep(NightSkyAppearanceStart, NightSkyAppearanceEnd, sunsetTime);
		q.colorA := Vec4.Make(0.1 - 0.04 * k, 0.06 + 0.04 * k, 0.015 + 0.1 * k, 0);
		q.colorB := q.colorA; q.colorB.w := 1;
		for isign := 0 to 1 do
		begin
			sign := IfThen(isign = 0, -1, +1);
			q.transform := Translate(sign * 0.5 * land^.size.x, mgr^.nvp.y) * Rotate(-Pi/2);
			q.Draw(nil, Vec2.Zero, Vec2.Make(2 * mgr^.nvp.y, sign * -0.05 * land^.size.x), Vec2.Zero, Vec2.Zero);
			q.Draw(nil, Vec2.Zero, Vec2.Make(2 * mgr^.nvp.y, sign * (mgr^.nvp.x - 0.5 * land^.size.x)), Vec2.Zero, Vec2.Zero);
		end;

		case fadeMode of
			FadeIn, FadeOut:
				begin
					q.fields := [q.Field.Color];
					case fadeMode of
						FadeIn: q.color := Vec4.Make(0, 0, 0, 1 - fade);
						else {FadeOut} q.color := Vec4.Make(0, 0, 0, fade);
					end;
					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
				end;
		end;
	end;

	function Mv_Ending.PutOnLand(actor: pActor; const x, y: float): pActor;
	begin
		result := actor;
		actor^.angle := Pi/2;
		actor^.local := Translate(land^.PointOn(Vec2.Make(x, y)) - Vec2.Make(0.5 * actor^.size.x, 0));
		location^.Add(actor);
	end;

end.

