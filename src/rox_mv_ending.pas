{$include opts.inc}
unit rox_mv_ending;

interface

uses
	USystem, UMath, Utils, UClasses, GLUtils,
	rox_state_adventure, rox_world, rox_location, rox_decoration, rox_paths, rox_actor, rox_ep_bar, rox_gfx, rox_timer, rox_dialogue,
	rox_state_mainmenu;

type
	pMv_Ending = ^Mv_Ending;
	Mv_Ending = object(Adventure)
		land: pDecoration;
		state: (Setup, Idle);
		fadeMode: (NoFade, FadeIn, FadeOut, FadeOutOut, RedFlash);
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
		RotatingVelocity = 4.0;
		MovingVelocity = 0.6 * WalkingVelocity;
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
		Release(sunset); Release(space);
		inherited Done;
	end;

	procedure Ed_19_Fadeout(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.fadeMode := FadeOut;
		e^.fade := 0;
	end;

	procedure Ed_18_Rox(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.dlg.Init(e,
			'twinkle [face = scared.png]: 14.png >>' +
			'rox: 14.png >>' +
			'rox: 15.png >>' +
			'twinkle [face = scared.png, letterTimeout = 0.4, delay = 2]: 15.png')^.Callbacks(nil, @Ed_19_Fadeout, e);
	end;

	procedure Ed_17_RoxMovesOut(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.player^.MoveTo(Vec2.Make(0, -e^.mgr^.nvp.y - e^.player^.size.y - 0.3), e^.MovingVelocity, @Ed_18_Rox, e);
	end;

	procedure Ed_16_CaveDialogueItem(id: uint; what: Dialogue.ItemEvent; param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		case what of
			ItemNewline:
				case id of
					2:
						begin
							e^.fadeMode := RedFlash;
							e^.fade := 0;
							if Assigned(e^.dlg.active) then e^.dlg.active^.skip := no;
							if e^.dlg.Valid then e^.dlg.ForceFace('twinkle', 'scared.png');
						end;
				end;
		end;
	end;

	procedure Ed_16_CaveDialogue(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.player^.RotateTo(e^.player^.HeartPos + Vec2.NegativeY);
		e^.dlg.Init(e,
			'twinkle [face = x-eyes.png, delay = 2]: 12.png >>' +
			'kazah [face = suspicious.png, delay = 2]: 11.png >>' +
			'twinkle [face = x-eyes.png, newlineDelay = 2]: 13.png >>' +
			'valera [face = gloomy.png]: 17.png')^.Callbacks(@Ed_16_CaveDialogueItem, @Ed_17_RoxMovesOut, e);
	end;

	procedure Ed_15_KazahMovesOut(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.kazah^.MoveTo(Vec2.Make(0, -e^.mgr^.nvp.y - e^.kazah^.size.y - 0.3), e^.MovingVelocity, nil, nil);
		e^.AddTimer(6.0, nil, @Ed_16_CaveDialogue, e);
	end;

	procedure Ed_14_TwinkleMovesOut(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.twinkle^.MoveTo(Vec2.Make(0, -e^.mgr^.nvp.y - e^.twinkle^.size.y - 0.3), e^.MovingVelocity, @Ed_15_KazahMovesOut, e);
	end;

	procedure Ed_13_TwinkleRotates(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.twinkle^.RotateTo(e^.twinkle^.HeartPos + Vec2.PositiveY);
		e^.AddTimer(2, nil, @Ed_14_TwinkleMovesOut, e);
	end;

	procedure Ed_12_TwinkleRotates(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.twinkle^.RotateTo(e^.twinkle^.HeartPos + Vec2.PositiveX);
		e^.AddTimer(1, nil, @Ed_13_TwinkleRotates, e);
	end;

	procedure Ed_11_TwinkleRotates(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.twinkle^.RotateTo(e^.twinkle^.HeartPos + Vec2.NegativeX);
		e^.AddTimer(1, nil, @Ed_12_TwinkleRotates, e);
	end;

	procedure Ed_10_ValeraMovesOut(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.valera^.MoveTo(Vec2.Make(0, -e^.mgr^.nvp.y - e^.valera^.size.y - 0.3), e^.MovingVelocity, nil, nil);
		e^.AddTimer(4.0, nil, @Ed_11_TwinkleRotates, e);
	end;

	procedure Ed_9_Valera(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.dlg.Done; e^.dlg.Init(e, 'valera: 16.png')^.Callbacks(nil, @Ed_10_ValeraMovesOut, e);
	end;

	procedure Ed_8_ValeraRotates(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.valera^.RotateTo(e^.valera^.HeartPos + Vec2.MinusOnes);
		e^.AddTimer(2.0, nil, @Ed_9_Valera, e);
	end;

	procedure Ed_7_Twinkle(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.dlg.Init(e, 'twinkle [face = scared.png]: 11.png')^.Callbacks(nil, @Ed_8_ValeraRotates, e);
	end;

	procedure Ed_6_Wait(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.AddTimer(5, nil, @Ed_7_Twinkle, e);
	end;

	procedure Ed_5_Valera(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.dlg.Init(e, 'valera: 15.png')^.Callbacks(nil, @Ed_6_Wait, e);
	end;

	procedure Ed_4_Wait(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.AddTimer(4, nil, @Ed_5_Valera, e);
	end;

	procedure Ed_3_Kazah(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.dlg.Init(e, 'kazah: 10.png')^.Callbacks(nil, @Ed_4_Wait, e);
	end;

	procedure Ed_2_Wait(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.AddTimer(2, nil, @Ed_3_Kazah, e);
	end;

	procedure Ed_1_Valera(param: pointer);
	var
		e: pMv_Ending absolute param;
	begin
		e^.dlg.Init(e, 'valera: 14.png')^.Callbacks(nil, @Ed_2_Wait, e);
	end;

	procedure Mv_Ending.HandleUpdate(const dt: float);
	var
		fadeVel: float;
	begin
		inherited HandleUpdate(dt);
		sunsetTime += dt;

		case state of
			Setup:
				begin
					pNode(land) :=
						(new(pDecoration, Init(Environment('ed-land.png'), Transform2.Identity, Vec2.Make(1.0, Deduce))))^.SetLayer(-2)^.AddTo(location);
					land^.local := Translate(-0.5 * land^.size.x, -mgr^.nvp.y);

					kazah := PutOnLand(CreateKolobok('kazah', 'suit.png'), 0.2, 0.14);
					twinkle := PutOnLand(CreateKolobok('twinkle', 'suit.png'), 0.4, 0.2);
					PutOnLand(player, 0.6, 0.15);
					valera := PutOnLand(CreateKolobok('valera', 'suit.png'), 0.8, 0.16);

					AddTimer(12.0, nil, @Ed_1_Valera, @self);
					state := Idle;
				end;
		end;

		if fadeMode <> NoFade then
		begin
			case fadeMode of
				FadeOut: fadeVel := 0.35;
				FadeOutOut: fadeVel := 0.6;
				RedFlash: fadeVel := 3;
				else fadeVel := 1;
			end;

			fade := min(fade + fadeVel * dt, 1);
			if fade = 1 then
			begin
				case fadeMode of
					FadeOut:
						begin
							fadeMode := FadeOutOut;
							fade := 0;
							display := no;
							mgr^.bgm.Priority(id)^.SetModifier(MuteMainTheme, op_Set, 0, +999);
						end;
					FadeOutOut: begin mgr^.Switch(new(pMainMenu, Init)); exit; end;
					else fadeMode := NoFade;
				end;
			end;
		end;
	end;

	procedure Mv_Ending.HandleDraw;
	const
		Start = 1.0;
		SunsetEnd = 6.5;
		NightSkyAppearanceStart = 4.0;
		NightSkyAppearanceEnd = 10.0;
		NightSkyShift = 0.4;
		ExtraNightShift = 0.012;
	var
		q: Quad;
		sign, k, baseAlpha: float;
		isign: sint;
		size: Vec2;
		i: uint;
		baseTrans: Transform2;
	begin
		if display then
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
				baseTrans := Translate(0, -NightSkyShift * smoothstep(Start, NightSkyAppearanceEnd, sunsetTime)) *
					Translate(0.5 * land^.size.x, -mgr^.nvp.y) * Rotate(Pi/2);
				q.transform := baseTrans;
				baseAlpha := smoothstep(NightSkyAppearanceStart, NightSkyAppearanceEnd, sunsetTime);
				q.color := Vec4.Make(1, 1, 1, baseAlpha);

				size := Vec2.Make(2 * mgr^.nvp.y + NightSkyShift + 2*ExtraNightShift, land^.size.x + 2*ExtraNightShift);
				for i := 0 to 2 do
				begin
					case i of
						1:
							begin
								k := 0.5 + 0.5 * sin(sunsetTime);
								q.transform := Translate((1 - k) * ExtraNightShift * Vec2.Make(cos(0.88*sunsetTime), sin(0.97*sunsetTime))) * baseTrans;
								q.color.data[3] := k * baseAlpha;
							end;
						2:
							begin
								k := 0.5 * (0.5 + 0.5 * sin(1 + 1.09*sunsetTime));
								q.transform := Translate((1 - k) * ExtraNightShift * Vec2.Make(cos(-1.08*sunsetTime), cos(0.91*sunsetTime))) * baseTrans;
								q.color.data[3] := k * baseAlpha;
							end;
					end;
					q.Draw(space, Vec2.Make(-ExtraNightShift), size, Vec2.Zero, AspectPair.Make(size).ReverseCombined(space^.ap).Aspect2(asp2_y1, 1));
				end;
			end;
		end;

		inherited HandleDraw;
		// градиентные рамки, т. к. локация не на весь экран
		if display then
		begin
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
		end;

		case fadeMode of
			FadeIn, FadeOut, FadeOutOut, RedFlash:
				begin
					q.fields := [q.Field.Color];
					case fadeMode of
						FadeIn: q.color := Vec4.Make(0, 0, 0, 1 - fade);
						FadeOut: q.color := Vec4.Make(0.2, 0, 0, fade);
						FadeOutOut: q.color := Vec4.Make(0.2, 0, 0, 1 - fade);
						else {RedFlash} q.color := Vec4.Make(0.8, 0, 0, (0.65 + 0.35 * sin(4*pi*fade) * smoothstep(0, 0.5, fade) * smoothstep(1, 0.5, fade)));
					end;
					q.Draw(nil, -mgr^.nvp, 2 * mgr^.nvp, Vec2.Zero, Vec2.Ones);
				end;
		end;
	end;

	function Mv_Ending.PutOnLand(actor: pActor; const x, y: float): pActor;
	begin
		actor^.idclip := yes;
		actor^.rtVel := RotatingVelocity;
		actor^.angle := Pi/2;
		actor^.local := Translate(land^.PointOn(Vec2.Make(x, y)) - Vec2.Make(0.5 * actor^.size.x, 0));
		location^.Add(actor);
		result := actor;
	end;

end.

