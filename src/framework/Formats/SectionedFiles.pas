{$include opts.inc}
unit SectionedFiles;

interface

uses
	USystem, Errors, UMath, Streams, Algo, Utils;

type
	SectionedFile = object
	type
		pSectionDesc = ^SectionDesc;
		SectionDesc = object
			name: string;
			offset: FilePos;
			length: FileSize;
			procedure PostRangeCheck(s: pStream);
		end;
		scoped_enum_ GetSectionMode = (Fast, Precise); _end

		function Open(newStream: pStream): SectionedFile; static;
		procedure Close;
		function TryGetSection(const name: string; mode: GetSectionMode = GetSectionMode.Fast): pSectionDesc;
		function TryGetSection(const name: string; out section: pSectionDesc; mode: GetSectionMode = GetSectionMode.Fast): pSectionDesc;
		function GetSection(const name: string; mode: GetSectionMode = GetSectionMode.Fast): pSectionDesc;

	private
		stream: pStream;
		ofsFormat: UiBinaryFormat;
		declaredSections, readSections: sint;
		headerPtr: FilePos;
		sections: array of SectionDesc;

		function ReadNextSection: pSectionDesc;
		function RangeCheckHeader(const hptr: FilePos): FilePos;

	public const
		Signature = 'S';
		SaneSectionsCountLimit = 10000;
		MaxHeaderEstimationTries = 3;
	type
		WriteSectionCallback = procedure(const name: string; s: pStream; param: pointer);
		Writer = object
			procedure Write(s: pStream; const sections: array of string; writeSection: WriteSectionCallback; param: pointer); static;
		end;
	end;

implementation

	procedure SectionedFile.SectionDesc.PostRangeCheck(s: pStream);
	begin
		if s^.Position > offset + length then
			raise Error('{0}: секция {1} повреждена.', StreamPath.Human(s^.path), name);
	end;

	function SectionedFile.Open(newStream: pStream): SectionedFile;
	var
		s: pStream absolute result.stream;
	begin
	FPC_3_BUG System.Initialize(result);
		result.stream := MakeRef(newStream);
		try
			Deserialize_signature(s, Signature);
			result.ofsFormat        := UiBinaryFormat(Deserialize_enum(s, UiBinaryFormatPrefixCodes));
			result.declaredSections := RangeCheck(VarInt.Read(s), SaneSectionsCountLimit, 'SectionsCount');
			result.readSections     := 0;
			result.headerPtr        := s^.Position;
		except
			result.Close;
			raise;
		end;
	end;

	procedure SectionedFile.Close;
	begin
		Release(stream);
	FPC_3_BUG System.Finalize(self); stream := nil;
	end;

	function SectionedFile.TryGetSection(const name: string; mode: GetSectionMode = GetSectionMode.Fast): pSectionDesc;
	var
		i: sint;
	begin
		for i := 0 to readSections - 1 do
			if sections[i].name = name then
			begin
				if (mode = GetSectionMode.Precise) and (i = readSections - 1) and (readSections < declaredSections) then
					ReadNextSection; // выставить точную длину
				exit(@sections[i]);
			end;

		while readSections < declaredSections do
		begin
			result := ReadNextSection;
			if result^.name = name then
			begin
				if (mode = GetSectionMode.Precise) and (readSections < declaredSections) then ReadNextSection;
				exit;
			end;
		end;
		result := nil;
	end;

	function SectionedFile.TryGetSection(const name: string; out section: pSectionDesc; mode: GetSectionMode = GetSectionMode.Fast): pSectionDesc;
	begin
		result := TryGetSection(name, mode);
		section := result;
	end;

	function SectionedFile.GetSection(const name: string; mode: GetSectionMode = GetSectionMode.Fast): pSectionDesc;
	begin
		result := TryGetSection(name, mode);
		if not Assigned(result) then raise Error('{0}: секция {1} не найдена.', StreamPath.Human(stream^.path), name);
	end;

	function SectionedFile.ReadNextSection: pSectionDesc;
	var
		t: SectionDesc;
		prevOffset: FilePos;
		dOffset: FileSize;
	begin
		Assert(readSections < declaredSections);
		stream^.Position := RangeCheckHeader(headerPtr);
		t.name := Deserialize_string(stream);
		if readSections > 0 then prevOffset := sections[readSections - 1].offset else prevOffset := FilePos.Zero;
		dOffset := FileSize.Explicit(RangeCheck(Deserialize_ui(stream, ofsFormat), (stream^.Size - prevOffset).value, 'SectionOffset'));
		t.offset := prevOffset + dOffset;
		if readSections > 0 then sections[readSections - 1].length := dOffset;
		t.length := stream^.Size - t.offset;
		headerPtr := RangeCheckHeader(stream^.Position);

		SetLength(sections, min(2 * (readSections + 1), declaredSections));
		result := @sections[readSections];
		inc(readSections);
		result^ := t;
	end;

	function SectionedFile.RangeCheckHeader(const hptr: FilePos): FilePos;
	begin
		result := hptr;
		if (readSections > 0) and (hptr > sections[0].offset) then raise Error('{0}: заголовок повреждён.', StreamPath.Human(stream^.path));
	end;

	procedure SectionedFile.Writer.Write(s: pStream; const sections: array of string; writeSection: WriteSectionCallback; param: pointer);
	var
		xsec: array of record
			temp: pStream;
		end;
		ofsFormat, prevOfsFormat: UiBinaryFormat;
		headerLen, prevHeaderLen: FilePos;

		function SectionOffset(id: sint): FilePos;
		begin
			if id = 0 then result := prevHeaderLen else result := xsec[id-1].temp^.Position;
		end;

		procedure EstimateHeader;
		var
			i: sint;
			c: UiBinaryFormatChooser;
		begin
			c.Init('форматов смещений');
			headerLen := FilePos.Zero;
			headerLen += length(Signature) * sizeof(char);
			headerLen += Serialize_enum_bytes(ord(prevOfsFormat), UiBinaryFormatPrefixCodes);
			headerLen += VarInt.Bytes(length(xsec));
			for i := 0 to High(xsec) do
			begin
				headerLen += Serialize_string_bytes(sections[i]);
				headerLen += Serialize_ui_bytes(SectionOffset(i).value, prevOfsFormat);
				c.Note(SectionOffset(i).value);
			end;
			c.Note(headerLen.value);
			ofsFormat := c.DestructiveChoose;
		end;

		procedure WriteHeader;
		var
			i: sint;
		begin
			Serialize_conststring(s, Signature);
			Serialize_enum(s, ord(ofsFormat), UiBinaryFormatPrefixCodes);
			VarInt.Write(s, length(xsec));
			for i := 0 to High(xsec) do
			begin
				Serialize_string(s, sections[i]);
				Serialize_ui(s, SectionOffset(i).value, ofsFormat);
			end;
		end;

		procedure WriteData;
		var
			i: sint;
			pendingOffset: FilePos;
		begin
			pendingOffset := SectionOffset(0);
			for i := 0 to High(xsec) do
			begin
				Assert(s^.Position = pendingOffset);
				s^.AppendFrom(xsec[i].temp, FilePos.Zero, xsec[i].temp^.Size);
				pendingOffset += xsec[i].temp^.Size;
			end;
		end;

	var
		i, triesLeft: sint;
	begin
		SetLength(xsec, RangeCheck(size_t(length(sections)), SaneSectionsCountLimit, 'SectionsCount'));
		for i := 0 to High(xsec) do xsec[i].temp := nil;
		MakeRef(s);
		try
			for i := 0 to High(xsec) do
			begin
				xsec[i].temp := FileStream.CreateTemp;
				writeSection(sections[i], xsec[i].temp, param);
			end;

			headerLen := FilePos.Zero; ofsFormat := se_ui8; triesLeft := MaxHeaderEstimationTries;
			repeat
				if triesLeft = 0 then raise Error('{0}: не удалось рассчитать заголовок.', StreamPath.Human(s^.path));
				dec(triesLeft);
				prevHeaderLen := headerLen; prevOfsFormat := ofsFormat;
				EstimateHeader;
			until (headerLen = prevHeaderLen) and (ofsFormat = prevOfsFormat);

			WriteHeader;
			WriteData;
		finally
			for i := 0 to High(xsec) do Release(xsec[i].temp);
			Release(s);
		end;
	end;

end.
