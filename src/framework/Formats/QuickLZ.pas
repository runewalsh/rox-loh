unit QuickLZ;

// http://www.quicklz.com/manual.html
{$include opts.inc}

interface

uses
	USystem;

	function CompressBound(uncompressed: size_t): size_t;
	function DecompressedSize(input: pointer; inputSize: size_t): size_t;
	function Compress(input: pointer; inputSize: size_t; output: pointer; outputSize: size_t): size_t;
	function Decompress(input: pointer; inputSize: size_t; output: pointer; outputSize: size_t): size_t;
	function Compress(const s: string): rawbytestring;
	function Decompress(const s: rawbytestring): string;

implementation

{$define COMPRESSION_LEVEL := 2} // 1 gives fastest compression speed. 2 gives fastest decompression speed and best
                                 // compression ratio.

{$define STREAMING_BUFFER := 0}        // If > 0, zero out both states prior to first call to qlz_compress() or qlz_decompress()
//{$define STREAMING_BUFFER := 100000} // and decompress packets in the same order as they were compressed
//{$define STREAMING_BUFFER := 1000000}

{$define MEMORY_SAFE}        // Guarantees that decompression of corrupted data cannot crash. Decreases decompression
                             // speed 10-20%. Compression speed not affected.

{$if (COMPRESSION_LEVEL <> 1) and (COMPRESSION_LEVEL <> 2)} // Verify compression level
	{$error COMPRESSION_LEVEL must be 1 or 2}
{$endif}

{$if COMPRESSION_LEVEL = 1}      // Decrease QLZ_POINTERS for level 2 to increase compression speed. Do not touch any other values!
	{$define QLZ_POINTERS := 4}
	{$define QLZ_HASH_VALUES := 2048}
{$elseif COMPRESSION_LEVEL = 2}
	{$define QLZ_POINTERS := 16}
	{$define QLZ_HASH_VALUES := 4096}
{$endif}

{$if defined(CPUI386) or defined(CPUX64) or defined(CPUX86_64)}
	{$define X86X64}
{$endif}

const
	MINOFFSET              = 2;
	UNCONDITIONAL_MATCHLEN = 6;
	UNCOMPRESSED_END       = 4;
	CWORD_LEN              = 4;

	function fast_read(src: pByte; bytes: uint32): uint32;
	begin
	{$ifndef X86X64}
		case bytes of
			4: result := uint32(src[0]) or (uint32(src[1]) shl 8) or (uint32(src[2]) shl 16) or (uint32(src[3]) shl 24);
			3: result := uint32(src[0]) or (uint32(src[1]) shl 8) or (uint32(src[2]) shl 16);
			2: result := uint32(src[0]) or (uint32(src[1]) shl 8);
			1: result := uint32(src[0]);
			else result := 0;
		end;
	{$else}
		if (bytes >= 1) and (bytes <= 4) then
			result := pUint32(src)^
		else
			result := 0;
	{$endif}
	end;

	function nbytes(source: pByte): uint32;
	begin
		if source[0] and %10 <> 0 then result := 4 else result := 1;
	end;

	function size_decompressed(source: pByte): size_t;
	var
		n: uint32;
	begin
		n := nbytes(source);
		result := fast_read(source + 1 + n, n);
		result := result and ($ffffffff shr ((4 - n)*8));
	end;

	function size_compressed(source: pByte): size_t;
	var
		n: uint32;
	begin
		n := nbytes(source);
		result := fast_read(source + 1, n);
		result := result and ($ffffffff shr ((4 - n)*8));
	end;

	procedure fast_write(f: uint32; dst: pByte; bytes: size_t);
	begin
	{$ifndef X86X64}
		case bytes of
			4: begin dst[0] := byte(f); dst[1] := byte(f shr 8); dst[2] := byte(f shr 16); dst[3] := byte(f shr 24); end;
			3: begin dst[0] := byte(f); dst[1] := byte(f shr 8); dst[2] := byte(f shr 16); end;
			2: begin dst[0] := byte(f); dst[1] := byte(f shr 8); end;
			1: begin dst[0] := byte(f); end;
		end;
	{$else}
		case bytes of
			4: pUint32(dst)^ := f;
			3: pUint32(dst)^ := f;
			2: pUint16(dst)^ := uint16(f);
			1:   pByte(dst)^ := byte(f);
		end;
	{$endif}
	end;

	function hash_func(i: uint32): uint32;
	begin
	{$if COMPRESSION_LEVEL = 1}
		result := ((i shr 9) xor (i shr 13) xor i) and (QLZ_HASH_VALUES - 1);
	{$else}
		result := ((i shr 12) xor i) and (QLZ_HASH_VALUES - 1);
	{$endif}
	end;

	function hashat(src: pByte): uint32;
	var
		fetch: uint32;
	begin
		fetch := fast_read(src, 3);
		result := hash_func(fetch);
	end;

	function size_header(source: pByte): size_t;
	begin
		result := 2 * nbytes(source) + 1;
	end;

	procedure memcpy_up(dst, src: pByte; n: uint32);
	{$ifndef X86X64} var &end: pByte; {$else} var f: uint32; {$endif}
	begin // Caution if modifying memcpy_up! Overlap of dst and src must be special handled.
	{$ifndef X86X64}
		&end := dst + n;
		while dst < &end do
		begin
			dst[0] := src[0];
			inc(dst);
			inc(src);
		end;
	{$else}
		f := 0;
		repeat
			pUint32(dst + f)^ := pUint32(src + f)^;
			f += MINOFFSET + 1;
		until f >= n;
	{$endif}
	end;

type
	local = object
	type
		hash_compress = record // hash entry
			offset: array[0 .. QLZ_POINTERS - 1] of pByte;
		end;

	{$if COMPRESSION_LEVEL = 1}
		hash_decompress = record
			offset: array[0 .. QLZ_POINTERS - 1] of pByte;
		end;
	{$endif}

		state_compress = record
		{$if STREAMING_BUFFER > 0} stream_buffer: array[0 .. STREAMING_BUFFER - 1] of byte; {$endif}
			stream_counter: size_t;
			hash: array[0 .. QLZ_HASH_VALUES - 1] of hash_compress;
			hash_counter: array[0 .. QLZ_HASH_VALUES - 1] of byte;
		end;

		state_decompress = record
		{$if STREAMING_BUFFER > 0} stream_buffer: array[0 .. STREAMING_BUFFER - 1] of byte; {$endif}
		{$if COMPRESSION_LEVEL = 1}
			hash: array[0 .. QLZ_HASH_VALUES - 1] of hash_decompress;
			hash_counter: array[0 .. QLZ_HASH_VALUES - 1] of byte;
		{$endif}
			stream_counter: size_t;
		end;

	var
		procedure reset_table_compress(var state: state_compress); static;
		procedure reset_table_decompress(var state: state_decompress); static;
	{$if COMPRESSION_LEVEL = 1}
		procedure update_hash(var state: state_decompress; s: pByte); static;
		procedure update_hash_upto(var state: state_decompress; var lh: pByte; max: pByte); static;
	{$endif}
		function compress_core(source, destination: pByte; size: size_t; var state: state_compress): size_t; static;
		function decompress_core(source, destination: pByte; size: size_t; var state: state_decompress; history: pByte): size_t; static;
		function compress(source, destination: pByte; size: size_t; var state: state_compress): size_t; static;
		function decompress(source, destination: pByte; var state: state_decompress): size_t; static;
	end;

	procedure local.reset_table_compress(var state: state_compress);
	var
		i: uint;
	begin
		for i := 0 to QLZ_HASH_VALUES - 1 do
			state.hash_counter[i] := 0;
	end;

	procedure local.reset_table_decompress(var state: state_decompress);
{$if COMPRESSION_LEVEL = 1}
	var
		i: uint;
	begin
		for i := 0 to QLZ_HASH_VALUES - 1 do
			state.hash_counter[i] := 0;
	end;
{$else}
	begin
		Assert(@state = @state);
	end;
{$endif}

{$if COMPRESSION_LEVEL = 1}
	procedure local.update_hash(var state: state_decompress; s: pByte);
	var
		hash: uint32;
		c: byte;
	begin
		hash := hashat(s);
		c := state.hash_counter[hash];
		state.hash[hash].offset[c and (QLZ_POINTERS - 1)] := s;
		inc(c);
		state.hash_counter[hash] := c;
	end;

	procedure local.update_hash_upto(var state: state_decompress; var lh: pByte; max: pByte);
	begin
		while lh < max do
		begin
			inc(lh);
			update_hash(state, lh);
		end;
	end;
{$endif}

	function local.compress_core(source, destination: pByte; size: size_t; var state: state_compress): size_t;
	var
		last_byte, src, cword_ptr, dst, last_matchstart, o, offset2: pByte;
		cword_val, fetch, hash, k, m, f {$if COMPRESSION_LEVEL = 1}, best_k {$endif} {$if COMPRESSION_LEVEL = 2}, u {$endif}: uint32;
		remaining {$if COMPRESSION_LEVEL = 2}, offset {$endif}: size_t;
		matchlen: uint32 ;
		c: byte;
	begin
		last_byte := source + size - 1;
		src       := source;
		cword_ptr := destination;
		dst       := destination + CWORD_LEN;
		cword_val := uint32(1) shl 31;
		last_matchstart := last_byte - UNCONDITIONAL_MATCHLEN - UNCOMPRESSED_END;
		fetch     := 0;

		if src <= last_matchstart then
			fetch := fast_read(src, 3);

		while src <= last_matchstart do
		begin
			if (cword_val and 1) = 1 then
			begin
				// store uncompressed if compression ratio is too low
				if (src > source + (size shr 1)) and (dst - destination > src - source - ((src - source) shr 5)) then
					exit(0);
				fast_write((cword_val shr 1) or (uint32(1) shl 31), cword_ptr, CWORD_LEN);

				cword_ptr := dst;
				dst       += CWORD_LEN;
				cword_val := uint(1) shl 31;
				fetch     := fast_read(src, 3);
			end;
		{$if COMPRESSION_LEVEL = 1} best_k := 0; {$endif}
			if last_byte - UNCOMPRESSED_END - src + 1 > 255 then remaining := 255 else remaining := last_byte - UNCOMPRESSED_END - src + 1;

			//hash = hashat(src);
			fetch   := fast_read(src, 3);
			hash    := hash_func(fetch);
			c       := state.hash_counter[hash];
			offset2 := state.hash[hash].offset[0];
			if (offset2 < src - MINOFFSET) and (c > 0) and (((fast_read(offset2, 3) xor fetch) and $ffffff) = 0) then
			begin
				matchlen := 3;
				if offset2[matchlen] = src[matchlen] then
				begin
					matchlen := 4;
					while (offset2[matchlen] = src[matchlen]) and (matchlen < remaining) do
						inc(matchlen);
				end;
			end else
				matchlen := 0;

			k := 1;
			while (k < QLZ_POINTERS) and (c > k) do
			begin
				o := state.hash[hash].offset[k];
			{$if COMPRESSION_LEVEL = 2}
				if ((fast_read(o, 3) xor fetch) and $ffffff = 0) and (o < src - MINOFFSET) then
			{$elseif COMPRESSION_LEVEL = 1}
				if (src[matchlen] = o[matchlen]) and ((fast_read(o, 3) xor fetch) and $ffffff = 0) and (o < src - MINOFFSET) then
			{$else} {$error} {$endif}
				begin
					m := 3;
					while (o[m] = src[m]) and (m < remaining) do
						inc(m);
				{$if COMPRESSION_LEVEL = 2}
					if (m > matchlen) or ((m = matchlen) and (o > offset2)) then
				{$elseif COMPRESSION_LEVEL = 1}
					if m > matchlen then
				{$else} {$error} {$endif}
					begin
						offset2  := o;
						matchlen := m;
					{$if COMPRESSION_LEVEL = 1} best_k   := k; {$endif}
					end;
				end;
				inc(k);
			end;

			o := offset2;
			state.hash[hash].offset[c and (QLZ_POINTERS - 1)] := src;
		unchecked inc(c); end_unchecked
			state.hash_counter[hash] := c;

		{$if COMPRESSION_LEVEL = 2}
			if (matchlen > 2) and (src - o < 131071) then
			begin
				offset := src - o;

				for u := 1 to matchlen - 1 do
				begin
					hash := hashat(src + u);
					c := state.hash_counter[hash];
				unchecked inc(state.hash_counter[hash]); end_unchecked
					state.hash[hash].offset[c and (QLZ_POINTERS - 1)] := src + u;
				end;

				cword_val := (cword_val shr 1) or (uint32(1) shl 31);
				src += matchlen;

				if (matchlen = 3) and (offset <= 63) then
				begin
					dst^ := byte(offset shl 2);
					inc(dst);
				end else if (matchlen = 3) and (offset <= 16383) then
				begin
					f := uint32((offset shl 2) or 1);
					fast_write(f, dst, 2);
					dst += 2;
				end else if (matchlen <= 18) and (offset <= 1023) then
				begin
					f := ((matchlen - 3) shl 2) or (uint32(offset) shl 6) or 2;
					fast_write(f, dst, 2);
					dst += 2;
				end else if(matchlen <= 33) then
				begin
					f := ((matchlen - 2) shl 2) or (uint32(offset) shl 7) or 3;
					fast_write(f, dst, 3);
					dst += 3;
				end else
				begin
					f := ((matchlen - 3) shl 7) or (uint32(offset) shl 15) or 3;
					fast_write(f, dst, 4);
					dst += 4;
				end;
			end else
			begin
				dst^ := src^; inc(src); inc(dst);
				cword_val := cword_val shr 1;
			end;
		{$elseif COMPRESSION_LEVEL = 1}
			if matchlen > 2 then
			begin
				cword_val := (cword_val shr 1) or (uint32(1) shl 31);
				src += matchlen;

				if matchlen < 10 then
				begin
					f := best_k or ((matchlen - 2) shl 2) or (hash shl 5);
					fast_write(f, dst, 2);
					dst += 2;
				end else
				begin
					f := best_k or (matchlen shl 16) or (hash shl 5);
					fast_write(f, dst, 3);
					dst += 3;
				end;
			end else
			begin
				dst^ := src^; inc(src); inc(dst);
				cword_val := cword_val shr 1;
			end;
		{$else} {$error} {$endif}
		end;

		while src <= last_byte do
		begin
			if cword_val and 1 = 1 then
			begin
				fast_write((cword_val shr 1) or (uint32(1) shl 31), cword_ptr, CWORD_LEN);
				cword_ptr := dst;
				dst += CWORD_LEN;
				cword_val := uint32(1) shl 31;
			end;

		{$if COMPRESSION_LEVEL = 1}
			if src <= last_byte - 3 then
			begin
				hash := hashat(src);
				c := state.hash_counter[hash];
				state.hash[hash].offset[c and (QLZ_POINTERS - 1)] := src;
				inc(c);
				state.hash_counter[hash] := c;
			end;
		{$endif}
			dst^ := src^; inc(src); inc(dst);
			cword_val := cword_val shr 1;
		end;

		while cword_val and 1 <> 1 do
			cword_val := cword_val shr 1;
		fast_write((cword_val shr 1) or (uint32(1) shl 31), cword_ptr, CWORD_LEN);

		// min. size must be 9 bytes so that the qlz_size functions can take 9 bytes as argument
		if dst - destination < 9 then result := 9 else result := dst - destination;
	end;

	function local.decompress_core(source, destination: pByte; size: size_t; var state: state_decompress; history: pByte): size_t;
	const
		bitlut: array[0 .. 15] of uint = (4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0);
	var
		src, dst, last_destination_byte, last_matchstart, {$if COMPRESSION_LEVEL = 1} last_hashed, {$endif} last_source_byte, offset2: pByte;
		cword_val, fetch, matchlen {$if COMPRESSION_LEVEL = 1}, hash {$endif} {$if COMPRESSION_LEVEL = 2}, offset {$endif}: uint32;
	{$if COMPRESSION_LEVEL = 1} c: byte; {$endif}
		n: uint;
	begin
		src := source + size_header(source);
		dst := destination;
		last_destination_byte := destination + size - 1;
		cword_val := 1;
		last_matchstart := last_destination_byte - UNCONDITIONAL_MATCHLEN - UNCOMPRESSED_END;
	{$if COMPRESSION_LEVEL = 1} last_hashed := destination - 1; {$endif}
		last_source_byte := source + size_compressed(source) - 1;
		Assert(@state = @state);

		repeat
			if cword_val = 1 then
			begin
			{$ifdef MEMORY_SAFE}
				if src + CWORD_LEN - 1 > last_source_byte then exit(0);
			{$endif}
				cword_val := fast_read(src, CWORD_LEN);
				src += CWORD_LEN;
			end;

		{$ifdef MEMORY_SAFE}
			if src + 4 - 1 > last_source_byte then exit(0);
		{$endif}
			fetch := fast_read(src, 4);

			if cword_val and 1 = 1 then
			begin
			{$if COMPRESSION_LEVEL = 1}
				cword_val := cword_val shr 1;
				hash := (fetch shr 5) and $7ff;
				c := byte(fetch and $3);
				offset2 := state.hash[hash].offset[c];

				if fetch and 28 <> 0 then
				begin
					matchlen := (fetch shr 2) and $7 + 2;
					src += 2;
				end else
				begin
					matchlen := src[2];
					src += 3;
				end;
			{$elseif COMPRESSION_LEVEL = 2}
				cword_val := cword_val shr 1;
				if fetch and 3 = 0 then
				begin
					offset := (fetch and $ff) shr 2;
					matchlen := 3;
					inc(src);
				end else if fetch and 2 = 0 then
				begin
					offset := (fetch and $ffff) shr 2;
					matchlen := 3;
					src += 2;
				end else if fetch and 1 = 0 then
				begin
					offset := (fetch and $ffff) shr 6;
					matchlen := (fetch shr 2) and 15 + 3;
					src += 2;
				end else if fetch and 127 <> 3 then
				begin
					offset := (fetch shr 7) and $1ffff;
					matchlen := (fetch shr 2) and $1f + 2;
					src += 3;
				end else
				begin
					offset := fetch shr 15;
					matchlen := (fetch shr 7) and 255 + 3;
					src += 4;
				end;

				offset2 := dst - offset;
			{$endif}

			{$ifdef MEMORY_SAFE}
				if (offset2 < history) or (offset2 > dst - MINOFFSET - 1) then exit(0);
				if matchlen > uint32(last_destination_byte - dst - UNCOMPRESSED_END + 1) then exit(0);
			{$endif}
				memcpy_up(dst, offset2, matchlen);
				dst += matchlen;

			{$if COMPRESSION_LEVEL = 1}
				update_hash_upto(state, last_hashed, dst - matchlen);
				last_hashed := dst - 1;
			{$endif}
			end else
			begin
				if dst < last_matchstart then
				begin
					n := bitlut[cword_val and $f];
				{$ifdef X86X64} pUint32(dst)^ := pUint32(src)^; {$else} memcpy_up(dst, src, 4); {$endif}
					cword_val := cword_val shr n;
					dst += n; src += n;
				{$if COMPRESSION_LEVEL = 1}
					update_hash_upto(state, last_hashed, dst - 3);
				{$endif}
				end else
				begin
					while dst <= last_destination_byte do
					begin
						if cword_val = 1 then
						begin
							src += CWORD_LEN;
							cword_val := uint32(1) shl 31;
						end;

					{$ifdef MEMORY_SAFE}
						if src >= last_source_byte + 1 then exit(0);
					{$endif}
						dst^ := src^; inc(dst); inc(src);
						cword_val := cword_val shr 1;
					end;

				{$if COMPRESSION_LEVEL = 1}
					update_hash_upto(state, last_hashed, last_destination_byte - 3); // todo, use constant
				{$endif}
					exit(size);
				end;
			end;
		until false;
	end;

	function local.compress(source, destination: pByte; size: size_t; var state: state_compress): size_t;
	var
		compressed: uint;
		base: size_t;
	{$if STREAMING_BUFFER > 0} src: pByte; {$endif}
	begin
		if (size = 0) or (size > $ffffffff - 400) then exit(0);
		if size < 216 then base := 3 else base := 9;

	{$if STREAMING_BUFFER > 0}
		if state.stream_counter + size > STREAMING_BUFFER then
	{$endif}
		begin
			reset_table_compress(state);
			result := base + compress_core(source, destination + base, size, state);
		{$if STREAMING_BUFFER > 0} reset_table_compress(state); {$endif}
			if result = base then
			begin
				Move(source^, (destination + base)^, size);
				result := size + base;
				compressed := 0;
			end else
				compressed := 1;
			state.stream_counter := 0;
		end
	{$if STREAMING_BUFFER > 0} else
		begin
			src := pByte(state.stream_buffer) + state.stream_counter;

			Move(source^, src^, size);
			result := base + compress_core(src, destination + base, size, state);

			if result = base then
			begin
				Move(src^, (destination + base)^, size);
				result := size + base;
				compressed := 0;
				reset_table_compress(state);
			end else
				compressed := 1;
			state.stream_counter += size;
		end
	{$endif};

		if base = 3 then
		begin
			destination[0] := byte(0 or compressed);
			destination[1] := byte(result);
			destination[2] := byte(size);
		end else
		begin
			destination[0] := byte(2 or compressed);
			fast_write(uint32(result), destination + 1, 4);
			fast_write(uint32(size), destination + 5, 4);
		end;

		destination^ := destination^ or (COMPRESSION_LEVEL << 2);
		destination^ := destination^ or (1 << 6);
	{$if STREAMING_BUFFER = 0} destination^ := destination^ or (0 shl 4);
	{$elseif STREAMING_BUFFER = 100000} destination^ := destination^ or (1 shl 4);
	{$elseif STREAMING_BUFFER = 1000000} destination^ := destination^ or (2 shl 4);
	{$else} destination^ := destination^ or (3 shl 4);
	{$endif}

		// 76543210
		// 01SSLLHC
	end;

	function local.decompress(source, destination: pByte; var state: state_decompress): size_t;
	var
		dsiz: size_t;
	{$if STREAMING_BUFFER > 0} dst: pByte; {$endif}
	begin
		dsiz := size_decompressed(source);

	{$if STREAMING_BUFFER > 0}
		if state.stream_counter + dsiz > STREAMING_BUFFER then
	{$endif}
		begin
			if source[0] and 1 = 1 then
			begin
				reset_table_decompress(state);
				dsiz := decompress_core(source, destination, dsiz, state, destination);
			end else
				Move((source + size_header(source))^, destination^, dsiz);

			state.stream_counter := 0;
			reset_table_decompress(state);
		end

	{$if STREAMING_BUFFER > 0} else
		begin
			dst := pByte(state.stream_buffer) + state.stream_counter;
			if source[0] and 1 = 1 then
				dsiz := decompress_core(source, dst, dsiz, state, state.stream_buffer)
			else
			begin
				Move((source + size_header(source))^, dst^, dsiz);
				reset_table_decompress(state);
			end;
			Move(dst^, destination^, dsiz);
			state.stream_counter += dsiz;
		end
	{$endif};
		result := dsiz;
	end;

	function CompressBound(uncompressed: size_t): size_t;
	begin
		result := uncompressed + 400;
	end;

	function DecompressedSize(input: pointer; inputSize: size_t): size_t;
	begin
		if inputSize < 9 then result := 0 else result := size_decompressed(input);
	end;

	function Compress(input: pointer; inputSize: size_t; output: pointer; outputSize: size_t): size_t;
	var
		state: local.state_compress;
	begin
		if outputSize < CompressBound(inputSize) then exit(0); // не знаю, как проверить точно, так что...
	{$if STREAMING_BUFFER > 0} fillchar(state, sizeof(state), 0); {$endif}
		result := local.compress(input, output, inputSize, (@state)^);
	end;

	function Decompress(input: pointer; inputSize: size_t; output: pointer; outputSize: size_t): size_t;
	var
		decompSize: size_t;
		state: local.state_decompress;
	begin
	{$if STREAMING_BUFFER > 0} fillchar(state, sizeof(state), 0); {$endif}
		decompSize := DecompressedSize(input, inputSize);
		if (decompSize = 0) or (outputSize < decompSize) or (inputSize < size_compressed(input)) then exit(0);
		result := local.decompress(input, output, (@state)^);
	end;

	function Compress(const s: string): rawbytestring;
	begin
		SetLength(result, CompressBound(length(s) * sizeof(char)));
		SetLength(result, Compress(pointer(s), length(s) * sizeof(char), pointer(result), length(result)));
	end;

	function Decompress(const s: rawbytestring): string;
	begin
		SetLength(result, (DecompressedSize(pointer(s), length(s)) + (sizeof(char) - 1)) div sizeof(char));
		SetLength(result, Decompress(pointer(s), length(s), pointer(result), length(result) * sizeof(char)) div sizeof(char));
	end;

end.
