{$include opts.inc}
unit ZlibHeaders;
{$define ShareLzhamStates}

interface

uses
	ctypes, USystem, Errors, DynamicLoader;

type
	z = class
	type
		stream = record
			next_in  : pointer;           // next input byte
			avail_in : cuint;             // number of bytes available at next_in
			total_in : culong;            // total nb of input bytes read so far

			next_out : pointer;           // next output byte should be put there
			avail_out: cuint;             // remaining free space at next_out
			total_out: culong;            // total nb of bytes output so far

			msg      : pchar;             // last error message, NULL if no error
			state    : pointer;           // not visible by applications

			zalloc   : function(opaque: pointer; items, size: cuint): pointer; cdecl; // used to allocate the internal state
			zfree    : procedure(opaque, block: pointer); cdecl; // used to free the internal state
			opaque   : pointer;           // private data object passed to zalloc and zfree

			data_type: cint;              // best guess about the data type: binary or text
			adler    : culong;            // adler32 value of the uncompressed data
			reserved : culong;            // reserved for future use
		end;
{
		The application must update next_in and avail_in when avail_in has dropped to zero. It must update next_out and avail_out when
	avail_out has dropped to zero. The application must initialize zalloc, zfree and opaque before calling the init function. All other
	fields are set by the compression library and must not be updated by the application.

		The opaque value provided by the application will be passed as the first parameter for calls of zalloc and zfree. This can be useful
	for custom memory management. The compression library attaches no meaning to the opaque value.

		zalloc must return Z_NULL if there is not enough memory for the object. If zlib is used in a multi-threaded application, zalloc and
	zfree must be thread safe.

		On 16-bit systems, the functions zalloc and zfree must be able to allocate exactly 65536 bytes, but will not be required to allocate
	more than this if the symbol MAXSEG_64K is defined (see zconf.h). WARNING: On MSDOS, pointers returned by zalloc for objects of exactly
	65536 bytes *must* have their offset normalized to zero. The default allocation function provided by this library ensures this
	(see zutil.c). To reduce memory requirements and avoid any allocation of 64K objects, at the expense of compression ratio, compile the
	library with -DMAX_WBITS=14 (see zconf.h).

		The fields total_in and total_out can be used for statistics or progress reports. After compression, total_in holds the total size of
	the uncompressed data and may be saved for use in the decompressor (particularly if the decompressor wants to decompress everything in
	a single step).
}

	const
		VERSION = '1.2.3';

		NO_FLUSH = 0; // Allowed flush values; see deflate() and inflate() below for details
		SYNC_FLUSH = 2;
		FULL_FLUSH = 3;
		FINISH = 4;
		BLOCK = 5;

		// Return codes for the compression/decompression functions.
		// Negative values are errors, positive values are used for special but normal events.
		OK = 0;
		STREAM_END = 1;
		NEED_DICT = 2;
		ERRNO = (-1);
		STREAM_ERROR = (-2);
		DATA_ERROR = (-3);
		MEM_ERROR = (-4);
		BUF_ERROR = (-5);
		VERSION_ERROR = (-6);

		// compression levels
		NO_COMPRESSION = 0;
		BEST_SPEED = 1;
		BEST_COMPRESSION = 9;
		DEFAULT_COMPRESSION = -(1);

		// compression strategy; see deflateInit2() below for details
		FILTERED = 1;
		HUFFMAN_ONLY = 2;
		RLE = 3;
		FIXED = 4;
		DEFAULT_STRATEGY = 0;

		// The deflate compression method (the only one supported in this version)
		DEFLATED = 8;

	class var
		// zlibVersion: function: pchar; cdecl;
{
		The application can compare zlibVersion and ZLIB_VERSION for consistency. If the first character differs, the library code actually
	used is not compatible with the zlib.h header file used by the application. This check is automatically made by deflateInit and inflateInit.
}

		// deflateInit: function(var strm: z_stream; level: cint; version: pchar; stream_size: cint): cint; cdecl;
{
		Initializes the internal stream state for compression. The fields zalloc, zfree and opaque must be initialized before by the caller.
	If zalloc and zfree are set to Z_NULL, deflateInit updates them to use default allocation functions.

		The compression level must be Z_DEFAULT_COMPRESSION, or between 0 and 9: 1 gives best speed, 9 gives best compression, 0 gives no
	compression at all (the input data is simply copied a block at a time). Z_DEFAULT_COMPRESSION requests a default compromise between speed
	and compression (currently equivalent to level 6).

		deflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if level is not a valid compression
	level, Z_VERSION_ERROR if the zlib library version (zlib_version) is incompatible with the version assumed by the caller (ZLIB_VERSION).
	msg is set to null if there is no error message. deflateInit does not perform any compression: this will be done by deflate().
}

		deflate: function(var strm: stream; flush: cint): cint; cdecl;
{
		deflate compresses as much data as possible, and stops when the input buffer becomes empty or the output buffer becomes full. It may
	introduce some output latency (reading input without producing any output) except when forced to flush.

		The detailed semantics are as follows. deflate performs one or both of the following actions:

	- Compress more input starting at next_in and update next_in and avail_in accordingly. If not all input can be processed (because there
		is not enough room in the output buffer), next_in and avail_in are updated and processing will resume at this point for the next call
		of deflate().

	- Provide more output starting at next_out and update next_out and avail_out accordingly. This action is forced if the parameter flush
		is non zero. Forcing flush frequently degrades the compression ratio, so this parameter should be set only when necessary (in
		interactive applications). Some output may be provided even if flush is not set.

		Before the call of deflate(), the application should ensure that at least one of the actions is possible, by providing more input
	and/or consuming more output, and updating avail_in or avail_out accordingly; avail_out should never be zero before the call. The
	application can consume the compressed output when it wants, for example when the output buffer is full (avail_out == 0), or after each
	call of deflate(). If deflate returns Z_OK and with zero avail_out, it must be called again after making room in the output buffer
	because there might be more output pending.

		Normally the parameter flush is set to Z_NO_FLUSH, which allows deflate to decide how much data to accumualte before producing output,
	in order to maximize compression.

		If the parameter flush is set to Z_SYNC_FLUSH, all pending output is flushed to the output buffer and the output is aligned on a byte
	boundary, so that the decompressor can get all input data available so far. (In particular avail_in is zero after the call if enough
	output space has been provided before the call.) Flushing may degrade compression for some compression algorithms and so it should be
	used only when necessary.

		If flush is set to Z_FULL_FLUSH, all output is flushed as with Z_SYNC_FLUSH, and the compression state is reset so that decompression
	can restart from this point if previous compressed data has been damaged or if random access is desired. Using Z_FULL_FLUSH too often
	can seriously degrade compression.

		If deflate returns with avail_out == 0, this function must be called again with the same value of the flush parameter and more output
	space (updated avail_out), until the flush is complete (deflate returns with non-zero avail_out). In the case of a Z_FULL_FLUSH or
	Z_SYNC_FLUSH, make sure that avail_out is greater than six to avoid repeated flush markers due to avail_out == 0 on return.

		If the parameter flush is set to Z_FINISH, pending input is processed, pending output is flushed and deflate returns with Z_STREAM_END
	if there was enough output space; if deflate returns with Z_OK, this function must be called again with Z_FINISH and more output space
	(updated avail_out) but no more input data, until it returns with Z_STREAM_END or an error. After deflate has returned Z_STREAM_END, the
	only possible operations on the stream are deflateReset or deflateEnd.

		Z_FINISH can be used immediately after deflateInit if all the compression is to be done in a single step. In this case, avail_out must
	be at least the value returned by deflateBound (see below). If deflate does not return Z_STREAM_END, then it must be called again as
	described above.

		deflate() sets strm->adler to the adler32 checksum of all input read so far (that is, total_in bytes).

		deflate() may update strm->data_type if it can make a good guess about the input data type (Z_BINARY or Z_TEXT). In doubt, the data is
	considered binary. This field is only for information purposes and does not affect the compression algorithm in any manner.

		deflate() returns Z_OK if some progress has been made (more input processed or more output produced), Z_STREAM_END if all input has
	been consumed and all output has been produced (only when flush is set to Z_FINISH), Z_STREAM_ERROR if the stream state was inconsistent
	(for example if next_in or next_out was NULL), Z_BUF_ERROR if no progress is possible (for example avail_in or avail_out was zero). Note
	that Z_BUF_ERROR is not fatal, and deflate() can be called again with more input and more output space to continue compressing.
}

		deflateEnd: function(var strm: stream): cint; cdecl;
{
		All dynamically allocated data structures for this stream are freed. This function discards any unprocessed input and does not flush
	any pending output.

		deflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state was inconsistent, Z_DATA_ERROR if the stream was freed
	prematurely (some input or output was discarded). In the error case, msg may be set but then points to a static string (which must not be
	deallocated).
}

		// inflateInit: function(var strm: z_stream; version: pchar; stream_size: cint): cint; cdecl;
{
		Initializes the internal stream state for decompression. The fields next_in, avail_in, zalloc, zfree and opaque must be initialized
	before by the caller. If next_in is not Z_NULL and avail_in is large enough (the exact value depends on the compression method),
	inflateInit determines the compression method from the zlib header and allocates all data structures accordingly; otherwise the allocation
	will be deferred to the first call of inflate. If zalloc and zfree are set to Z_NULL, inflateInit updates them to use default allocation
	functions.

		inflateInit returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_VERSION_ERROR if the zlib library version is
	incompatible with the version assumed by the caller. msg is set to null if there is no error message. inflateInit does not perform any
	decompression apart from reading the zlib header if present: this will be done by inflate(). (So next_in and avail_in may be modified,
	but next_out and avail_out are unchanged.)
}

		inflate: function(var strm: stream; flush: cint): cint; cdecl;
{
		inflate decompresses as much data as possible, and stops when the input buffer becomes empty or the output buffer becomes full. It
	may introduce some output latency (reading input without producing any output) except when forced to flush.

		The detailed semantics are as follows. inflate performs one or both of the following actions:

	- Decompress more input starting at next_in and update next_in and avail_in accordingly. If not all input can be processed (because there
		is not enough room in the output buffer), next_in is updated and processing will resume at this point for the next call of inflate().

	- Provide more output starting at next_out and update next_out and avail_out accordingly. inflate() provides as much output as possible,
		until there is no more input data or no more space in the output buffer (see below about the flush parameter).

		Before the call of inflate(), the application should ensure that at least one of the actions is possible, by providing more input
	and/or consuming more output, and updating the next_* and avail_* values accordingly. The application can consume the uncompressed output
	when it wants, for example when the output buffer is full (avail_out == 0), or after each call of inflate(). If inflate returns Z_OK and
	with zero avail_out, it must be called again after making room in the output buffer because there might be more output pending.

		The flush parameter of inflate() can be Z_NO_FLUSH, Z_SYNC_FLUSH, Z_FINISH, or Z_BLOCK. Z_SYNC_FLUSH requests that inflate() flush as
	much output as possible to the output buffer. Z_BLOCK requests that inflate() stop if and when it gets to the next deflate block boundary.
	When decoding the zlib or gzip format, this will cause inflate() to return immediately after the header and before the first block. When
	doing a raw inflate, inflate() will go ahead and process the first block, and will return when it gets to the end of that block, or when
	it runs out of data.

		The Z_BLOCK option assists in appending to or combining deflate streams. Also to assist in this, on return inflate() will set
	strm->data_type to the number of unused bits in the last byte taken from strm->next_in, plus 64 if inflate() is currently decoding the
	last block in the deflate stream, plus 128 if inflate() returned immediately after decoding an end-of-block code or decoding the complete
	header up to just before the first byte of the deflate stream. The end-of-block will not be indicated until all of the uncompressed data
	from that block has been written to strm->next_out. The number of unused bits may in general be greater than seven, except when bit 7
	of data_type is set, in which case the number of unused bits will be less than eight.

		inflate() should normally be called until it returns Z_STREAM_END or an error. However if all decompression is to be performed in a
	single step (a single call of inflate), the parameter flush should be set to Z_FINISH. In this case all pending input is processed and
	all pending output is flushed; avail_out must be large enough to hold all the uncompressed data. (The size of the uncompressed data may
	have been saved by the compressor for this purpose.) The next operation on this stream must be inflateEnd to deallocate the decompression
	state. The use of Z_FINISH is never required, but can be used to inform inflate that a faster approach may be used for the single
	inflate() call.

		In this implementation, inflate() always flushes as much output as possible to the output buffer, and always uses the faster approach
	on the first call. So the only effect of the flush parameter in this implementation is on the return value of inflate(), as noted below,
	or when it returns early because Z_BLOCK is used.

		If a preset dictionary is needed after this call (see inflateSetDictionary below), inflate sets strm->adler to the adler32 checksum of
	the dictionary chosen by the compressor and returns Z_NEED_DICT; otherwise it sets strm->adler to the adler32 checksum of all output
	produced so far (that is, total_out bytes) and returns Z_OK, Z_STREAM_END or an error code as described below. At the end of the stream,
	inflate() checks that its computed adler32 checksum is equal to that saved by the compressor and returns Z_STREAM_END only if the
	checksum is correct.

		inflate() will decompress and check either zlib-wrapped or gzip-wrapped deflate data. The header type is detected automatically. Any
	information contained in the gzip header is not retained, so applications that need that information should instead use raw inflate, see
	inflateInit2() below, or inflateBack() and perform their own processing of the gzip header and trailer.

		inflate() returns Z_OK if some progress has been made (more input processed or more output produced), Z_STREAM_END if the end of the
	compressed data has been reached and all uncompressed output has been produced, Z_NEED_DICT if a preset dictionary is needed at this
	point, Z_DATA_ERROR if the input data was corrupted (input stream not conforming to the zlib format or incorrect check value),
	Z_STREAM_ERROR if the stream structure was inconsistent (for example if next_in or next_out was NULL), Z_MEM_ERROR if there was not
	enough memory, Z_BUF_ERROR if no progress is possible or if there was not enough room in the output buffer when Z_FINISH is used. Note
	that Z_BUF_ERROR is not fatal, and inflate() can be called again with more input and more output space to continue decompressing. If
	Z_DATA_ERROR is returned, the application may then call inflateSync() to look for a good compression block if a partial recovery of the
	data is desired.
}

		inflateEnd: function(var strm: stream): cint; cdecl;
{
		All dynamically allocated data structures for this stream are freed. This function discards any unprocessed input and does not flush
	any pending output.

		inflateEnd returns Z_OK if success, Z_STREAM_ERROR if the stream state was inconsistent. In the error case, msg may be set but then
	points to a static string (which must not be deallocated).
}

		deflateInit2: function(var strm: stream; level, method, windowBits, memLevel, strategy: cint; version: pchar;
		                       stream_size: cint): cint; cdecl;
{
		This is another version of deflateInit with more compression options. The fields next_in, zalloc, zfree and opaque must be initialized
	before by the caller.

		The method parameter is the compression method. It must be Z_DEFLATED in this version of the library.

		The windowBits parameter is the base two logarithm of the window size (the size of the history buffer). It should be in the range
	8..15 for this version of the library. Larger values of this parameter result in better compression at the expense of memory usage.
	The default value is 15 if deflateInit is used instead.

		windowBits can also be -8..-15 for raw deflate. In this case, -windowBits determines the window size. deflate() will then generate raw
	deflate data with no zlib header or trailer, and will not compute an adler32 check value.

		windowBits can also be greater than 15 for optional gzip encoding. Add 16 to windowBits to write a simple gzip header and trailer
	around the compressed data instead of a zlib wrapper. The gzip header will have no file name, no extra data, no comment, no modification
	time (set to zero), no header crc, and the operating system will be set to 255 (unknown). If a gzip stream is being written, strm->adler
	is a crc32 instead of an adler32.

		The memLevel parameter specifies how much memory should be allocated for the internal compression state. memLevel=1 uses minimum
	memory but is slow and reduces compression ratio; memLevel=9 uses maximum memory for optimal speed. The default value is 8. See zconf.h
	for total memory usage as a function of windowBits and memLevel.

		The strategy parameter is used to tune the compression algorithm. Use the value Z_DEFAULT_STRATEGY for normal data, Z_FILTERED for
	data produced by a filter (or predictor), Z_HUFFMAN_ONLY to force Huffman encoding only (no string match), or Z_RLE to limit match
	distances to one (run-length encoding). Filtered data consists mostly of small values with a somewhat random distribution. In this case,
	the compression algorithm is tuned to compress them better. The effect of Z_FILTERED is to force more Huffman coding and less string
	matching; it is somewhat intermediate between Z_DEFAULT and Z_HUFFMAN_ONLY. Z_RLE is designed to be almost as fast as Z_HUFFMAN_ONLY, but
	give better compression for PNG image data. The strategy parameter only affects the compression ratio but not the correctness of the
	compressed output even if it is not set appropriately. Z_FIXED prevents the use of dynamic Huffman codes, allowing for a simpler decoder
	for special applications.

		deflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if a parameter is invalid (such as
	an invalid method). msg is set to null if there is no error message. deflateInit2 does not perform any compression: this will be done by
	deflate().
}

		// deflateSetDictionary: function(var strm: z_stream; dictionary: pointer; dictLength: cuint): cint; cdecl;
{
		Initializes the compression dictionary from the given byte sequence without producing any compressed output. This function must be
	called immediately after deflateInit, deflateInit2 or deflateReset, before any call of deflate. The compressor and decompressor must use
	exactly the same dictionary (see inflateSetDictionary).

		The dictionary should consist of strings (byte sequences) that are likely to be encountered later in the data to be compressed, with
	the most commonly used strings preferably put towards the end of the dictionary. Using a dictionary is most useful when the data to be
	compressed is short and can be predicted with good accuracy; the data can then be compressed better than with the default empty dictionary.

		Depending on the size of the compression data structures selected by deflateInit or deflateInit2, a part of the dictionary may in
	effect be discarded, for example if the dictionary is larger than the window size in deflate or deflate2. Thus the strings most likely to
	be useful should be put at the end of the dictionary, not at the front. In addition, the current implementation of deflate will use at
	most the window size minus 262 bytes of the provided dictionary.

		Upon return of this function, strm->adler is set to the adler32 value of the dictionary; the decompressor may later use this value to
	determine which dictionary has been used by the compressor. (The adler32 value applies to the whole dictionary even if only a subset of
	the dictionary is actually used by the compressor.) If a raw deflate was requested, then the adler32 value is not computed and strm->adler
	is not set.

		deflateSetDictionary returns Z_OK if success, or Z_STREAM_ERROR if a parameter is invalid (such as NULL dictionary) or the stream state
	is inconsistent (for example if deflate has already been called for this stream or if the compression method is bsort).
	deflateSetDictionary does not perform any compression: this will be done by deflate().
}

		// deflateCopy: function(var dest, source: z_stream): cint; cdecl;
{
		Sets the destination stream as a complete copy of the source stream.

		This function can be useful when several compression strategies will be tried, for example when there are several ways of
	pre-processing the input data with a filter. The streams that will be discarded should then be freed by calling deflateEnd. Note that
	deflateCopy duplicates the internal compression state which can be quite large, so this strategy is slow and can consume lots of memory.

		deflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if the source stream state was
	inconsistent (such as zalloc being NULL). msg is left unchanged in both source and destination.
}

		// deflateReset: function(var strm: z_stream): cint; cdecl;
{
		This function is equivalent to deflateEnd followed by deflateInit, but does not free and reallocate all the internal compression
	state. The stream will keep the same compression level and any other attributes that may have been set by deflateInit2.

		deflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source stream state was inconsistent (such as zalloc or state being NULL).
}

		// deflateParams: function(var strm: z_stream; level: cint; strategy: cint): cint; cdecl;
{
		Dynamically update the compression level and compression strategy. The interpretation of level and strategy is as in deflateInit2.
	This can be used to switch between compression and straight copy of the input data, or to switch to a different kind of input data
	requiring a different strategy. If the compression level is changed, the input available so far is compressed with the old level (and may
	be flushed); the new level will take effect only at the next call of deflate().

		Before the call of deflateParams, the stream state must be set as for a call of deflate(), since the currently available input may
	have to be compressed and flushed. In particular, strm->avail_out must be non-zero.

		deflateParams returns Z_OK if success, Z_STREAM_ERROR if the source stream state was inconsistent or if a parameter was invalid,
	Z_BUF_ERROR if strm->avail_out was zero.
}

		deflateBound: function(var strm: stream; sourceLen: culong): culong; cdecl;
{
		deflateBound() returns an upper bound on the compressed size after deflation of sourceLen bytes. It must be called after deflateInit()
	or deflateInit2(). This would be used to allocate an output buffer for deflation in a single pass, and so would be called before deflate().
}

		inflateInit2: function(var strm: stream; windowBits: cint; version:pchar; stream_size: cint): cint; cdecl;
{
		This is another version of inflateInit with an extra parameter. The fields next_in, avail_in, zalloc, zfree and opaque must be
	initialized before by the caller.

		The windowBits parameter is the base two logarithm of the maximum window size (the size of the history buffer). It should be in the
	range 8..15 for this version of the library. The default value is 15 if inflateInit is used instead. windowBits must be greater than or
	equal to the windowBits value provided to deflateInit2() while compressing, or it must be equal to 15 if deflateInit2() was not used. If
	a compressed stream with a larger window size is given as input, inflate() will return with the error code Z_DATA_ERROR instead of trying
	to allocate a larger window.

		windowBits can also be -8..-15 for raw inflate. In this case, -windowBits determines the window size. inflate() will then process raw
	deflate data, not looking for a zlib or gzip header, not generating a check value, and not looking for any check values for comparison at
	the end of the stream. This is for use with other formats that use the deflate compressed data format such as zip. Those formats provide
	their own check values. If a custom format is developed using the raw deflate format for compressed data, it is recommended that a check
	value such as an adler32 or a crc32 be applied to the uncompressed data as is done in the zlib, gzip, and zip formats. For most applications,
	the zlib format should be used as is. Note that comments above on the use in deflateInit2() applies to the magnitude of windowBits.

		windowBits can also be greater than 15 for optional gzip decoding. Add 32 to windowBits to enable zlib and gzip decoding with automatic
	header detection, or add 16 to decode only the gzip format (the zlib format will return a Z_DATA_ERROR). If a gzip stream is being decoded,
	strm->adler is a crc32 instead of an adler32.

		inflateInit2 returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if a parameter is invalid (such as a
	null strm). msg is set to null if there is no error message. inflateInit2 does not perform any decompression apart from reading the zlib
	header if present: this will be done by inflate(). (So next_in and avail_in may be modified, but next_out and avail_out are unchanged.)
}

		// inflateSetDictionary: function(var strm: z_stream; dictionary: pointer; dictLength: cuint): cint; cdecl;
{
		Initializes the decompression dictionary from the given uncompressed byte sequence. This function must be called immediately after a
	call of inflate, if that call returned Z_NEED_DICT. The dictionary chosen by the compressor can be determined from the adler32 value
	returned by that call of inflate. The compressor and decompressor must use exactly the same dictionary (see deflateSetDictionary). For
	raw inflate, this function can be called immediately after inflateInit2() or inflateReset() and before any call of inflate() to set the
	dictionary. The application must insure that the dictionary that was used for compression is provided.

		inflateSetDictionary returns Z_OK if success, Z_STREAM_ERROR if a parameter is invalid (such as NULL dictionary) or the stream state
	is inconsistent, Z_DATA_ERROR if the given dictionary doesn't match the expected one (incorrect adler32 value). inflateSetDictionary does
	not perform any decompression: this will be done by subsequent calls of inflate().
}

		// inflateSync: function(var strm: z_stream): cint; cdecl;
{
		Skips invalid compressed data until a full flush point (see above the description of deflate with Z_FULL_FLUSH) can be found, or until
	all available input is skipped. No output is provided.

		inflateSync returns Z_OK if a full flush point has been found, Z_BUF_ERROR if no more input was provided, Z_DATA_ERROR if no flush
	point has been found, or Z_STREAM_ERROR if the stream structure was inconsistent. In the success case, the application may save the
	current current value of total_in which indicates where valid compressed data was found. In the error case, the application may repeatedly
	call inflateSync, providing more input each time, until success or end of the input data.
}

		// inflateCopy: function(var dest, source: z_stream): cint; cdecl;
{
		Sets the destination stream as a complete copy of the source stream.

		This function can be useful when randomly accessing a large stream. The first pass through the stream can periodically record the
	inflate state, allowing restarting inflate at those points when randomly accessing the stream.

		inflateCopy returns Z_OK if success, Z_MEM_ERROR if there was not enough memory, Z_STREAM_ERROR if the source stream state was
	inconsistent (such as zalloc being NULL). msg is left unchanged in both source and destination.
}

		// inflateReset: function(var strm: z_stream): cint; cdecl;
{
		This function is equivalent to inflateEnd followed by inflateInit, but does not free and reallocate all the internal decompression
	state. The stream will keep attributes that may have been set by inflateInit2.

		inflateReset returns Z_OK if success, or Z_STREAM_ERROR if the source stream state was inconsistent (such as zalloc or state being NULL).
}

		// inflateBackInit: function(var strm: z_stream; windowBits: cint; window: pointer; version: pchar; stream_size: cint): cint; cdecl;
{
		Initialize the internal stream state for decompression using inflateBack() calls. The fields zalloc, zfree and opaque in strm must be
	initialized before the call. If zalloc and zfree are Z_NULL, then the default library-derived memory allocation routines are used.
	windowBits is the base two logarithm of the window size, in the range 8..15. window is a caller supplied buffer of that size. Except for
	special applications where it is assured that deflate was used with small window sizes, windowBits must be 15 and a 32K byte window must
	be supplied to be able to decompress general deflate streams.

		See inflateBack() for the usage of these routines.

		inflateBackInit will return Z_OK on success, Z_STREAM_ERROR if any of the paramaters are invalid, Z_MEM_ERROR if the internal state
	could not be allocated, or Z_VERSION_ERROR if the version of the library does not match the version of the header file.
}

	type
		// in_func = function(in_desc: pointer; var c: pcuchar): cuint; cdecl;
		// out_func = function(out_desc: pointer; c: pcuchar; i: cuint): cint; cdecl;
	class var
		// inflateBack: function(var strm: z_stream; inf: in_func; in_desc: pointer; outf: out_func; out_desc: pointer): cint; cdecl;
{
		inflateBack() does a raw inflate with a single call using a call-back interface for input and output. This is more efficient than
	inflate() for file i/o applications in that it avoids copying between the output and the sliding window by simply making the window
	itself the output buffer. This function trusts the application to not change the output buffer passed by the output function, at least
	until inflateBack() returns.

		inflateBackInit() must be called first to allocate the internal state and to initialize the state with the user-provided window
	buffer. inflateBack() may then be used multiple times to inflate a complete, raw deflate stream with each call. inflateBackEnd() is then
	called to free the allocated state.

		A raw deflate stream is one with no zlib or gzip header or trailer. This routine would normally be used in a utility that reads zip or
	gzip files and writes out uncompressed files. The utility would decode the header and process the trailer on its own, hence this routine
	expects only the raw deflate stream to decompress. This is different from the normal behavior of inflate(), which expects either a zlib
	or gzip header and trailer around the deflate stream.

		inflateBack() uses two subroutines supplied by the caller that are then called by inflateBack() for input and output. inflateBack()
	calls those routines until it reads a complete deflate stream and writes out all of the uncompressed data, or until it encounters an
	error. The function's parameters and return types are defined above in the in_func and out_func typedefs. inflateBack() will call
	in(in_desc, &buf) which should return the number of bytes of provided input, and a pointer to that input in buf. If there is no input
	available, in() must return zero - buf is ignored in that case - and inflateBack() will return a buffer error. inflateBack() will call
	out(out_desc, buf, len) to write the uncompressed data buf[0..len-1]. out() should return zero on success, or non-zero on failure. If
	out() returns non-zero, inflateBack() will return with an error. Neither in() nor out() are permitted to change the contents of the
	window provided to inflateBackInit(), which is also the buffer that out() uses to write from. The length written by out() will be at most
	the window size. Any non-zero amount of input may be provided by in().

		For convenience, inflateBack() can be provided input on the first call by setting strm->next_in and strm->avail_in. If that input is
	exhausted, then in() will be called. Therefore strm->next_in must be initialized before calling inflateBack(). If strm->next_in is Z_NULL,
	then in() will be called immediately for input. If strm->next_in is not Z_NULL, then strm->avail_in must also be initialized, and then if
	strm->avail_in is not zero, input will initially be taken from strm->next_in[0 .. strm->avail_in - 1].

		The in_desc and out_desc parameters of inflateBack() is passed as the first parameter of in() and out() respectively when they are
	called. These descriptors can be optionally used to pass any information that the caller-supplied in() and out() functions need to do
	their job.

		On return, inflateBack() will set strm->next_in and strm->avail_in to pass back any unused input that was provided by the last in()
	call. The return values of inflateBack() can be Z_STREAM_END on success, Z_BUF_ERROR if in() or out() returned an error, Z_DATA_ERROR if
	there was a format error in the deflate stream (in which case strm->msg is set to indicate the nature of the error), or Z_STREAM_ERROR if
	the stream was not properly initialized. In the case of Z_BUF_ERROR, an input or output error can be distinguished using strm->next_in
	which will be Z_NULL only if in() returned an error. If strm->next is not Z_NULL, then the Z_BUF_ERROR was due to out() returning non-zero.
	(in() will always be called before out(), so strm->next_in is assured to be defined if out() returns non-zero.) Note that inflateBack()
	cannot return Z_OK.
}

		// inflateBackEnd: function(var strm: z_stream): cint; cdecl;
{
		All memory allocated by inflateBackInit() is freed.

		inflateBackEnd() returns Z_OK on success, or Z_STREAM_ERROR if the stream state was inconsistent.
}

		class function EmptyStream: stream;
		class function ErrorMessage(code: cint; const strm: stream): string;

	class var
		loader: DLLoader;
	end;

	lzo = class
	const
		OK                  = 0;
		ERROR               = -1;
		OUT_OF_MEMORY       = -2;
		NOT_COMPRESSIBLE    = -3;
		INPUT_OVERRUN       = -4;
		OUTPUT_OVERRUN      = -5;
		LOOKBEHIND_OVERRUN  = -6;
		EOF_NOT_FOUND       = -7;
		INPUT_NOT_CONSUMED  = -8;
		NOT_YET_IMPLEMENTED = -9;
		INVALID_ARGUMENT    = -10;
		COMPRESS_WORK_MEM = 8 * 16384 * sizeof(cshort);

	class var
		compress: function(src: pointer; srcLen: cuint; dst: pointer; var dstLen: cuint; workMem: pointer): cint; cdecl;
		decompress: function(src: pointer; srcLen: cuint; dst: pointer; var dstLen: cuint; workMem: pointer): cint; cdecl;

		class function bound(uncompressed: size_t): size_t;
		class function ErrorMessage(code: cint): string;

	class var
		loader: DLLoader;
	end;

	bzip2 = class
	type
		stream = record
			next_in: pointer;
			avail_in: cuint;
			total_in_lo32, total_in_hi32: cuint;

			next_out: pointer;
			avail_out: cuint;
			total_out_lo32, total_out_hi32: cuint;

			state: pointer;

			bzalloc: function(opaque: pointer; items, size: cint): pointer; cdecl;
			bzfree: procedure(opaque, block: pointer); cdecl;
			opaque: pointer;
		end;

	const
		RUN    = 0;
		FLUSH  = 1;
		FINISH = 2;

		OK              = 0;
		RUN_OK          = 1;
		FLUSH_OK        = 2;
		FINISH_OK       = 3;
		STREAM_END      = 4;
		SEQUENCE_ERROR  = -1;
		PARAM_ERROR     = -2;
		MEM_ERROR       = -3;
		DATA_ERROR      = -4;
		DATA_ERROR_MAGIC = -5;
		IO_ERROR        = -6;
		UNEXPECTED_EOF  = -7;
		OUTBUFF_FULL    = -8;
		CONFIG_ERROR    = -9;

	class var
		CompressInit: function(var strm: stream; blockSize100k, verbosity, workFactor: cint): cint; cdecl;
		Compress: function(var strm: stream; action: cint): cint; cdecl;
		CompressEnd: function(var strm: stream): cint; cdecl;
		DecompressInit: function(var strm: stream; verbosity, small: cint): cint; cdecl;
		Decompress: function(var strm: stream): cint; cdecl;
		DecompressEnd: function(var strm: stream): cint; cdecl;

		class function Bound(uncompressed: size_t): size_t;
		class function EmptyStream: stream;
		class function ErrorMessage(code: cint): string;

	class var
		loader: DLLoader;
	end;

	lzham = class
	const
		COMP_LEVEL_FASTEST = 0;
		COMP_LEVEL_FASTER  = 1;
		COMP_LEVEL_DEFAULT = 2;
		COMP_LEVEL_BETTER  = 3;
		COMP_LEVEL_UBER    = 4;

		COMP_STATUS_NOT_FINISHED        = 0;
		COMP_STATUS_NEEDS_MORE_INPUT    = 1;
		COMP_STATUS_HAS_MORE_OUTPUT     = 2;
		COMP_STATUS_SUCCESS             = 3;
		COMP_STATUS_FAILED              = 4;
		COMP_STATUS_FAILED_INITIALIZING = 5;
		COMP_STATUS_INVALID_PARAMETER   = 6;
		COMP_STATUS_OUTPUT_BUF_TOO_SMALL = 7;

		COMP_FLAG_FORCE_POLAR_CODING    = 1; // Forces Polar codes vs. Huffman, for a slight increase in decompression speed.
		COMP_FLAG_EXTREME_PARSING       = 2; // Improves ratio by allowing the compressor's parse graph to grow "higher" (up to 4 parent nodes per output node), but is much slower.
		COMP_FLAG_DETERMINISTIC_PARSING = 4; // Guarantees that the compressed output will always be the same given the same input and parameters (no variation between runs due to kernel threading scheduling).

		// If enabled, the compressor is free to use any optimizations which could lower the decompression rate (such
		// as adaptively resetting the Huffman table update rate to maximum frequency, which is costly for the decompressor).
		COMP_FLAG_TRADEOFF_DECOMPRESSION_RATE_FOR_COMP_RATIO = 16;
		COMP_FLAG_WRITE_ZLIB_STREAM = 32;

		// DECOMP_STATUS_NOT_FINISHED indicates that the decompressor is flushing its internal buffer to the caller's output buffer.
		// There may be more bytes available to decompress on the next call, but there is no guarantee.
		DECOMP_STATUS_NOT_FINISHED    = 0;

		// DECOMP_STATUS_HAS_MORE_OUTPUT indicates that the decompressor is trying to flush its internal buffer to the caller's output buffer,
		// but the caller hasn't provided any space to copy this data to the caller's output buffer. Call the decompress() again with a non-empty sized output buffer.
		DECOMP_STATUS_HAS_MORE_OUTPUT = 1;

		// DECOMP_STATUS_NEEDS_MORE_INPUT indicates that the decompressor has consumed all input bytes, has not encountered an "end of stream" code,
		// and the caller hasn't set no_more_input_bytes_flag to yes, so it's expecting more input to proceed.
		DECOMP_STATUS_NEEDS_MORE_INPUT = 2;

		// DECOMP_STATUS_SUCCESS indicates decompression has successfully completed.
		DECOMP_STATUS_SUCCESS = 3;

		// The remaining status codes indicate a failure of some sort. Most failures are unrecoverable. TODO: Document which codes are recoverable.
		DECOMP_STATUS_FAILED_INITIALIZING            = 4;
		DECOMP_STATUS_FAILED_DEST_BUF_TOO_SMALL      = 5;
		DECOMP_STATUS_FAILED_EXPECTED_MORE_RAW_BYTES = 6;
		DECOMP_STATUS_FAILED_BAD_CODE                = 7;
		DECOMP_STATUS_FAILED_ADLER32                 = 8;
		DECOMP_STATUS_FAILED_BAD_RAW_BLOCK           = 9;
		DECOMP_STATUS_FAILED_BAD_COMP_BLOCK_SYNC_CHECK = 10;
		DECOMP_STATUS_FAILED_BAD_ZLIB_HEADER         = 11;
		DECOMP_STATUS_FAILED_NEED_SEED_BYTES         = 12;
		DECOMP_STATUS_FAILED_BAD_SEED_BYTES          = 13;
		DECOMP_STATUS_FAILED_BAD_SYNC_BLOCK          = 14;
		DECOMP_STATUS_INVALID_PARAMETER              = 15;

		DECOMP_FLAG_OUTPUT_UNBUFFERED = 1;
		DECOMP_FLAG_COMPUTE_ADLER32   = 2;
		DECOMP_FLAG_READ_ZLIB_STREAM  = 4;

		MIN_DICT_SIZE_LOG2 = 15;
		MAX_DICT_SIZE_LOG2 = 26;

	type
		compress_state = record p: pointer; end;
		decompress_state = record p: pointer; end;

		pcompress_params = ^compress_params;
		compress_params = record
			struct_size: cuint;          // set to sizeof(compress_params)
			dict_size_log2: cuint;       // set to the log2(dictionary_size), must range between [MIN_DICT_SIZE_LOG2, MAX_DICT_SIZE_LOG2_X86] for x86 MAX_DICT_SIZE_LOG2_X64 for x64
			level: cuint;                // set to COMP_LEVEL_FASTEST, etc.
			max_helper_threads: cuint;   // max # of additional "helper" threads to create, must range between [0, MAX_HELPER_THREADS]
			cpucache_total_lines: cuint; // set to 0 (optimize compressed stream to avoid L1/L2 cache misses - not currently supported)
			cpucache_line_size: cuint;   // set to 0
			compress_flags: cuint;       // optional compression flags (see compress_flags enum)
			num_seed_bytes: cuint;       // for delta compression (optional) - number of seed bytes pointed to by m_pSeed_bytes
			pSeed_bytes: pointer;         // for delta compression (optional) - pointer to seed bytes buffer, must be at least m_num_seed_bytes long
		end;

		pdecompress_params = ^decompress_params;
		decompress_params = record
			struct_size: cuint;       // set to sizeof(decompress_params)
			dict_size_log2: cuint;    // set to the log2(dictionary_size), must range between [MIN_DICT_SIZE_LOG2, MAX_DICT_SIZE_LOG2_X86] for x86 MAX_DICT_SIZE_LOG2_X64 for x64
			decompress_flags: cuint;  // optional decompression flags (see decompress_flags enum)
			num_seed_bytes: cuint;    // for delta compression (optional) - number of seed bytes pointed to by m_pSeed_bytes
			pSeed_bytes: pointer;     // for delta compression (optional) - pointer to seed bytes buffer, must be at least m_num_seed_bytes long
		end;

	const
		DefaultCompressionParams: compress_params =
		(
			struct_size: sizeof(compress_params);
			dict_size_log2: 22; // 4 Mb
			level: COMP_LEVEL_UBER;
			max_helper_threads: 0;
			cpucache_total_lines: 0;
			cpucache_line_size: 0;
			compress_flags: {COMP_FLAG_EXTREME_PARSING}0;
			num_seed_bytes: 0;
			pSeed_bytes: nil;
		);

		DefaultDecompressionParams: decompress_params =
		(
			struct_size: sizeof(decompress_params);
			dict_size_log2: 22; // 4 Mb
			decompress_flags: 0;
			num_seed_bytes: 0;
			pSeed_bytes: nil;
		);

	class var
		// Initializes a compressor. Returns a pointer to the compressor's internal state, or NULL on failure.
		// pParams cannot be NULL. Be sure to initialize the pParams->m_struct_size member to sizeof(compress_params) (along with the other members to reasonable values) before calling this function.
		// TODO: With large dictionaries this function could take a while (due to memory allocation). I need to add a reinit() API for compression (decompression already has one).
		compress_init: function(params: pcompress_params): compress_state; cdecl;
		// compress_reinit: function(state: compress_state): compress_state; cdecl;

		// Deinitializes a compressor, releasing all allocated memory.
		// returns adler32 of source data (valid only on success).
		compress_deinit: function(state: compress_state): cuint32; cdecl;

		// Compresses an arbitrarily sized block of data, writing as much available compressed data as possible to the output buffer.
		// This method may be called as many times as needed, but for best perf. try not to call it with tiny buffers.
		// pState - Pointer to internal compression state, created by compress_init.
		// pIn_buf, pIn_buf_size - Pointer to input data buffer, and pointer to a size_t containing the number of bytes available in this buffer.
		//                         On return, *pIn_buf_size will be set to the number of bytes read from the buffer.
		// pOut_buf, pOut_buf_size - Pointer to the output data buffer, and a pointer to a size_t containing the max number of bytes that can be written to this buffer.
		//                         On return, *pOut_buf_size will be set to the number of bytes written to this buffer.
		// no_more_input_bytes_flag - Set to yes to indicate that no more input bytes are available to compress (EOF). Once you call this function with this param set to yes, it must stay set to yes in all future calls.
		//
		// Normal return status codes:
		//    COMP_STATUS_NOT_FINISHED - Compression can continue, but the compressor needs more input, or it needs more room in the output buffer.
		//    COMP_STATUS_NEEDS_MORE_INPUT - Compression can contintue, but the compressor has no more output, and has no input but we're not at EOF. Supply more input to continue.
		// Success/failure return status codes:
		//    COMP_STATUS_SUCCESS - Compression has completed successfully.
		//    COMP_STATUS_FAILED, COMP_STATUS_FAILED_INITIALIZING, COMP_STATUS_INVALID_PARAMETER - Something went wrong.
		compress: function(state: compress_state; pIn_buf: pointer; var pIn_buf_size: csize_t; pOut_buf: pointer; var pOut_buf_size: csize_t;
			no_more_input_bytes_flag: cuint): uint; cdecl;

		// Initializes a decompressor.
		// pParams cannot be NULL. Be sure to initialize the pParams->m_struct_size member to sizeof(decompress_params) (along with the other members to reasonable values) before calling this function.
		// Note: With large dictionaries this function could take a while (due to memory allocation). To serially decompress multiple streams, it's faster to init a compressor once and
		// reuse it using by calling decompress_reinit().
		decompress_init: function(params: pdecompress_params): decompress_state; cdecl;

		// Quickly re-initializes the decompressor to its initial state given an already allocated/initialized state (doesn't do any memory alloc unless necessary).
	{$ifdef ShareLzhamStates}
		decompress_reinit: function(state: decompress_state; params: pdecompress_params): decompress_state; cdecl;
	{$endif}

		// Deinitializes a decompressor.
		// returns adler32 of decompressed data if compute_adler32 was yes, otherwise it returns the adler32 from the compressed stream.
		decompress_deinit: function(state: decompress_state): uint32; cdecl;

		// Decompresses an arbitrarily sized block of compressed data, writing as much available decompressed data as possible to the output buffer.
		// This method is implemented as a coroutine so it may be called as many times as needed. However, for best perf. try not to call it with tiny buffers.
		// pState - Pointer to internal decompression state, originally created by decompress_init.
		// pIn_buf, pIn_buf_size - Pointer to input data buffer, and pointer to a size_t containing the number of bytes available in this buffer.
		//                         On return, *pIn_buf_size will be set to the number of bytes read from the buffer.
		// pOut_buf, pOut_buf_size - Pointer to the output data buffer, and a pointer to a size_t containing the max number of bytes that can be written to this buffer.
		//                         On return, *pOut_buf_size will be set to the number of bytes written to this buffer.
		// no_more_input_bytes_flag - Set to yes to indicate that no more input bytes are available to compress (EOF). Once you call this function with this param set to yes, it must stay set to yes in all future calls.
		// Notes:
		// In unbuffered mode, the output buffer MUST be large enough to hold the entire decompressed stream. Otherwise, you'll receive the
		//  DECOMP_STATUS_FAILED_DEST_BUF_TOO_SMALL error (which is currently unrecoverable during unbuffered decompression).
		// In buffered mode, if the output buffer's size is 0 bytes, the caller is indicating that no more output bytes are expected from the
		//  decompressor. In this case, if the decompressor actually has more bytes you'll receive the DECOMP_STATUS_FAILED_HAVE_MORE_OUTPUT
		//  error (which is recoverable in the buffered case - just call decompress() again with a non-zero size output buffer).
		decompress: function(state: decompress_state; pIn_buf: pointer; var pIn_buf_size: csize_t; pOut_buf: pointer; var pOut_buf_size: csize_t;
			no_more_input_bytes_flag: cuint): cuint; cdecl;

		class function bound(uncompressed: size_t): size_t;
		class function ErrorMessage(code: cuint; isCompress: boolean): string;
		class function BorrowDecompressionState: decompress_state;
		class procedure ReturnDecompressionState(state: decompress_state);

	class var
		loader: DLLoader;

	private class var
	{$ifdef ShareLzhamStates}
		states: array of decompress_state;
		timer: ThreadTimer;
		timerLock: ThreadLock;
		timerCancel: boolean;
	{$endif}
		class procedure Init;
		class procedure Done;
	end;

implementation

{$ifdef Debug}
uses
	ULog;
{$endif}

	function zAlloc(opaque: pointer; items, size: cuint): Pointer; cdecl;
	begin
		Assert(@opaque = @opaque);
		Result := GetMem(Items * Size);
		fillchar(result^, items * size, 0);
	end;

	procedure zFree(opaque, block: Pointer); cdecl;
	begin
		Assert(@opaque = @opaque);
		FreeMem(Block);
	end;

	class function z.EmptyStream: stream;
	const
		Empty: stream =
		(
			next_in: nil;  avail_in: 0;  total_in: 0;
			next_out: nil; avail_out: 0; total_out: 0;
			msg: nil; state: nil;
			zalloc: @zAlloc; zfree: @zFree; opaque: nil;
			data_type: 0; adler: 0; reserved: 0;
		);
	begin
		result := Empty;
	end;

	class function z.ErrorMessage(code: cint; const strm: stream): string;
	begin
		case code of
			STREAM_END:    result := 'Конец потока.';
			NEED_DICT:     result := 'Нужен словарь.';
			STREAM_ERROR:  result := 'Z_stream не валиден.';
			DATA_ERROR:    result := 'Данные повреждены.';
			MEM_ERROR:     result := 'Недостаточно памяти.';
			BUF_ERROR:     result := 'Нет места в буфере.';
			VERSION_ERROR: result := 'Несовместимая версия.';
			else result := UnknownErrorCodeMsg(code);
		end;
		if Assigned(strm.msg) then result += ' (' + string(strm.msg) + ').';
	end;

	class function lzo.bound(uncompressed: size_t): size_t;
	begin
		result := uncompressed + 32 + (uncompressed + 7) div 8;
	end;

	class function lzo.ErrorMessage(code: cint): string;
	begin
		case code of
			OUT_OF_MEMORY:       result := 'Недостаточно памяти.';
			NOT_COMPRESSIBLE:    result := 'Данные несжимаемы.';
			INPUT_OVERRUN:       result := 'Переполнение входного буфера.';
			OUTPUT_OVERRUN:      result := 'Переполнение выходного буфера.';
			LOOKBEHIND_OVERRUN:  result := 'Переполнение lookbehind.';
			EOF_NOT_FOUND:       result := 'EOF не найден.';
			INPUT_NOT_CONSUMED:  result := 'Данные недобработаны.';
			NOT_YET_IMPLEMENTED: result := 'Фича не реализована.';
			INVALID_ARGUMENT:    result := 'Неверный параметр.';
			else result := UnknownErrorCodeMsg(code);
		end;
	end;

	function bzAlloc(opaque: pointer; items, size: cint): pointer; cdecl;
	begin
		Assert(@opaque = @opaque);
		result := GetMem(items * size);
	end;

	class function bzip2.Bound(uncompressed: size_t): size_t;
	begin
		result := uncompressed + 512 + (uncompressed + 15) div 16;
	end;

	procedure bzFree(opaque, addr: pointer); cdecl;
	begin
		Assert(@opaque = @opaque);
		FreeMem(addr);
	end;

	class function bzip2.EmptyStream: stream;
	const
		Empty: stream =
		(
			next_in: nil;  avail_in: 0;  total_in_lo32: 0;  total_in_hi32: 0;
			next_out: nil; avail_out: 0; total_out_lo32: 0; total_out_hi32: 0;
			state: nil;
			bzalloc: @bzAlloc; bzfree: @bzFree; opaque: nil
		);
	begin
		result := Empty;
	end;

	class function bzip2.ErrorMessage(code: cint): string;
	begin
		case code of
			STREAM_END:     result := 'Конец потока.';
			SEQUENCE_ERROR: result := 'Операция недопустима в текущем состоянии потока.';
			PARAM_ERROR:    result := 'Неверный параметр.';
			MEM_ERROR:      result := 'Недостаточно памяти.';
			DATA_ERROR:     result := 'Данные повреждены.';
			DATA_ERROR_MAGIC: result := 'Неверная сигнатура.';
			CONFIG_ERROR:   result := 'Неверные настройки компиляции.';
			else            result := UnknownErrorCodeMsg(code);
		end;
	end;

	class function lzham.bound(uncompressed: size_t): size_t;
	begin
		result := uncompressed + 32 + (uncompressed + 63) div 64;
	end;

	class function lzham.ErrorMessage(code: cuint; isCompress: boolean): string;
	begin
		if isCompress then
			case code of
				COMP_STATUS_NOT_FINISHED:        result := 'Нужно больше данных / места.';
				COMP_STATUS_NEEDS_MORE_INPUT:    result := 'Нужно больше данных.';
				COMP_STATUS_HAS_MORE_OUTPUT:     result := 'Нужно больше места.';
				COMP_STATUS_FAILED:              result := 'Ошибка без обоснования.';
				COMP_STATUS_FAILED_INITIALIZING: result := 'Ошибка инициализации.';
				COMP_STATUS_INVALID_PARAMETER:   result := 'Неверный параметр.';
				else                                   result := UnknownErrorCodeMsg(code);
			end
		else
			case code of
				DECOMP_STATUS_NOT_FINISHED:           result := 'Нужно больше данных / места.';
				DECOMP_STATUS_FAILED_EXPECTED_MORE_RAW_BYTES: result := 'EXPECTED_MORE_RAW_BYTES.';
				DECOMP_STATUS_FAILED_BAD_CODE:        result := 'BAD_CODE.';
				DECOMP_STATUS_FAILED_ADLER32:         result := 'Adler32 не сходится.';
				DECOMP_STATUS_FAILED_BAD_RAW_BLOCK:   result := 'BAD_RAW_BLOCK.';
				DECOMP_STATUS_FAILED_BAD_COMP_BLOCK_SYNC_CHECK: result := 'BAD_COMP_BLOCK_SYNC_CHECK.';
				DECOMP_STATUS_FAILED_BAD_ZLIB_HEADER: result := 'BAD_ZLIB_HEADER.';
				DECOMP_STATUS_FAILED_NEED_SEED_BYTES: result := 'Не задано seed.';
				DECOMP_STATUS_FAILED_BAD_SEED_BYTES:  result := 'Неверное значение seed.';
				DECOMP_STATUS_FAILED_BAD_SYNC_BLOCK:  result := 'BAD_SYNC_BLOCK.';
				DECOMP_STATUS_INVALID_PARAMETER:      result := 'Неверный параметр.';
				else                                  result := UnknownErrorCodeMsg(code);
			end;
	end;

{$ifdef ShareLzhamStates}
	procedure LzhamDecompressStateTimer(var instance: ThreadTimer.CallbackInstance);
	var
		state: lzham.decompress_state;
	begin
		lzham.timerLock.Enter;
		state.p := nil;
		if not lzham.timerCancel then
		begin
			if length(lzham.states) > 0 then
			begin
		{$ifdef Debug} Log('Состояние LZHAM уничтожено.', logDebug); {$endif}
				state := lzham.states[High(lzham.states)];
				SetLength(lzham.states, length(lzham.states) - 1);
			end;
			if length(lzham.states) = 0 then
			begin
				lzham.timerCancel := yes;
				instance.Close;
			end;
		end;
		lzham.timerLock.Leave;

		if Assigned(state.p) then lzham.decompress_deinit(state);
	end;
{$endif}

	class function lzham.BorrowDecompressionState: decompress_state;
	begin
	{$ifdef ShareLzhamStates}
		result.p := nil;
		SingletonLock.Enter;
		timerLock.Enter;
		if length(states) > 0 then
		begin
			if length(states) = 1 then
			begin
				timerCancel := yes;
				timerLock.Leave;
				timer.Close;
				timerLock.Enter;
				timerCancel := no;
			end;
			result := states[High(states)];
			SetLength(states, length(states) - 1);
		end;
		timerLock.Leave;
		SingletonLock.Leave;

		if Assigned(result.p) then
			result := decompress_reinit(result, @DefaultDecompressionParams)
		else
		begin
	{$endif}

			result := decompress_init(@DefaultDecompressionParams);

	{$ifdef ShareLzhamStates}
		{$ifdef Debug} Log('Создано состояние LZHAM.', logDebug); {$endif}
		end;
	{$endif}
	end;

	class procedure lzham.ReturnDecompressionState(state: decompress_state);
	{$ifdef ShareLzhamStates}
	const
		Due = 5000;
		Period = 3000;
	var
		timerCreated: boolean;
	{$endif}
	begin
	{$ifdef ShareLzhamStates}
		SingletonLock.Enter;
		timerLock.Enter;
		SetLength(states, length(states) + 1);
		states[High(states)] := state;
		timerCreated := length(states) = 1;
		Assert(timerCreated = not timer.Valid);
		if timerCreated then ThreadTimer.Open(timer, @LzhamDecompressStateTimer, nil, Due, Period, [ThreadTimer.NonCritical]);
		timerLock.Leave;
		if not timerCreated then timer.Reset(Due, Period);
		SingletonLock.Leave;
	{$else}
		decompress_deinit(state);
	{$endif}
	end;

	class procedure lzham.Init;
	begin
	{$ifdef ShareLzhamStates}
		timer.Invalidate;
		timerLock.Init;
	{$endif}
	end;

	class procedure lzham.Done;
{$ifdef ShareLzhamStates} var i: sint; {$endif}
	begin
	{$ifdef ShareLzhamStates}
		timer.Close;
		timerLock.Done;
		for i := 0 to High(states) do
			decompress_deinit(states[i]);
		states := nil;
	{$endif}
	end;

	procedure DescribeZlibFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
   	.Func(@z.deflateInit2, 'deflateInit2_')^
		.Func(@z.deflate,      'deflate')^
		.Func(@z.deflateEnd,   'deflateEnd')^
		.Func(@z.deflateBound, 'deflateBound')^
		.Func(@z.inflateInit2, 'inflateInit2_')^
		.Func(@z.inflate,      'inflate')^
		.Func(@z.inflateEnd,   'inflateEnd');
	end;

	procedure DescribeLzoFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@lzo.compress,   '999_compress')^
		.Func(@lzo.decompress, 'decompress');
	end;

	procedure DescribeBzip2Functions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@bzip2.CompressInit,   'CompressInit')^
		.Func(@bzip2.Compress,       'Compress')^
		.Func(@bzip2.CompressEnd,    'CompressEnd')^
		.Func(@bzip2.DecompressInit, 'DecompressInit')^
		.Func(@bzip2.Decompress,     'Decompress')^
		.Func(@bzip2.DecompressEnd,  'DecompressEnd');
	end;

	procedure DescribeLzhamFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@lzham.compress_init,     'compress_init')^
		// .Func(@lzham.compress_reinit,   'compress_reinit')^
		.Func(@lzham.compress_deinit,   'compress_deinit')^
		.Func(@lzham.compress,          'compress')^
		.Func(@lzham.decompress_init,   'decompress_init')^
	{$ifdef ShareLzhamStates}.Func(@lzham.decompress_reinit, 'decompress_reinit')^ {$endif}
		.Func(@lzham.decompress_deinit, 'decompress_deinit')^
		.Func(@lzham.decompress,        'decompress');
	end;

	procedure Init;
	begin
		z.loader.Init('z(perm)', @DescribeZlibFunctions);
		lzo.loader.Init('lzo(prefix = lzo2a_, perm)', @DescribeLzoFunctions);
		bzip2.loader.Init('bzip2(prefix = BZ2_bz, perm)', @DescribeBzip2Functions);
		lzham.loader.Init('lzham(prefix = lzham_, perm)', @DescribeLzhamFunctions);
		lzham.Init;
	end;

	procedure Done;
	begin
		lzham.Done;
		lzham.loader.Done;
		bzip2.loader.Done;
		lzo.loader.Done;
		z.loader.Done;
	end;

initialization
	&Unit('Zlib').Initialize(@Init, @Done);
end.
