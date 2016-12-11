unit BassLib;

{$include opts.inc}
{$ifdef Windows}
	{$define basscall:=stdcall}
{$else}
	{$define basscall:=cdecl}
{$endif}

interface

uses
{$ifdef Windows} Windows, {$endif}
	ctypes, USystem, Errors, UMath, DynamicLoader, Utils, Human {$ifdef Debug}, ULog, Streams {$endif};

type
	Bass = class
	const
		// Use these to test for error from functions that return a DWORD or QWORD
		DW_ERROR = DWord(-1); // -1 (DWORD)
		QW_ERROR = QWord(-1); // -1 (QWORD)

		// Error codes returned by ErrorGetCode()
		OK                 = 0;    // all is OK
		ERROR_MEM          = 1;    // memory error
		ERROR_FILEOPEN     = 2;    // can't open the file
		ERROR_DRIVER       = 3;    // can't find a free sound driver
		ERROR_BUFLOST      = 4;    // the sample buffer was lost
		ERROR_HANDLE       = 5;    // invalid handle
		ERROR_FORMAT       = 6;    // unsupported sample format
		ERROR_POSITION     = 7;    // invalid position
		ERROR_INIT         = 8;    // Init() has not been successfully called
		ERROR_START        = 9;    // Start() has not been successfully called
		ERROR_ALREADY      = 14;   // already initialized/paused/whatever
		ERROR_NOCHAN       = 18;   // can't get a free channel
		ERROR_ILLTYPE      = 19;   // an illegal type was specified
		ERROR_ILLPARAM     = 20;   // an illegal parameter was specified
		ERROR_NO3D         = 21;   // no 3D support
		ERROR_NOEAX        = 22;   // no EAX support
		ERROR_DEVICE       = 23;   // illegal device number
		ERROR_NOPLAY       = 24;   // not playing
		ERROR_FREQ         = 25;   // illegal sample rate
		ERROR_NOTFILE      = 27;   // the stream is not a file stream
		ERROR_NOHW         = 29;   // no hardware voices available
		ERROR_EMPTY        = 31;   // the MOD music has no sequence data
		ERROR_NONET        = 32;   // no internet connection could be opened
		ERROR_CREATE       = 33;   // couldn't create the file
		ERROR_NOFX         = 34;   // effects are not available
		ERROR_NOTAVAIL     = 37;   // requested data is not available
		ERROR_DECODE       = 38;   // the channel is/isn't a "decoding channel"
		ERROR_DX           = 39;   // a sufficient DirectX version is not installed
		ERROR_TIMEOUT      = 40;   // connection timedout
		ERROR_FILEFORM     = 41;   // unsupported file format
		ERROR_SPEAKER      = 42;   // unavailable speaker
		ERROR_VERSION      = 43;   // invalid BASS version (used by add-ons)
		ERROR_CODEC        = 44;   // codec is not available/supported
		ERROR_ENDED        = 45;   // the channel/file has ended
		ERROR_BUSY         = 46;   // the device is busy
		ERROR_UNKNOWN      = -1;   // some other mystery problem

		// SetConfig options
		CONFIG_BUFFER        = 0;
		CONFIG_UPDATEPERIOD  = 1;
		CONFIG_GVOL_SAMPLE   = 4;
		CONFIG_GVOL_STREAM   = 5;
		CONFIG_GVOL_MUSIC    = 6;
		CONFIG_CURVE_VOL     = 7;
		CONFIG_CURVE_PAN     = 8;
		CONFIG_FLOATDSP      = 9;
		CONFIG_3DALGORITHM   = 10;
		CONFIG_NET_TIMEOUT   = 11;
		CONFIG_NET_BUFFER    = 12;
		CONFIG_PAUSE_NOPLAY  = 13;
		CONFIG_NET_PREBUF    = 15;
		CONFIG_NET_PASSIVE   = 18;
		CONFIG_REC_BUFFER    = 19;
		CONFIG_NET_PLAYLIST  = 21;
		CONFIG_MUSIC_VIRTUAL = 22;
		CONFIG_VERIFY        = 23;
		CONFIG_UPDATETHREADS = 24;
		CONFIG_DEV_BUFFER    = 27;
		CONFIG_VISTA_TRUEPOS = 30;
		CONFIG_IOS_MIXAUDIO  = 34;
		CONFIG_DEV_DEFAULT   = 36;
		CONFIG_NET_READTIMEOUT = 37;
		CONFIG_VISTA_SPEAKERS = 38;
		CONFIG_IOS_SPEAKER   = 39;
		CONFIG_MF_DISABLE    = 40;
		CONFIG_HANDLES       = 41;
		CONFIG_UNICODE       = 42;
		CONFIG_SRC           = 43;
		CONFIG_SRC_SAMPLE    = 44;
		CONFIG_ASYNCFILE_BUFFER = 45;
		CONFIG_OGG_PRESCAN   = 47;
		CONFIG_MF_VIDEO      = 48;
		CONFIG_AIRPLAY       = 49;
		CONFIG_DEV_NONSTOP   = 50;
		CONFIG_IOS_NOCATEGORY = 51;
		CONFIG_VERIFY_NET    = 52;
		CONFIG_DEV_PERIOD    = 53;
		CONFIG_FLOAT         = 54;

		// SetConfigPtr options
		CONFIG_NET_AGENT     = 16;
		CONFIG_NET_PROXY     = 17;

		// Init() flags
		DEVICE_8BITS       = 1;    // 8 bit
		DEVICE_MONO        = 2;    // mono
		DEVICE_3D          = 4;    // enable 3D functionality
		DEVICE_16BITS      = 8;    // limit output to 16 bit
		DEVICE_LATENCY     = $100;  // calculate device latency (INFO struct)
		DEVICE_CPSPEAKERS  = $400; // detect speakers via Windows control panel
		DEVICE_SPEAKERS    = $800; // force enabling of speaker assignment
		DEVICE_NOSPEAKER   = $1000; // ignore speaker arrangement
		DEVICE_DMIX        = $2000; // use ALSA "dmix" plugin
		DEVICE_FREQ        = $4000; // set device sample rate
		DEVICE_STEREO      = $8000; // limit output to stereo

		// DirectSound interfaces (for use with GetDSoundObject)
		OBJECT_DS          = 1;   // IDirectSound
		OBJECT_DS3DL       = 2;   // IDirectSound3DListener

		// DEVICEINFO flags
		DEVICE_ENABLED     = 1;
		DEVICE_DEFAULT     = 2;
		DEVICE_INIT        = 4;

		DEVICE_TYPE_MASK        = $ff000000;
		DEVICE_TYPE_NETWORK     = $01000000;
		DEVICE_TYPE_SPEAKERS    = $02000000;
		DEVICE_TYPE_LINE        = $03000000;
		DEVICE_TYPE_HEADPHONES  = $04000000;
		DEVICE_TYPE_MICROPHONE  = $05000000;
		DEVICE_TYPE_HEADSET     = $06000000;
		DEVICE_TYPE_HANDSET     = $07000000;
		DEVICE_TYPE_DIGITAL     = $08000000;
		DEVICE_TYPE_SPDIF       = $09000000;
		DEVICE_TYPE_HDMI        = $0a000000;
		DEVICE_TYPE_DISPLAYPORT = $40000000;

		// GetDeviceInfo flags
		DEVICES_AIRPLAY         = $1000000;

		// INFO flags (from DSOUND.H)
		DSCAPS_CONTINUOUSRATE   = $00000010;     // supports all sample rates between min/maxrate
		DSCAPS_EMULDRIVER       = $00000020;     // device does NOT have hardware DirectSound support
		DSCAPS_CERTIFIED        = $00000040;     // device driver has been certified by Microsoft
		DSCAPS_SECONDARYMONO    = $00000100;     // mono
		DSCAPS_SECONDARYSTEREO  = $00000200;     // stereo
		DSCAPS_SECONDARY8BIT    = $00000400;     // 8 bit
		DSCAPS_SECONDARY16BIT   = $00000800;     // 16 bit

		// RECORDINFO flags (from DSOUND.H)
		DSCCAPS_EMULDRIVER = DSCAPS_EMULDRIVER;  // device does NOT have hardware DirectSound recording support
		DSCCAPS_CERTIFIED = DSCAPS_CERTIFIED;    // device driver has been certified by Microsoft

		// defines for formats field of RECORDINFO (from MMSYSTEM.H)
		WAVE_FORMAT_1M08       = $00000001;      // 11.025 kHz, Mono,   8-bit
		WAVE_FORMAT_1S08       = $00000002;      // 11.025 kHz, Stereo, 8-bit
		WAVE_FORMAT_1M16       = $00000004;      // 11.025 kHz, Mono,   16-bit
		WAVE_FORMAT_1S16       = $00000008;      // 11.025 kHz, Stereo, 16-bit
		WAVE_FORMAT_2M08       = $00000010;      // 22.05  kHz, Mono,   8-bit
		WAVE_FORMAT_2S08       = $00000020;      // 22.05  kHz, Stereo, 8-bit
		WAVE_FORMAT_2M16       = $00000040;      // 22.05  kHz, Mono,   16-bit
		WAVE_FORMAT_2S16       = $00000080;      // 22.05  kHz, Stereo, 16-bit
		WAVE_FORMAT_4M08       = $00000100;      // 44.1   kHz, Mono,   8-bit
		WAVE_FORMAT_4S08       = $00000200;      // 44.1   kHz, Stereo, 8-bit
		WAVE_FORMAT_4M16       = $00000400;      // 44.1   kHz, Mono,   16-bit
		WAVE_FORMAT_4S16       = $00000800;      // 44.1   kHz, Stereo, 16-bit

		SAMPLE_8BITS       = 1;   // 8 bit
		SAMPLE_FLOAT       = 256; // 32 bit floating-point
		SAMPLE_MONO        = 2;   // mono
		SAMPLE_LOOP        = 4;   // looped
		SAMPLE_3D          = 8;   // 3D functionality
		SAMPLE_SOFTWARE    = 16;  // not using hardware mixing
		SAMPLE_MUTEMAX     = 32;  // mute at max distance (3D only)
		SAMPLE_OVER_VOL    = $10000; // override lowest volume
		SAMPLE_OVER_POS    = $20000; // override longest playing
		SAMPLE_OVER_DIST   = $30000; // override furthest from listener (3D only)

		STREAM_PRESCAN     = $20000; // enable pin-point seeking/length (MP3/MP2/MP1)
		MP3_SETPOS         = STREAM_PRESCAN;
		STREAM_AUTOFREE    = $40000; // automatically free the stream when it stop/ends
		STREAM_RESTRATE    = $80000; // restrict the download rate of internet file streams
		STREAM_BLOCK       = $100000;// download/play internet file stream in small blocks
		STREAM_DECODE      = $200000;// don't play the stream, only decode (ChannelGetData)
		STREAM_STATUS      = $800000;// give server status info (HTTP/ICY tags) in DOWNLOADPROC

		MUSIC_FLOAT        = SAMPLE_FLOAT;
		MUSIC_MONO         = SAMPLE_MONO;
		MUSIC_LOOP         = SAMPLE_LOOP;
		MUSIC_3D           = SAMPLE_3D;
		MUSIC_AUTOFREE     = STREAM_AUTOFREE;
		MUSIC_DECODE       = STREAM_DECODE;
		MUSIC_PRESCAN      = STREAM_PRESCAN; // calculate playback length
		MUSIC_CALCLEN      = MUSIC_PRESCAN;
		MUSIC_RAMP         = $200;  // normal ramping
		MUSIC_RAMPS        = $400;  // sensitive ramping
		MUSIC_SURROUND     = $800;  // surround sound
		MUSIC_SURROUND2    = $1000; // surround sound (mode 2)
		MUSIC_FT2PAN       = $2000; // apply FastTracker 2 panning to XM files
		MUSIC_FT2MOD       = $2000; // play .MOD as FastTracker 2 does
		MUSIC_PT1MOD       = $4000; // play .MOD as ProTracker 1 does
		MUSIC_NONINTER     = $10000; // non-interpolated sample mixing
		MUSIC_SINCINTER    = $800000; // sinc interpolated sample mixing
		MUSIC_POSRESET     = $8000; // stop all notes when moving position
		MUSIC_POSRESETEX   = $400000; // stop all notes and reset bmp/etc when moving position
		MUSIC_STOPBACK     = $80000; // stop the music on a backwards jump effect
		MUSIC_NOSAMPLE     = $100000; // don't load the samples

		// Speaker assignment flags
		SPEAKER_FRONT      = $1000000;  // front speakers
		SPEAKER_REAR       = $2000000;  // rear/side speakers
		SPEAKER_CENLFE     = $3000000;  // center & LFE speakers (5.1)
		SPEAKER_REAR2      = $4000000;  // rear center speakers (7.1)
		SPEAKER_LEFT       = $10000000; // modifier: left
		SPEAKER_RIGHT      = $20000000; // modifier: right
		SPEAKER_FRONTLEFT  = SPEAKER_FRONT or SPEAKER_LEFT;
		SPEAKER_FRONTRIGHT = SPEAKER_FRONT or SPEAKER_RIGHT;
		SPEAKER_REARLEFT   = SPEAKER_REAR or SPEAKER_LEFT;
		SPEAKER_REARRIGHT  = SPEAKER_REAR or SPEAKER_RIGHT;
		SPEAKER_CENTER     = SPEAKER_CENLFE or SPEAKER_LEFT;
		SPEAKER_LFE        = SPEAKER_CENLFE or SPEAKER_RIGHT;
		SPEAKER_REAR2LEFT  = SPEAKER_REAR2 or SPEAKER_LEFT;
		SPEAKER_REAR2RIGHT = SPEAKER_REAR2 or SPEAKER_RIGHT;

		ASYNCFILE          = $40000000;
		UNICODE            = $80000000;

		RECORD_PAUSE       = $8000; // start recording paused

		// CHANNELINFO types
		CTYPE_SAMPLE       = 1;
		CTYPE_RECORD       = 2;
		CTYPE_STREAM       = $10000;
		CTYPE_STREAM_OGG   = $10002;
		CTYPE_STREAM_MP1   = $10003;
		CTYPE_STREAM_MP2   = $10004;
		CTYPE_STREAM_MP3   = $10005;
		CTYPE_STREAM_AIFF  = $10006;
		CTYPE_STREAM_WAV   = $40000; // WAVE flag, LOWORD=codec
		CTYPE_STREAM_WAV_PCM = $50001;
		CTYPE_STREAM_WAV_FLOAT = $50003;
		CTYPE_MUSIC_MOD    = $20000;
		CTYPE_MUSIC_MTM    = $20001;
		CTYPE_MUSIC_S3M    = $20002;
		CTYPE_MUSIC_XM     = $20003;
		CTYPE_MUSIC_IT     = $20004;
		CTYPE_MUSIC_MO3    = $00100; // MO3 flag
		CTYPE_STREAM_FLAC  = 67840;
		CTYPE_STREAM_AAC   = 68352;

		// 3D channel modes
		_3DMODE_NORMAL      = 0; // normal 3D processing
		_3DMODE_RELATIVE    = 1; // position is relative to the listener
		_3DMODE_OFF         = 2; // no 3D processing

		// software 3D mixing algorithms (used with CONFIG_3DALGORITHM)
		_3DALG_DEFAULT      = 0;
		_3DALG_OFF          = 1;
		_3DALG_FULL         = 2;
		_3DALG_LIGHT        = 3;

		// EAX environments, use with SetEAXParameters
		EAX_ENVIRONMENT_GENERIC           = 0;
		EAX_ENVIRONMENT_PADDEDCELL        = 1;
		EAX_ENVIRONMENT_ROOM              = 2;
		EAX_ENVIRONMENT_BATHROOM          = 3;
		EAX_ENVIRONMENT_LIVINGROOM        = 4;
		EAX_ENVIRONMENT_STONEROOM         = 5;
		EAX_ENVIRONMENT_AUDITORIUM        = 6;
		EAX_ENVIRONMENT_CONCERTHALL       = 7;
		EAX_ENVIRONMENT_CAVE              = 8;
		EAX_ENVIRONMENT_ARENA             = 9;
		EAX_ENVIRONMENT_HANGAR            = 10;
		EAX_ENVIRONMENT_CARPETEDHALLWAY   = 11;
		EAX_ENVIRONMENT_HALLWAY           = 12;
		EAX_ENVIRONMENT_STONECORRIDOR     = 13;
		EAX_ENVIRONMENT_ALLEY             = 14;
		EAX_ENVIRONMENT_FOREST            = 15;
		EAX_ENVIRONMENT_CITY              = 16;
		EAX_ENVIRONMENT_MOUNTAINS         = 17;
		EAX_ENVIRONMENT_QUARRY            = 18;
		EAX_ENVIRONMENT_PLAIN             = 19;
		EAX_ENVIRONMENT_PARKINGLOT        = 20;
		EAX_ENVIRONMENT_SEWERPIPE         = 21;
		EAX_ENVIRONMENT_UNDERWATER        = 22;
		EAX_ENVIRONMENT_DRUGGED           = 23;
		EAX_ENVIRONMENT_DIZZY             = 24;
		EAX_ENVIRONMENT_PSYCHOTIC         = 25;
		// total number of environments
		EAX_ENVIRONMENT_COUNT             = 26;

		STREAMPROC_END = $80000000; // end of user stream flag

		// StreamCreateFileUser file systems
		STREAMFILE_NOBUFFER     = 0;
		STREAMFILE_BUFFER       = 1;
		STREAMFILE_BUFFERPUSH   = 2;

		// StreamPutFileData options
		FILEDATA_END       = 0; // end & close the file

		// StreamGetFilePosition modes
		FILEPOS_CURRENT    = 0;
		FILEPOS_DECODE     = FILEPOS_CURRENT;
		FILEPOS_DOWNLOAD   = 1;
		FILEPOS_END        = 2;
		FILEPOS_START      = 3;
		FILEPOS_CONNECTED  = 4;
		FILEPOS_BUFFER     = 5;
		FILEPOS_SOCKET     = 6;
		FILEPOS_ASYNCBUF   = 7;
		FILEPOS_SIZE       = 8;

		// ChannelSetSync types
		SYNC_POS           = 0;
		SYNC_END           = 2;
		SYNC_META          = 4;
		SYNC_SLIDE         = 5;
		SYNC_STALL         = 6;
		SYNC_DOWNLOAD      = 7;
		SYNC_FREE          = 8;
		SYNC_SETPOS        = 11;
		SYNC_MUSICPOS      = 10;
		SYNC_MUSICINST     = 1;
		SYNC_MUSICFX       = 3;
		SYNC_OGG_CHANGE    = 12;
		SYNC_MIXTIME       = $40000000; // flag: sync at mixtime, else at playtime
		SYNC_ONETIME       = $80000000; // flag: sync only once, else continuously

		// ChannelIsActive return values
		ACTIVE_STOPPED = 0;
		ACTIVE_PLAYING = 1;
		ACTIVE_STALLED = 2;
		ACTIVE_PAUSED  = 3;

		// Channel attributes
		ATTRIB_FREQ                  = 1;
		ATTRIB_VOL                   = 2;
		ATTRIB_PAN                   = 3;
		ATTRIB_EAXMIX                = 4;
		ATTRIB_NOBUFFER              = 5;
		ATTRIB_VBR                   = 6;
		ATTRIB_CPU                   = 7;
		ATTRIB_SRC                   = 8;
		ATTRIB_NET_RESUME            = 9;
		ATTRIB_SCANINFO              = 10;
		ATTRIB_NORAMP                = 11;
		ATTRIB_BITRATE               = 12;
		ATTRIB_MUSIC_AMPLIFY         = $100;
		ATTRIB_MUSIC_PANSEP          = $101;
		ATTRIB_MUSIC_PSCALER         = $102;
		ATTRIB_MUSIC_BPM             = $103;
		ATTRIB_MUSIC_SPEED           = $104;
		ATTRIB_MUSIC_VOL_GLOBAL      = $105;
		ATTRIB_MUSIC_ACTIVE          = $106;
		ATTRIB_MUSIC_VOL_CHAN        = $200; // + channel #
		ATTRIB_MUSIC_VOL_INST        = $300; // + instrument #

		// ChannelGetData flags
		DATA_AVAILABLE = 0;        // query how much data is buffered
		DATA_FIXED     = $20000000; // flag: return 8.24 fixed-point data
		DATA_FLOAT     = $40000000; // flag: return floating-point sample data
		DATA_FFT256    = $80000000; // 256 sample FFT
		DATA_FFT512    = $80000001; // 512 FFT
		DATA_FFT1024   = $80000002; // 1024 FFT
		DATA_FFT2048   = $80000003; // 2048 FFT
		DATA_FFT4096   = $80000004; // 4096 FFT
		DATA_FFT8192   = $80000005; // 8192 FFT
		DATA_FFT16384  = $80000006; // 16384 FFT
		DATA_FFT32768  = $80000007; // 32768 FFT
		DATA_FFT_INDIVIDUAL = $10; // FFT flag: FFT for each channel, else all combined
		DATA_FFT_NOWINDOW = $20;   // FFT flag: no Hanning window
		DATA_FFT_REMOVEDC = $40;   // FFT flag: pre-remove DC bias
		DATA_FFT_COMPLEX = $80;    // FFT flag: return complex data

		// ChannelGetLevelEx flags
		LEVEL_MONO     = 1;
		LEVEL_STEREO   = 2;
		LEVEL_RMS      = 4;

		// ChannelGetLength/GetPosition/SetPosition modes
		POS_BYTE           = 0; // byte position
		POS_MUSIC_ORDER    = 1; // order.row position, MAKELONG(order,row)
		POS_OGG            = 3; // OGG bitstream number
		POS_INEXACT        = $8000000; // flag: allow seeking to inexact position
		POS_DECODE         = $10000000; // flag: get the decoding (not playing) position
		POS_DECODETO       = $20000000; // flag: decode to the position instead of seeking
		POS_SCAN           = $40000000; // flag: scan to the position

		// RecordSetInput flags
		INPUT_OFF    = $10000;
		INPUT_ON     = $20000;

		INPUT_TYPE_MASK    = $FF000000;
		INPUT_TYPE_UNDEF   = $00000000;
		INPUT_TYPE_DIGITAL = $01000000;
		INPUT_TYPE_LINE    = $02000000;
		INPUT_TYPE_MIC     = $03000000;
		INPUT_TYPE_SYNTH   = $04000000;
		INPUT_TYPE_CD      = $05000000;
		INPUT_TYPE_PHONE   = $06000000;
		INPUT_TYPE_SPEAKER = $07000000;
		INPUT_TYPE_WAVE    = $08000000;
		INPUT_TYPE_AUX     = $09000000;
		INPUT_TYPE_ANALOG  = $0A000000;

	type
		Handle = DWORD;
		HMUSIC = DWORD;       // MOD music handle
		HSAMPLE = DWORD;      // sample handle
		HCHANNEL = DWORD;     // playing sample's channel handle
		HSTREAM = DWORD;      // sample stream handle
		HRECORD = DWORD;      // recording handle
		HSYNC = DWORD;        // synchronizer handle
		HDSP = DWORD;         // DSP handle
		HPLUGIN = DWORD;      // Plugin handle

		// Device info structure
		DEVICEINFO = record
			name: PAnsiChar;    // description
			driver: PAnsiChar;  // driver
			flags: DWORD;
		end;

		INFO = record
			flags: DWORD;       // device capabilities (DSCAPS_xxx flags)
			hwsize: DWORD;      // size of total device hardware memory
			hwfree: DWORD;      // size of free device hardware memory
			freesam: DWORD;     // number of free sample slots in the hardware
			free3d: DWORD;      // number of free 3D sample slots in the hardware
			minrate: DWORD;     // min sample rate supported by the hardware
			maxrate: DWORD;     // max sample rate supported by the hardware
			eax: LongBool;          // device supports EAX? (always FALSE if DEVICE_3D was not used)
			minbuf: DWORD;      // recommended minimum buffer length in ms (requires DEVICE_LATENCY)
			dsver: DWORD;       // DirectSound version
			latency: DWORD;     // delay (in ms) before start of playback (requires DEVICE_LATENCY)
			initflags: DWORD;   // Init "flags" parameter
			speakers: DWORD;    // number of speakers available
			freq: DWORD;        // current output rate
		end;

		// Recording device info structure
		RECORDINFO = record
			flags: DWORD;       // device capabilities (DSCCAPS_xxx flags)
			formats: DWORD;     // supported standard formats (WAVE_FORMAT_xxx flags)
			inputs: DWORD;      // number of inputs
			singlein: LongBool;     // only 1 input can be set at a time
			freq: DWORD;        // current input rate
		end;

		// Sample info structure
		SAMPLE = record
			freq: DWORD;        // default playback rate
			volume: Single;     // default volume (0-100)
			pan: Single;        // default pan (-100=left, 0=middle, 100=right)
			flags: DWORD;       // SAMPLE_xxx flags
			length: DWORD;      // length (in samples, not bytes)
			max: DWORD;         // maximum simultaneous playbacks
			origres: DWORD;     // original resolution
			chans: DWORD;       // number of channels
			mingap: DWORD;      // minimum gap (ms) between creating channels
			mode3d: DWORD;      // 3DMODE_xxx mode
			mindist: Single;    // minimum distance
			maxdist: Single;    // maximum distance
			iangle: DWORD;      // angle of inside projection cone
			oangle: DWORD;      // angle of outside projection cone
			outvol: Single;     // delta-volume outside the projection cone
			vam: DWORD;         // voice allocation/management flags (VAM_xxx)
			priority: DWORD;    // priority (0=lowest, $ffffffff=highest)
		end;

		// Channel info structure
		CHANNELINFO = record
			freq: DWORD;        // default playback rate
			chans: DWORD;       // channels
			flags: DWORD;       // SAMPLE/STREAM/MUSIC/SPEAKER flags
			ctype: DWORD;       // type of channel
			origres: DWORD;     // original resolution
			plugin: HPLUGIN;    // plugin
			sample: HSAMPLE;    // sample
			{$IFDEF CPUX64}
			padding: DWORD;
			{$ENDIF}
			filename: PChar;    // filename
		end;

		PLUGINFORM = record
			ctype: DWORD;       // channel type
			{$IFDEF CPUX64}
			padding: DWORD;
			{$ENDIF}
			name: PAnsiChar;    // format description
			exts: PAnsiChar;    // file extension filter (*.ext1;*.ext2;etc...)
		end;
		PPLUGINFORMS = ^TPLUGINFORMS;
		TPLUGINFORMS = array[0..maxInt div sizeOf(PLUGINFORM) - 1] of PLUGINFORM;

		PPLUGININFO = ^PLUGININFO;
		PLUGININFO = record
			version: DWORD;             // version (same form as GetVersion)
			formatc: DWORD;             // number of formats
			formats: PPLUGINFORMS; // the array of formats
		end;

		p3DVECTOR = ^_3DVECTOR;
		_3DVECTOR = record     // 3D vector (for 3D positions/velocities/orientations)
			x: Single;          // +=right, -=left
			y: Single;          // +=up, -=down
			z: Single;          // +=front, -=behind
		end;

		// User file stream callback functions
		FILECLOSEPROC = procedure(user: Pointer); basscall;
		FILELENPROC = function(user: Pointer): QWORD; basscall;
		FILEREADPROC = function(buffer: Pointer; length: DWORD; user: Pointer): DWORD; basscall;
		FILESEEKPROC = function(offset: QWORD; user: Pointer): LongBool; basscall;

		FILEPROCS = record
			close: FILECLOSEPROC;
			length: FILELENPROC;
			read: FILEREADPROC;
			seek: FILESEEKPROC;
		end;

		// callback function types
		STREAMPROC = function(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD; basscall;
		{
			User stream callback function. NOTE: A stream function should obviously be as
			quick as possible, other streams (and MOD musics) can't be mixed until
			it's finished.
			handle : The stream that needs writing
			buffer : Buffer to write the samples in
			length : Number of bytes to write
			user   : The 'user' parameter value given when calling StreamCreate
			RETURN : Number of bytes written. Set the STREAMPROC_END flag to end
			         the stream.
		}

	const
		// special STREAMPROCs
		STREAMPROC_DUMMY = Pointer(0);   // "dummy" stream
		STREAMPROC_PUSH = Pointer(-1);   // push stream

	type

		DOWNLOADPROC = procedure(buffer: Pointer; length: DWORD; user: Pointer); basscall;
		{
			Internet stream download callback function.
			buffer : Buffer containing the downloaded data... NULL=end of download
			length : Number of bytes in the buffer
			user   : The 'user' parameter value given when calling StreamCreateURL
		}

		SYNCPROC = procedure(handle: HSYNC; channel, data: DWORD; user: Pointer); basscall;
		{
			Sync callback function. NOTE: a sync callback function should be very
			quick as other syncs cannot be processed until it has finished. If the
			sync is a "mixtime" sync, then other streams and MOD musics can not be
			mixed until it's finished either.
			handle : The sync that has occured
			channel: Channel that the sync occured in
			data   : Additional data associated with the sync's occurance
			user   : The 'user' parameter given when calling ChannelSetSync
		}

		DSPPROC = procedure(handle: HDSP; channel: DWORD; buffer: Pointer; length: DWORD; user: Pointer); basscall;
		{
			DSP callback function. NOTE: A DSP function should obviously be as quick
			as possible... other DSP functions, streams and MOD musics can not be
			processed until it's finished.
			handle : The DSP handle
			channel: Channel that the DSP is being applied to
			buffer : Buffer to apply the DSP to
			length : Number of bytes in the buffer
			user   : The 'user' parameter given when calling ChannelSetDSP
		}

		RECORDPROC = function(handle: HRECORD; buffer: Pointer; length: DWORD; user: Pointer): LongBool; basscall;
		{
			Recording callback function.
			handle : The recording handle
			buffer : Buffer containing the recorded sample data
			length : Number of bytes
			user   : The 'user' parameter value given when calling RecordStart
			RETURN : TRUE = continue recording, FALSE = stop
		}

	class var
		SetConfig: function(option, value: DWORD): LongBool; basscall;
		// GetConfig: function(option: DWORD): DWORD; basscall;
		// SetConfigPtr: function(option: DWORD; value: Pointer): LongBool; basscall;
		// GetConfigPtr: function(option: DWORD): Pointer; basscall;
		GetVersion: function: DWORD; basscall;
		ErrorGetCode: function: sint32; basscall;
		GetDeviceInfo: function(device: DWORD; out info: DEVICEINFO): LongBool; basscall;
		Init: function(device: sint32; freq, flags: DWORD; win: {$ifdef Windows} HWND {$else} pointer {$endif}; clsid: PGUID): LongBool; basscall;
		// SetDevice: function(device: DWORD): LongBool; basscall;
		// GetDevice: function: DWORD; basscall;
		Free_: function: LongBool; basscall;
		// GetInfo: function(out info: INFO): LongBool; basscall;
		// Update: function(length: DWORD): LongBool; basscall;
		// GetCPU: function: cfloat; basscall;
		Start: function: LongBool; basscall;
		// Stop: function: LongBool; basscall;
		Pause: function: LongBool; basscall;
		// SetVolume: function(volume: cfloat): LongBool; basscall;
		// GetVolume: function: cfloat; basscall;

		PluginLoad: function(filename: PChar; flags: DWORD): HPLUGIN; basscall;
		PluginFree: function(handle: HPLUGIN): LongBool; basscall;
		// PluginGetInfo: function(handle: HPLUGIN): PPLUGININFO; basscall;

		Set3DFactors: function(distf, rollf, doppf: cfloat): LongBool; basscall;
		// Get3DFactors: function(out distf, rollf, doppf: cfloat): LongBool; basscall;
		Set3DPosition: function(constref pos, vel, front, top: _3DVECTOR): LongBool; basscall;
		Set3DPosition_p: function(pos, vel, front, top: p3DVECTOR): LongBool; basscall;
		// Get3DPosition: function(out pos, vel, front, top: _3DVECTOR): LongBool; basscall;
		Apply3D: procedure; basscall;
		// SetEAXParameters: function(env: LongInt; vol, decay, damp: cfloat): LongBool; basscall;
		// GetEAXParameters: function(out env: DWORD; out vol, decay, damp: cfloat): LongBool; basscall;

		MusicLoad: function(mem: LongBool; f: Pointer; offset: QWORD; length, flags, freq: DWORD): HMUSIC; basscall;
		MusicFree: function(handle: HMUSIC): LongBool; basscall;

		SampleLoad: function(mem: LongBool; f: Pointer; offset: QWORD; length, max, flags: DWORD): HSAMPLE; basscall;
		// SampleCreate: function(length, freq, chans, max, flags: DWORD): HSAMPLE; basscall;
		SampleFree: function(handle: HSAMPLE): LongBool; basscall;
		// SampleSetData: function(handle: HSAMPLE; buffer: Pointer): LongBool; basscall;
		// SampleGetData: function(handle: HSAMPLE; buffer: Pointer): LongBool; basscall;
		SampleGetInfo: function(handle: HSAMPLE; out info: SAMPLE): LongBool; basscall;
		SampleSetInfo: function(handle: HSAMPLE; constref info: SAMPLE): LongBool; basscall;
		SampleGetChannel: function(handle: HSAMPLE; onlynew: LongBool): HCHANNEL; basscall;
		// SampleGetChannels: function(handle: HSAMPLE; channels: Pointer): DWORD; basscall;
		// SampleStop: function(handle: HSAMPLE): LongBool; basscall;

		// StreamCreate: function(freq, chans, flags: DWORD; proc: STREAMPROC; user: Pointer): HSTREAM; basscall;
		// StreamCreateFile: function(mem: LongBool; f: Pointer; offset, length: QWORD; flags: DWORD): HSTREAM; basscall;
		// StreamCreateURL: function(url: PAnsiChar; offset: DWORD; flags: DWORD; proc: DOWNLOADPROC; user: Pointer):HSTREAM; basscall;
		StreamCreateFileUser: function(system, flags: DWORD; constref procs: FILEPROCS; user: Pointer): HSTREAM; basscall;
		StreamFree: function(handle: HSTREAM): LongBool; basscall;
		// StreamGetFilePosition: function(handle: HSTREAM; mode: DWORD): QWORD; basscall;
		// StreamPutData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; basscall;
		// StreamPutFileData: function(handle: HSTREAM; buffer: Pointer; length: DWORD): DWORD; basscall;

		// RecordGetDeviceInfo: function(device: DWORD; var info: DEVICEINFO): LongBool; basscall;
		// RecordInit: function(device: sint32): LongBool; basscall;
		// RecordSetDevice: function(device: DWORD): LongBool; basscall;
		// RecordGetDevice: function: DWORD; basscall;
		// RecordFree: function: LongBool; basscall;
		// RecordGetInfo: function(var info: RECORDINFO): LongBool; basscall;
		// RecordGetInputName: function(input: sint32): PAnsiChar; basscall;
		// RecordSetInput: function(input: sint32; flags: DWORD; volume: cfloat): LongBool; basscall;
		// RecordGetInput: function(input: sint32; var volume: cfloat): DWORD; basscall;
		// RecordStart: function(freq, chans, flags: DWORD; proc: RECORDPROC; user: Pointer): HRECORD; basscall;

		ChannelBytes2Seconds: function(handle: DWORD; pos: QWORD): Double; basscall;
		ChannelSeconds2Bytes: function(handle: DWORD; pos: Double): QWORD; basscall;
		// ChannelGetDevice: function(handle: DWORD): DWORD; basscall;
		// ChannelSetDevice: function(handle, device: DWORD): LongBool; basscall;
		ChannelIsActive: function(handle: DWORD): DWORD; basscall;
		// ChannelGetInfo: function(handle: DWORD; out info: CHANNELINFO): LongBool; basscall;
		// ChannelGetTags: function(handle: HSTREAM; tags: DWORD): PAnsiChar; basscall;
		ChannelFlags: function(handle, flags, mask: DWORD): DWORD; basscall;
		// ChannelUpdate: function(handle, length: DWORD): LongBool; basscall;
		// ChannelLock: function(handle: DWORD; lock: LongBool): LongBool; basscall;
		ChannelPlay: function(handle: DWORD; restart: LongBool): LongBool; basscall;
		ChannelStop: function(handle: DWORD): LongBool; basscall;
		ChannelPause: function(handle: DWORD): LongBool; basscall;
		ChannelSetAttribute: function(handle, attrib: DWORD; value: cfloat): LongBool; basscall;
		ChannelGetAttribute: function(handle, attrib: DWORD; out value: cfloat): LongBool; basscall;
		ChannelSlideAttribute: function(handle, attrib: DWORD; value: cfloat; time: DWORD): LongBool; basscall;
		ChannelIsSliding: function(handle, attrib: DWORD): LongBool; basscall;
		ChannelSet3DAttributes: function(handle: DWORD; mode: sint32; min, max: cfloat; iangle, oangle: sint32; outvol: cfloat): LongBool; basscall;
		// ChannelGet3DAttributes: function(handle: DWORD; out mode: DWORD; out min, max: cfloat; out iangle, oangle, outvol: DWORD): LongBool; basscall;
		ChannelSet3DPosition: function(handle: DWORD; pos, orient, vel: p3DVECTOR): LongBool; basscall;
		// ChannelGet3DPosition: function(handle: DWORD; out pos, orient, vel: _3DVECTOR): LongBool; basscall;
		ChannelGetLength: function(handle, mode: DWORD): QWORD; basscall;
		ChannelSetPosition: function(handle: DWORD; pos: QWORD; mode: DWORD): LongBool; basscall;
		ChannelGetPosition: function(handle, mode: DWORD): QWORD; basscall;
		// ChannelGetLevel: function(handle: DWORD): DWORD; basscall;
		ChannelGetLevelEx: function(handle: DWORD; levels: pcfloat; length: cfloat; flags: dword): LongBool; basscall;
		ChannelGetData: function(handle: DWORD; buffer: Pointer; length: DWORD): DWORD; basscall;
		// ChannelSetSync: function(handle: DWORD; type_: DWORD; param: QWORD; proc: SYNCPROC; user: Pointer): HSYNC; basscall;
		// ChannelRemoveSync: function(handle: DWORD; sync: HSYNC): LongBool; basscall;
		// ChannelSetDSP: function(handle: DWORD; proc: DSPPROC; user: Pointer; priority: sint32): HDSP; basscall;
		// ChannelRemoveDSP: function(handle: DWORD; dsp: HDSP): LongBool; basscall;
		// ChannelSetLink: function(handle, chan: DWORD): LongBool; basscall;
		// ChannelRemoveLink: function(handle, chan: DWORD): LongBool; basscall;
		// ChannelSetFX: function(handle, type_: DWORD; priority: sint32): HFX; basscall;
		// ChannelRemoveFX: function(handle: DWORD; fx: HFX): LongBool; basscall;

		// FXSetParameters: function(handle: HFX; par: Pointer): LongBool; basscall;
		// FXGetParameters: function(handle: HFX; par: Pointer): LongBool; basscall;
		// FXReset: function(handle: HFX): LongBool; basscall;

		class function DescribeError(code: sint): string;
		class function DescribeCurrentError: string;
		class function DescribeChannelType(code: dword): string;
		class function OperationFailedMessage(const op, func: string; code: dword): string;
		class function LastOperationFailedMessage(const op, func: string): string;
		class function LastOperationFailed(const op, func: string): Exception;
	class var
		loader: DLLoader;
	end;
	operator :=(const v: Vec3): Bass._3DVECTOR;

type
	Midi = class
	const
		CONFIG_DEFFONT = $10403;

		ATTRIB_HANS    = $12002;

		FONT_MEM       = $10000;
		FONT_MMAP      = $20000;

	type
		HSOUNDFONT = DWORD;   // soundfont handle

		PFONT = ^FONT;
		FONT = record
			font: HSOUNDFONT;   // soundfont
			preset: LongInt;    // preset number (-1=all)
			bank: Longint;
		end;

		FONTINFO = record
			name: PAnsiChar;
			copyright: PAnsiChar;
			comment: PAnsiChar;
			presets: DWORD;     // number of presets/instruments
			samsize: DWORD;     // total size (in bytes) of the sample data
			samload: DWORD;     // amount of sample data currently loaded
			samtype: DWORD;     // sample format (CTYPE) if packed
		end;

	class var
		FontInit: function(fname: PChar; flags: DWORD): HSOUNDFONT; basscall;
		FontFree: function(handle: HSOUNDFONT): LongBool; basscall;
		FontGetInfo: function(handle: HSOUNDFONT; out info: FONTINFO): LongBool; basscall;
		FontGetPresets: function(handle: HSOUNDFONT; presets: pDWORD): LongBool; basscall;
		FontGetPreset: function(handle: HSOUNDFONT; preset, bank: longint): PAnsiChar; basscall;
		StreamSetFonts: function(handle: Bass.HSTREAM; fonts: PFONT; count: DWORD): LongBool; basscall;
		FontPack: function(handle: HSOUNDFONT; outfile, encoder: PChar; flags: DWORD): LongBool; basscall;
		FontUnpack: function(handle: HSOUNDFONT; outfile: PChar; flags: DWORD): LongBool; basscall;
		// StreamEvent: function(handle: HSTREAM; chan: DWORD; event: DWORD; param: DWORD): LongBool; basscall;

		class function FontInitU8(const fname: string; flags: DWORD): HSOUNDFONT;
		class function FontPackU8(handle: HSOUNDFONT; const outfile, encoder: string; flags: DWORD): boolean;
		class function FontUnpackU8(handle: HSOUNDFONT; const outfile: string; flags: DWORD): boolean;
	end;

implementation

type
	PluginEnum = (Bass_Midi, Bass_Aac, Bass_Flac);

const
	EnabledPlugins = [Low(PluginEnum) .. High(PluginEnum)] - [Bass_Aac];
	procedure DescribeMidiFunctions(var fns: DLLoader.FunctionsList); forward;

type
	PluginInstance = object
		lib, fnPrefix: string;
		getf: DLLoader.GetFunctions;
		loader: pDLLoader;
		bassh: Bass.HPLUGIN;
		function LibPath: string;
		procedure Load;
		procedure Shutdown;
		procedure Unload;
	end;

	function PluginInstance.LibPath: string;
	begin
		result := Paths.DLL(lib);
	end;

	procedure PluginInstance.Load;
	begin
		if Assigned(getf) then
		begin
			loader := loader^.Create(Format('{0} (prefix = {1})', lib, fnPrefix), getf);
			try
				loader^.Load;
			except
				loader^.Free(loader);
			{$ifdef Debug} Log(Exception.Message, logError); {$endif}
				exit;
			end;
		end;

	{$ifdef Debug} LogR('Подключение плагина ' + StreamPath.Log(LibPath) + '... '); {$endif}
		bassh := Bass.PluginLoad(pointer(pWideChar(UTF8Decode(LibPath))), Bass.UNICODE);
		if bassh = 0 then
		begin
		{$ifdef Debug} Log('Ошибка: ' + Bass.DescribeCurrentError, logError); {$endif}
			Unload;
		end {$ifdef Debug} else Log('OK', logOK) {$endif};
	end;

	procedure PluginInstance.Shutdown;
	begin
		if bassh <> 0 then
		begin
		{$ifdef Debug} LogR('Отключение плагина ' + StreamPath.Log(LibPath) + '... '); {$endif}
		{$ifdef Debug} if {$endif} Bass.PluginFree(bassh) {$ifdef Debug} then Log('OK', logOK) else Log('Ошибка: ' + Bass.DescribeCurrentError, logError) {$endif};
			bassh := 0;
		end;
	end;

	procedure PluginInstance.Unload;
	begin
		if Assigned(loader) then
		begin
			loader^.Unload;
			loader^.Free(loader);
		end;
	end;

var
	Plugins: array[PluginEnum] of PluginInstance =
	(
		(lib: 'bassmidi'; fnPrefix: 'BASS_MIDI_'; getf: @DescribeMidiFunctions; loader: nil; bassh: 0),
		(lib: 'bass_aac'; fnPrefix: ''; getf: nil; loader: nil; bassh: 0),
		(lib: 'bassflac'; fnPrefix: ''; getf: nil; loader: nil; bassh: 0)
	);

	procedure AfterLoad;
	var
		plugin: PluginEnum;
	begin
		for plugin in EnabledPlugins do
			Plugins[plugin].Load;
	end;

	procedure BeforeUnload;
	var
		plugin: PluginEnum;
	begin
		for plugin in EnabledPlugins do
			Plugins[plugin].Shutdown;
	end;

	procedure AfterUnload;
	var
		plugin: PluginEnum;
	begin
		for plugin in EnabledPlugins do
			Plugins[plugin].Unload;
	end;

	class function Bass.DescribeError(code: sint): string;
	begin
		case code of
			Bass.ERROR_MEM:      result := 'Недостаточно памяти.';
			Bass.ERROR_FILEOPEN: result := 'Не удалось открыть файл.';
			Bass.ERROR_DRIVER:   result := 'Звуковой драйвер недоступен.';
			Bass.ERROR_BUFLOST:  result := 'Буфер сэмплов утерян.';
			Bass.ERROR_HANDLE:   result := 'Неверный дескриптор.';
			Bass.ERROR_FORMAT:   result := 'Неподдерживаемый формат сэмпла.';
			Bass.ERROR_POSITION: result := 'Неверная позиция.';
			Bass.ERROR_INIT:     result := 'Bass.Init не выполнена.';
			Bass.ERROR_START:    result := 'Bass.Start не вызвана.';
			Bass.ERROR_ALREADY:  result := 'Уже инициализировано / приостановлено / etc..';
			Bass.ERROR_NOCHAN:   result := 'Нет свободных каналов.';
			Bass.ERROR_ILLTYPE:  result := 'Указан неверный тип.';
			Bass.ERROR_ILLPARAM: result := 'Указан неверный параметр.';
			Bass.ERROR_NO3D:     result := '3D не поддерживается.';
			Bass.ERROR_NOEAX:    result := 'EAX не поддерживается.';
			Bass.ERROR_DEVICE:   result := 'Неверный номер устройства.';
			Bass.ERROR_NOPLAY:   result := 'Не воспроизводится.';
			Bass.ERROR_FREQ:     result := 'Неверная частота дискретизации.';
			Bass.ERROR_NOTFILE:  result := 'Поток не является файловым потоком.';
			Bass.ERROR_NOHW:     result := 'Аппаратные голоса недоступны.';
			Bass.ERROR_EMPTY:    result := 'Нет последовательных данных MOD музыки.';
			Bass.ERROR_NONET:    result := 'Невозможно открыть Интернет-соединение.';
			Bass.ERROR_CREATE:   result := 'Невозможно создать файл.';
			Bass.ERROR_NOFX:     result := 'Эффекты не включены.';
			Bass.ERROR_NOTAVAIL: result := 'Запрошенные данные недоступны.';
			Bass.ERROR_DECODE:   result := 'Канал является "декодирующим каналом".';
			Bass.ERROR_DX:       result := 'Требуемая версия DirectX не установлена.';
			Bass.ERROR_TIMEOUT:  result := 'Слишком быстро.';
			Bass.ERROR_FILEFORM: result := 'Неподдерживаемый формат файла.';
			Bass.ERROR_SPEAKER:  result := 'Микрофон недоступен.';
			Bass.ERROR_VERSION:  result := 'Неверная версия BASS [add-on].';
			Bass.ERROR_CODEC:    result := 'Кодек недоступен / не поддерживается.';
			Bass.ERROR_ENDED:    result := 'Канал / файл закончился.';
			Bass.ERROR_BUSY:     result := 'Устройство занято.';
			else                 result := UnknownErrorCodeMsg(code);
		end;
	end;

	class function Bass.DescribeCurrentError: string;
	begin
		result := DescribeError(ErrorGetCode());
	end;

	class function Bass.DescribeChannelType(code: dword): string;
	begin
		case code of
			Bass.CTYPE_STREAM_OGG: result := 'OGG';
			Bass.CTYPE_STREAM_MP3: result := 'MP3';
			Bass.CTYPE_STREAM_WAV: result := 'WAV';
			Bass.CTYPE_STREAM_WAV_PCM: result := 'WAV (PCM)';
			Bass.CTYPE_STREAM_WAV_FLOAT: result := 'WAV (float)';
			Bass.CTYPE_MUSIC_XM:   result := 'XM';
			Bass.CTYPE_MUSIC_IT:   result := 'IT';
			Bass.CTYPE_STREAM_FLAC: result := 'FLAC';
			Bass.CTYPE_STREAM_AAC: result := 'AAC';
			dword(-1):             result := 'неподдерживаемый формат (не подключен плагин?)';
			else result := 'код ' + Utils.ToString(code, IntFormat.Hex);
		end;
	end;

	class function Bass.OperationFailedMessage(const op, func: string; code: dword): string;
	begin
		result := Format('Не удалось {0} ({1}). {2}', op, func, DescribeError(code));
	end;

	class function Bass.LastOperationFailedMessage(const op, func: string): string;
	begin
		result := OperationFailedMessage(op, func, ErrorGetCode());
	end;

	class function Bass.LastOperationFailed(const op, func: string): Exception;
	begin
		result := Error(LastOperationFailedMessage(op, func));
	end;

	operator :=(const v: Vec3): Bass._3DVECTOR;
	type
		TestSize = Bass._3DVECTOR;
	begin
	{$if sizeof(v) = sizeof(TestSize)}
		Bass._3DVECTOR(result) := Bass._3DVECTOR(v.data);
	{$else} {$note Bass._3DVECTOR size mismatch}
		result.x := v.x;
		result.y := v.y;
		result.z := v.z;
	{$endif}
	end;

	class function Midi.FontInitU8(const fname: string; flags: DWORD): HSOUNDFONT;
	var
		wfname: widestring;
	begin
		wfname := UTF8Decode(fname);
		result := FontInit(pointer(pWideChar(wfname)), flags or Bass.UNICODE);
	end;

	class function Midi.FontPackU8(handle: HSOUNDFONT; const outfile, encoder: string; flags: DWORD): boolean;
	var
		woutfile, wencoder: widestring;
	begin
		woutfile := UTF8Decode(outfile);
		wencoder := UTF8Decode(encoder);
		result := FontPack(handle, pointer(pWideChar(woutfile)), pointer(pWideChar(wencoder)), flags or Bass.UNICODE);
	end;

	class function Midi.FontUnpackU8(handle: HSOUNDFONT; const outfile: string; flags: DWORD): boolean;
	var
		woutfile: widestring;
	begin
		woutfile := UTF8Decode(outfile);
		result := FontUnpack(handle, pointer(pWideChar(woutfile)), flags or Bass.UNICODE);
	end;

{$push} {$typedaddress off}
	procedure DescribeBassFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@Bass.SetConfig,            'SetConfig')^
		.Func(@Bass.GetVersion,           'GetVersion')^
		.Func(@Bass.ErrorGetCode,         'ErrorGetCode')^
		.Func(@Bass.GetDeviceInfo,        'GetDeviceInfo')^
		.Func(@Bass.Init,                 'Init')^
		.Func(@Bass.Free_,                'Free')^
		.Func(@Bass.Start,                'Start')^
		.Func(@Bass.Pause,                'Pause')^
		.Func(@Bass.PluginLoad,           'PluginLoad')^
		.Func(@Bass.PluginFree,           'PluginFree')^
		.Func(@Bass.Set3DFactors,         'Set3DFactors')^
		.Func(@Bass.Set3DPosition,        'Set3DPosition')^
		.Func(@Bass.Apply3D,              'Apply3D')^
		.Func(@Bass.MusicLoad,            'MusicLoad')^
		.Func(@Bass.MusicFree,            'MusicFree')^
		.Func(@Bass.SampleLoad,           'SampleLoad')^
		.Func(@Bass.SampleFree,           'SampleFree')^
		.Func(@Bass.SampleGetInfo,        'SampleGetInfo')^
		.Func(@Bass.SampleSetInfo,        'SampleSetInfo')^
		.Func(@Bass.SampleGetChannel,     'SampleGetChannel')^
		.Func(@Bass.StreamCreateFileUser, 'StreamCreateFileUser')^
		.Func(@Bass.StreamFree,           'StreamFree')^
		.Func(@Bass.ChannelBytes2Seconds, 'ChannelBytes2Seconds')^
		.Func(@Bass.ChannelSeconds2Bytes, 'ChannelSeconds2Bytes')^
		.Func(@Bass.ChannelIsActive,      'ChannelIsActive')^
		.Func(@Bass.ChannelFlags,         'ChannelFlags')^
		.Func(@Bass.ChannelPlay,          'ChannelPlay')^
		.Func(@Bass.ChannelStop,          'ChannelStop')^
		.Func(@Bass.ChannelPause,         'ChannelPause')^
		.Func(@Bass.ChannelSetAttribute,  'ChannelSetAttribute')^
		.Func(@Bass.ChannelGetAttribute,  'ChannelGetAttribute')^
		.Func(@Bass.ChannelSlideAttribute,  'ChannelSlideAttribute')^
		.Func(@Bass.ChannelIsSliding,     'ChannelIsSliding')^
		.Func(@Bass.ChannelSet3DAttributes, 'ChannelSet3DAttributes')^
		.Func(@Bass.ChannelSet3DPosition, 'ChannelSet3DPosition')^
		.Func(@Bass.ChannelGetLength,     'ChannelGetLength')^
		.Func(@Bass.ChannelSetPosition,   'ChannelSetPosition')^
		.Func(@Bass.ChannelGetPosition,   'ChannelGetPosition')^
		.Func(@Bass.ChannelGetLevelEx,    'ChannelGetLevelEx')^
		.Func(@Bass.ChannelGetData,       'ChannelGetData');
	end;

	procedure DescribeMidiFunctions(var fns: DLLoader.FunctionsList);
	begin
		fns
		.Func(@Midi.FontInit,       'FontInit')^
		.Func(@Midi.FontFree,       'FontFree')^
		.Func(@Midi.FontGetInfo,    'FontGetInfo')^
		.Func(@Midi.FontGetPresets, 'FontGetPresets')^
		.Func(@Midi.FontGetPreset,  'FontGetPreset')^
		.Func(@Midi.StreamSetFonts, 'StreamSetFonts')^
		.Func(@Midi.FontPack,       'FontPack')^
		.Func(@Midi.FontUnpack,     'FontUnpack');
	end;
{$pop}

	procedure Init;
	begin
		Bass.loader.Init('bass(prefix = BASS_, perm, lock)', @DescribeBassFunctions);
		Bass.loader.Hook(+1).AfterLoad(@AfterLoad).BeforeUnload(@BeforeUnload).AfterUnload(@AfterUnload);
	end;

	procedure Done;
	begin
		Bass.loader.Done;
	end;

initialization
	&Unit('BASS').Initialize(@Init, @Done);
end.
