#include <sourcemod>
#include <popspawner>

native int set_upgrades_file_internal(const char[] path, char[] new_path, int len, bool use_crc);

#undef REQUIRE_EXTENSIONS
#tryinclude <system2>
#tryinclude <bzip2>
//#tryinclude <latedl>
//#tryinclude <>
#define REQUIRE_EXTENSIONS

#if !defined _bzip2_included
enum BZ_Error {
	BZ_OK				= 0,
	BZ_RUN_OK			= 1,
	BZ_FLUSH_OK			= 2,
	BZ_FINISH_OK		= 3,
	BZ_STREAM_END		= 4,
	BZ_SEQUENCE_ERROR	= -1,
	BZ_PARAM_ERROR		= -2,
	BZ_MEM_ERROR		= -3,
	BZ_DATA_ERROR		= -4,
	BZ_DATA_ERROR_MAGIC	= -5,
	BZ_IO_ERROR			= -6,
	BZ_UNEXPECTED_EOF	= -7,
	BZ_OUTBUFF_FULL		= -8,
	BZ_CONFIG_ERROR		= -9,
	BZ_IO_ERROR_INPUT	= -101,
	BZ_IO_ERROR_OUTPUT	= -102,
};
#endif

static bool system2_loaded;
static bool bzip2_loaded;

static ConVar auto_bz2;
static ConVar bz2_compression;
static ConVar upload_ftp;
static ConVar ftp_user;
static ConVar ftp_pass;
static ConVar delete_bz2;
static ConVar ftp_url;
static ConVar bz2_folder;

public APLRes AskPluginLoad2(Handle myself, bool late, char[] error, int length)
{
	if(LibraryExists("system2") ||
		GetExtensionFileStatus("system2.ext") == 1) {
		system2_loaded = true;
	}

	if(LibraryExists("bzip2") ||
		GetExtensionFileStatus("smbz2.ext") == 1) {
		bzip2_loaded = true;
	}

	CreateNative("set_upgrades_file", native_set_upgrades_file);

	return APLRes_Success;
}

#if defined _system2_included
void on_upload(bool success, const char[] error, System2FTPRequest request, System2FTPResponse response)
{
	if(success) {
		//???

		if(delete_bz2.BoolValue) {

		}
	} else {
		//???
	}
}

void on_copy(bool success, const char[] from, const char[] to)
{
	if(success) {
		DeleteFile(from);
	} else {
		if(!RenameFile(to, from, true)) {
			LogError("could not copy bz2 to: %s", to);
		}
	}
}
#endif

static void on_compress(DataPack data, BZ_Error error)
{
	data.Reset();

	int len = data.ReadCell();

	char[] filename = new char[len];
	data.ReadString(filename, len);

	delete data;

	if(error == BZ_OK) {
	#if defined _system2_included
		if(upload_ftp.BoolValue) {
			char url[128];
			ftp_url.GetString(url, sizeof(url));

			char usrname[32];
			ftp_user.GetString(usrname, sizeof(usrname));

			char pass[32];
			ftp_pass.GetString(pass, sizeof(pass));

			if(url[0] != '\0' && usrname[0] != '\0' && pass[0] != '\0') {
				System2FTPRequest ftp = new System2FTPRequest(on_upload, "%s/scripts/items/", url);
				ftp.SetAuthentication(usrname, pass);
				ftp.SetInputFile(filename);
				ftp.StartRequest();
			}
		}

		char folder[PLATFORM_MAX_PATH];
		bz2_folder.GetString(folder, PLATFORM_MAX_PATH);

		if(folder[0] != '\0') {
			StrCat(folder, PLATFORM_MAX_PATH, filename);

			System2_CopyFile(on_copy, filename, folder);
		} else {
			//AddLateDownload(filename, false);
		}
	#endif
	} else {
		LogError("compression of %s failed error %i", filename, error);
	}
}

#if defined _system2_included
static void on_compress_sys2(bool success, const char[] command, System2ExecuteOutput output, any data)
{
	on_compress(view_as<DataPack>(data), success ? BZ_OK : BZ_IO_ERROR);
}
#endif

#if defined _bzip2_included
static void on_compress_bzip2(BZ_Error iError, char[] inFile, char[] outFile, any data)
{
	on_compress(view_as<DataPack>(data), iError);
}
#endif

static int native_set_upgrades_file(Handle plugin, int params)
{
	int len;
	GetNativeStringLength(1, len);
	char[] input_path = new char[++len];
	GetNativeString(1, input_path, len);

	bool use_crc = GetNativeCell(2) != 0;

	char new_path[PLATFORM_MAX_PATH];
	set_upgrades_file_internal(input_path, new_path, PLATFORM_MAX_PATH, use_crc);

	if(auto_bz2.BoolValue) {
		char bz2[PLATFORM_MAX_PATH];
		strcopy(bz2, PLATFORM_MAX_PATH, new_path);
		StrCat(bz2, PLATFORM_MAX_PATH, ".bz2");

		CompressLevel compress = LEVEL_9;
		switch(bz2_compression.IntValue) {
			case 1: { compress = LEVEL_1; }
			case 3: { compress = LEVEL_3; }
			case 5: { compress = LEVEL_5; }
			case 7: { compress = LEVEL_7; }
			case 9: { compress = LEVEL_9; }
		}

		DataPack data = new DataPack();

		data.WriteCell(strlen(bz2)+1);
		data.WriteString(bz2);

	#if defined _bzip2_included
		if(!bzip2_loaded || !BZ2_CompressFile(new_path, bz2, view_as<int>(compress), on_compress_bzip2, data)) {
	#endif
		#if defined _system2_included
			if(!system2_loaded || !System2_Compress(on_compress_sys2, new_path, bz2, ARCHIVE_BZIP2, compress, data)) {
		#endif
				delete data;
				LogError("could not compress upgrades: %s", new_path);
		#if defined _system2_included
			}
		#endif
	#if defined _bzip2_included
		}
	#endif
	}

	return 0;
}

public void OnLibraryAdded(const char[] name)
{
	if(StrEqual(name, "system2")) {
		system2_loaded = true;
	} else if(StrEqual(name, "bzip2")) {
		bzip2_loaded = true;
	}
}

public void OnLibraryRemoved(const char[] name)
{
	if(StrEqual(name, "system2")) {
		system2_loaded = false;
	} else if(StrEqual(name, "bzip2")) {
		bzip2_loaded = false;
	}
}

public void OnPluginStart()
{
	HookEvent("upgrades_file_changed", upgrades_file_changed);

	auto_bz2 = CreateConVar("popspawner_upgrades_bz2", "1");
	bz2_compression = CreateConVar("popspawner_upgrades_bz2_compression", "9", "1,3,5,7,9");
	upload_ftp = CreateConVar("popspawner_upgrades_upload_ftp", "1");
	ftp_user = CreateConVar("popspawner_upgrades_ftp_user", "");
	ftp_pass = CreateConVar("popspawner_upgrades_ftp_pass", "");
	delete_bz2 = CreateConVar("popspawner_upgrades_delete_bz2", "1");
	ftp_url = CreateConVar("popspawner_upgrades_ftp_url", "");
	bz2_folder = CreateConVar("popspawner_upgrades_bz2_folder", "");
}

static void upgrades_file_changed(Event event, const char[] name, bool dontBroadcast)
{
	char path[PLATFORM_MAX_PATH];
	event.GetString("path", path, PLATFORM_MAX_PATH);

	
}