#if defined popspawner_included
	#endinput
#endif
#define popspawner_included

#include <tf2>

enum AttributeType
{
	REMOVE_ON_DEATH				= 1<<0,					// kick bot from server when killed
	AGGRESSIVE					= 1<<1,					// in MvM mode, push for the cap point
	IS_NPC						= 1<<2,					// a non-player support character
	SUPPRESS_FIRE				= 1<<3,
	DISABLE_DODGE				= 1<<4,
	BECOME_SPECTATOR_ON_DEATH	= 1<<5,					// move bot to spectator team when killed
	QUOTA_MANANGED				= 1<<6,					// managed by the bot quota in CTFBotManager 
	RETAIN_BUILDINGS			= 1<<7,					// don't destroy this bot's buildings when it disconnects
	SPAWN_WITH_FULL_CHARGE		= 1<<8,					// all weapons start with full charge (ie: uber)
	ALWAYS_CRIT					= 1<<9,					// always fire critical hits
	IGNORE_ENEMIES				= 1<<10,
	HOLD_FIRE_UNTIL_FULL_RELOAD	= 1<<11,				// don't fire our barrage weapon until it is full reloaded (rocket launcher, etc)
	PRIORITIZE_DEFENSE			= 1<<12,				// bot prioritizes defending when possible
	ALWAYS_FIRE_WEAPON			= 1<<13,				// constantly fire our weapon
	TELEPORT_TO_HINT			= 1<<14,				// bot will teleport to hint target instead of walking out from the spawn point
	MINIBOSS					= 1<<15,				// is miniboss?
	USE_BOSS_HEALTH_BAR			= 1<<16,				// should I use boss health bar?
	IGNORE_FLAG					= 1<<17,				// don't pick up flag/bomb
	AUTO_JUMP					= 1<<18,				// auto jump
	AIR_CHARGE_ONLY				= 1<<19,				// demo knight only charge in the air
	PREFER_VACCINATOR_BULLETS	= 1<<20,				// When using the vaccinator, prefer to use the bullets shield
	PREFER_VACCINATOR_BLAST		= 1<<21,				// When using the vaccinator, prefer to use the blast shield
	PREFER_VACCINATOR_FIRE		= 1<<22,				// When using the vaccinator, prefer to use the fire shield
	BULLET_IMMUNE				= 1<<23,				// Has a shield that makes the bot immune to bullets
	BLAST_IMMUNE				= 1<<24,				// "" blast
	FIRE_IMMUNE					= 1<<25,				// "" fire
	PARACHUTE					= 1<<26,				// demo/soldier parachute when falling
	PROJECTILE_SHIELD			= 1<<27,				// medic projectile shield
};

#define NPC_POP_FLAGS (REMOVE_ON_DEATH|AGGRESSIVE|IS_NPC|DISABLE_DODGE|IGNORE_FLAG)
#define BOSS_NPC_POP_FLAGS (NPC_POP_FLAGS|MINIBOSS|USE_BOSS_HEALTH_BAR)

typedef parse_func_t = function bool (CustomPopulationSpawner spawner, KeyValues data);
typedef spawn_func_t = function bool (CustomPopulationSpawner spawner, const float pos[3], ArrayList result);
typedef eventchange_func_t = function bool (CustomPopulationSpawner spawner, const char[] name);
typedef class_func_t = function TFClassType (CustomPopulationSpawner spawner, int num);
typedef health_func_t = function int (CustomPopulationSpawner spawner, int num);
typedef classicon_func_t = function bool (CustomPopulationSpawner spawner, int num, char[] str, int len);
typedef miniboss_func_t = function bool (CustomPopulationSpawner spawner, int num);
typedef attr_func_t = function bool (CustomPopulationSpawner spawner, AttributeType attr, int num);
typedef del_func_t = function void (CustomPopulationSpawner spawner);

enum populator_type_t
{
	populator_none,
	populator_unknown,
	populator_wave,
	populator_wavespawn,
	populator_mission,
};

methodmap IPopulator
{
	public native bool Parse(KeyValues data);

	public native bool HasEventChangeAttributes(const char[] pszEventName);

	property IPopulationSpawner Spawner
	{
		public native get();
	}

	property populator_type_t Type
	{
		public native get();
	}

	public native void Delete();
};

#define IPopulator_Null (view_as<IPopulator>(Address_Null))

methodmap IPopulationSpawner
{
	public native bool Parse(KeyValues data);
	public native bool Spawn(const float here[3], ArrayList result = null);

	property bool WhereRequired
	{
		public native get();
	}

	property bool Various
	{
		public native get();
	}

	public native TFClassType GetClass(int nSpawnNum = -1);
	public native int GetClassIcon(char[] icon, int len, int nSpawnNum = -1);
	public native int GetHealth(int nSpawnNum = -1);
	public native bool IsMiniBoss(int nSpawnNum = -1);
	public native bool HasAttribute(AttributeType type, int nSpawnNum = -1);
	public native bool HasEventChangeAttributes(const char[] pszEventName);

	property IPopulator Populator
	{
		public native get();
	}

	property bool Custom
	{
		public native get();
	}

	public native void Delete();
};

#define IPopulationSpawner_Null (view_as<IPopulationSpawner>(Address_Null))

methodmap CustomPopulationSpawner < IPopulationSpawner
{
	public native void set_data(const char[] name, any data);
	public native any get_data(const char[] name);
	public native bool has_data(const char[] name);
	
	public native void set_data_array(const char[] name, any[] data, int len);
	public native void get_data_array(const char[] name, any[] data, int len);

	property CustomPopulationSpawnerEntry Entry
	{
		public native get();
	}
};

methodmap CustomPopulationSpawnerEntry
{
	property parse_func_t Parse
	{
		public native set(parse_func_t func);
	}

	property spawn_func_t Spawn
	{
		public native set(spawn_func_t func);
	}

	property eventchange_func_t HasEventChangeAttributes
	{
		public native set(eventchange_func_t func);
	}

	property class_func_t GetClass
	{
		public native set(class_func_t func);
	}

	property health_func_t GetHealth
	{
		public native set(health_func_t func);
	}

	property classicon_func_t GetClassIcon
	{
		public native set(classicon_func_t func);
	}

	property miniboss_func_t IsMiniBoss
	{
		public native set(miniboss_func_t func);
	}

	property attr_func_t HasAttribute
	{
		public native set(attr_func_t func);
	}

	property bool WhereRequired
	{
		public native set(bool func);
	}

	property bool IsVarious
	{
		public native set(bool func);
	}

	property del_func_t Delete
	{
		public native set(del_func_t func);
	}
};

native CustomPopulationSpawnerEntry register_popspawner(const char[] name);
native IPopulationSpawner create_spawner(const char[] name, IPopulator populator, KeyValues data = null);
native IPopulator create_populator(const char[] name, KeyValues data);

native bool IsSpaceToSpawnHere(const float pos[3]);

enum RelativePositionType
{
	UNDEFINED = 0,
	AHEAD,
	BEHIND,
	ANYWHERE
};

methodmap SpawnLocation
{
	property RelativePositionType Relative
	{
		public native get();
		public native set(RelativePositionType value);
	}

	property bool ClosestPointOnNav
	{
		public native get();
		public native set(bool value);
	}
};

forward Action find_spawn_location(IPopulator populator, SpawnLocation location, float pos[3]);

methodmap CWaveSpawnPopulator < IPopulator
{
	public native bool ParseAdditive(KeyValues data);

	public native int GetWaitForAllSpawned(char[] str, int len);
	public native int GetWaitForAllDead(char[] str, int len);

	property int TotalCount
	{
		public native get();
		public native set(int value);
	}

	property int MaxActive
	{
		public native get();
		public native set(int value);
	}

	property int SpawnCount
	{
		public native get();
		public native set(int value);
	}

	property float WaitBeforeStarting
	{
		public native get();
		public native set(float value);
	}

	property float WaitBetweenSpawns
	{
		public native get();
		public native set(float value);
	}

	property float WaitBetweenSpawnsAfterDeath
	{
		public native get();
		public native set(float value);
	}

	property bool RandomSpawn
	{
		public native get();
		public native set(bool value);
	}

	property int TotalCurrency
	{
		public native get();
		public native set(int value);
	}

	property CWave Parent
	{
		public native get();
	}
};

#define CWaveSpawnPopulator_Null (view_as<CWaveSpawnPopulator>(Address_Null))

methodmap CWave < IPopulator
{
	public native bool ParseAdditive(KeyValues data);

	public native bool AddWaveSpawn(CWaveSpawnPopulator wavePopulator);

	property float WaitWhenDone
	{
		public native get();
		public native set(float value);
	}

	property int Index
	{
		public native get();
	}

	property int WaveSpawnCount
	{
		public native get();
	}

	public native CWaveSpawnPopulator GetWaveSpawn(int idx);
};

#define CWave_Null (view_as<CWave>(Address_Null))

native float pop_health_multiplier(bool tank);
native float pop_damage_multiplier();

native CWave current_wave();
native int current_wave_index();
native CWave get_wave(int i);
native int wave_count();
native IPopulator get_populator(int i);
native int populator_count();

native bool init_pop();
native bool set_pop_filename(const char[] path);
native bool merge_pop(KeyValues data);

native bool add_populator(IPopulator populator);

forward Action pop_parse(KeyValues data, bool &result);
forward Action wavespawn_parse(CWaveSpawnPopulator populator, KeyValues data, bool &result);
forward Action wave_parse(CWave populator, KeyValues data, bool &result);
forward Action spawner_parse(IPopulator populator, IPopulationSpawner spawner, KeyValues data, bool &result);
forward Action spawnlocation_parse(IPopulator populator, SpawnLocation location, KeyValues data, bool &result);

forward Action is_bonus_wave(bool &result, int &boss, float pos[3]);

forward void pop_event_fired(const char[] name);

forward void pop_entity_spawned(IPopulator populator, IPopulationSpawner spawner, SpawnLocation location, int entity);

#if !defined REQUIRE_EXTENSIONS
public void __ext_popspawner_SetNTVOptional()
{
	
}
#endif

public Extension __ext_popspawner = 
{
	name = "popspawner", 
	file = "popspawner.ext", 
	
#if defined AUTOLOAD_EXTENSIONS
	autoload = 1,
#else
	autoload = 0,
#endif

#if defined REQUIRE_EXTENSIONS
	required = 1,
#else
	required = 0,
#endif
};
