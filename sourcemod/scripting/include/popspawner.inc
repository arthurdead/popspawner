#if defined popspawner_included
	#endinput
#endif
#define popspawner_included

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

typeset popspawner_functions
{
	//Parse
	function bool (CustomPopulationSpawner spawner, KeyValues data);

	//Spawn
	function bool (CustomPopulationSpawner spawner, float pos[3], ArrayList result);

	//HasEventChangeAttributes
	function bool (CustomPopulationSpawner spawner, const char[] name);

	//GetClass, GetHealth
	function int (CustomPopulationSpawner spawner, int num);

	//GetClassIcon
	function bool (CustomPopulationSpawner spawner, int num, char[] str, int len);

	//IsMiniBoss
	function bool (CustomPopulationSpawner spawner, int num);

	//HasAttribute
	function bool (CustomPopulationSpawner spawner, AttributeType attr, int num);
};

methodmap IPopulator
{

};

#define IPopulator_Null (view_as<IPopulator>(Address_null))

methodmap CustomPopulationSpawner
{
	property popspawner_functions Parse
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions Spawn
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions HasEventChangeAttributes
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions GetClass
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions GetHealth
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions GetClassIcon
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions IsMiniBoss
	{
		public native set(popspawner_functions func);
	}

	property popspawner_functions HasAttribute
	{
		public native set(popspawner_functions func);
	}

	property bool WhereRequired
	{
		public native set(bool func);
	}

	property bool IsVarious
	{
		public native set(bool func);
	}

	property IPopulator Populator
	{
		public native get();
	}

	public native void set_data(const char[] name, any data);
	public native any get_data(const char[] name);
};

native CustomPopulationSpawner register_popspawner(const char[] name);

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
