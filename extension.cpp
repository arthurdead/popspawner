/**
 * vim: set ts=4 :
 * =============================================================================
 * SourceMod Sample Extension
 * Copyright (C) 2004-2008 AlliedModders LLC.  All rights reserved.
 * =============================================================================
 *
 * This program is free software; you can redistribute it and/or modify it under
 * the terms of the GNU General Public License, version 3.0, as published by the
 * Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
 * details.
 *
 * You should have received a copy of the GNU General Public License along with
 * this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * As a special exception, AlliedModders LLC gives you permission to link the
 * code of this program (as well as its derivative works) to "Half-Life 2," the
 * "Source Engine," the "SourcePawn JIT," and any Game MODs that run on software
 * by the Valve Corporation.  You must obey the GNU General Public License in
 * all respects for all other code used.  Additionally, AlliedModders LLC grants
 * this exception to all derivative works.  AlliedModders LLC defines further
 * exceptions, found in LICENSE.txt (as of this writing, version JULY-31-2007),
 * or <http://www.sourcemod.net/license.php>.
 *
 * Version: $Id$
 */

#include <unordered_map>
#include <algorithm>
#include <utility>
#include <string>
#include <string_view>
#include <cstring>

#include "extension.h"

#include <CDetour/detours.h>
#include <tier1/utlvector.h>
#include <ehandle.h>
#include <ICellArray.h>
using EHANDLE = CHandle<CBaseEntity>;
#include <takedamageinfo.h>
#include <toolframework/itoolentity.h>

/**
 * @file extension.cpp
 * @brief Implement extension code here.
 */

Sample g_Sample;		/**< Global singleton for extension's main interface */

SMEXT_LINK(&g_Sample);

typedef CUtlVector< CHandle< CBaseEntity > > EntityHandleVector_t;

void *AllocPooledStringPtr = nullptr;
void *IsSpaceToSpawnHerePtr = nullptr;
ICvar *icvar = nullptr;

enum
{
	TF_CLASS_UNDEFINED = 0,

	TF_CLASS_SCOUT,			// TF_FIRST_NORMAL_CLASS
    TF_CLASS_SNIPER,
    TF_CLASS_SOLDIER,
	TF_CLASS_DEMOMAN,
	TF_CLASS_MEDIC,
	TF_CLASS_HEAVYWEAPONS,
	TF_CLASS_PYRO,
	TF_CLASS_SPY,
	TF_CLASS_ENGINEER,		

	// Add any new classes after Engineer
	TF_CLASS_CIVILIAN,		// TF_LAST_NORMAL_CLASS
	TF_CLASS_COUNT_ALL,

	TF_CLASS_RANDOM
};

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

#define NUM_BOT_ATTRS 28

class CPopulationManager;
class IPopulationSpawner;
class CTFPlayer;

class IPopulator
{
public:
	IPopulator( CPopulationManager *manager )
	{
		m_manager = manager;
		m_spawner = NULL;
	}

	virtual ~IPopulator();

	virtual bool Parse( KeyValues *data ) = 0;

	virtual void PostInitialize( void ) { }		// create initial population at start of scenario
	virtual void Update( void ) { }				// continuously invoked to modify population over time
	virtual void UnpauseSpawning() {}

	virtual void OnPlayerKilled( CTFPlayer *corpse ) { }

	CPopulationManager *GetManager( void ) const { return m_manager; }

	virtual bool HasEventChangeAttributes( const char* pszEventName ) const;

	IPopulationSpawner *m_spawner;

private:
	CPopulationManager *m_manager;
};

class IPopulationSpawner
{
public:
	// We need a virtual destructor or else the derived-class destructors won't be called,
	// leading to memory leaks. Found via clang warning.
	virtual ~IPopulationSpawner()
	{
	}

	IPopulationSpawner( IPopulator *populator )
	{
		m_populator = populator;
	}

	IPopulator *GetPopulator( void ) const
	{
		return m_populator;
	}

	virtual bool Parse( KeyValues *data ) = 0;
	virtual bool Spawn( const Vector &here, EntityHandleVector_t *result = NULL ) = 0;
	virtual bool IsWhereRequired( void ) const = 0;
	
	virtual bool IsVarious( void ) = 0;
	virtual int GetClass( int nSpawnNum = -1 ) = 0;
	virtual string_t GetClassIcon( int nSpawnNum = -1 ) = 0;
	virtual int GetHealth( int nSpawnNum = -1  ) = 0;
	virtual bool IsMiniBoss( int nSpawnNum = -1 ) = 0;
	virtual bool HasAttribute( AttributeType type, int nSpawnNum = -1 ) = 0;
	virtual bool HasEventChangeAttributes( const char* pszEventName ) const = 0;

protected:
	IPopulator *m_populator = nullptr;
};

bool IPopulator::HasEventChangeAttributes( const char* pszEventName ) const
{
	if ( m_spawner )
	{
		return m_spawner->HasEventChangeAttributes( pszEventName );
	}

	return false;
}

IPopulator::~IPopulator()
{
	if ( m_spawner )
	{
		delete m_spawner;
	}
	m_spawner = NULL;
}

struct pop_entry_t;
class SPPopulationSpawner;

using spvarmap_t = std::unordered_map<std::string, std::vector<cell_t>>;

using pop_spawner_map_t = std::unordered_map<std::string, SPPopulationSpawner *>;
pop_spawner_map_t popspawnermap{};

struct pop_entry_t
{
	pop_entry_t(std::string &&name_);
	~pop_entry_t();
	
	IPluginFunction *Parse = nullptr;
	IPluginFunction *Spawn = nullptr;
	IPluginFunction *HasEventChangeAttributes = nullptr;
	IPluginFunction *GetClass = nullptr;
	IPluginFunction *GetHealth = nullptr;
	IPluginFunction *GetClassIcon = nullptr;
	IPluginFunction *IsMiniBoss = nullptr;
	IPluginFunction *HasAttribute = nullptr;
	IPluginFunction *Delete = nullptr;
	bool WhereRequired = true;
	bool IsVarious = false;
	std::string name{};
	IdentityToken_t *owner = nullptr;
	Handle_t hndl = BAD_HANDLE;
};

#include "icandowhateveriwantthefactthattheresnowaytodothisstillisridiculous.h"

ConVar popspawner_maxiconlen("popspawner_maxiconlen", "50");

class SPPopulationSpawner : public IPopulationSpawner
{
public:
	SPPopulationSpawner(pop_entry_t *entry_, IPopulator *populator_)
		: IPopulationSpawner{populator_}, entry{entry_}, name{entry_->name}
	{
		popspawnermap.emplace(name, this);
	}

	void deleted()
	{
		if(entry) {
			IPluginFunction *func = entry->Delete;
			if(func) {
				func->PushCell((cell_t)this);
				func->Execute(nullptr);
			}
		}
		entry = nullptr;
	}
	
	~SPPopulationSpawner()
	{
		deleted();
		if(m_populator) {
			m_populator->m_spawner = nullptr;
		}
		if(erase) {
			popspawnermap.erase(name);
		}
	}
	
	bool Parse( KeyValues *data )
	{
		if(!entry) {
			return false;
		}

		IPluginFunction *func = entry->Parse;
		if(!func) {
			return true;
		}
		
		HandleError err{};
		Handle_t hndl = ((HandleSystemHack *)handlesys)->CreateKeyValuesHandle(data, entry->owner, &err);
		if(err != HandleError_None) {
			func->GetParentContext()->ReportError("Invalid KeyValues handle %x (error %d).", hndl, err);
			return false;
		}

		func->PushCell((cell_t)this);
		func->PushCell(hndl);
		cell_t res = 0;
		func->Execute(&res);
		handlesys->FreeHandle(hndl, nullptr);
		
		return res;
	}
	
	bool Spawn( const Vector &here, EntityHandleVector_t *result = NULL )
	{
		if(!entry) {
			return false;
		}
		
		IPluginFunction *func = entry->Spawn;
		if(!func) {
			return false;
		}

		Handle_t hndl = 0;
		ICellArray *arr = nullptr;
		if(result) {
			HandleError err{};
			hndl = ((HandleSystemHack *)handlesys)->CreateCellArrayHandle(arr, entry->owner, &err);
			if(err != HandleError_None) {
				func->GetParentContext()->ReportError("Invalid ArrayList handle %x (error %d).", hndl, err);
				return false;
			}
		}
		
		func->PushCell((cell_t)this);
		cell_t vec[3];
		vec[0] = sp_ftoc(here.x);
		vec[1] = sp_ftoc(here.y);
		vec[2] = sp_ftoc(here.z);
		func->PushArray(vec, 3);
		func->PushCell(hndl);
		cell_t res = 0;
		func->Execute(&res);
		if(result) {
			size_t len = arr->size();
			for(int i = 0; i < len; ++i) {
				cell_t index = arr->at(i)[0];
				edict_t *pEnt = gamehelpers->EdictOfIndex(index);
				if(pEnt) {
					CBaseHandle hndl{};
					gamehelpers->SetHandleEntity(hndl, pEnt);
					result->AddToTail(hndl);
				}
			}
			
			handlesys->FreeHandle(hndl, nullptr);
		}
		
		return res;
	}
	
	bool HasEventChangeAttributes( const char* pszEventName ) const
	{
		if(!entry) {
			return false;
		}
		
		IPluginFunction *func = entry->HasEventChangeAttributes;
		if(!func) {
			return false;
		}
		
		func->PushCell((cell_t)this);
		func->PushString(pszEventName);
		cell_t res = 0;
		func->Execute(&res);
		
		return res;
	}
	
	int GetClass( int nSpawnNum = -1 )
	{
		if(!entry) {
			return TF_CLASS_UNDEFINED;
		}
		
		IPluginFunction *func = entry->GetClass;
		if(!func) {
			return TF_CLASS_UNDEFINED;
		}
		
		func->PushCell((cell_t)this);
		func->PushCell(nSpawnNum);
		cell_t res = 0;
		func->Execute(&res);
		
		return res;
	}
	
	int GetHealth( int nSpawnNum = -1  )
	{
		if(!entry) {
			return 0;
		}
		
		IPluginFunction *func = entry->GetHealth;
		if(!func) {
			func = entry->GetClass;
			if(func) {
				func->PushCell((cell_t)this);
				func->PushCell(nSpawnNum);
				cell_t res = 0;
				func->Execute(&res);
				switch(res) {
					case TF_CLASS_SCOUT: return 125;
					case TF_CLASS_SNIPER: return 125;
					case TF_CLASS_SOLDIER: return 200;
					case TF_CLASS_DEMOMAN: return 175;
					case TF_CLASS_MEDIC: return 150;
					case TF_CLASS_HEAVYWEAPONS: return 300;
					case TF_CLASS_PYRO: return 175;
					case TF_CLASS_SPY: return 125;
					case TF_CLASS_ENGINEER: return 125;
				}
			}
			return 0;
		}
		
		func->PushCell((cell_t)this);
		func->PushCell(nSpawnNum);
		cell_t res = 0;
		func->Execute(&res);
		
		return res;
	}
	
	string_t GetClassIcon( int nSpawnNum = -1 )
	{
		if(!entry) {
			return NULL_STRING;
		}
		
		IPluginFunction *func = entry->GetClassIcon;
		if(!func) {
			func = entry->GetClass;
			if(func) {
				func->PushCell((cell_t)this);
				func->PushCell(nSpawnNum);
				cell_t res = 0;
				func->Execute(&res);
				switch(res) {
					case TF_CLASS_SCOUT: return MAKE_STRING( "scout" );
					case TF_CLASS_SNIPER: return MAKE_STRING( "sniper" );
					case TF_CLASS_SOLDIER: return MAKE_STRING( "soldier" );
					case TF_CLASS_DEMOMAN: return MAKE_STRING( "demo" );
					case TF_CLASS_MEDIC: return MAKE_STRING( "medic" );
					case TF_CLASS_HEAVYWEAPONS: return MAKE_STRING( "heavy" );
					case TF_CLASS_PYRO: return MAKE_STRING( "pyro" );
					case TF_CLASS_SPY: return MAKE_STRING( "spy" );
					case TF_CLASS_ENGINEER: return MAKE_STRING( "engineer" );
				}
			}
			return NULL_STRING;
		}
		
		int len = popspawner_maxiconlen.GetInt();
		if(len < 2) len = 2;

		char *str = new char[len];
		str[0] = '\0';
		
		func->PushCell((cell_t)this);
		func->PushCell(nSpawnNum);
		func->PushStringEx(str, len, SM_PARAM_STRING_UTF8, SM_PARAM_COPYBACK);
		func->PushCell(len);
		cell_t res = 0;
		func->Execute(&res);
		
		if(!res) {
			delete[] str;
			return NULL_STRING;
		}
		
		string_t id = ((string_t(*)(const char *))AllocPooledStringPtr)( str );
		
		delete[] str;
		
		return id;
	}
	
	bool IsMiniBoss( int nSpawnNum = -1 )
	{
		if(!entry) {
			return false;
		}
		
		IPluginFunction *func = entry->IsMiniBoss;
		if(!func) {
			func = entry->HasAttribute;
			if(func) {
				func->PushCell((cell_t)this);
				func->PushCell(MINIBOSS);
				func->PushCell(nSpawnNum);
				cell_t res = 0;
				func->Execute(&res);
				return res;
			}
			return false;
		}
		
		func->PushCell((cell_t)this);
		func->PushCell(nSpawnNum);
		cell_t res = 0;
		func->Execute(&res);
		
		return res;
	}
	
	bool HasAttribute( AttributeType type, int nSpawnNum = -1 )
	{
		if(!entry) {
			return false;
		}
		
		IPluginFunction *func = entry->HasAttribute;
		if(!func) {
			func = entry->IsMiniBoss;
			if(func && type == MINIBOSS) {
				func->PushCell((cell_t)this);
				func->PushCell(nSpawnNum);
				cell_t res = 0;
				func->Execute(&res);
				return res;
			}
			return false;
		}
		
		func->PushCell((cell_t)this);
		func->PushCell(type);
		func->PushCell(nSpawnNum);
		cell_t res = 0;
		func->Execute(&res);
		
		return res;
	}
	
	bool IsWhereRequired( void ) const
	{
		return entry ? entry->WhereRequired : true;
	}
	
	bool IsVarious( void )
	{
		return entry ? entry->IsVarious : false;
	}

	spvarmap_t data{};
	pop_entry_t *entry = nullptr;
	std::string name;
	bool erase{true};
};

using pop_entry_map_t = std::unordered_map<std::string, pop_entry_t *>;
pop_entry_map_t poentrypmap{};

pop_entry_t::pop_entry_t(std::string &&name_)
	: name{std::move(name_)}
{
	poentrypmap.emplace(name, this);
}

pop_entry_t::~pop_entry_t()
{
	pop_spawner_map_t::iterator it{popspawnermap.begin()};
	while(it != popspawnermap.end()) {
		SPPopulationSpawner *ptr{it->second};
		if(ptr->entry == this) {
			ptr->deleted();
			ptr->entry = nullptr;
			ptr->erase = false;
			it = popspawnermap.erase(it);
			continue;
		}
		
		++it;
	}

	poentrypmap.erase(name);
}

class CTakeDamageInfo;

SH_DECL_HOOK2(IPopulationSpawner, Spawn, SH_NOATTRIB, 0, bool, const Vector &, EntityHandleVector_t *);
SH_DECL_MANUALHOOK0_void(GenericDtor, 1, 0, 0)
SH_DECL_MANUALHOOK1_void(Event_Killed, 0, 0, 0, const CTakeDamageInfo &)

struct entpopdata_t
{
	string_t icon{NULL_STRING};
	int attrs{0};
	bool killed{false};
};

static std::unordered_map<int, entpopdata_t> entpopdata{};

static int objective_resource_ref = INVALID_EHANDLE_INDEX;

IServerTools *servertools = nullptr;

void Sample::OnCoreMapStart(edict_t *pEdictList, int edictCount, int clientMax)
{
	CBaseEntity *pEntity = servertools->FindEntityByClassname(nullptr, "tf_objective_resource");
	if(pEntity) {
		objective_resource_ref = gamehelpers->EntityToBCompatRef(pEntity);
	}
}

class CBaseEntity : public IServerEntity
{
};

static int m_iszMannVsMachineWaveClassNames_offset{-1};
static int m_iszMannVsMachineWaveClassNames2_offset{-1};

static int m_nMannVsMachineWaveClassFlags_offset{-1};
static int m_nMannVsMachineWaveClassFlags2_offset{-1};

static int m_bMannVsMachineWaveClassActive_offset{-1};
static int m_bMannVsMachineWaveClassActive2_offset{-1};

static int m_nMannVsMachineWaveClassCounts_offset{-1};
static int m_nMannVsMachineWaveClassCounts2_offset{-1};

#define MVM_CLASS_TYPES_PER_WAVE_MAX 12

void SetEdictStateChanged(CBaseEntity *pEntity, int offset)
{
	IServerNetworkable *pNet = pEntity->GetNetworkable();
	edict_t *edict = pNet->GetEdict();
	
	gamehelpers->SetEdictStateChanged(edict, offset);
}

class CTFObjectiveResource : public CBaseEntity
{
public:
	string_t *m_iszMannVsMachineWaveClassNames()
	{
		if(m_iszMannVsMachineWaveClassNames_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_iszMannVsMachineWaveClassNames", &info);
			m_iszMannVsMachineWaveClassNames_offset = info.actual_offset;
		}

		return (string_t *)(((unsigned char *)this) + m_iszMannVsMachineWaveClassNames_offset);
	}

	string_t *m_iszMannVsMachineWaveClassNames2()
	{
		if(m_iszMannVsMachineWaveClassNames2_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_iszMannVsMachineWaveClassNames2", &info);
			m_iszMannVsMachineWaveClassNames2_offset = info.actual_offset;
		}

		return (string_t *)(((unsigned char *)this) + m_iszMannVsMachineWaveClassNames2_offset);
	}

	unsigned int *m_nMannVsMachineWaveClassFlags()
	{
		if(m_nMannVsMachineWaveClassFlags_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_nMannVsMachineWaveClassFlags", &info);
			m_nMannVsMachineWaveClassFlags_offset = info.actual_offset;
		}

		return (unsigned int *)(((unsigned char *)this) + m_nMannVsMachineWaveClassFlags_offset);
	}

	unsigned int *m_nMannVsMachineWaveClassFlags2()
	{
		if(m_nMannVsMachineWaveClassFlags2_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_nMannVsMachineWaveClassFlags2", &info);
			m_nMannVsMachineWaveClassFlags2_offset = info.actual_offset;
		}

		return (unsigned int *)(((unsigned char *)this) + m_nMannVsMachineWaveClassFlags2_offset);
	}

	int *m_nMannVsMachineWaveClassCounts()
	{
		if(m_nMannVsMachineWaveClassCounts_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_nMannVsMachineWaveClassCounts", &info);
			m_nMannVsMachineWaveClassCounts_offset = info.actual_offset;
		}

		return (int *)(((unsigned char *)this) + m_nMannVsMachineWaveClassCounts_offset);
	}

	int *m_nMannVsMachineWaveClassCounts2()
	{
		if(m_nMannVsMachineWaveClassCounts2_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_nMannVsMachineWaveClassCounts2", &info);
			m_nMannVsMachineWaveClassCounts2_offset = info.actual_offset;
		}

		return (int *)(((unsigned char *)this) + m_nMannVsMachineWaveClassCounts2_offset);
	}

	bool *m_bMannVsMachineWaveClassActive()
	{
		if(m_bMannVsMachineWaveClassActive_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_bMannVsMachineWaveClassActive", &info);
			m_bMannVsMachineWaveClassActive_offset = info.actual_offset;
		}

		return (bool *)(((unsigned char *)this) + m_bMannVsMachineWaveClassActive_offset);
	}

	bool *m_bMannVsMachineWaveClassActive2()
	{
		if(m_bMannVsMachineWaveClassActive2_offset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_bMannVsMachineWaveClassActive2", &info);
			m_bMannVsMachineWaveClassActive2_offset = info.actual_offset;
		}

		return (bool *)(((unsigned char *)this) + m_bMannVsMachineWaveClassActive2_offset);
	}

	void SetMannVsMachineWaveClassActive(string_t iszClassIconName, bool bActive = true)
	{
		for(int i = 0 ; i < MVM_CLASS_TYPES_PER_WAVE_MAX; ++i) {
			if((m_iszMannVsMachineWaveClassNames()[i] == iszClassIconName)) {
				m_bMannVsMachineWaveClassActive()[i] = bActive;
				SetEdictStateChanged(this, m_bMannVsMachineWaveClassActive_offset + (sizeof(bool) * i));
				return;
			}
		}

		for(int i = 0 ; i < MVM_CLASS_TYPES_PER_WAVE_MAX; ++i) {
			if((m_iszMannVsMachineWaveClassNames2()[i] == iszClassIconName)) {
				m_bMannVsMachineWaveClassActive2()[i] = bActive;
				SetEdictStateChanged(this, m_bMannVsMachineWaveClassActive2_offset + (sizeof(bool) * i));
				return;
			}
		}
	}

	void DecrementMannVsMachineWaveClassCount(string_t iszClassIconName, unsigned int iFlags)
	{
		int i = 0;
		for(i = 0 ; i < MVM_CLASS_TYPES_PER_WAVE_MAX; ++i) {
			if((m_iszMannVsMachineWaveClassNames()[i] == iszClassIconName) && (m_nMannVsMachineWaveClassFlags()[i] & iFlags)) {
				m_nMannVsMachineWaveClassCounts()[i] -= 1;

				if(m_nMannVsMachineWaveClassCounts()[i] < 0) {
					m_nMannVsMachineWaveClassCounts()[i] = 0;
				}

				SetEdictStateChanged(this, m_nMannVsMachineWaveClassCounts_offset + (sizeof(int) * i));

				if(!m_nMannVsMachineWaveClassCounts()[i]) {
					SetMannVsMachineWaveClassActive(iszClassIconName, false);
				}

				return;
			}
		}

		for(i = 0 ; i < MVM_CLASS_TYPES_PER_WAVE_MAX; ++i) {
			if((m_iszMannVsMachineWaveClassNames2()[i] == iszClassIconName) && (m_nMannVsMachineWaveClassFlags2()[i] & iFlags)) {
				m_nMannVsMachineWaveClassCounts2()[i] -= 1;

				if(m_nMannVsMachineWaveClassCounts2()[i] < 0) {
					m_nMannVsMachineWaveClassCounts2()[i] = 0;
				}

				SetEdictStateChanged(this, m_nMannVsMachineWaveClassCounts2_offset + (sizeof(int) * i));

				if(!m_nMannVsMachineWaveClassCounts2()[i]) {
					SetMannVsMachineWaveClassActive(iszClassIconName, false);
				}

				return;
			}
		}
	}
};

static void hook_entity_killed(const CTakeDamageInfo &info)
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);

	int ref = gamehelpers->EntityToBCompatRef(pEntity);

	auto it{entpopdata.find(ref)};
	if(it != entpopdata.cend()) {
		entpopdata_t &data{it->second};

		CTFObjectiveResource *pObjectiveResource = (CTFObjectiveResource *)gamehelpers->ReferenceToEntity(objective_resource_ref);
		if(pObjectiveResource) {
			data.killed = true;
			pObjectiveResource->DecrementMannVsMachineWaveClassCount(data.icon, data.attrs);
		}

		//TODO!!!! drop money
	}

	RETURN_META(MRES_HANDLED);
}

static void hook_entity_dtor()
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);

	int ref = gamehelpers->EntityToBCompatRef(pEntity);

	auto it{entpopdata.find(ref)};
	if(it != entpopdata.cend()) {
		entpopdata_t &data{it->second};

		if(!data.killed) {
			CTFObjectiveResource *pObjectiveResource = (CTFObjectiveResource *)gamehelpers->ReferenceToEntity(objective_resource_ref);
			if(pObjectiveResource) {
				pObjectiveResource->DecrementMannVsMachineWaveClassCount(data.icon, data.attrs);
			}
		}

		entpopdata.erase(it);
	}

	SH_REMOVE_MANUALHOOK(Event_Killed, pEntity, SH_STATIC(hook_entity_killed), false);
	SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_STATIC(hook_entity_dtor), false);

	RETURN_META(MRES_HANDLED);
}

static bool hook_spawner_spawn(const Vector &here, EntityHandleVector_t *result)
{
	if(result) {
		IPopulationSpawner *spawner = META_IFACEPTR(IPopulationSpawner);

		int num_players = playerhelpers->GetNumPlayers();

		int count = result->Count();
		for(int i = 0; i < count; ++i) {
			CBaseHandle &hndl = (*result)[i];

			int entidx = hndl.GetEntryIndex();
			if(entidx >= 1 && entidx <= num_players) {
				continue;
			}

			CBaseEntity *pEntity{gamehelpers->ReferenceToEntity(entidx)};
			if(!pEntity) {
				continue;
			}

			const char *classname{gamehelpers->GetEntityClassname(pEntity)};
			if(strcmp(classname, "tank_boss") == 0) {
				continue;
			}

			string_t icon = spawner->GetClassIcon(i);

			CTFObjectiveResource *pObjectiveResource = (CTFObjectiveResource *)gamehelpers->ReferenceToEntity(objective_resource_ref);
			if(pObjectiveResource) {
				//TODO!!!!!!!! only do this in WaveSpawnPopulator
				pObjectiveResource->SetMannVsMachineWaveClassActive(icon);
			}

			int ref = gamehelpers->EntityToBCompatRef(pEntity);

			if(entpopdata.find(ref) != entpopdata.cend()) {
				continue;
			}

			SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_STATIC(hook_entity_dtor), false);
			SH_ADD_MANUALHOOK(Event_Killed, pEntity, SH_STATIC(hook_entity_killed), false);

			entpopdata_t data;
			data.icon = icon;
			for(int j = 0; j < NUM_BOT_ATTRS; ++j) {
				AttributeType attr{static_cast<AttributeType>(1 << j)};
				if(spawner->HasAttribute(attr, i)) {
					data.attrs |= attr;
				}
			}

			entpopdata.emplace(ref, std::move(data));
		}
	}

	RETURN_META_VALUE(MRES_HANDLED, false);
}

static void hook_spawner_dtor()
{
	IPopulationSpawner *spawner = META_IFACEPTR(IPopulationSpawner);

	SH_REMOVE_HOOK(IPopulationSpawner, Spawn, spawner, SH_STATIC(hook_spawner_spawn), true);
	SH_REMOVE_MANUALHOOK(GenericDtor, spawner, SH_STATIC(hook_spawner_dtor), false);

	RETURN_META(MRES_HANDLED);
}

DETOUR_DECL_STATIC2(ParseSpawner, IPopulationSpawner *, IPopulator *, populator, KeyValues *, data)
{
	IPopulationSpawner *spawner = DETOUR_STATIC_CALL(ParseSpawner)(populator, data);
	if(!spawner) {
		const char *name_ptr = data->GetName();
		std::string name{name_ptr};
		
		pop_entry_map_t::const_iterator it{
			std::find_if(poentrypmap.cbegin(), poentrypmap.cend(),
				[&name = std::as_const(name)](const auto &it) noexcept -> bool {
					return (strncasecmp(it.first.c_str(), name.c_str(), it.first.length()) == 0);
				}
			)
		};
		if(it != poentrypmap.end()) {
			pop_entry_t *entry = it->second;
			spawner = new SPPopulationSpawner{entry, populator};
			
			if(!spawner->Parse(data)) {
				Warning( "Warning reading %s spawner definition\n", name_ptr );
				delete spawner;
				spawner = nullptr;
			}
		}
	}

	if(spawner) {
		SH_ADD_MANUALHOOK(GenericDtor, spawner, SH_STATIC(hook_spawner_dtor), false);
		SH_ADD_HOOK(IPopulationSpawner, Spawn, spawner, SH_STATIC(hook_spawner_spawn), true);
	}

	return spawner;
}

HandleType_t popspawner_handle = 0;

cell_t register_popspawner(IPluginContext *pContext, const cell_t *params)
{
	char *name_ptr = nullptr;
	pContext->LocalToString(params[1], &name_ptr);
	std::string name{name_ptr};
	
	pop_entry_map_t::iterator it{poentrypmap.find(name)};
	if(it != poentrypmap.end()) {
		return pContext->ThrowNativeError("%s is already registered", name_ptr);
	}
	
	pop_entry_t *obj = new pop_entry_t{std::move(name)};
	obj->owner = pContext->GetIdentity();
	obj->hndl = handlesys->CreateHandle(popspawner_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	return obj->hndl;
}

cell_t create_spawner(IPluginContext *pContext, const cell_t *params)
{
	char *name_ptr = nullptr;
	pContext->LocalToString(params[1], &name_ptr);

	if(params[3] != BAD_HANDLE) {
		HandleError err{};
		KeyValues *pKv = smutils->ReadKeyValuesHandle(params[3], &err);
		if(err != HandleError_None) {
			return pContext->ThrowNativeError("Invalid KeyValues handle %x (error %d).", params[3], err);
		}

		return (cell_t)ParseSpawner((IPopulator *)params[2], pKv);
	} else {
		KeyValues *pKv = new KeyValues{name_ptr};
		cell_t ret = (cell_t)ParseSpawner((IPopulator *)params[2], pKv);
		pKv->deleteThis();
		return ret;
	}
}

cell_t Parseset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->Parse = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t Spawnset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->Spawn = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t HasEventChangeAttributesset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->HasEventChangeAttributes = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t GetClassset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->GetClass = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t GetHealthset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->GetHealth = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t GetClassIconset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->GetClassIcon = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t IsMiniBossset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->IsMiniBoss = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t HasAttributeset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->HasAttribute = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t WhereRequiredset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->WhereRequired = params[2];
	
	return 0;
}

cell_t IsVariousset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->IsVarious = params[2];
	
	return 0;
}

cell_t Deleteset(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	obj->Delete = pContext->GetFunctionById(params[2]);
	
	return 0;
}

cell_t set_data(IPluginContext *pContext, const cell_t *params)
{
	SPPopulationSpawner *obj{(SPPopulationSpawner *)params[1]};
	spvarmap_t &data = obj->data;

	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	auto it{data.find(name)};
	if(it == data.end()) {
		it = data.emplace(spvarmap_t::value_type{name, {}}).first;
	}
	
	std::vector<cell_t> &vec = it->second;
	if(vec.size() == 0) {
		vec.resize(1);
	}
	
	vec[0] = params[3];
	
	return 0;
}

cell_t get_data(IPluginContext *pContext, const cell_t *params)
{
	SPPopulationSpawner *obj{(SPPopulationSpawner *)params[1]};
	spvarmap_t &data = obj->data;

	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	auto it{data.find(name)};
	if(it == data.end() || it->second.size() == 0) {
		return pContext->ThrowNativeError("theres no data with the name %s", name);
	}
	
	return it->second[0];
}

cell_t has_data(IPluginContext *pContext, const cell_t *params)
{
	SPPopulationSpawner *obj{(SPPopulationSpawner *)params[1]};
	spvarmap_t &data = obj->data;

	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	auto it{data.find(name)};
	return (it != data.end() && it->second.size() > 0);
}

cell_t set_data_array(IPluginContext *pContext, const cell_t *params)
{
	SPPopulationSpawner *obj{(SPPopulationSpawner *)params[1]};
	spvarmap_t &data = obj->data;

	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	auto it{data.find(name)};
	if(it == data.end()) {
		it = data.emplace(spvarmap_t::value_type{name, {}}).first;
	}
	
	std::vector<cell_t> &vec = it->second;
	
	cell_t *addr = nullptr;
	pContext->LocalToPhysAddr(params[3], &addr);
	
	size_t len = params[4];
	vec.resize(len);
	for(int i = 0; i < len; ++i) {
		vec[i] = addr[i];
	}
	
	return 0;
}

cell_t get_data_array(IPluginContext *pContext, const cell_t *params)
{
	SPPopulationSpawner *obj{(SPPopulationSpawner *)params[1]};
	spvarmap_t &data = obj->data;

	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	auto it{data.find(name)};
	if(it == data.end()) {
		it = data.emplace(spvarmap_t::value_type{name, {}}).first;
	}
	
	std::vector<cell_t> &vec = it->second;
	
	cell_t *addr = nullptr;
	pContext->LocalToPhysAddr(params[3], &addr);
	
	size_t len = params[4];
	vec.resize(len);
	for(int i = 0; i < len; ++i) {
		addr[i] = vec[i];
	}
	
	return 0;
}

cell_t IsSpaceToSpawnHere(IPluginContext *pContext, const cell_t *params)
{
	cell_t *addr = nullptr;
	pContext->LocalToPhysAddr(params[1], &addr);

	Vector vec{sp_ctof(addr[0]), sp_ctof(addr[1]), sp_ctof(addr[2])};

	return ((bool(*)(const Vector &))IsSpaceToSpawnHerePtr)( vec );
}

cell_t IPopulationSpawnerParse(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};

	HandleError err{};
	KeyValues *pKv = smutils->ReadKeyValuesHandle(params[2], &err);
	if(err != HandleError_None) {
		return pContext->ThrowNativeError("Invalid KeyValues handle %x (error %d).", params[2], err);
	}

	return obj->Parse(pKv);
}

cell_t IPopulationSpawnerSpawn(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};

	cell_t *addr = nullptr;
	pContext->LocalToPhysAddr(params[2], &addr);

	Vector here{sp_ctof(addr[0]), sp_ctof(addr[1]), sp_ctof(addr[2])};

	if(params[3] != BAD_HANDLE) {
		HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
		
		ICellArray *arr = nullptr;
		HandleError err = ((HandleSystemHack *)handlesys)->ReadCoreHandle(params[3], arraylist_handle, &security, (void **)&arr);
		if(err != HandleError_None)
		{
			return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[3], err);
		}

		EntityHandleVector_t result;
		bool res = obj->Spawn(here, &result);
		size_t res_len{result.Count()};
		size_t arr_len = arr->size();
		size_t start{arr_len};
		arr_len += res_len;
		arr->resize(arr_len);
		for(size_t i{0}; i < res_len; ++i) {
			const CHandle<CBaseEntity> &it{result[i]};

			*arr->at(start + i) = gamehelpers->ReferenceToBCompatRef(it.GetEntryIndex());
		}
		return res;
	} else {
		return obj->Spawn(here, nullptr);
	}
}

cell_t IPopulationSpawnerWhereRequiredget(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->IsWhereRequired();
}

cell_t IPopulationSpawnerVariousget(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->IsVarious();
}

cell_t IPopulationSpawnerPopulatorget(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return (cell_t)obj->GetPopulator();
}

cell_t IPopulationSpawnerGetClass(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->GetClass(params[2]);
}

cell_t IPopulationSpawnerGetHealth(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->GetHealth(params[2]);
}

cell_t IPopulationSpawnerIsMiniBoss(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->IsMiniBoss(params[2]);
}

cell_t IPopulationSpawnerHasAttribute(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->HasAttribute((AttributeType)params[2], params[3]);
}

cell_t IPopulationSpawnerHasEventChangeAttributes(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};

	char *name_ptr = nullptr;
	pContext->LocalToString(params[2], &name_ptr);

	return obj->HasEventChangeAttributes(name_ptr);
}

cell_t IPopulationSpawnerGetClassIcon(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};

	string_t icon{obj->GetClassIcon(params[4])};

	size_t written = 0;
	pContext->StringToLocalUTF8(params[2], params[3], STRING(icon), &written);

	return written;
}

cell_t IPopulationSpawnerDelete(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	delete obj;
	return 0;
}

sp_nativeinfo_t natives[] =
{
	{"CustomPopulationSpawner.set_data", set_data},
	{"CustomPopulationSpawner.get_data", get_data},
	{"CustomPopulationSpawner.has_data", has_data},
	{"CustomPopulationSpawner.set_data_array", set_data_array},
	{"CustomPopulationSpawner.get_data_array", get_data_array},
	{"CustomPopulationSpawnerEntry.Parse.set", Parseset},
	{"CustomPopulationSpawnerEntry.Spawn.set", Spawnset},
	{"CustomPopulationSpawnerEntry.HasEventChangeAttributes.set", HasEventChangeAttributesset},
	{"CustomPopulationSpawnerEntry.GetClass.set", GetClassset},
	{"CustomPopulationSpawnerEntry.GetHealth.set", GetHealthset},
	{"CustomPopulationSpawnerEntry.GetClassIcon.set", GetClassIconset},
	{"CustomPopulationSpawnerEntry.IsMiniBoss.set", IsMiniBossset},
	{"CustomPopulationSpawnerEntry.HasAttribute.set", HasAttributeset},
	{"CustomPopulationSpawnerEntry.WhereRequired.set", WhereRequiredset},
	{"CustomPopulationSpawnerEntry.IsVarious.set", IsVariousset},
	{"CustomPopulationSpawnerEntry.Delete.set", Deleteset},
	{"register_popspawner", register_popspawner},
	{"create_spawner", create_spawner},
	{"IsSpaceToSpawnHere", IsSpaceToSpawnHere},
	{"IPopulationSpawner.Parse", IPopulationSpawnerParse},
	{"IPopulationSpawner.Spawn", IPopulationSpawnerSpawn},
	{"IPopulationSpawner.WhereRequired.get", IPopulationSpawnerWhereRequiredget},
	{"IPopulationSpawner.Various.get", IPopulationSpawnerVariousget},
	{"IPopulationSpawner.GetClass", IPopulationSpawnerGetClass},
	{"IPopulationSpawner.GetHealth", IPopulationSpawnerGetHealth},
	{"IPopulationSpawner.IsMiniBoss", IPopulationSpawnerIsMiniBoss},
	{"IPopulationSpawner.HasAttribute", IPopulationSpawnerHasAttribute},
	{"IPopulationSpawner.GetClassIcon", IPopulationSpawnerGetClassIcon},
	{"IPopulationSpawner.Populator.get", IPopulationSpawnerPopulatorget},
	{"IPopulationSpawner.Delete", IPopulationSpawnerDelete},
	{NULL, NULL}
};

void Sample::OnPluginUnloaded(IPlugin *plugin)
{
	
}

void Sample::OnHandleDestroy(HandleType_t type, void *object)
{
	if(type == popspawner_handle) {
		pop_entry_t *obj = (pop_entry_t *)object;
		delete obj;
	}
}

IForward *find_spawn_location = nullptr;

enum SpawnLocationResult
{
	SPAWN_LOCATION_NOT_FOUND = 0,
	SPAWN_LOCATION_NAV,
	SPAWN_LOCATION_TELEPORTER
};

DETOUR_DECL_MEMBER1(FindSpawnLocation, SpawnLocationResult, Vector &, vSpawnPosition)
{
	if(find_spawn_location->GetFunctionCount() > 0) {
		cell_t pos[3]{sp_ftoc(vSpawnPosition.x), sp_ftoc(vSpawnPosition.y), sp_ftoc(vSpawnPosition.z)};
		find_spawn_location->PushArray(pos, 3, SM_PARAM_COPYBACK);
		cell_t res = 0;
		find_spawn_location->Execute(&res);

		switch(res) {
			case Pl_Changed: {
				vSpawnPosition.x = sp_ctof(pos[0]);
				vSpawnPosition.y = sp_ctof(pos[1]);
				vSpawnPosition.z = sp_ctof(pos[2]);
				return SPAWN_LOCATION_NAV;
			}
			case Pl_Handled:
			case Pl_Stop: {
				return SPAWN_LOCATION_NOT_FOUND;
			}
		}
	}

	return DETOUR_MEMBER_CALL(FindSpawnLocation)(vSpawnPosition);
}

CDetour *pParseSpawner = nullptr;
CDetour *pFindSpawnLocation = nullptr;

bool Sample::RegisterConCommandBase(ConCommandBase *pCommand)
{
	META_REGCVAR(pCommand);
	return true;
}

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	GET_V_IFACE_CURRENT(GetEngineFactory, icvar, ICvar, CVAR_INTERFACE_VERSION);
	GET_V_IFACE_CURRENT(GetServerFactory, servertools, IServerTools, VSERVERTOOLS_INTERFACE_VERSION);
	g_pCVar = icvar;
	ConVar_Register(0, this);
	return true;
}

IGameConfig *g_pGameConf = nullptr;

bool Sample::SDK_OnLoad(char *error, size_t maxlen, bool late)
{
	gameconfs->LoadGameConfigFile("popspawner", &g_pGameConf, error, maxlen);
	
	CDetourManager::Init(g_pSM->GetScriptingEngine(), g_pGameConf);
	
	pParseSpawner = DETOUR_CREATE_STATIC(ParseSpawner, "IPopulationSpawner::ParseSpawner")
	pParseSpawner->EnableDetour();

	pFindSpawnLocation = DETOUR_CREATE_MEMBER(FindSpawnLocation, "CSpawnLocation::FindSpawnLocation")
	pFindSpawnLocation->EnableDetour();
	
	g_pGameConf->GetMemSig("AllocPooledString", &AllocPooledStringPtr);

	g_pGameConf->GetMemSig("IsSpaceToSpawnHere", &IsSpaceToSpawnHerePtr);
	
	int offset = -1;
	g_pGameConf->GetOffset("CBaseCombatCharacter::Event_Killed", &offset);
	SH_MANUALHOOK_RECONFIGURE(Event_Killed, offset, 0, 0);
	
	popspawner_handle = handlesys->CreateType("popspawner", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);

	find_spawn_location = forwards->CreateForward("find_spawn_location", ET_Hook, 1, nullptr, Param_Array);

	sharesys->AddNatives(myself, natives);
	
	sharesys->RegisterLibrary(myself, "popspawner");
	
	HandleSystemHack::init();
	
	return true;
}

void Sample::SDK_OnUnload()
{
	pParseSpawner->Destroy();
	pFindSpawnLocation->Destroy();
	gameconfs->CloseGameConfigFile(g_pGameConf);
	forwards->ReleaseForward(find_spawn_location);
	handlesys->RemoveType(popspawner_handle, myself->GetIdentity());
}
