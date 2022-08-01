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

#include "extension.h"

#include <CDetour/detours.h>
#include <tier1/utlvector.h>
#include <ehandle.h>
#include <unordered_map>
#include <ICellArray.h>

/**
 * @file extension.cpp
 * @brief Implement extension code here.
 */

Sample g_Sample;		/**< Global singleton for extension's main interface */

SMEXT_LINK(&g_Sample);

typedef CUtlVector< CHandle< CBaseEntity > > EntityHandleVector_t;

void *AllocPooledStringPtr = nullptr;
ICvar *icvar = nullptr;

#define TF_CLASS_UNDEFINED 0

enum AttributeType : int;

class IPopulator;

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

struct pop_entry_t;
class SPPopulationSpawner;

using pop_spawner_map_t = std::unordered_map<pop_entry_t *, SPPopulationSpawner *>;
pop_spawner_map_t popspawnermap{};

using data_map_t = std::unordered_map<std::string, cell_t>;

struct pop_entry_t
{
	pop_entry_t(const std::string &name_);
	~pop_entry_t();
	
	IPluginFunction *Parse = nullptr;
	IPluginFunction *Spawn = nullptr;
	IPluginFunction *HasEventChangeAttributes = nullptr;
	IPluginFunction *GetClass = nullptr;
	IPluginFunction *GetHealth = nullptr;
	IPluginFunction *GetClassIcon = nullptr;
	IPluginFunction *IsMiniBoss = nullptr;
	IPluginFunction *HasAttribute = nullptr;
	bool WhereRequired = true;
	bool IsVarious = false;
	std::string name{};
	IdentityToken_t *owner = nullptr;
	Handle_t hndl = BAD_HANDLE;
	data_map_t data{};
	IPopulator *populator = nullptr;
};

#include "icandowhateveriwantthefactthattheresnowaytodothisstillisridiculous.h"

ConVar popspawner_maxiconlen("popspawner_maxiconlen", "50");

class SPPopulationSpawner : public IPopulationSpawner
{
public:
	SPPopulationSpawner(pop_entry_t *entry_, IPopulator *populator_)
		: IPopulationSpawner{populator_}, entry{entry_}
	{
		popspawnermap[entry] = this;
	}
	
	~SPPopulationSpawner()
	{
		entry->populator = nullptr;
		popspawnermap.erase(entry);
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

		func->PushCell(entry->hndl);
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
		
		func->PushCell(entry->hndl);
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
		
		func->PushCell(entry->hndl);
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
		
		func->PushCell(entry->hndl);
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
			return 0;
		}
		
		func->PushCell(entry->hndl);
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
			return NULL_STRING;
		}
		
		int len = popspawner_maxiconlen.GetInt();
		if(len < 2) len = 2;

		char *str = new char[len];
		str[0] = '\0';
		
		func->PushCell(entry->hndl);
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
			return false;
		}
		
		func->PushCell(entry->hndl);
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
			return false;
		}
		
		func->PushCell(entry->hndl);
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
	
	pop_entry_t *entry = nullptr;
};

using pop_entry_map_t = std::unordered_map<std::string, pop_entry_t *>;
pop_entry_map_t poentrypmap{};

pop_entry_t::pop_entry_t(const std::string &name_)
	: name{name_}
{
	poentrypmap[name] = this;
}

pop_entry_t::~pop_entry_t()
{
	poentrypmap.erase(name);
	
	pop_spawner_map_t::iterator it{popspawnermap.begin()};
	while(it != popspawnermap.end()) {
		if(it->second->entry == this) {
			it->second->entry = nullptr;
			break;
		}
		
		++it;
	}
}

DETOUR_DECL_STATIC2(ParseSpawner, IPopulationSpawner *, IPopulator *, populator, KeyValues *, data)
{
	IPopulationSpawner *spawner = DETOUR_STATIC_CALL(ParseSpawner)(populator, data);
	if(spawner) {
		return spawner;
	}
	
	const char *name = data->GetName();
	
	pop_entry_map_t::iterator it{poentrypmap.find(name)};
	if(it != poentrypmap.end()) {
		pop_entry_t *entry = it->second;
		entry->populator = populator;
		IPopulationSpawner *spawner = new SPPopulationSpawner{entry, populator};
		
		if(!spawner->Parse(data)) {
			Warning( "Warning reading %s spawner definition\n", name );
			delete spawner;
			spawner = nullptr;
		}
		
		return spawner;
	}
	
	return nullptr;
}

HandleType_t popspawner_handle = 0;

cell_t register_popspawner(IPluginContext *pContext, const cell_t *params)
{
	char *name = nullptr;
	pContext->LocalToString(params[1], &name);
	
	pop_entry_map_t::iterator it{poentrypmap.find(name)};
	if(it != poentrypmap.end()) {
		return pContext->ThrowNativeError("%s is already registered", name);
	}
	
	pop_entry_t *obj = new pop_entry_t{name};
	obj->owner = pContext->GetIdentity();
	obj->hndl = handlesys->CreateHandle(popspawner_handle, obj, pContext->GetIdentity(), myself->GetIdentity(), nullptr);
	return obj->hndl;
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

cell_t Populatorget(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	return (cell_t)obj->populator;
}

cell_t set_data(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	data_map_t::iterator it{obj->data.find(name)};
	if(it == obj->data.end()) {
		it = obj->data.emplace(data_map_t::value_type{name, 0}).first;
	}
	
	it->second = params[3];
	
	return 0;
}

cell_t get_data(IPluginContext *pContext, const cell_t *params)
{
	HandleSecurity security(pContext->GetIdentity(), myself->GetIdentity());
	
	pop_entry_t *obj = nullptr;
	HandleError err = handlesys->ReadHandle(params[1], popspawner_handle, &security, (void **)&obj);
	if(err != HandleError_None)
	{
		return pContext->ThrowNativeError("Invalid Handle %x (error: %d)", params[1], err);
	}
	
	char *name = nullptr;
	pContext->LocalToString(params[2], &name);
	
	data_map_t::iterator it{obj->data.find(name)};
	if(it == obj->data.end()) {
		return pContext->ThrowNativeError("theres no data with the name %s", name);
	}
	
	return it->second;
}

sp_nativeinfo_t natives[] =
{
	{"CustomPopulationSpawner.Parse.set", Parseset},
	{"CustomPopulationSpawner.Spawn.set", Spawnset},
	{"CustomPopulationSpawner.HasEventChangeAttributes.set", HasEventChangeAttributesset},
	{"CustomPopulationSpawner.GetClass.set", GetClassset},
	{"CustomPopulationSpawner.GetHealth.set", GetHealthset},
	{"CustomPopulationSpawner.GetClassIcon.set", GetClassIconset},
	{"CustomPopulationSpawner.IsMiniBoss.set", IsMiniBossset},
	{"CustomPopulationSpawner.HasAttribute.set", HasAttributeset},
	{"CustomPopulationSpawner.WhereRequired.set", WhereRequiredset},
	{"CustomPopulationSpawner.IsVarious.set", IsVariousset},
	{"CustomPopulationSpawner.Populator.get", Populatorget},
	{"CustomPopulationSpawner.set_data", set_data},
	{"CustomPopulationSpawner.get_data", get_data},
	{"register_popspawner", register_popspawner},
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

CDetour *pParseSpawner = nullptr;

bool Sample::RegisterConCommandBase(ConCommandBase *pCommand)
{
	META_REGCVAR(pCommand);
	return true;
}

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	GET_V_IFACE_CURRENT(GetEngineFactory, icvar, ICvar, CVAR_INTERFACE_VERSION);
	g_pCVar = icvar;
	ConVar_Register(0, this);
	return true;
}

bool Sample::SDK_OnLoad(char *error, size_t maxlen, bool late)
{
	IGameConfig *g_pGameConf = nullptr;
	gameconfs->LoadGameConfigFile("popspawner", &g_pGameConf, error, maxlen);
	
	CDetourManager::Init(g_pSM->GetScriptingEngine(), g_pGameConf);
	
	pParseSpawner = DETOUR_CREATE_STATIC(ParseSpawner, "IPopulationSpawner::ParseSpawner")
	pParseSpawner->EnableDetour();
	
	g_pGameConf->GetMemSig("AllocPooledString", &AllocPooledStringPtr);
	
	gameconfs->CloseGameConfigFile(g_pGameConf);
	
	popspawner_handle = handlesys->CreateType("popspawner", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);
	
	sharesys->AddNatives(myself, natives);
	
	sharesys->RegisterLibrary(myself, "popspawner");
	
	HandleSystemHack::init();
	
	return true;
}

void Sample::SDK_OnUnload()
{
	pParseSpawner->Destroy();
	handlesys->RemoveType(popspawner_handle, myself->GetIdentity());
}
