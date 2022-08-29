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

#ifndef FMTFUNCTION
#define FMTFUNCTION(...)
#endif

#include "extension.h"

#include <CDetour/detours.h>
#include <tier1/utlvector.h>
#include <ehandle.h>
#include <ICellArray.h>
using EHANDLE = CHandle<CBaseEntity>;
#include <takedamageinfo.h>
#include <toolframework/itoolentity.h>
typedef wchar_t locchar_t;
#include <tier1/utlmap.h>
#include <vstdlib/random.h>
#include <shareddefs.h>
#include <util.h>
#include <ServerNetworkProperty.h>
#define DECLARE_PREDICTABLE()
#include <collisionproperty.h>
#include <tier1/fmtstr.h>
#include <filesystem.h>

/**
 * @file extension.cpp
 * @brief Implement extension code here.
 */

Sample g_Sample;		/**< Global singleton for extension's main interface */

SMEXT_LINK(&g_Sample);

class CTFTeamSpawn;

typedef CUtlVector< CHandle< CBaseEntity > > EntityHandleVector_t;
typedef CUtlVector< CHandle< CTFTeamSpawn > > TFTeamSpawnVector_t;

void *AllocPooledStringPtr = nullptr;
void *IsSpaceToSpawnHerePtr = nullptr;
ICvar *icvar = nullptr;
CBaseEntityList *g_pEntityList = nullptr;
CGlobalVars *gpGlobals = nullptr;
IFileSystem *filesystem = nullptr;
IServerTools *servertools = nullptr;
IEntityFactoryDictionary *dictionary{nullptr};

size_t info_populator_size{-1};

template <typename R, typename T, typename ...Args>
R call_mfunc(T *pThisPtr, void *offset, Args ...args)
{
	class VEmptyClass {};
	
	void **this_ptr = *reinterpret_cast<void ***>(&pThisPtr);
	
	union
	{
		R (VEmptyClass::*mfpnew)(Args...);
#ifndef PLATFORM_POSIX
		void *addr;
	} u;
	u.addr = offset;
#else
		struct  
		{
			void *addr;
			intptr_t adjustor;
		} s;
	} u;
	u.s.addr = offset;
	u.s.adjustor = 0;
#endif
	
	return (R)(reinterpret_cast<VEmptyClass *>(this_ptr)->*u.mfpnew)(args...);
}

template <typename R, typename T, typename ...Args>
R call_vfunc(T *pThisPtr, size_t offset, Args ...args)
{
	void **vtable = *reinterpret_cast<void ***>(pThisPtr);
	void *vfunc = vtable[offset];
	
	return call_mfunc<R, T, Args...>(pThisPtr, vfunc, args...);
}

template <typename R, typename T, typename ...Args>
R call_vfunc(const T *pThisPtr, size_t offset, Args ...args)
{
	return call_vfunc<R, T, Args...>(const_cast<T *>(pThisPtr), offset, args...);
}

template <typename T>
T void_to_func(void *ptr)
{
	union { T f; void *p; };
	p = ptr;
	return f;
}

template <typename T>
int vfunc_index(T func)
{
	SourceHook::MemFuncInfo info{};
	SourceHook::GetFuncInfo<T>(func, info);
	return info.vtblindex;
}

//TODO!!!!!!!!! update tf2sdk
class CUtlStringHack
{
public:
	CUtlStringHack() = delete;

	void *AllocMemory( uint32 length )
	{
		void *pMemoryBlock;
		if ( m_pString )
		{
			pMemoryBlock = realloc( m_pString, length + 1 );
		}
		else
		{
			pMemoryBlock = malloc( length + 1 );
		}
		m_pString = (char*)pMemoryBlock;
		m_pString[ length ] = 0;

		return pMemoryBlock;
	}

	void SetDirect( const char *pValue, int nChars )
	{
		if ( pValue && nChars > 0 )
		{
			if ( pValue == m_pString )
			{
				AssertMsg( nChars == Q_strlen(m_pString), "CUtlString::SetDirect does not support resizing strings in place." );
				return; // Do nothing. Realloc in AllocMemory might move pValue's location resulting in a bad memcpy.
			}

			Assert( nChars <= Min<int>( strnlen(pValue, nChars) + 1, nChars ) );
			AllocMemory( nChars );
			Q_memcpy( m_pString, pValue, nChars );
		}
		else
		{
			Purge();
		}
	}

	void Purge()
	{
	    free( m_pString );
	    m_pString = NULL;
	}

	void Set( const char *pValue )
	{
		int length = pValue ? V_strlen( pValue ) : 0;
		SetDirect( pValue, length );
	}

	CUtlStringHack &operator=(const char *pValue)
	{
		Set(pValue);
		return *this;
	}

	char *m_pString;
};

//TODO!!!!!!!!! update tf2sdk
#define FmtStrVSNPrintfHack( szBuf, nBufSize, bQuietTruncation, ppszFormat, nPrevLen, lastArg ) \
	do \
	{ \
		int     result; \
		va_list arg_ptr; \
		bool bTruncated = false; \
		static int scAsserted = 0; \
	\
		va_start(arg_ptr, lastArg); \
		result = vsnprintf( (szBuf), (nBufSize)-1, (*(ppszFormat)), arg_ptr ); \
		va_end(arg_ptr); \
	\
		(szBuf)[(nBufSize)-1] = 0; \
		if ( bTruncated && !(bQuietTruncation) && scAsserted < 5 ) \
		{ \
			Warning( "FmtStrVSNPrintf truncated to %d without QUIET_TRUNCATION specified!\n", ( int )( nBufSize ) ); \
			AssertMsg( 0, "FmtStrVSNPrintf truncated without QUIET_TRUNCATION specified!\n" ); \
			scAsserted++; \
		} \
		m_nLength = nPrevLen + result; \
	} \
	while (0)

//TODO!!!!!!!!! update tf2sdk
class CFmtStrHack
{
public:
	CFmtStrHack() = delete;

	virtual void InitQuietTruncation()
	{
		m_bQuietTruncation = false; 
	}

	const char *sprintf(PRINTF_FORMAT_STRING const char *pszFormat, ...) FMTFUNCTION( 2, 3 )
	{
		InitQuietTruncation();
		FmtStrVSNPrintfHack(m_szBuf, FMTSTR_STD_LEN, m_bQuietTruncation, &pszFormat, 0, pszFormat ); 
		return m_szBuf;
	}

	bool m_bQuietTruncation;
	char m_szBuf[FMTSTR_STD_LEN];
	int m_nLength;
};

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
class CBaseCombatCharacter;
struct PlayerUpgradeHistory;
class IPopulator;
class CWave;
class CMannVsMachineStats;
class CWaveSpawnPopulator;

#define MAX_ATTRIBUTE_DESCRIPTION_LENGTH		( 256 / sizeof( locchar_t ) )

class CMvMBotUpgrade
{
public:
	char	szAttrib[ MAX_ATTRIBUTE_DESCRIPTION_LENGTH ];	// Debug
	int		iAttribIndex;	
	float	flValue;
	float	flMax;
	int		nCost;
	bool    bIsBotAttr;
	bool	bIsSkillAttr;		// Probably want to make these an enum or flag later
};

void *CPopulationManagerOnPlayerKilled = nullptr;
void *g_pPopulationManagerPtr{nullptr};

CBaseEntity *CreateEntityByName( const char *szName )
{
	return servertools->CreateEntityByName(szName);
}

int DispatchSpawn( CBaseEntity *pEntity )
{
	servertools->DispatchSpawn(pEntity);
	return 0;
}

void RemoveEntity( CBaseEntity *pEntity )
{
	servertools->RemoveEntity( pEntity );
}

int m_angRotationOffset = -1;
int m_iParentAttachmentOffset = -1;
int m_iEFlagsOffset = -1;
int m_flSimulationTimeOffset = -1;
int m_vecOriginOffset = -1;
int m_hMoveChildOffset = -1;
int m_hMoveParentOffset = -1;
int m_hMovePeerOffset = -1;

int CBaseEntitySetOwnerEntity = -1;
int CBaseEntityWorldSpaceCenter = -1;

float k_flMaxEntityEulerAngle = 360.0 * 1000.0f;
float k_flMaxEntityPosCoord = MAX_COORD_FLOAT;

inline bool IsEntityQAngleReasonable( const QAngle &q )
{
	float r = k_flMaxEntityEulerAngle;
	return
		q.x > -r && q.x < r &&
		q.y > -r && q.y < r &&
		q.z > -r && q.z < r;
}

inline bool IsEntityPositionReasonable( const Vector &v )
{
	float r = k_flMaxEntityPosCoord;
	return
		v.x > -r && v.x < r &&
		v.y > -r && v.y < r &&
		v.z > -r && v.z < r;
}

enum InvalidatePhysicsBits_t
{
	POSITION_CHANGED	= 0x1,
	ANGLES_CHANGED		= 0x2,
	VELOCITY_CHANGED	= 0x4,
	ANIMATION_CHANGED	= 0x8,
};

void SetEdictStateChanged(CBaseEntity *pEntity, int offset);

class IEntityListener
{
public:
	virtual void OnEntityCreated( CBaseEntity *pEntity ) {};
	virtual void OnEntitySpawned( CBaseEntity *pEntity ) {};
	virtual void OnEntityDeleted( CBaseEntity *pEntity ) {};
};

class CGlobalEntityList : public CBaseEntityList
{
public:
	int m_iHighestEnt; // the topmost used array index
	int m_iNumEnts;
	int m_iNumEdicts;
	bool m_bClearingEntities;
	CUtlVector<IEntityListener *>	m_entityListeners;

	void NotifyCreateEntity( CBaseEntity *pEnt )
	{
		if ( !pEnt )
			return;

		//DevMsg(2,"Deleted %s\n", pBaseEnt->GetClassname() );
		for ( int i = m_entityListeners.Count()-1; i >= 0; i-- )
		{
			m_entityListeners[i]->OnEntityCreated( pEnt );
		}
	}
};

int m_iNameOffset = -1;

void *CBaseEntitySetAbsOrigin = nullptr;
void *CBaseEntityCalcAbsolutePosition = nullptr;
int m_vecAbsOriginOffset = -1;

class CBasePlayer;

class CBaseEntity : public IServerEntity
{
public:
	int entindex()
	{
		return gamehelpers->EntityToBCompatRef(this);
	}

	CBasePlayer *IsPlayer()
	{
		int idx = gamehelpers->EntityToBCompatRef(this);
		if(idx >= 1 && idx <= playerhelpers->GetNumPlayers()) {
			return (CBasePlayer *)this;
		} else {
			return nullptr;
		}
	}

	int GetParentAttachment()
	{
		if(m_iParentAttachmentOffset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_iParentAttachment", &info);
			m_iParentAttachmentOffset = info.actual_offset;
		}

		return *(unsigned char *)(((unsigned char *)this) + m_iParentAttachmentOffset);
	}

	void DispatchUpdateTransmitState()
	{

	}

	int GetIEFlags()
	{
		if(m_iEFlagsOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);
			m_iEFlagsOffset = info.actual_offset;
		}
		
		return *(int *)((unsigned char *)this + m_iEFlagsOffset);
	}

	void AddIEFlags(int flags)
	{
		if(m_iEFlagsOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_iEFlags", &info);
			m_iEFlagsOffset = info.actual_offset;
		}

		*(int *)((unsigned char *)this + m_iEFlagsOffset) |= flags;

		if ( flags & ( EFL_FORCE_CHECK_TRANSMIT | EFL_IN_SKYBOX ) )
		{
			DispatchUpdateTransmitState();
		}
	}

	CBaseEntity *FirstMoveChild()
	{
		if(m_hMoveChildOffset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_hMoveChild", &info);
			m_hMoveChildOffset = info.actual_offset;
		}

		return (*(EHANDLE *)(((unsigned char *)this) + m_hMoveChildOffset)).Get();
	}

	CBaseEntity *GetMoveParent()
	{
		if(m_hMoveParentOffset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_hMoveParent", &info);
			m_hMoveParentOffset = info.actual_offset;
		}

		return (*(EHANDLE *)(((unsigned char *)this) + m_hMoveParentOffset)).Get();
	}

	CBaseEntity *NextMovePeer()
	{
		if(m_hMovePeerOffset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_hMovePeer", &info);
			m_hMovePeerOffset = info.actual_offset;
		}

		return (*(EHANDLE *)(((unsigned char *)this) + m_hMovePeerOffset)).Get();
	}

	CCollisionProperty		*CollisionProp() { return (CCollisionProperty		*)GetCollideable(); }
	const CCollisionProperty*CollisionProp() const { return (const CCollisionProperty*)const_cast<CBaseEntity *>(this)->GetCollideable(); }

	CServerNetworkProperty *NetworkProp() { return (CServerNetworkProperty *)GetNetworkable(); }
	const CServerNetworkProperty *NetworkProp() const { return (const CServerNetworkProperty *)const_cast<CBaseEntity *>(this)->GetNetworkable(); }

	void InvalidatePhysicsRecursive( int nChangeFlags )
	{
		// Main entry point for dirty flag setting for the 90% case
		// 1) If the origin changes, then we have to update abstransform, Shadow projection, PVS, KD-tree, 
		//    client-leaf system.
		// 2) If the angles change, then we have to update abstransform, Shadow projection,
		//    shadow render-to-texture, client-leaf system, and surrounding bounds. 
		//	  Children have to additionally update absvelocity, KD-tree, and PVS.
		//	  If the surrounding bounds actually update, when we also need to update the KD-tree and the PVS.
		// 3) If it's due to attachment, then all children who are attached to an attachment point
		//    are assumed to have dirty origin + angles.

		// Other stuff:
		// 1) Marking the surrounding bounds dirty will automatically mark KD tree + PVS dirty.
		
		int nDirtyFlags = 0;

		if ( nChangeFlags & VELOCITY_CHANGED )
		{
			nDirtyFlags |= EFL_DIRTY_ABSVELOCITY;
		}

		if ( nChangeFlags & POSITION_CHANGED )
		{
			nDirtyFlags |= EFL_DIRTY_ABSTRANSFORM;

	#ifndef CLIENT_DLL
			NetworkProp()->MarkPVSInformationDirty();
	#endif

			// NOTE: This will also mark shadow projection + client leaf dirty
			CollisionProp()->MarkPartitionHandleDirty();
		}

		// NOTE: This has to be done after velocity + position are changed
		// because we change the nChangeFlags for the child entities
		if ( nChangeFlags & ANGLES_CHANGED )
		{
			nDirtyFlags |= EFL_DIRTY_ABSTRANSFORM;
			if ( CollisionProp()->DoesRotationInvalidateSurroundingBox() )
			{
				// NOTE: This will handle the KD-tree, surrounding bounds, PVS
				// render-to-texture shadow, shadow projection, and client leaf dirty
				CollisionProp()->MarkSurroundingBoundsDirty();
			}
			else
			{
	#ifdef CLIENT_DLL
				MarkRenderHandleDirty();
				g_pClientShadowMgr->AddToDirtyShadowList( this );
				g_pClientShadowMgr->MarkRenderToTextureShadowDirty( GetShadowHandle() );
	#endif
			}

			// This is going to be used for all children: children
			// have position + velocity changed
			nChangeFlags |= POSITION_CHANGED | VELOCITY_CHANGED;
		}

		AddIEFlags( nDirtyFlags );

		// Set flags for children
		bool bOnlyDueToAttachment = false;
		if ( nChangeFlags & ANIMATION_CHANGED )
		{
	#ifdef CLIENT_DLL
			g_pClientShadowMgr->MarkRenderToTextureShadowDirty( GetShadowHandle() );
	#endif

			// Only set this flag if the only thing that changed us was the animation.
			// If position or something else changed us, then we must tell all children.
			if ( !( nChangeFlags & (POSITION_CHANGED | VELOCITY_CHANGED | ANGLES_CHANGED) ) )
			{
				bOnlyDueToAttachment = true;
			}

			nChangeFlags = POSITION_CHANGED | ANGLES_CHANGED | VELOCITY_CHANGED;
		}

		for (CBaseEntity *pChild = FirstMoveChild(); pChild; pChild = pChild->NextMovePeer())
		{
			// If this is due to the parent animating, only invalidate children that are parented to an attachment
			// Entities that are following also access attachments points on parents and must be invalidated.
			if ( bOnlyDueToAttachment )
			{
	#ifdef CLIENT_DLL
				if ( (pChild->GetParentAttachment() == 0) && !pChild->IsFollowingEntity() )
					continue;
	#else
				if ( pChild->GetParentAttachment() == 0 )
					continue;
	#endif
			}
			pChild->InvalidatePhysicsRecursive( nChangeFlags );
		}

		//
		// This code should really be in here, or the bone cache should not be in world space.
		// Since the bone transforms are in world space, if we move or rotate the entity, its
		// bones should be marked invalid.
		//
		// As it is, we're near ship, and don't have time to setup a good A/B test of how much
		// overhead this fix would add. We've also only got one known case where the lack of
		// this fix is screwing us, and I just fixed it, so I'm leaving this commented out for now.
		//
		// Hopefully, we'll put the bone cache in entity space and remove the need for this fix.
		//
		//#ifdef CLIENT_DLL
		//	if ( nChangeFlags & (POSITION_CHANGED | ANGLES_CHANGED | ANIMATION_CHANGED) )
		//	{
		//		C_BaseAnimating *pAnim = GetBaseAnimating();
		//		if ( pAnim )
		//			pAnim->InvalidateBoneCache();		
		//	}
		//#endif
	}

	void SetLocalAngles( const QAngle& angles )
	{
		if(m_angRotationOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_angRotation", &info);
			m_angRotationOffset = info.actual_offset;
		}

		// NOTE: The angle normalize is a little expensive, but we can save
		// a bunch of time in interpolation if we don't have to invalidate everything
		// and sometimes it's off by a normalization amount

		// FIXME: The normalize caused problems in server code like momentary_rot_button that isn't
		//        handling things like +/-180 degrees properly. This should be revisited.
		//QAngle angleNormalize( AngleNormalize( angles.x ), AngleNormalize( angles.y ), AngleNormalize( angles.z ) );

		// Safety check against NaN's or really huge numbers
		if ( !IsEntityQAngleReasonable( angles ) )
		{
			return;
		}

		if (*(QAngle *)((unsigned char *)this + m_angRotationOffset) != angles)
		{
			InvalidatePhysicsRecursive( ANGLES_CHANGED );
			*(QAngle *)((unsigned char *)this + m_angRotationOffset) = angles;
			SetEdictStateChanged(this, m_angRotationOffset);
			SetSimulationTime( gpGlobals->curtime );
		}
	}

	void SetAbsOrigin( const Vector& origin )
	{
		call_mfunc<void, CBaseEntity, const Vector &>(this, CBaseEntitySetAbsOrigin, origin);
	}

	const Vector &GetAbsOrigin()
	{
		if(m_vecAbsOriginOffset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_vecAbsOrigin", &info);
			m_vecAbsOriginOffset = info.actual_offset;
		}
		
		if(GetIEFlags() & EFL_DIRTY_ABSTRANSFORM) {
			CalcAbsolutePosition();
		}
		
		return *(Vector *)(((unsigned char *)this) + m_vecAbsOriginOffset);
	}

	void CalcAbsolutePosition()
	{
		call_mfunc<void, CBaseEntity>(this, CBaseEntityCalcAbsolutePosition);
	}

	void SetSimulationTime(float time)
	{
		if(m_flSimulationTimeOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_flSimulationTime", &info);
			m_flSimulationTimeOffset = info.actual_offset;
		}

		*(float *)((unsigned char *)this + m_flSimulationTimeOffset) = time;
		SetEdictStateChanged(this, m_flSimulationTimeOffset);
	}

	void SetLocalOrigin( const Vector& origin )
	{
		if(m_vecOriginOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_vecOrigin", &info);
			m_vecOriginOffset = info.actual_offset;
		}

		// Safety check against NaN's or really huge numbers
		if ( !IsEntityPositionReasonable( origin ) )
		{
			return;
		}

	//	if ( !origin.IsValid() )
	//	{
	//		AssertMsg( 0, "Bad origin set" );
	//		return;
	//	}

		if (*(Vector *)((unsigned char *)this + m_vecOriginOffset) != origin)
		{
			InvalidatePhysicsRecursive( POSITION_CHANGED );
			*(Vector *)((unsigned char *)this + m_vecOriginOffset) = origin;
			SetEdictStateChanged(this, m_vecOriginOffset);
			SetSimulationTime( gpGlobals->curtime );
		}
	}

	void SetOwnerEntity( CBaseEntity* pOwner )
	{
		call_vfunc<void, CBaseEntity, CBaseEntity *>(this, CBaseEntitySetOwnerEntity, pOwner);
	}

	const Vector &WorldSpaceCenter( ) const
	{
		return call_vfunc<const Vector &>(this, CBaseEntityWorldSpaceCenter);
	}

	string_t GetEntityName()
	{
		if(m_iNameOffset == -1) {
			sm_datatable_info_t info{};
			datamap_t *pMap = gamehelpers->GetDataMap(this);
			gamehelpers->FindDataMapInfo(pMap, "m_iName", &info);
			m_iNameOffset = info.actual_offset;
		}

		return *(string_t *)((unsigned char *)this + m_iNameOffset);
	}

	static CBaseEntity *CreateNoSpawn( const char *szName, const Vector &vecOrigin, const QAngle &vecAngles, CBaseEntity *pOwner = NULL )
	{
		CBaseEntity *pEntity{CreateEntityByName(szName)};

		pEntity->SetLocalOrigin( vecOrigin );
		pEntity->SetLocalAngles( vecAngles );
		pEntity->SetOwnerEntity( pOwner );

		((CGlobalEntityList *)g_pEntityList)->NotifyCreateEntity( pEntity );

		return pEntity;
	}
};

class CBaseCombatCharacter : public CBaseEntity
{
};

void CCollisionProperty::MarkSurroundingBoundsDirty()
{
	GetOuter()->AddIEFlags( EFL_DIRTY_SURROUNDING_COLLISION_BOUNDS );
	MarkPartitionHandleDirty();

#ifdef CLIENT_DLL
	g_pClientShadowMgr->MarkRenderToTextureShadowDirty( GetOuter()->GetShadowHandle() );
#else
	GetOuter()->NetworkProp()->MarkPVSInformationDirty();
#endif
}

void CCollisionProperty::MarkPartitionHandleDirty()
{
	// don't bother with the world
	if ( m_pOuter->entindex() == 0 )
		return;
	
	if ( !(m_pOuter->GetIEFlags() & EFL_DIRTY_SPATIAL_PARTITION) )
	{
		m_pOuter->AddIEFlags( EFL_DIRTY_SPATIAL_PARTITION );
		//s_DirtyKDTree.AddEntity( m_pOuter );
	}

#ifdef CLIENT_DLL
	GetOuter()->MarkRenderHandleDirty();
	g_pClientShadowMgr->AddToDirtyShadowList( GetOuter() );
#endif
}

struct CPopulationManager_members_t
{
	CUtlVector< IPopulator * > m_populatorVector;
	char m_popfileFull[ MAX_PATH ];
	char m_popfileShort[ MAX_PATH ];
	
	KeyValues	*m_pTemplates;

	bool m_bIsInitialized;
	bool m_bAllocatedBots;
	
	bool m_bBonusRound;
	CHandle< CBaseCombatCharacter > m_hBonusBoss;

	int m_nStartingCurrency;
	int m_nLobbyBonusCurrency;
	int m_nMvMEventPopfileType;
	int m_nRespawnWaveTime;
	bool m_bFixedRespawnWaveTime;
	bool m_canBotsAttackWhileInSpawnRoom;
	int m_sentryBusterDamageDealtThreshold;
	int m_sentryBusterKillThreshold;

	uint32	m_iCurrentWaveIndex;
	CUtlVector< CWave * > m_waveVector;		// pointers to waves within m_populationVector

	float m_flMapRestartTime;					// Restart the Map if gameover and this time elapses

	CUtlVector< PlayerUpgradeHistory * > m_playerUpgrades;		// list of all players and their upgrades who have played on this MVM rotation

	bool m_isRestoringCheckpoint;

	bool m_bAdvancedPopFile;
	bool m_bCheckForCurrencyAchievement;

	CMannVsMachineStats *m_pMVMStats;
	KeyValues *m_pKvpMvMMapCycle;

	bool m_bSpawningPaused;
	bool m_bIsWaveJumping;
	bool m_bEndlessOn;
	CUtlVector< CMvMBotUpgrade > m_BotUpgradesList;
	CUtlVector< CMvMBotUpgrade > m_EndlessActiveBotUpgrades;
	CUniformRandomStream m_randomizer;
	CUtlVector< int > m_EndlessSeeds;
	bool m_bShouldResetFlag;
	CUtlVector< const CTFPlayer* > m_donePlayers;

	CUtlStringHack m_defaultEventChangeAttributesName;

	// Respec
	CUtlMap< uint64, int > m_PlayerRespecPoints;	// The number of upgrade respecs players (steamID) have
	int m_nRespecsAwarded;
	int m_nRespecsAwardedInWave;
	int m_nCurrencyCollectedForRespec;

	// Buyback
	CUtlMap< uint64, int > m_PlayerBuybackPoints;	// The number of times a player can buyback
};

ConVar *tf_populator_health_multiplier{nullptr};
ConVar *tf_mvm_endless_tank_boost{nullptr};
ConVar *tf_mvm_endless_force_on{nullptr};
ConVar *tf_populator_damage_multiplier{nullptr};
ConVar *tf_mvm_endless_damage_boost_rate{nullptr};
ConVar *tf_mvm_endless_scale_rate{nullptr};

class CPopulationManager : public CBaseEntity
{
public:
	CPopulationManager_members_t &GetMembers()
	{
		return *(CPopulationManager_members_t *)(((unsigned char *)this) + ((info_populator_size - sizeof(CPopulationManager_members_t))));
	}

	bool DetourParse( void );

	KeyValues *GetTemplate( const char *pszName )
	{
		CPopulationManager_members_t &members{GetMembers()};

		KeyValues *pTemplate = nullptr;

		if(members.m_pTemplates) {
			pTemplate = members.m_pTemplates->FindKey( pszName );
		}

		return pTemplate;
	}

	void OnPlayerKilled( CTFPlayer *corpse )
	{
		call_mfunc<void, CPopulationManager, CTFPlayer *>(this, CPopulationManagerOnPlayerKilled, corpse);
	}

	float GetDamageMultiplier ()
	{
		CPopulationManager_members_t &members{GetMembers()};

		//if ( !IsInEndlessWaves() )
			return tf_populator_damage_multiplier->GetFloat();

		// Find out how many times over t
		// Floor of the result, ie 9 / 7 returns 1, 15 / 7 returns 2;
		//int nRepeatCount = members.m_iCurrentWaveIndex / tf_mvm_endless_scale_rate->GetInt();
		//return tf_populator_damage_multiplier->GetFloat() + tf_mvm_endless_damage_boost_rate->GetFloat() * nRepeatCount;
	}

	float GetHealthMultiplier ( bool bIsTank )
	{
		CPopulationManager_members_t &members{GetMembers()};

		if ( !IsInEndlessWaves() || !bIsTank )
			return tf_populator_health_multiplier->GetFloat();

		// Calculate how much health the tank should get per wave
		return tf_populator_health_multiplier->GetFloat() + members.m_iCurrentWaveIndex * tf_mvm_endless_tank_boost->GetFloat();
	}

	bool IsInEndlessWaves ( void )
	{
		CPopulationManager_members_t &members{GetMembers()};

		return (members.m_bEndlessOn || tf_mvm_endless_force_on->GetBool() ) && members.m_waveVector.Count() > 0;
	}

	CWave * GetCurrentWave( void )
	{
		CPopulationManager_members_t &members{GetMembers()};

		if ( !members.m_bIsInitialized || members.m_waveVector.Count() == 0 )
			return NULL;

		// Wrap for Infinite MVM
		if ( IsInEndlessWaves() )
		{
			return members.m_waveVector[members.m_iCurrentWaveIndex % members.m_waveVector.Count() ];
		}
		else if ( (int)members.m_iCurrentWaveIndex < members.m_waveVector.Count() )
		{
			return members.m_waveVector[members.m_iCurrentWaveIndex];
		}
		
		return NULL;
	}
};

CPopulationManager *GetPopulationManager()
{
	return ((CPopulationManager *)g_pPopulationManagerPtr);
}

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

SH_DECL_MANUALHOOK0_void(GenericDtor, 1, 0, 0)
SH_DECL_HOOK1(IPopulator, HasEventChangeAttributes, const, 0, bool, const char *);

enum populator_type_t
{
	populator_none,
	populator_unknown,
	populator_wavespawn,
	populator_mission,
};

static populator_type_t g_hackdetectpopulator{populator_none};

#define TF_BASE_BOSS_CURRENCY 125

void *CWaveSpawnPopulatorGetCurrencyAmountPerDeath{nullptr};

struct EventInfo;

enum RelativePositionType
{
	UNDEFINED = 0,
	AHEAD,
	BEHIND,
	ANYWHERE
};

void *CSpawnLocationParsePtr = nullptr;

class CSpawnLocation
{
public:
	bool Parse( KeyValues *data )
	{
		return call_mfunc<bool, CSpawnLocation, KeyValues *>(this, CSpawnLocationParsePtr, data);
	}

	bool DetourParse( KeyValues *data );

	RelativePositionType m_relative;
	TFTeamSpawnVector_t m_teamSpawnVector;

	int m_nSpawnCount;
	int m_nRandomSeed;
	bool m_bClosestPointOnNav;
};

class CTFTeamSpawn : public CBaseEntity
{
public:
	
};

static CUtlVector<CTFTeamSpawn *> *m_ITFTeamSpawnAutoListAutoList{nullptr};

IForward *spawnlocation_parse{nullptr};

static IPopulator *last_populator{nullptr};

#include "icandowhateveriwantthefactthattheresnowaytodothisstillisridiculous.h"

bool CSpawnLocation::DetourParse( KeyValues *data )
{
	const char *name = data->GetName();
	const char *value = data->GetString();

	if ( Q_strlen( name ) <= 0 )
	{
		return false;
	}

	if ( FStrEq( name, "Where" ) || FStrEq( name, "ClosestPoint" ) )
	{
		if ( FStrEq( value, "Ahead" ) )
		{
			m_relative = AHEAD;
		}
		else if ( FStrEq( value, "Behind" ) )
		{
			m_relative = BEHIND;
		}
		else if ( FStrEq( value, "Anywhere" ) )
		{
			m_relative = ANYWHERE;
		}
		else if ( FStrEq( value, "PluginSpawnLocation" ) )
		{
			if(spawnlocation_parse->GetFunctionCount() > 0) {
				HandleError err{};
				Handle_t hndl = ((HandleSystemHack *)handlesys)->CreateKeyValuesHandle(data, nullptr, &err);
				if(err != HandleError_None) {
					smutils->LogError(myself, "Invalid KeyValues handle %x (error %d).", hndl, err);
					return false;
				}

				spawnlocation_parse->PushCell((cell_t)last_populator);
				spawnlocation_parse->PushCell((cell_t)this);
				spawnlocation_parse->PushCell(hndl);
				cell_t result = 0;
				spawnlocation_parse->PushCellByRef(&result);
				cell_t res = 0;
				spawnlocation_parse->Execute(&res);

				handlesys->FreeHandle(hndl, nullptr);

				if(res == Pl_Changed) {
					if(!result) {
						return false;
					}
				} else if(res >= Pl_Handled) {
					return false;
				}
			}
		}
		else
		{
			m_bClosestPointOnNav = FStrEq( name, "ClosestPoint" );

			// collect entities with given name
			bool bFound = false;
			for ( int i=0; i<m_ITFTeamSpawnAutoListAutoList->Count(); ++i )
			{
				CTFTeamSpawn* pTeamSpawn = static_cast< CTFTeamSpawn* >( (*m_ITFTeamSpawnAutoListAutoList)[i] );
				if ( FStrEq( STRING( pTeamSpawn->GetEntityName() ), value ) )
				{
					m_teamSpawnVector.AddToTail( pTeamSpawn );
					bFound = true;
				}
			}

			if ( !bFound )
			{
				Warning( "Invalid Where argument '%s'\n", value );
				return false;
			}
		}

		return true;
	}

	return false;
}

DETOUR_DECL_MEMBER1(SpawnLocationParse, bool, KeyValues *, values)
{
	return ((CSpawnLocation *)this)->CSpawnLocation::DetourParse(values);
}

enum SpawnLocationResult
{
	SPAWN_LOCATION_NOT_FOUND = 0,
	SPAWN_LOCATION_NAV,
	SPAWN_LOCATION_TELEPORTER
};

#define MVM_CLASS_FLAG_NONE				0
#define MVM_CLASS_FLAG_NORMAL			(1<<0)
#define MVM_CLASS_FLAG_SUPPORT			(1<<1)
#define MVM_CLASS_FLAG_MISSION			(1<<2)
#define MVM_CLASS_FLAG_MINIBOSS			(1<<3)
#define MVM_CLASS_FLAG_ALWAYSCRIT		(1<<4)
#define MVM_CLASS_FLAG_SUPPORT_LIMITED	(1<<5)

void *CWaveSpawnPopulatorCTOR{nullptr};
void *CRandomPlacementPopulatorCTOR{nullptr};
void *CPeriodicSpawnPopulatorCTOR{nullptr};
void *CMissionPopulatorCTOR{nullptr};
void *CWaveCTOR{nullptr};

class CWaveSpawnPopulator : public IPopulator
{
public:
	static CWaveSpawnPopulator *create(CPopulationManager *pManager)
	{
		CWaveSpawnPopulator *bytes = (CWaveSpawnPopulator *)calloc(1, sizeof(CWaveSpawnPopulator));
		call_mfunc<void, CWaveSpawnPopulator, CPopulationManager *>(bytes, CWaveSpawnPopulatorCTOR, pManager);
		SH_ADD_MANUALHOOK(GenericDtor, bytes, SH_MEMBER(bytes, &CWaveSpawnPopulator::dtor), false);
		SH_ADD_HOOK(IPopulator, HasEventChangeAttributes, bytes, SH_MEMBER(bytes, &CWaveSpawnPopulator::HookHasEventChangeAttributes), false);
		return bytes;
	}

	void dtor()
	{
		CWaveSpawnPopulator *bytes = META_IFACEPTR(CWaveSpawnPopulator);
		SH_REMOVE_MANUALHOOK(GenericDtor, bytes, SH_MEMBER(this, &CWaveSpawnPopulator::dtor), false);
		SH_REMOVE_HOOK(IPopulator, HasEventChangeAttributes, bytes, SH_MEMBER(this, &CWaveSpawnPopulator::HookHasEventChangeAttributes), false);
		RETURN_META(MRES_HANDLED);
	}

	bool HookHasEventChangeAttributes( const char* pszEventName ) const
	{
		if(strcmp(pszEventName, "__hack_detect_populator__") == 0) {
			g_hackdetectpopulator = populator_wavespawn;
			RETURN_META_VALUE(MRES_SUPERCEDE, false);
		}

		RETURN_META_VALUE(MRES_IGNORED, false);
	}

	int GetCurrencyAmountPerDeath( void )
	{
		return call_mfunc<int, CWaveSpawnPopulator>(this, CWaveSpawnPopulatorGetCurrencyAmountPerDeath);
	}

	bool DetourParse( KeyValues *values );

	bool IsLimitedSupportWave( void ) const { return m_bLimitedSupport; }
	bool IsSupportWave( void ) const { return m_bSupportWave; }

	void SetParent( CWave *pParent ) { m_pParent = pParent; }

	CSpawnLocation m_where;
	int m_totalCount;
	int m_remainingCount;
	int m_nClassCounts;
	int m_maxActive;						// the maximum number of entities active at one time
	int m_spawnCount;						// the number of entities to spawn at once
	float m_waitBeforeStarting;
	float m_waitBetweenSpawns;				// between spawns of mobs
	bool m_bWaitBetweenSpawnAfterDeath;

	CFmtStrHack m_startWaveWarningSound;
	EventInfo *m_startWaveOutput;

	CFmtStrHack m_firstSpawnWarningSound;
	EventInfo *m_firstSpawnOutput;

	CFmtStrHack m_lastSpawnWarningSound;
	EventInfo *m_lastSpawnOutput;

	CFmtStrHack m_doneWarningSound;
	EventInfo *m_doneOutput;

	int		m_totalCurrency;
	int		m_unallocatedCurrency;

	CUtlStringHack m_name;
	CUtlStringHack m_waitForAllSpawned;
	CUtlStringHack m_waitForAllDead;

	CountdownTimer m_timer;
	EntityHandleVector_t m_activeVector;
	int m_countSpawnedSoFar;
	int m_myReservedSlotCount;
	
	bool m_bSupportWave;
	bool m_bLimitedSupport;
	CWave *m_pParent;

	enum InternalStateType
	{
		PENDING,
		PRE_SPAWN_DELAY,
		SPAWNING,
		WAIT_FOR_ALL_DEAD,
		DONE
	};
	InternalStateType m_state;

	bool m_bRandomSpawn;
	SpawnLocationResult m_spawnLocationResult;
	Vector m_vSpawnPosition;
};

using MissionType = int;

class CMissionPopulator : public IPopulator
{
public:
	static CMissionPopulator *create(CPopulationManager *pManager)
	{
		CMissionPopulator *bytes = (CMissionPopulator *)calloc(1, sizeof(CMissionPopulator));
		call_mfunc<void, CMissionPopulator, CPopulationManager *>(bytes, CMissionPopulatorCTOR, pManager);
		SH_ADD_MANUALHOOK(GenericDtor, bytes, SH_MEMBER(bytes, &CMissionPopulator::dtor), false);
		SH_ADD_HOOK(IPopulator, HasEventChangeAttributes, bytes, SH_MEMBER(bytes, &CMissionPopulator::HookHasEventChangeAttributes), false);
		return bytes;
	}

	void dtor()
	{
		CMissionPopulator *bytes = META_IFACEPTR(CMissionPopulator);
		SH_REMOVE_MANUALHOOK(GenericDtor, bytes, SH_MEMBER(this, &CMissionPopulator::dtor), false);
		SH_REMOVE_HOOK(IPopulator, HasEventChangeAttributes, bytes, SH_MEMBER(this, &CMissionPopulator::HookHasEventChangeAttributes), false);
		RETURN_META(MRES_HANDLED);
	}

	bool HookHasEventChangeAttributes( const char* pszEventName ) const
	{
		if(strcmp(pszEventName, "__hack_detect_populator__") == 0) {
			g_hackdetectpopulator = populator_mission;
			RETURN_META_VALUE(MRES_SUPERCEDE, false);
		}

		RETURN_META_VALUE(MRES_IGNORED, false);
	}

	int BeginAtWave( void ) { return m_beginAtWaveIndex; }
	int StopAtWave( void ) { return m_stopAtWaveIndex; }

	MissionType m_mission;
	CSpawnLocation m_where;

	enum StateType
	{
		NOT_STARTED,
		INITIAL_COOLDOWN,
		RUNNING
	};

	StateType m_state;

	float m_initialCooldown;
	float m_cooldownDuration;
	CountdownTimer m_cooldownTimer;
	CountdownTimer m_checkForDangerousSentriesTimer;
	int m_desiredCount;
	int m_beginAtWaveIndex;					// this mission becomes active at this wave number
	int m_stopAtWaveIndex;					// stop when this wave becomes active
};

struct WaveClassCount_t
{
	int nClassCount;
	string_t iszClassIconName;
	unsigned int iFlags;
};

class CWave : public IPopulator
{
public:
	struct vars_t
	{
		size_t index{0};
	};

	unsigned char *vars_ptr()
	{ return (((unsigned char *)this) + sizeof(CWave)); }
	vars_t &getvars()
	{ return *(vars_t *)vars_ptr(); }

	static CWave *create(CPopulationManager *pManager)
	{
		CWave *bytes = (CWave *)calloc(1, sizeof(CWave));
		call_mfunc<void, CWave, CPopulationManager *>(bytes, CWaveCTOR, pManager);
		new (bytes->vars_ptr()) vars_t{};
		SH_ADD_MANUALHOOK(GenericDtor, bytes, SH_MEMBER(bytes, &CWave::dtor), false);
		return bytes;
	}

	void dtor()
	{
		CWave *bytes = META_IFACEPTR(CWave);
		bytes->getvars().~vars_t();
		SH_REMOVE_MANUALHOOK(GenericDtor, bytes, SH_MEMBER(this, &CWave::dtor), false);
		RETURN_META(MRES_HANDLED);
	}

	bool DetourParse( KeyValues *data );

	void AddClassType( string_t iszClassIconName, int nCount, unsigned int iFlags )
	{
		int nIndex = -1;
		for ( int nClass = 0; nClass < m_nWaveClassCounts.Count(); ++nClass )
		{
			if ( ( m_nWaveClassCounts[ nClass ].iszClassIconName == iszClassIconName ) && ( m_nWaveClassCounts[ nClass ].iFlags & iFlags ) )
			{
				nIndex = nClass;
				break;
			}
		}

		if ( nIndex == -1 )
		{
			nIndex = m_nWaveClassCounts.AddToTail();
			m_nWaveClassCounts[ nIndex ].iszClassIconName = iszClassIconName;
			m_nWaveClassCounts[ nIndex ].nClassCount = 0;
			m_nWaveClassCounts[ nIndex ].iFlags = MVM_CLASS_FLAG_NONE;
		}

		m_nWaveClassCounts[ nIndex ].nClassCount += nCount;
		m_nWaveClassCounts[ nIndex ].iFlags |= iFlags;
	}

	CUtlVector< CWaveSpawnPopulator * > m_waveSpawnVector;

	bool m_isStarted;
	bool m_bFiredInitWaveOutput;
	int m_iEnemyCount;
	int m_nTanksSpawned;
	int m_nSentryBustersSpawned;
	int m_nNumEngineersTeleportSpawned;

	int m_nNumSentryBustersKilled;

	CUtlVector< WaveClassCount_t > m_nWaveClassCounts;
	int	m_totalCurrency;

	EventInfo *m_startOutput;
	EventInfo *m_doneOutput;
	EventInfo *m_initOutput;

	CFmtStrHack m_description;
	CFmtStrHack m_soundName;
	
	float m_waitWhenDone;
	CountdownTimer m_doneTimer;	

	bool m_bCheckBonusCreditsMin;
	bool m_bCheckBonusCreditsMax;
	float m_flBonusCreditsTime;

	bool m_bPlayedUpgradeAlert;
	CountdownTimer m_GetUpgradesAlertTimer;

	bool m_isEveryContainedWaveSpawnDone;
	float m_flStartTime;
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

ConVar popspawner_maxiconlen("popspawner_maxiconlen", "260");

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

		KeyValues *pTemplate = data->FindKey("Template");
		if ( pTemplate )
		{
			KeyValues *pTemplateKV = GetPopulator()->GetManager()->GetTemplate( pTemplate->GetString() );
			if ( pTemplateKV )
			{
				// Pump all the keys into ourself now
				if ( Parse( pTemplateKV ) == false )
				{
					return false;
				}
			}
			else
			{
				Warning( "Unknown Template '%s' in %s definition\n", pTemplate->GetString(), name.c_str() );
			}
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

		Handle_t hndl = BAD_HANDLE;
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
		if(strcmp(pszEventName, "__hack_detect_spawner__") == 0) {
			return true;
		}

		if(!entry) {
			return false;
		}
		
		IPluginFunction *func = entry->HasEventChangeAttributes;
		if(!func) {
			return false;
		}
		
		func->PushCell((cell_t)this);
		func->PushStringEx((char *)pszEventName, strlen(pszEventName)+1, SM_PARAM_STRING_COPY|SM_PARAM_STRING_UTF8, 0);
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

		cell_t res = 0;

		IPluginFunction *func = entry->GetHealth;
		if(func) {
			func->PushCell((cell_t)this);
			func->PushCell(nSpawnNum);
			func->Execute(&res);
		}
		
		if(res == 0) {
			func = entry->GetClass;
			if(func) {
				func->PushCell((cell_t)this);
				func->PushCell(nSpawnNum);
				res = 0;
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
		}
		
		return res;
	}
	
	string_t GetClassIcon( int nSpawnNum = -1 )
	{
		if(!entry) {
			return NULL_STRING;
		}

		cell_t res = 0;

		string_t id = NULL_STRING;

		IPluginFunction *func = entry->GetClassIcon;
		if(func) {
			int len = popspawner_maxiconlen.GetInt();
			if(len < 2) len = 2;

			char *str = new char[len];
			str[0] = '\0';
			
			func->PushCell((cell_t)this);
			func->PushCell(nSpawnNum);
			func->PushStringEx(str, len, SM_PARAM_STRING_UTF8, SM_PARAM_COPYBACK);
			func->PushCell(len);
			func->Execute(&res);
			
			if(res) {
				id = ((string_t(*)(const char *))AllocPooledStringPtr)( str );
			}
			
			delete[] str;
		}
		
		if(!res) {
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
		}

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

	Parse = nullptr;
	Spawn = nullptr;
	HasEventChangeAttributes = nullptr;
	GetClass = nullptr;
	GetHealth = nullptr;
	GetClassIcon = nullptr;
	IsMiniBoss = nullptr;
	HasAttribute = nullptr;
	Delete = nullptr;
	WhereRequired = false;
	IsVarious = false;
	name.clear();
	owner = nullptr;
	hndl = BAD_HANDLE;
}

class CTakeDamageInfo;

SH_DECL_HOOK2(IPopulationSpawner, Spawn, SH_NOATTRIB, 0, bool, const Vector &, EntityHandleVector_t *);
SH_DECL_MANUALHOOK1_void(Event_Killed, 0, 0, 0, const CTakeDamageInfo &)

static int objective_resource_ref = INVALID_EHANDLE_INDEX;

CBaseEntity *FindEntityByClassname(CBaseEntity *pStart, const char *classname)
{
	return servertools->FindEntityByClassname(pStart, classname);
}

void Sample::OnCoreMapStart(edict_t *pEdictList, int edictCount, int clientMax)
{
	CBaseEntity *pEntity = FindEntityByClassname(nullptr, "tf_objective_resource");
	if(pEntity) {
		objective_resource_ref = gamehelpers->EntityToReference(pEntity);
	}
}

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

	void IncrementMannVsMachineWaveClassCount(string_t iszClassIconName, unsigned int iFlags)
	{
		int i = 0;
		for(i = 0 ; i < MVM_CLASS_TYPES_PER_WAVE_MAX; ++i) {
			if((m_iszMannVsMachineWaveClassNames()[i] == iszClassIconName) && (m_nMannVsMachineWaveClassFlags()[i] & iFlags)) {
				m_nMannVsMachineWaveClassCounts()[i] += 1;

				if(m_nMannVsMachineWaveClassCounts()[i] <= 0) {
					m_nMannVsMachineWaveClassCounts()[i] = 1;
				}

				SetEdictStateChanged(this, m_nMannVsMachineWaveClassCounts_offset + (sizeof(int) * i));
				return;
			}
		}

		for(i = 0 ; i < MVM_CLASS_TYPES_PER_WAVE_MAX; ++i) {
			if((m_iszMannVsMachineWaveClassNames2()[i] == iszClassIconName) && (m_nMannVsMachineWaveClassFlags2()[i] & iFlags)) {
				m_nMannVsMachineWaveClassCounts2()[i] -= 1;

				if(m_nMannVsMachineWaveClassCounts2()[i] <= 0) {
					m_nMannVsMachineWaveClassCounts2()[i] = 1;
				}

				SetEdictStateChanged(this, m_nMannVsMachineWaveClassCounts2_offset + (sizeof(int) * i));
				return;
			}
		}
	}
};

struct entpopdata_t
{
	bool removed{false};
	string_t icon{NULL_STRING};
	int attrs{0};
	size_t m_currencyValue{TF_BASE_BOSS_CURRENCY};
	IPopulationSpawner *m_spawner{nullptr};
	CWaveSpawnPopulator *m_pWaveSpawnPopulator{nullptr};
};

static std::unordered_map<int, entpopdata_t> entpopdata{};

class CBaseCombatCharacter;

void *CTFPowerupDropSingleInstance{nullptr};

class CTFPowerup : public CBaseEntity
{
public:
	void DropSingleInstance( Vector &vecLaunchVel, CBaseCombatCharacter *pThrower, float flThrowerTouchDelay, float flResetTime = 0.1f )
	{
		call_mfunc<void, CTFPowerup, Vector &, CBaseCombatCharacter *, float, float>(this, CTFPowerupDropSingleInstance, vecLaunchVel, pThrower, flThrowerTouchDelay, flResetTime);
	}
};

int m_nAmountOffset = -1;

class CCurrencyPackCustom : public CTFPowerup
{
public:
	void SetAmount( float flAmount )
	{
		if(m_nAmountOffset == -1) {
			datamap_t *map = gamehelpers->GetDataMap(this);
			sm_datatable_info_t info{};
			gamehelpers->FindDataMapInfo(map, "m_iszModel", &info);
			m_nAmountOffset = info.actual_offset + sizeof(string_t) + sizeof(float) + 4;
		}

		*(int *)(((unsigned char *)this) + m_nAmountOffset) = flAmount;
	}
};

static void pop_remove_entity(CBaseEntity *pEntity, entpopdata_t &data)
{
	if(!data.removed) {

		CTFObjectiveResource *pObjectiveResource = (CTFObjectiveResource *)gamehelpers->ReferenceToEntity(objective_resource_ref);
		if(pObjectiveResource) {
			pObjectiveResource->DecrementMannVsMachineWaveClassCount(data.icon, data.attrs);
		}

		CPopulationManager *PopulationManager = GetPopulationManager();
		if(PopulationManager) {
			PopulationManager->OnPlayerKilled((CTFPlayer *)pEntity);
		}

		data.removed = true;
	}
}

static void hook_entity_killed_pre(const CTakeDamageInfo &info)
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);

	int ref = gamehelpers->EntityToReference(pEntity);

	auto it{entpopdata.find(ref)};
	if(it != entpopdata.cend()) {
		entpopdata_t &data{it->second};

		pop_remove_entity(pEntity, data);

		RETURN_META(MRES_HANDLED);
	}

	RETURN_META(MRES_IGNORED);
}

static void hook_entity_killed_post(const CTakeDamageInfo &info)
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);

	int ref = gamehelpers->EntityToReference(pEntity);

	auto it{entpopdata.find(ref)};
	if(it != entpopdata.cend()) {
		entpopdata_t &data{it->second};

		size_t nRemainingMoney{data.m_currencyValue};
		if(data.m_pWaveSpawnPopulator) {
			nRemainingMoney = data.m_pWaveSpawnPopulator->GetCurrencyAmountPerDeath();
		}

		QAngle angRand = vec3_angle;

		while( nRemainingMoney > 0 )
		{
			int nAmount = 0;

			if ( nRemainingMoney >= 100 )
			{
				nAmount = 25;
			}
			else if ( nRemainingMoney >= 40 )
			{
				nAmount = 10;
			}
			else if ( nRemainingMoney >= 5 )
			{
				nAmount = 5;
			}
			else
			{
				nAmount = nRemainingMoney;
			}

			nRemainingMoney -= nAmount;

			angRand.y = RandomFloat( -180.0f, 180.0f );

			CCurrencyPackCustom *pCurrencyPack = (CCurrencyPackCustom *)CBaseEntity::CreateNoSpawn( "item_currencypack_custom", pEntity->WorldSpaceCenter(), angRand, pEntity );
			
			if ( pCurrencyPack )
			{
				pCurrencyPack->SetAmount( nAmount );

				Vector vecImpulse = RandomVector( -1,1 );
				//vecImpulse.z = RandomFloat( 5.0f, 20.0f );
				vecImpulse.z = 1;
				VectorNormalize( vecImpulse );
				//Vector vecVelocity = vecImpulse * 250.0 * RandomFloat( 1.0f, 4.0f );
				Vector vecVelocity = vecImpulse * 250.0;

				DispatchSpawn( pCurrencyPack );
				pCurrencyPack->DropSingleInstance( vecVelocity, (CBaseCombatCharacter *)pEntity, 0, 0 );
			}
		}

		RETURN_META(MRES_HANDLED);
	}

	RETURN_META(MRES_IGNORED);
}

static void hook_entity_dtor()
{
	CBaseEntity *pEntity = META_IFACEPTR(CBaseEntity);

	int ref = gamehelpers->EntityToReference(pEntity);

	auto it{entpopdata.find(ref)};
	if(it != entpopdata.cend()) {
		entpopdata_t &data{it->second};

		pop_remove_entity(pEntity, data);

		entpopdata.erase(it);
	}

	SH_REMOVE_MANUALHOOK(Event_Killed, pEntity, SH_STATIC(hook_entity_killed_pre), false);
	SH_REMOVE_MANUALHOOK(Event_Killed, pEntity, SH_STATIC(hook_entity_killed_post), true);
	SH_REMOVE_MANUALHOOK(GenericDtor, pEntity, SH_STATIC(hook_entity_dtor), false);

	RETURN_META(MRES_HANDLED);
}

populator_type_t get_populator_type(IPopulator *populator)
{
	g_hackdetectpopulator = populator_none;
	populator->HasEventChangeAttributes("__hack_detect_populator__");
	populator_type_t type = g_hackdetectpopulator;
	g_hackdetectpopulator = populator_none;
	if(type != populator_none) {
		return type;
	}

	return populator_unknown;
}

IForward *pop_entity_spawned{nullptr};

static bool hook_spawner_spawn(const Vector &here, EntityHandleVector_t *result)
{
	IPopulationSpawner *spawner = META_IFACEPTR(IPopulationSpawner);

	IPopulator *populator = spawner->GetPopulator();

	populator_type_t populator_type{get_populator_type(populator)};

	if(result) {
		int count = result->Count();
		for(int i = 0; i < count; ++i) {
			CBaseHandle &hndl = (*result)[i];

			CBaseEntity *pEntity{gamehelpers->ReferenceToEntity(hndl.GetEntryIndex())};
			if(!pEntity) {
				continue;
			}

			const char *classname{gamehelpers->GetEntityClassname(pEntity)};

			if(pop_entity_spawned->GetFunctionCount() > 0) {
				pop_entity_spawned->PushCell(gamehelpers->EntityToBCompatRef(pEntity));
				pop_entity_spawned->Execute(nullptr);
			}

			if(pEntity->IsPlayer() ||
				strcmp(classname, "tank_boss") == 0) {
				continue;
			}

			int ref = gamehelpers->EntityToReference(pEntity);

			auto it{entpopdata.find(ref)};
			if(it == entpopdata.cend()) {
				it = entpopdata.emplace(ref, entpopdata_t{}).first;

				SH_ADD_MANUALHOOK(GenericDtor, pEntity, SH_STATIC(hook_entity_dtor), false);
				SH_ADD_MANUALHOOK(Event_Killed, pEntity, SH_STATIC(hook_entity_killed_pre), false);
				SH_ADD_MANUALHOOK(Event_Killed, pEntity, SH_STATIC(hook_entity_killed_post), true);
			}

			entpopdata_t &data{it->second};

			string_t icon = spawner->GetClassIcon(i);
			if(icon == NULL_STRING) {
				icon = data.icon;
			}

			CTFObjectiveResource *pObjectiveResource = (CTFObjectiveResource *)gamehelpers->ReferenceToEntity(objective_resource_ref);
			if(pObjectiveResource) {
				if(populator_type == populator_wavespawn) {
					pObjectiveResource->SetMannVsMachineWaveClassActive(icon);
				} else if(populator_type == populator_mission) {
					unsigned int iFlags = MVM_CLASS_FLAG_MISSION;
					/*if ( bot->IsMiniBoss() )
					{
						iFlags |= MVM_CLASS_FLAG_MINIBOSS;
					}
					if ( bot->HasAttribute( CTFBot::ALWAYS_CRIT ) )
					{
						iFlags |= MVM_CLASS_FLAG_ALWAYSCRIT;
					}*/
					pObjectiveResource->IncrementMannVsMachineWaveClassCount(icon, iFlags);
				}
			}

			if(populator_type == populator_wavespawn) {
				data.m_pWaveSpawnPopulator = (CWaveSpawnPopulator *)populator;
			}

			data.m_spawner = spawner;

			if(icon != NULL_STRING) {
				data.icon = icon;
			}

			for(int j = 0; j < NUM_BOT_ATTRS; ++j) {
				AttributeType attr{static_cast<AttributeType>(1 << j)};
				if(spawner->HasAttribute(attr, i)) {
					data.attrs |= attr;
				}
			}
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

cell_t CustomPopulationSpawnerEntryget(IPluginContext *pContext, const cell_t *params)
{
	SPPopulationSpawner *obj{(SPPopulationSpawner *)params[1]};

	if(!obj->entry) {
		return BAD_HANDLE;
	}

	return obj->entry->hndl;
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

cell_t IPopulationSpawnerCustomget(IPluginContext *pContext, const cell_t *params)
{
	IPopulationSpawner *obj{(IPopulationSpawner *)params[1]};
	return obj->HasEventChangeAttributes("__hack_detect_spawner__");
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
	IPopulator *populator{obj->GetPopulator()};
	if(populator && populator->m_spawner && populator->m_spawner == obj) {
		populator->m_spawner = nullptr;
	}
	delete obj;
	return 0;
}

cell_t IPopulatorHasEventChangeAttributes(IPluginContext *pContext, const cell_t *params)
{
	IPopulator *obj{(IPopulator *)params[1]};

	char *pszEventName{nullptr};
	pContext->LocalToString(params[2], &pszEventName);

	return obj->HasEventChangeAttributes(pszEventName);
}

cell_t IPopulatorSpawnerget(IPluginContext *pContext, const cell_t *params)
{
	IPopulator *obj{(IPopulator *)params[1]};

	return (cell_t)obj->m_spawner;
}

cell_t CWaveSpawnPopulatorTotalCountset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	obj->m_remainingCount = obj->m_totalCount = params[2];
	return 0;
}

cell_t CWaveSpawnPopulatorTotalCountget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return obj->m_totalCount;
}

cell_t CWaveSpawnPopulatorMaxActiveset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	obj->m_maxActive = params[2];
	return 0;
}

cell_t CWaveSpawnPopulatorMaxActiveget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return obj->m_maxActive;
}

cell_t CWaveSpawnPopulatorSpawnCountset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	obj->m_spawnCount = params[2];
	return 0;
}

cell_t CWaveSpawnPopulatorSpawnCountget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return obj->m_spawnCount;
}

cell_t CWaveSpawnPopulatorWaitBeforeStartingset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	obj->m_waitBeforeStarting = sp_ctof(params[2]);
	return 0;
}

cell_t CWaveSpawnPopulatorWaitBeforeStartingget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return sp_ftoc(obj->m_waitBeforeStarting);
}

cell_t CWaveSpawnPopulatorWaitBetweenSpawnsset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};

	if ( obj->m_waitBetweenSpawns != 0.f && obj->m_bWaitBetweenSpawnAfterDeath )
	{
		return pContext->ThrowNativeError( "Already specified WaitBetweenSpawnsAfterDeath time\n" );
	}

	obj->m_waitBetweenSpawns = sp_ctof(params[2]);
	return 0;
}

cell_t CWaveSpawnPopulatorWaitBetweenSpawnsget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return sp_ftoc(obj->m_waitBetweenSpawns);
}

cell_t CWaveSpawnPopulatorWaitBetweenSpawnsAfterDeathset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};

	if ( obj->m_waitBetweenSpawns != 0.f )
	{
		return pContext->ThrowNativeError( "Already specified WaitBetweenSpawns time\n" );
	}

	obj->m_bWaitBetweenSpawnAfterDeath = true;
	obj->m_waitBetweenSpawns = sp_ctof(params[2]);
	return 0;
}

cell_t CWaveSpawnPopulatorWaitBetweenSpawnsAfterDeathget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return sp_ftoc(obj->m_waitBetweenSpawns);
}

cell_t CWaveSpawnPopulatorRandomSpawnset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	obj->m_bRandomSpawn = params[2];
	return 0;
}

cell_t CWaveSpawnPopulatorRandomSpawnget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return obj->m_bRandomSpawn;
}

cell_t CWaveSpawnPopulatorTotalCurrencyset(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	obj->m_unallocatedCurrency = obj->m_totalCurrency = params[2];
	return 0;
}

cell_t CWaveSpawnPopulatorTotalCurrencyget(IPluginContext *pContext, const cell_t *params)
{
	CWaveSpawnPopulator *obj{(CWaveSpawnPopulator *)params[1]};
	return obj->m_totalCurrency;
}

cell_t CWaveWaitWhenDoneset(IPluginContext *pContext, const cell_t *params)
{
	CWave *obj{(CWave *)params[1]};
	obj->m_waitWhenDone = sp_ctof(params[2]);
	return 0;
}

cell_t CWaveWaitWhenDoneget(IPluginContext *pContext, const cell_t *params)
{
	CWave *obj{(CWave *)params[1]};
	return sp_ftoc(obj->m_waitWhenDone);
}

cell_t CWaveIndexget(IPluginContext *pContext, const cell_t *params)
{
	CWave *obj{(CWave *)params[1]};
	return obj->getvars().index;
}

cell_t pop_health_multiplier(IPluginContext *pContext, const cell_t *params)
{
	float mult = 1.0f;

	CPopulationManager *PopulationManager{GetPopulationManager()};
	if(PopulationManager) {
		mult = PopulationManager->GetHealthMultiplier(params[1]);
	}

	return sp_ftoc(mult);
}

cell_t pop_damage_multiplier(IPluginContext *pContext, const cell_t *params)
{
	float mult = 1.0f;

	CPopulationManager *PopulationManager{GetPopulationManager()};
	if(PopulationManager) {
		mult = PopulationManager->GetDamageMultiplier();
	}

	return sp_ftoc(mult);
}

cell_t current_wave(IPluginContext *pContext, const cell_t *params)
{
	CPopulationManager *PopulationManager{GetPopulationManager()};
	if(PopulationManager) {
		return (cell_t)PopulationManager->GetCurrentWave();
	}

	return 0;
}

cell_t current_wave_index(IPluginContext *pContext, const cell_t *params)
{
	CPopulationManager *PopulationManager{GetPopulationManager()};
	if(PopulationManager) {
		return PopulationManager->GetMembers().m_iCurrentWaveIndex;
	}

	return 0;
}

sp_nativeinfo_t natives[] =
{
	{"CustomPopulationSpawner.set_data", set_data},
	{"CustomPopulationSpawner.get_data", get_data},
	{"CustomPopulationSpawner.has_data", has_data},
	{"CustomPopulationSpawner.set_data_array", set_data_array},
	{"CustomPopulationSpawner.get_data_array", get_data_array},
	{"CustomPopulationSpawner.Entry.get", CustomPopulationSpawnerEntryget},
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
	{"IPopulationSpawner.Custom.get", IPopulationSpawnerCustomget},
	{"IPopulationSpawner.GetClass", IPopulationSpawnerGetClass},
	{"IPopulationSpawner.GetHealth", IPopulationSpawnerGetHealth},
	{"IPopulationSpawner.IsMiniBoss", IPopulationSpawnerIsMiniBoss},
	{"IPopulationSpawner.HasAttribute", IPopulationSpawnerHasAttribute},
	{"IPopulationSpawner.GetClassIcon", IPopulationSpawnerGetClassIcon},
	{"IPopulationSpawner.Populator.get", IPopulationSpawnerPopulatorget},
	{"IPopulationSpawner.Delete", IPopulationSpawnerDelete},
	{"IPopulator.HasEventChangeAttributes", IPopulatorHasEventChangeAttributes},
	{"IPopulator.Spawner.get", IPopulatorSpawnerget},
	{"CWaveSpawnPopulator.TotalCount.set", CWaveSpawnPopulatorTotalCountset},
	{"CWaveSpawnPopulator.TotalCount.get", CWaveSpawnPopulatorTotalCountget},
	{"CWaveSpawnPopulator.MaxActive.set", CWaveSpawnPopulatorMaxActiveset},
	{"CWaveSpawnPopulator.MaxActive.get", CWaveSpawnPopulatorMaxActiveget},
	{"CWaveSpawnPopulator.SpawnCount.set", CWaveSpawnPopulatorSpawnCountset},
	{"CWaveSpawnPopulator.SpawnCount.get", CWaveSpawnPopulatorSpawnCountget},
	{"CWaveSpawnPopulator.WaitBeforeStarting.set", CWaveSpawnPopulatorWaitBeforeStartingset},
	{"CWaveSpawnPopulator.WaitBeforeStarting.get", CWaveSpawnPopulatorWaitBeforeStartingget},
	{"CWaveSpawnPopulator.WaitBetweenSpawns.set", CWaveSpawnPopulatorWaitBetweenSpawnsset},
	{"CWaveSpawnPopulator.WaitBetweenSpawns.get", CWaveSpawnPopulatorWaitBetweenSpawnsget},
	{"CWaveSpawnPopulator.WaitBetweenSpawnsAfterDeath.set", CWaveSpawnPopulatorWaitBetweenSpawnsAfterDeathset},
	{"CWaveSpawnPopulator.WaitBetweenSpawnsAfterDeath.get", CWaveSpawnPopulatorWaitBetweenSpawnsAfterDeathget},
	{"CWaveSpawnPopulator.RandomSpawn.set", CWaveSpawnPopulatorRandomSpawnset},
	{"CWaveSpawnPopulator.RandomSpawn.get", CWaveSpawnPopulatorRandomSpawnget},
	{"CWaveSpawnPopulator.TotalCurrency.set", CWaveSpawnPopulatorTotalCurrencyset},
	{"CWaveSpawnPopulator.TotalCurrency.get", CWaveSpawnPopulatorTotalCurrencyget},
	{"CWave.WaitWhenDone.set", CWaveWaitWhenDoneset},
	{"CWave.WaitWhenDone.get", CWaveWaitWhenDoneget},
	{"CWave.Index.get", CWaveIndexget},
	{"pop_health_multiplier", pop_health_multiplier},
	{"pop_damage_multiplier", pop_damage_multiplier},
	{"current_wave", current_wave},
	{"current_wave_index", current_wave_index},
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
IForward *wavespawn_parse = nullptr;

DETOUR_DECL_MEMBER1(FindSpawnLocation, SpawnLocationResult, Vector &, vSpawnPosition)
{
	if(find_spawn_location->GetFunctionCount() > 0) {
		find_spawn_location->PushCell((cell_t)last_populator);
		find_spawn_location->PushCell((cell_t)this);
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
CDetour *pWaveSpawnPopulatorParse = nullptr;

bool Sample::RegisterConCommandBase(ConCommandBase *pCommand)
{
	META_REGCVAR(pCommand);
	return true;
}

bool Sample::SDK_OnMetamodLoad(ISmmAPI *ismm, char *error, size_t maxlen, bool late)
{
	gpGlobals = ismm->GetCGlobals();
	GET_V_IFACE_CURRENT(GetEngineFactory, icvar, ICvar, CVAR_INTERFACE_VERSION);
	GET_V_IFACE_CURRENT(GetServerFactory, servertools, IServerTools, VSERVERTOOLS_INTERFACE_VERSION);
	GET_V_IFACE_CURRENT(GetEngineFactory, filesystem, IFileSystem, FILESYSTEM_INTERFACE_VERSION)
	g_pCVar = icvar;
	ConVar_Register(0, this);

	tf_populator_health_multiplier = g_pCVar->FindVar("tf_populator_health_multiplier");
	tf_mvm_endless_tank_boost = g_pCVar->FindVar("tf_mvm_endless_tank_boost");
	tf_mvm_endless_force_on = g_pCVar->FindVar("tf_mvm_endless_force_on");
	tf_populator_damage_multiplier = g_pCVar->FindVar("tf_populator_damage_multiplier");
	tf_mvm_endless_damage_boost_rate = g_pCVar->FindVar("tf_mvm_endless_damage_boost_rate");
	tf_mvm_endless_scale_rate = g_pCVar->FindVar("tf_mvm_endless_scale_rate");

	popspawner_maxiconlen.SetValue( MAX_PATH );

	return true;
}

struct EventInfo
{
	CFmtStrHack m_target;
	CFmtStrHack m_action;
};

void *ParseEventPtr{nullptr};

IForward *pop_event_fired{nullptr};

#define DETOUR_DECL_STATIC2_callconv(name, ret, callconv, p1type, p1name, p2type, p2name) \
	ret callconv (*name##_Actual)(p1type, p2type) = NULL; \
	ret callconv name(p1type p1name, p2type p2name)

static EventInfo * __attribute__((__regparm__(1))) ParseEvent( KeyValues *values )
{
	return (void_to_func<EventInfo * __attribute__((__regparm__(1))) (*)(KeyValues *)>(ParseEventPtr))(values);
}

DETOUR_DECL_STATIC2_callconv(FireEvent, void, __attribute__((__regparm__(2))), EventInfo *,eventInfo, const char *,eventName)
{
	if(pop_event_fired->GetFunctionCount() > 0) {
		pop_event_fired->PushStringEx((char *)eventName, strlen(eventName)+1, SM_PARAM_STRING_COPY|SM_PARAM_STRING_UTF8, 0);
		pop_event_fired->Execute(nullptr);
	}

	DETOUR_STATIC_CALL(FireEvent)(eventInfo, eventName);
}

bool CWaveSpawnPopulator::DetourParse( KeyValues *values )
{
	// First, see if we have any Template keys
	KeyValues *pTemplate = values->FindKey( "Template" );
	if ( pTemplate )
	{
		KeyValues *pTemplateKV = GetManager()->GetTemplate( pTemplate->GetString() );
		if ( pTemplateKV )
		{
			// Pump all the keys into ourself now
			if ( DetourParse( pTemplateKV ) == false )
			{
				return false;
			}
		}
		else
		{
			Warning( "Unknown Template '%s' in WaveSpawn definition\n", pTemplate->GetString() );
		}
	}

	for ( KeyValues *data = values->GetFirstSubKey(); data != NULL; data = data->GetNextKey() )
	{
		const char *name = data->GetName();

		if ( Q_strlen( name ) <= 0 )
		{
			continue;
		}

		if ( m_where.DetourParse( data ) )
		{
			continue;
		}

		// Skip templates when looping through the rest of the keys
		if ( !Q_stricmp( name, "Template" ) )
			continue;

		if ( !Q_stricmp( name, "TotalCount" ) )
		{
			m_totalCount = data->GetInt();
		}
		else if ( !Q_stricmp( name, "MaxActive" ) )
		{
			m_maxActive = data->GetInt();
		}
		else if ( !Q_stricmp( name, "SpawnCount" ) )
		{
			m_spawnCount = data->GetInt();
		}
		else if ( !Q_stricmp( name, "WaitBeforeStarting" ) )
		{
			m_waitBeforeStarting = data->GetFloat();
		}
		else if ( !Q_stricmp( name, "WaitBetweenSpawns" ) )
		{
			if ( m_waitBetweenSpawns != 0.f && m_bWaitBetweenSpawnAfterDeath )
			{
				Warning( "Already specified WaitBetweenSpawnsAfterDeath time, WaitBetweenSpawns won't be used\n" );
				continue;
			}

			m_waitBetweenSpawns = data->GetFloat();
		}
		else if ( !Q_stricmp( name, "WaitBetweenSpawnsAfterDeath" ) )
		{
			if ( m_waitBetweenSpawns != 0.f )
			{
				Warning( "Already specified WaitBetweenSpawns time, WaitBetweenSpawnsAfterDeath won't be used\n" );
				continue;
			}

			m_bWaitBetweenSpawnAfterDeath = true;
			m_waitBetweenSpawns = data->GetFloat();
		}
		else if ( !Q_stricmp( name, "StartWaveWarningSound" ) )
		{
			m_startWaveWarningSound.sprintf( "%s", data->GetString() );
		}
		else if ( !Q_stricmp( name, "StartWaveOutput" ) )
		{
			m_startWaveOutput = ParseEvent( data );
		}
		else if ( !Q_stricmp( name, "FirstSpawnWarningSound" ) )
		{
			m_firstSpawnWarningSound.sprintf( "%s", data->GetString() );
		}
		else if ( !Q_stricmp( name, "FirstSpawnOutput" ) )
		{
			m_firstSpawnOutput = ParseEvent( data );
		}
		else if ( !Q_stricmp( name, "LastSpawnWarningSound" ) )
		{
			m_lastSpawnWarningSound.sprintf( "%s", data->GetString() );
		}
		else if ( !Q_stricmp( name, "LastSpawnOutput" ) )
		{
			m_lastSpawnOutput = ParseEvent( data );
		}
		else if ( !Q_stricmp( name, "DoneWarningSound" ) )
		{
			m_doneWarningSound.sprintf( "%s", data->GetString() );
		}
		else if ( !Q_stricmp( name, "DoneOutput" ) )
		{
			m_doneOutput = ParseEvent( data );
		}
		else if ( !Q_stricmp( name, "TotalCurrency" ) )
		{
			m_totalCurrency = data->GetInt();
		}
		else if ( !Q_stricmp( name, "Name" ) )
		{
			m_name = data->GetString();
		}
		else if ( !Q_stricmp( name, "WaitForAllSpawned" ) )
		{
			m_waitForAllSpawned = data->GetString();
		}
		else if ( !Q_stricmp( name, "WaitForAllDead" ) )
		{
			m_waitForAllDead = data->GetString();
		}
		else if ( !Q_stricmp( name, "Support" ) )
		{
			m_bLimitedSupport = !Q_stricmp( data->GetString(), "Limited" );
			m_bSupportWave = true;
		}
		else if ( !Q_stricmp( name, "RandomSpawn" ) )
		{
			m_bRandomSpawn = data->GetBool();
		}
		else if ( !Q_stricmp( name, "Plugin" ) )
		{
			if(wavespawn_parse->GetFunctionCount() > 0) {
				HandleError err{};
				Handle_t hndl = ((HandleSystemHack *)handlesys)->CreateKeyValuesHandle(data, nullptr, &err);
				if(err != HandleError_None) {
					smutils->LogError(myself, "Invalid KeyValues handle %x (error %d).", hndl, err);
					return false;
				}

				wavespawn_parse->PushCell((cell_t)this);
				wavespawn_parse->PushCell(hndl);
				cell_t result = 0;
				wavespawn_parse->PushCellByRef(&result);
				cell_t res = 0;
				wavespawn_parse->Execute(&res);

				handlesys->FreeHandle(hndl, nullptr);

				if(res == Pl_Changed) {
					if(!result) {
						return false;
					}
				} else if(res >= Pl_Handled) {
					return false;
				}
			}
		}
		else
		{
			m_spawner = ParseSpawner( this, data );

			if ( m_spawner == NULL )
			{
				Warning( "Unknown attribute '%s' in WaveSpawn definition.\n", name );
			}
		}

		// These allow us to avoid rounding errors later when divvying money to bots
		m_unallocatedCurrency = m_totalCurrency;
		m_remainingCount = m_totalCount;
	}

	return true;
}

DETOUR_DECL_MEMBER1(WaveSpawnPopulatorParse, bool, KeyValues *, values)
{
	return ((CWaveSpawnPopulator *)this)->CWaveSpawnPopulator::DetourParse(values);
}

IForward *wave_parse{nullptr};

bool CWave::DetourParse( KeyValues *data )
{
	m_iEnemyCount = 0;
	m_nWaveClassCounts.RemoveAll();
	m_totalCurrency = 0;

	FOR_EACH_SUBKEY( data, kvWave )
	{
		if ( !Q_stricmp( kvWave->GetName(), "WaveSpawn" ) )
		{
			CWaveSpawnPopulator *wavePopulator = CWaveSpawnPopulator::create(GetManager());

			last_populator = wavePopulator;

			if ( wavePopulator->DetourParse( kvWave ) == false )
			{
				Warning( "Error reading WaveSpawn definition\n" );
				delete wavePopulator;
				last_populator = nullptr;
				return false;
			}

			last_populator = nullptr;

			m_waveSpawnVector.AddToTail( wavePopulator );

			if ( !wavePopulator->IsSupportWave() )
			{
				// this is a total of all enemies we have to fight that are NOT support enemies
				m_iEnemyCount += wavePopulator->m_totalCount;
			}
			m_totalCurrency += wavePopulator->m_totalCurrency;

			wavePopulator->SetParent( this );

			if ( wavePopulator->m_spawner )
			{
				if ( wavePopulator->m_spawner->IsVarious() )
				{
					for ( int i = 0; i < wavePopulator->m_totalCount; ++i )
					{
						unsigned int iFlags = wavePopulator->IsSupportWave() ? MVM_CLASS_FLAG_SUPPORT : MVM_CLASS_FLAG_NORMAL;
						if ( wavePopulator->m_spawner->IsMiniBoss( i ) )
						{
							iFlags |= MVM_CLASS_FLAG_MINIBOSS;
						}
						if ( wavePopulator->m_spawner->HasAttribute( ALWAYS_CRIT, i ) )
						{
							iFlags |= MVM_CLASS_FLAG_ALWAYSCRIT;
						}
						if ( wavePopulator->IsLimitedSupportWave() )
						{
							iFlags |= MVM_CLASS_FLAG_SUPPORT_LIMITED;
						}
						AddClassType( wavePopulator->m_spawner->GetClassIcon( i ), 1, iFlags );
					}
				}
				else
				{
					unsigned int iFlags = wavePopulator->IsSupportWave() ? MVM_CLASS_FLAG_SUPPORT : MVM_CLASS_FLAG_NORMAL;
					if ( wavePopulator->m_spawner->IsMiniBoss() )
					{
						iFlags |= MVM_CLASS_FLAG_MINIBOSS;
					}
					if ( wavePopulator->m_spawner->HasAttribute( ALWAYS_CRIT ) )
					{
						iFlags |= MVM_CLASS_FLAG_ALWAYSCRIT;
					}
					if ( wavePopulator->IsLimitedSupportWave() )
					{
						iFlags |= MVM_CLASS_FLAG_SUPPORT_LIMITED;
					}
					AddClassType( wavePopulator->m_spawner->GetClassIcon(), wavePopulator->m_totalCount, iFlags );
				}
			}
		}
		else if ( !Q_stricmp( kvWave->GetName(), "Sound" ) )
		{
			m_soundName.sprintf( "%s", kvWave->GetString() );
		}
		else if ( !Q_stricmp( kvWave->GetName(), "Description" ) )
		{
			m_description.sprintf( "%s", kvWave->GetString() );
		}
		else if ( !Q_stricmp( kvWave->GetName(), "WaitWhenDone" ) )
		{
			m_waitWhenDone = kvWave->GetFloat();
		}
		else if ( !Q_stricmp( kvWave->GetName(), "Checkpoint" ) )
		{
			//m_isCheckpoint = true;
		}
		else if ( !Q_stricmp( kvWave->GetName(), "StartWaveOutput" ) )
		{
			m_startOutput = ParseEvent( kvWave );
		}
		else if ( !Q_stricmp( kvWave->GetName(), "DoneOutput" ) )
		{
			m_doneOutput = ParseEvent( kvWave );
		}
		else if ( !Q_stricmp( kvWave->GetName(), "InitWaveOutput" ) )
		{
			m_initOutput = ParseEvent( kvWave );
		}
		else if ( !Q_stricmp( kvWave->GetName(), "Plugin" ) )
		{
			if(wave_parse->GetFunctionCount() > 0) {
				HandleError err{};
				Handle_t hndl = ((HandleSystemHack *)handlesys)->CreateKeyValuesHandle(data, nullptr, &err);
				if(err != HandleError_None) {
					smutils->LogError(myself, "Invalid KeyValues handle %x (error %d).", hndl, err);
					return false;
				}

				wave_parse->PushCell((cell_t)this);
				wave_parse->PushCell(hndl);
				cell_t result = 0;
				wave_parse->PushCellByRef(&result);
				cell_t res = 0;
				wave_parse->Execute(&res);

				handlesys->FreeHandle(hndl, nullptr);

				if(res == Pl_Changed) {
					if(!result) {
						return false;
					}
				} else if(res >= Pl_Handled) {
					return false;
				}
			}
		}
		else
		{
			Warning( "Unknown attribute '%s' in Wave definition.\n", kvWave->GetName() );
		}
	}

	return true;
}

enum 
{
	MVM_EVENT_POPFILE_NONE = 0,
	MVM_EVENT_POPFILE_HALLOWEEN,

	MVM_EVENT_POPFILE_MAX_TYPES,
};

class CRandomPlacementPopulator : public IPopulator
{
public:
	static CRandomPlacementPopulator *create(CPopulationManager *pManager)
	{
		CRandomPlacementPopulator *bytes = (CRandomPlacementPopulator *)calloc(1, sizeof(CRandomPlacementPopulator));
		call_mfunc<void, CRandomPlacementPopulator, CPopulationManager *>(bytes, CRandomPlacementPopulatorCTOR, pManager);
		return bytes;
	}

	int m_count;
	float m_minSeparation;
	unsigned int m_navAreaFilter;
};

class CPeriodicSpawnPopulator : public IPopulator
{
public:
	static CPeriodicSpawnPopulator *create(CPopulationManager *pManager)
	{
		CPeriodicSpawnPopulator *bytes = (CPeriodicSpawnPopulator *)calloc(1, sizeof(CPeriodicSpawnPopulator));
		call_mfunc<void, CPeriodicSpawnPopulator, CPopulationManager *>(bytes, CPeriodicSpawnPopulatorCTOR, pManager);
		return bytes;
	}

	CSpawnLocation m_where;
	float m_minInterval;
	float m_maxInterval;

	CountdownTimer m_timer;
};

IForward *pop_parse{nullptr};

bool CPopulationManager::DetourParse( void )
{
	CPopulationManager_members_t &members{GetMembers()};

	if ( members.m_popfileFull[ 0 ] == '\0' )
	{
		Warning( "No population file specified.\n" );
		return false;
	}

	//if ( m_bIsInitialized )
//		return true;

	KeyValues *values = new KeyValues( "Population" );
	if ( !values->LoadFromFile( filesystem, members.m_popfileFull, "POPULATION" ) )
	{
		Warning( "Can't open %s.\n", members.m_popfileFull );
		values->deleteThis();
		return false;
	}

	// Clear out existing Data structures
	members.m_populatorVector.PurgeAndDeleteElements();
	members.m_waveVector.RemoveAll();
	members.m_bEndlessOn = false;

	if ( members.m_pTemplates )
	{
		members.m_pTemplates->deleteThis();
		members.m_pTemplates = NULL;
	}

	// find templates first
	KeyValues *pTemplates = values->FindKey( "Templates" );

	if ( pTemplates )
	{
		members.m_pTemplates = pTemplates->MakeCopy();
	}

	for ( KeyValues *data = values->GetFirstSubKey(); data != NULL; data = data->GetNextKey() )
	{
		const char *name = data->GetName();

		if ( Q_strlen( name ) <= 0 )
		{
			continue;
		}

		if ( !Q_stricmp( name, "StartingCurrency" ) )
		{
			members.m_nStartingCurrency = data->GetInt();
		}
		else if ( !Q_stricmp( name, "RespawnWaveTime" ) )
		{
			members.m_nRespawnWaveTime = data->GetInt();
		}
		else if ( !Q_stricmp( name, "EventPopfile" ) )
		{
			if ( !Q_stricmp( data->GetString(), "Halloween" ) )
			{
				members.m_nMvMEventPopfileType = MVM_EVENT_POPFILE_HALLOWEEN;
			}
			else
			{
				members.m_nMvMEventPopfileType = MVM_EVENT_POPFILE_NONE;
			}
		}
		else if ( !Q_stricmp( name, "FixedRespawnWaveTime" ) )
		{
			members.m_bFixedRespawnWaveTime = true;
		}
		else if ( !Q_stricmp( name, "AddSentryBusterWhenDamageDealtExceeds" ) )
		{
			members.m_sentryBusterDamageDealtThreshold = data->GetInt();
		}
		else if ( !Q_stricmp( name, "AddSentryBusterWhenKillCountExceeds" ) )
		{
			members.m_sentryBusterKillThreshold = data->GetInt();
		}
		else if ( !Q_stricmp( name, "CanBotsAttackWhileInSpawnRoom" ) )
		{
			if ( !Q_stricmp( data->GetString(), "no" ) || !Q_stricmp( data->GetString(), "false" ) )
			{
				members.m_canBotsAttackWhileInSpawnRoom = false;
			}
			else
			{
				members.m_canBotsAttackWhileInSpawnRoom = true;
			}
		}
		else if ( !Q_stricmp( name, "RandomPlacement" ) )
		{
			CRandomPlacementPopulator *randomPopulator = CRandomPlacementPopulator::create(this);

			last_populator = randomPopulator;

			if ( randomPopulator->Parse( data ) == false )
			{
				Warning( "Error reading RandomPlacement definition\n" );
				delete randomPopulator;
				last_populator = nullptr;
				return false;
			}

			last_populator = nullptr;

			members.m_populatorVector.AddToTail( randomPopulator );
		}
		else if ( !Q_stricmp( name, "PeriodicSpawn" ) )
		{
			CPeriodicSpawnPopulator *periodicPopulator = CPeriodicSpawnPopulator::create(this);

			last_populator = periodicPopulator;

			if ( periodicPopulator->Parse( data ) == false )
			{
				Warning( "Error reading PeriodicSpawn definition\n" );
				delete periodicPopulator;
				last_populator = nullptr;
				return false;
			}

			last_populator = nullptr;

			members.m_populatorVector.AddToTail( periodicPopulator );
		}
		else if ( !Q_stricmp( name, "Wave" ) )
		{
			CWave *wave = CWave::create(this);

			wave->getvars().index = members.m_waveVector.Count();

			last_populator = wave;

			if ( !wave->DetourParse( data ) )
			{
				Warning( "Error reading Wave definition\n" );
				delete wave;
				last_populator = nullptr;
				return false;
			}

			last_populator = nullptr;

			// also keep vector of wave pointers for convenience
			members.m_waveVector.AddToTail( wave );
		}
		else if ( !Q_stricmp( name, "Mission" ) )
		{
			CMissionPopulator *missionPopulator = CMissionPopulator::create(this);

			last_populator = missionPopulator;

			if ( missionPopulator->Parse( data ) == false )
			{
				Warning( "Error reading Mission definition\n" );
				delete missionPopulator;
				last_populator = nullptr;
				return false;
			}

			last_populator = nullptr;

			members.m_populatorVector.AddToTail( missionPopulator );
		}
		else if ( !Q_stricmp( name, "Templates" ) )
		{
			// handled above
		}
		else if ( !Q_stricmp( name, "Advanced" ) )
		{
			members.m_bAdvancedPopFile = true;
		}
		else if ( !Q_stricmp( name, "IsEndless" ) )
		{
			members.m_bEndlessOn = true;
		}
		else if ( !Q_stricmp( name, "Plugin" ) )
		{
			if(pop_parse->GetFunctionCount() > 0) {
				HandleError err{};
				Handle_t hndl = ((HandleSystemHack *)handlesys)->CreateKeyValuesHandle(data, nullptr, &err);
				if(err != HandleError_None) {
					smutils->LogError(myself, "Invalid KeyValues handle %x (error %d).", hndl, err);
					return false;
				}

				pop_parse->PushCell(hndl);
				cell_t result = 0;
				pop_parse->PushCellByRef(&result);
				cell_t res = 0;
				pop_parse->Execute(&res);

				handlesys->FreeHandle(hndl, nullptr);

				if(res == Pl_Changed) {
					if(!result) {
						return false;
					}
				} else if(res >= Pl_Handled) {
					return false;
				}
			}
		}
		else
		{
			Warning( "Invalid populator '%s'\n", name );
			return false;
		}
	}

	for ( int nPopulator = 0; nPopulator < members.m_populatorVector.Count(); ++nPopulator )
	{
		IPopulator *populator{members.m_populatorVector[ nPopulator ]};

		CMissionPopulator *pMission = ((get_populator_type(populator) == populator_mission) ? (CMissionPopulator *)populator : nullptr);

		if ( pMission )
		{
			// FIXME: Need a way to handle missions that spawn multiple types
			int nStartWave = pMission->BeginAtWave();
			int nStopWave = pMission->StopAtWave();

			if ( pMission->m_spawner && !pMission->m_spawner->IsVarious() )
			{
				for ( int i = nStartWave; i < nStopWave; ++i )
				{
					if ( members.m_waveVector.IsValidIndex( i ) )
					{
						CWave *pWave = members.m_waveVector[ i ];
					
						unsigned int iFlags = MVM_CLASS_FLAG_MISSION;
						if ( pMission->m_spawner->IsMiniBoss() )
						{
							iFlags |= MVM_CLASS_FLAG_MINIBOSS;
						}
						if ( pMission->m_spawner->HasAttribute( ALWAYS_CRIT ) )
						{
							iFlags |= MVM_CLASS_FLAG_ALWAYSCRIT;
						}
						pWave->AddClassType( pMission->m_spawner->GetClassIcon(), 0, iFlags );
					}
				}
			}
		}
	}

	values->deleteThis();

	return true;
}

DETOUR_DECL_MEMBER1(WaveParse, bool, KeyValues *, values)
{
	return ((CWave *)this)->CWave::DetourParse(values);
}

static bool g_bInPopParse{false};

DETOUR_DECL_MEMBER4(KeyValuesLoadFromFile, bool, IBaseFileSystem *,filesystem, const char *,resourceName, const char *,pathID, bool, refreshCache)
{
	if(g_bInPopParse) {
		pathID = "POPULATION";
	}
	return DETOUR_MEMBER_CALL(KeyValuesLoadFromFile)(filesystem, resourceName, pathID, refreshCache);
}

DETOUR_DECL_MEMBER0(PopulationManagerParse, bool)
{
	g_bInPopParse = true;
	bool ret = ((CPopulationManager *)this)->CPopulationManager::DetourParse();
	g_bInPopParse = false;
	return ret;
}

ConVar tf_mvm_bonus( "tf_mvm_bonus", "0" );
ConVar tf_mvm_bonus_boss_classname( "tf_mvm_bonus_boss_classname", "eyeball_boss" );
ConVar tf_mvm_bonus_spawnpoint_name( "tf_mvm_bonus_spawnpoint_name", "spawn_boss_startpoint" );
ConVar tf_mvm_bonus_spawnpoint_classname( "tf_mvm_bonus_spawnpoint_classname", "info_target" );

IForward *is_bonus_wave{nullptr};

DETOUR_DECL_MEMBER0(StartCurrentWave, void)
{
	DETOUR_MEMBER_CALL(StartCurrentWave)();

	CPopulationManager *pThis{(CPopulationManager *)this};

	CPopulationManager_members_t &members{pThis->GetMembers()};

	bool is_bonus = false;
	CBaseCombatCharacter *pBossEntity = nullptr;
	Vector boss_pos{};
	boss_pos.Zero();

	if(is_bonus_wave->GetFunctionCount() > 0) {
		cell_t is = 0;
		is_bonus_wave->PushCellByRef(&is);
		cell_t boss_idx = -1;
		is_bonus_wave->PushCellByRef(&boss_idx);
		cell_t pos[3]{sp_ftoc(0.0f),sp_ftoc(0.0f),sp_ftoc(0.0f)};
		is_bonus_wave->PushArray(pos, 3, SM_PARAM_COPYBACK);
		cell_t res = 0;
		is_bonus_wave->Execute(&res);

		if(res == Pl_Changed) {
			is_bonus = is;
			pBossEntity = (CBaseCombatCharacter *)gamehelpers->ReferenceToEntity(boss_idx);
			boss_pos.x = sp_ctof(pos[0]);
			boss_pos.y = sp_ctof(pos[1]);
			boss_pos.z = sp_ctof(pos[2]);
		}
	}

	members.m_bBonusRound = (tf_mvm_bonus.GetBool() || is_bonus);
	if ( members.m_bBonusRound )
	{
		bool created = false;

		if(!pBossEntity) {
			members.m_hBonusBoss = (CBaseCombatCharacter *)CreateEntityByName( tf_mvm_bonus_boss_classname.GetString() );
			created = true;
		} else {
			members.m_hBonusBoss = pBossEntity;
		}

		if ( members.m_hBonusBoss )
		{
			if(boss_pos.IsZero()) {
				bool bFoundSpawnPoint = false;
				CBaseEntity *spawnPoint = NULL;
				while( ( spawnPoint = FindEntityByClassname( spawnPoint, tf_mvm_bonus_spawnpoint_classname.GetString() ) ) != NULL )
				{
					if ( FStrEq( STRING( spawnPoint->GetEntityName() ), tf_mvm_bonus_spawnpoint_name.GetString() ) )
					{
						bFoundSpawnPoint = true;
						break;
					}
				}

				if(!bFoundSpawnPoint) {
					Warning( "CPopulationManager::StartCurrentWave trying to spawn a bonus boss, but cannot find %s %s in the map", tf_mvm_bonus_spawnpoint_name.GetString(), tf_mvm_bonus_spawnpoint_classname.GetString() );
					RemoveEntity( members.m_hBonusBoss );
					members.m_hBonusBoss = NULL;
					members.m_bBonusRound = false;
					return;
				}

				boss_pos = spawnPoint->GetAbsOrigin();
			}

			members.m_hBonusBoss->SetAbsOrigin( boss_pos );
			if(created) {
				DispatchSpawn( members.m_hBonusBoss );
			}
		}
	}
}

IGameConfig *g_pGameConf = nullptr;

CDetour *pPopulationManagerParse{nullptr};
CDetour *pKeyValuesLoadFromFile{nullptr};
CDetour *pWaveParse{nullptr};
CDetour *pFireEvent{nullptr};
CDetour *pStartCurrentWave{nullptr};
CDetour *pSpawnLocationParse{nullptr};

bool Sample::SDK_OnLoad(char *error, size_t maxlen, bool late)
{
	char pPath[MAX_PATH];
	smutils->BuildPath(Path_Game, pPath, MAX_PATH, nullptr);
	filesystem->AddSearchPath( pPath, "POPULATION" );
	smutils->BuildPath(Path_Game, pPath, MAX_PATH, "scripts/population");
	filesystem->AddSearchPath( pPath, "POPULATION" );
	smutils->BuildPath(Path_SM, pPath, MAX_PATH, "configs/population");
	filesystem->AddSearchPath( pPath, "POPULATION" );
	smutils->BuildPath(Path_SM, pPath, MAX_PATH, "data/population");
	filesystem->AddSearchPath( pPath, "POPULATION" );

	gameconfs->LoadGameConfigFile("popspawner", &g_pGameConf, error, maxlen);
	
	CDetourManager::Init(g_pSM->GetScriptingEngine(), g_pGameConf);
	
	pParseSpawner = DETOUR_CREATE_STATIC(ParseSpawner, "IPopulationSpawner::ParseSpawner")
	pParseSpawner->EnableDetour();

	pFindSpawnLocation = DETOUR_CREATE_MEMBER(FindSpawnLocation, "CSpawnLocation::FindSpawnLocation")
	pFindSpawnLocation->EnableDetour();

	pSpawnLocationParse = DETOUR_CREATE_MEMBER(SpawnLocationParse, "CSpawnLocation::Parse")
	pSpawnLocationParse->EnableDetour();

	pWaveSpawnPopulatorParse = DETOUR_CREATE_MEMBER(WaveSpawnPopulatorParse, "CWaveSpawnPopulator::Parse")
	pWaveSpawnPopulatorParse->EnableDetour();

	pWaveParse = DETOUR_CREATE_MEMBER(WaveParse, "CWave::Parse")
	pWaveParse->EnableDetour();

	pFireEvent = DETOUR_CREATE_STATIC(FireEvent, "FireEvent")
	pFireEvent->EnableDetour();

	pPopulationManagerParse = DETOUR_CREATE_MEMBER(PopulationManagerParse, "CPopulationManager::Parse")
	pPopulationManagerParse->EnableDetour();

	pKeyValuesLoadFromFile = DETOUR_CREATE_MEMBER(KeyValuesLoadFromFile, "KeyValues::LoadFromFile")
	pKeyValuesLoadFromFile->EnableDetour();

	pStartCurrentWave = DETOUR_CREATE_MEMBER(StartCurrentWave, "CPopulationManager::StartCurrentWave")
	pStartCurrentWave->EnableDetour();

	g_pGameConf->GetMemSig("AllocPooledString", &AllocPooledStringPtr);

	g_pGameConf->GetMemSig("IsSpaceToSpawnHere", &IsSpaceToSpawnHerePtr);

	g_pGameConf->GetMemSig("CPopulationManager::OnPlayerKilled", &CPopulationManagerOnPlayerKilled);
	g_pGameConf->GetMemSig("g_pPopulationManager", &g_pPopulationManagerPtr);
	g_pGameConf->GetMemSig("CWaveSpawnPopulator::GetCurrencyAmountPerDeath", &CWaveSpawnPopulatorGetCurrencyAmountPerDeath);

	g_pGameConf->GetMemSig("CSpawnLocation::Parse", &CSpawnLocationParsePtr);

	g_pGameConf->GetMemSig("ITFTeamSpawnAutoList::m_ITFTeamSpawnAutoListAutoList", (void **)&m_ITFTeamSpawnAutoListAutoList);

	g_pGameConf->GetMemSig("CWaveSpawnPopulator::CWaveSpawnPopulator", &CWaveSpawnPopulatorCTOR);
	g_pGameConf->GetMemSig("CRandomPlacementPopulator::CRandomPlacementPopulator", &CRandomPlacementPopulatorCTOR);
	g_pGameConf->GetMemSig("CPeriodicSpawnPopulator::CPeriodicSpawnPopulator", &CPeriodicSpawnPopulatorCTOR);
	g_pGameConf->GetMemSig("CMissionPopulator::CMissionPopulator", &CMissionPopulatorCTOR);
	g_pGameConf->GetMemSig("CWave::CWave", &CWaveCTOR);

	g_pGameConf->GetMemSig("ParseEvent", &ParseEventPtr);

	g_pGameConf->GetMemSig("CTFPowerup::DropSingleInstance", &CTFPowerupDropSingleInstance);

	g_pGameConf->GetMemSig("CBaseEntity::CalcAbsolutePosition", &CBaseEntityCalcAbsolutePosition);
	g_pGameConf->GetMemSig("CBaseEntity::SetAbsOrigin", &CBaseEntitySetAbsOrigin);

	int offset = -1;
	g_pGameConf->GetOffset("CBaseCombatCharacter::Event_Killed", &offset);
	SH_MANUALHOOK_RECONFIGURE(Event_Killed, offset, 0, 0);

	g_pGameConf->GetOffset("CBaseEntity::SetOwnerEntity", &CBaseEntitySetOwnerEntity);
	g_pGameConf->GetOffset("CBaseEntity::WorldSpaceCenter", &CBaseEntityWorldSpaceCenter);

	g_pEntityList = reinterpret_cast<CBaseEntityList *>(gamehelpers->GetGlobalEntityList());

	dictionary = servertools->GetEntityFactoryDictionary();

	info_populator_size = dictionary->FindFactory("info_populator")->GetEntitySize();

	popspawner_handle = handlesys->CreateType("popspawner", this, 0, nullptr, nullptr, myself->GetIdentity(), nullptr);

	find_spawn_location = forwards->CreateForward("find_spawn_location", ET_Hook, 1, nullptr, Param_Array);
	pop_parse = forwards->CreateForward("pop_parse", ET_Hook, 2, nullptr, Param_Cell, Param_CellByRef);
	wavespawn_parse = forwards->CreateForward("wavespawn_parse", ET_Hook, 3, nullptr, Param_Cell, Param_Cell, Param_CellByRef);
	wave_parse = forwards->CreateForward("wave_parse", ET_Hook, 3, nullptr, Param_Cell, Param_Cell, Param_CellByRef);
	spawnlocation_parse = forwards->CreateForward("spawnlocation_parse", ET_Hook, 4, nullptr, Param_Cell, Param_Cell, Param_Cell, Param_CellByRef);
	pop_event_fired = forwards->CreateForward("pop_event_fired", ET_Ignore, 1, nullptr, Param_String);
	is_bonus_wave = forwards->CreateForward("is_bonus_wave", ET_Hook, 3, nullptr, Param_CellByRef, Param_CellByRef, Param_Array);
	pop_entity_spawned = forwards->CreateForward("pop_entity_spawned", ET_Ignore, 1, nullptr, Param_Cell);

	sharesys->AddNatives(myself, natives);
	
	sharesys->RegisterLibrary(myself, "popspawner");
	
	HandleSystemHack::init();
	
	return true;
}

void Sample::SDK_OnUnload()
{
	pParseSpawner->Destroy();
	pFindSpawnLocation->Destroy();
	pWaveSpawnPopulatorParse->Destroy();
	pPopulationManagerParse->Destroy();
	pKeyValuesLoadFromFile->Destroy();
	pWaveParse->Destroy();
	pStartCurrentWave->Destroy();
	gameconfs->CloseGameConfigFile(g_pGameConf);
	forwards->ReleaseForward(find_spawn_location);
	forwards->ReleaseForward(pop_parse);
	forwards->ReleaseForward(wavespawn_parse);
	forwards->ReleaseForward(wave_parse);
	forwards->ReleaseForward(spawnlocation_parse);
	forwards->ReleaseForward(pop_event_fired);
	forwards->ReleaseForward(is_bonus_wave);
	forwards->ReleaseForward(pop_entity_spawned);
	handlesys->RemoveType(popspawner_handle, myself->GetIdentity());
}
