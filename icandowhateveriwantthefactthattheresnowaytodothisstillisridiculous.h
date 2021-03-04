/*
if the person who is reading this is a sm dev
and you dont like this file gues what
i dont care!!!!
i will only remove this file once theres a api for
creating ArrayList and KeyValues handles from extensions
util then this file will remain here
*/

#include <core/logic/CellArray.h>
#include <core/smn_keyvalues.h>

#define private protected
#include <core/logic/HandleSys.h>

HandleType_t keyvalues_handle = 0;
HandleType_t arraylist_handle = 0;
IdentityType_t coreidenttype = 0;

class HandleSystemHack : public HandleSystem
{
public:
	Handle_t CreateKeyValuesHandle(KeyValues *data, IdentityToken_t *owner)
	{
		KeyValueStack *kvstack = new KeyValueStack{};
		kvstack->pBase = data;
		kvstack->m_bDeleteOnDestroy = false;
		return CreateCoreHandle(keyvalues_handle, kvstack, owner);
	}
	
	Handle_t CreateCellArrayHandle(ICellArray *&arr, IdentityToken_t *owner)
	{
		arr = new CellArray(0);
		return CreateCoreHandle(arraylist_handle, arr, owner);
	}
	
	static void init()
	{
		handlesys->FindHandleType("KeyValues", &keyvalues_handle);
		handlesys->FindHandleType("CellArray", &arraylist_handle);
		coreidenttype = sharesys->FindIdentType("CORE");
	}
	
private:
	Handle_t CreateCoreHandle(HandleType_t type, void *object, IdentityToken_t *owner)
	{
		if (!type 
			|| type >= HANDLESYS_TYPEARRAY_SIZE
			|| m_Types[type].dispatch == NULL)
		{
			return 0;
		}

		unsigned int index;
		Handle_t handle;
		QHandle *pHandle;
		HandleError _err;

		if ((_err=MakePrimHandle__(type, &pHandle, &index, &handle, owner, false)) != HandleError_None)
		{
			return 0;
		}

		pHandle->object = object;
		pHandle->clone = 0;
		pHandle->timestamp = g_pSM->GetAdjustedTime();
		return handle;
	}
	
	HandleError MakePrimHandle__(HandleType_t type, 
						   QHandle **in_pHandle, 
						   unsigned int *in_index, 
						   Handle_t *in_handle,
						   IdentityToken_t *owner,
						   bool identity)
	{
		HandleError err;
		unsigned int owner_index = 0;

		unsigned int handle;
		if ((err = TryAllocHandle__(&handle)) != HandleError_None)
		{
			return err;
		}

		QHandle *pHandle = &m_Handles[handle];
		
		assert(pHandle->set == false);

		if (++m_HSerial >= HANDLESYS_MAX_SERIALS)
		{
			m_HSerial = 1;
		}

		/* Set essential information */
		pHandle->set = identity ? HandleSet_Identity : HandleSet_Used;
		pHandle->refcount = 1;
		pHandle->type = type;
		pHandle->serial = m_HSerial;
		pHandle->owner = owner;
		pHandle->ch_next = 0;
		pHandle->access_special = false;
		pHandle->is_destroying = false;

		/* Create the hash value */
		Handle_t hash = pHandle->serial;
		hash <<= 16;
		hash |= handle;

		/* Add a reference count to the type */
		m_Types[type].opened++;

		/* Output */
		*in_pHandle = pHandle;
		*in_index = handle;
		*in_handle = hash;

		/* Decode the identity token 
		* For now, we don't allow nested ownership
		*/
		if (owner && !identity)
		{
			QHandle *pIdentity = &m_Handles[owner_index];
			if (pIdentity->ch_prev == 0)
			{
				pIdentity->ch_prev = handle;
				pIdentity->ch_next = handle;
				pHandle->ch_prev = 0;
			}
			else
			{
				/* Link previous node to us (forward) */
				m_Handles[pIdentity->ch_next].ch_next = handle;
				/* Link us to previous node (backwards) */
				pHandle->ch_prev = pIdentity->ch_next;
				/* Set new tail */
				pIdentity->ch_next = handle;
			}
			pIdentity->refcount++;
		}
		else
		{
			pHandle->ch_prev = 0;
		}

		return HandleError_None;
	}
	
	HandleError TryAllocHandle__(unsigned int *handle)
	{
		if (m_FreeHandles == 0)
		{
			if (m_HandleTail >= HANDLESYS_MAX_HANDLES)
			{
				return HandleError_Limit;;
			}
			*handle = ++m_HandleTail;
		}
		else
		{
			*handle = m_Handles[m_FreeHandles--].freeID;
		}

		return HandleError_None;
	}
};
