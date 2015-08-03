
module DDC.Core.Salt.Name.PrimStore
        ( PrimStore     (..)
        , readPrimStore)
where
import DDC.Base.Pretty
import Control.DeepSeq


-- | Raw access to the store.
data PrimStore
        -- Constants ------------------
        -- | Number of bytes needed to store a value of a primitive type.
        = PrimStoreSize

        -- | Log2 of number of bytes need to store a value of a primitive type.
        | PrimStoreSize2

        -- Allocation -----------------
        -- | Check whether there are at least this many bytes still available
        --   on the heap.
        | PrimStoreCheck

        -- | Force a garbage collection to recover at least this many bytes.
        | PrimStoreRecover

        -- | Allocate some space on the heap.
        --   There must be enough space available, else undefined.
        | PrimStoreAlloc

        -- | Allocate a pointer on the stack for a GC root.
        | PrimStoreAllocSlot

        -- Addr operations ------------
        -- | Read a value from the store at the given address and offset.
        | PrimStoreRead

        -- | Write a value to the store at the given address and offset.
        | PrimStoreWrite

        -- | Copy a block of memory from the source address to the destination address.
        | PrimStoreCopy

        -- | Add an offset in bytes to an address.
        | PrimStorePlusAddr

        -- | Subtract an offset in bytes from an address.
        | PrimStoreMinusAddr

        -- Ptr operations -------------
        -- | Read a value from a pointer plus the given offset.
        | PrimStorePeek

        -- | Write a value to a pointer plus the given offset.
        | PrimStorePoke

        -- | Add an offset in bytes to a pointer.
        | PrimStorePlusPtr

        -- | Subtract an offset in bytes from a pointer.
        | PrimStoreMinusPtr

        -- | Convert an raw address to a pointer.
        | PrimStoreMakePtr

        -- | Convert a pointer to a raw address.
        | PrimStoreTakePtr

        -- | Cast between pointer types.
        | PrimStoreCastPtr

        -- Slot Stack -----------------
        -- | Base address of the slot stack.
        | PrimStoreSlotBase

        -- | The top of the slot stack.
        --     The next slot is allocated here.
        | PrimStoreSlotTop

        -- | The maximum value that slotTop# can take without overflowing the stack.
        | PrimStoreSlotMax

        -- Heap -----------------------
        -- | Front Heap: The first object is allocated at this address.
        | PrimStoreHeapBase

        -- | Front Heap: The next object is allocated starting from this address.
        | PrimStoreHeapTop

        -- | Front Heap: Points to the last byte in the heap which can be allocated.
        | PrimStoreHeapMax

        -- | Back Heap: The first object is allocated at this address.
        | PrimStoreHeapBackBase

        -- | Back Heap: The next object is allocated starting from this address.
        | PrimStoreHeapBackTop

        -- | Back Heap: Points to the last byte in the heap which can be allocated.
        | PrimStoreHeapBackMax
        deriving (Eq, Ord, Show)


instance NFData PrimStore where
 rnf !_ = ()


instance Pretty PrimStore where
 ppr p
  = case p of        
        PrimStoreSize           -> text "size#"
        PrimStoreSize2          -> text "size2#"        

        PrimStoreCheck          -> text "check#"
        PrimStoreRecover        -> text "recover#"
        PrimStoreAlloc          -> text "alloc#"
        PrimStoreAllocSlot      -> text "allocSlot#"

        PrimStoreRead           -> text "read#"
        PrimStoreWrite          -> text "write#"
        PrimStoreCopy           -> text "copy#"
        PrimStorePlusAddr       -> text "plusAddr#"
        PrimStoreMinusAddr      -> text "minusAddr#"

        PrimStorePeek           -> text "peek#"
        PrimStorePoke           -> text "poke#"
        PrimStorePlusPtr        -> text "plusPtr#"
        PrimStoreMinusPtr       -> text "minusPtr#"
        PrimStoreMakePtr        -> text "makePtr#"
        PrimStoreTakePtr        -> text "takePtr#"
        PrimStoreCastPtr        -> text "castPtr#"

        PrimStoreSlotBase       -> text "slotBase#"
        PrimStoreSlotTop        -> text "slotTop#"
        PrimStoreSlotMax        -> text "slotMax#"
        PrimStoreHeapBase       -> text "heapBase#"
        PrimStoreHeapTop        -> text "heapTop#"
        PrimStoreHeapMax        -> text "heapMax#"
        PrimStoreHeapBackBase   -> text "heapBackBase#"
        PrimStoreHeapBackTop    -> text "heapBackTop#"
        PrimStoreHeapBackMax    -> text "heapBackMax#"


readPrimStore :: String -> Maybe PrimStore
readPrimStore str
 = case str of
        "size#"                 -> Just PrimStoreSize
        "size2#"                -> Just PrimStoreSize2

        "check#"                -> Just PrimStoreCheck
        "recover#"              -> Just PrimStoreRecover
        "alloc#"                -> Just PrimStoreAlloc
        "allocSlot#"            -> Just PrimStoreAllocSlot

        "read#"                 -> Just PrimStoreRead
        "write#"                -> Just PrimStoreWrite
        "copy#"                 -> Just PrimStoreCopy
        "plusAddr#"             -> Just PrimStorePlusAddr
        "minusAddr#"            -> Just PrimStoreMinusAddr

        "peek#"                 -> Just PrimStorePeek
        "poke#"                 -> Just PrimStorePoke
        "plusPtr#"              -> Just PrimStorePlusPtr
        "minusPtr#"             -> Just PrimStoreMinusPtr
        "makePtr#"              -> Just PrimStoreMakePtr
        "takePtr#"              -> Just PrimStoreTakePtr
        "castPtr#"              -> Just PrimStoreCastPtr

        "slotBase#"             -> Just PrimStoreSlotBase
        "slotTop#"              -> Just PrimStoreSlotTop
        "slotMax#"              -> Just PrimStoreSlotMax
        "heapBase#"             -> Just PrimStoreHeapBase
        "heapTop#"              -> Just PrimStoreHeapTop
        "heapMax#"              -> Just PrimStoreHeapMax
        "heapBackBase#"         -> Just PrimStoreHeapBackBase
        "heapBackTop#"          -> Just PrimStoreHeapBackTop
        "heapBackMax#"          -> Just PrimStoreHeapBackMax

        _                       -> Nothing

