DEFINT A-Z

TYPE RegTypeX
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE

CONST EMSBytesPerPage = &H4000
CONST EMSKbPerPage = &H10

DECLARE FUNCTION EMSAllocatePages (PageCount)
DECLARE FUNCTION EMSError$ (ErrorCode)
DECLARE FUNCTION EMSFreeHandles ()
DECLARE FUNCTION EMSFreePages ()
DECLARE FUNCTION EMSInstalled ()
DECLARE FUNCTION EMSPageFrameAddress ()
DECLARE FUNCTION EMSTotalPages ()
DECLARE FUNCTION EMSVersion$ ()
DECLARE SUB Demo ()
DECLARE SUB DisplayColoredBars ()
DECLARE SUB DisplayStatus ()
DECLARE SUB EMSCopyBaseToBase (Length&, SrcSegment, SrcOffset, DstSegment, DstOffset)
DECLARE SUB EMSCopyBaseToEMS (Length&, SrcSegment, SrcOffset, DstHandle, DstOffset, DstPage)
DECLARE SUB EMSCopyEMSToBase (Length&, SrcHandle, SrcOffset, SrcPage, DstSegment, DstOffset)
DECLARE SUB EMSCopyEMSToEMS (Length&, SrcHandle, SrcOffset, SrcPage, DstHandle, DstOffset, DstPage)
DECLARE SUB EMSExchangeBaseWithBase (Length&, Segment, Offset, OtherSegment, OtherOffset)
DECLARE SUB EMSExchangeBaseWithEMS (Length&, BaseSegment, BaseOffset, EMSHandle, EMSOffset, EMSPage)
DECLARE SUB EMSExchangeEMSWithEMS (Length&, Handle, Offset, Page, OtherHandle, OtherOffset, OtherPage)
DECLARE SUB EMSDeallocatepages (Handle)
DECLARE SUB InterruptX (intnum AS INTEGER, inreg AS RegTypeX, outreg AS RegTypeX)

DIM SHARED EMSErrorCode

DisplayStatus
Demo


SUB Demo
 DisplayColoredBars

 Handle = EMSAllocatePages(4)

 EMSCopyBaseToEMS &H4000, &HA000, &H0, Handle, &H0, &H0
 EMSCopyBaseToEMS &H4000, &HA000, &H4000, Handle, &H0, &H1
 EMSCopyBaseToEMS &H4000, &HA000, &H8000, Handle, &H0, &H2
 EMSCopyBaseToEMS &H3A00, &HAC00, &H0, Handle, &H0, &H3

 SLEEP: Key$ = INKEY$: CLS

 EMSCopyEMSToBase &H4000, Handle, &H0, &H0, &HA000, &H0
 EMSCopyEMSToBase &H4000, Handle, &H0, &H1, &HA000, &H4000
 EMSCopyEMSToBase &H4000, Handle, &H0, &H2, &HA000, &H8000
 EMSCopyEMSToBase &H3A00, Handle, &H0, &H3, &HAC00, &H0

 EMSDeallocatepages Handle
END SUB

SUB DisplayColoredBars
 SCREEN 13: CLS
 
 ColorV = 0
 FOR y = 0 TO 199 STEP 12
  LINE (0, y)-STEP(319, 12), ColorV, BF
  ColorV = ColorV + 1
 NEXT y
END SUB

SUB DisplayStatus
 SCREEN 0: WIDTH 80, 25: COLOR 7, 0: CLS

 PRINT "EMS free pages: "; EMSFreePages
 PRINT "EMS free handles: "; EMSFreeHandles
 PRINT "EMS installed: "; EMSInstalled
 PRINT "EMS page frame: "; HEX$(EMSPageFrameAddress)
 PRINT "EMS status: "; EMSError$(EMSErrorCode)
 PRINT "EMS total pages: "; EMSTotalPages
 PRINT "EMS version: "; EMSVersion

 SLEEP: Key$ = INKEY$
END SUB

FUNCTION EMSAllocatePages (PageCount)
DIM Registers AS RegTypeX

 Registers.ax = &H4300
 Registers.bx = PageCount
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100

 EMSAllocatePages = Registers.dx
END FUNCTION

SUB EMSCopyBaseToBase (Length&, SrcSegment, SrcOffset, DstSegment, DstOffset)
DIM Registers AS RegTypeX

 CopyInformation$ = MKL$(Length&) + CHR$(&H0) + MKI$(&H0) + MKI$(SrcOffset) + MKI$(SrcSegment) + CHR$(&H0) + MKI$(&H0) + MKI$(DstOffset) + MKI$(DstSegment)

 Registers.ax = &H5700
 Registers.ds = VARSEG(CopyInformation$)
 Registers.si = SADD(CopyInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSCopyBaseToEMS (Length&, SrcSegment, SrcOffset, DstHandle, DstOffset, DstPage)
DIM Registers AS RegTypeX

 CopyInformation$ = MKL$(Length&) + CHR$(&H0) + MKI$(&H0) + MKI$(SrcOffset) + MKI$(SrcSegment) + CHR$(&H1) + MKI$(DstHandle) + MKI$(DstOffset) + MKI$(DstPage)

 Registers.ax = &H5700
 Registers.ds = VARSEG(CopyInformation$)
 Registers.si = SADD(CopyInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSCopyEMSToBase (Length&, SrcHandle, SrcOffset, SrcPage, DstSegment, DstOffset)
DIM Registers AS RegTypeX

 CopyInformation$ = MKL$(Length&) + CHR$(&H1) + MKI$(SrcHandle) + MKI$(SrcOffset) + MKI$(SrcPage) + CHR$(&H0) + MKI$(&H0) + MKI$(DstOffset) + MKI$(DstSegment)

 Registers.ax = &H5700
 Registers.ds = VARSEG(CopyInformation$)
 Registers.si = SADD(CopyInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSCopyEMSToEMS (Length&, SrcHandle, SrcOffset, SrcPage, DstHandle, DstOffset, DstPage)
DIM Registers AS RegTypeX

 CopyInformation$ = MKL$(Length&) + CHR$(&H1) + MKI$(SrcHandle) + MKI$(SrcOffset) + MKI$(SrcPage) + CHR$(&H1) + MKI$(DstHandle) + MKI$(DstOffset) + MKI$(DstPage)

 Registers.ax = &H5700
 Registers.ds = VARSEG(CopyInformation$)
 Registers.si = SADD(CopyInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSDeallocatepages (Handle)
DIM Registers AS RegTypeX

 Registers.ax = &H4500
 Registers.dx = Handle
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

FUNCTION EMSError$ (ErrorCode)
DIM Registers AS RegTypeX

 SELECT CASE EMSErrorCode
  CASE &H0: Message$ = "No error."
  CASE &H80: Message$ = "Internal error."
  CASE &H81: Message$ = "Hardware malfunction."
  CASE &H82: Message$ = "Busy."
  CASE &H83: Message$ = "Invalid handle."
  CASE &H84: Message$ = "Undefined function requested by application."
  CASE &H85: Message$ = "No more handles available."
  CASE &H86: Message$ = "Error in save or restore of mapping context."
  CASE &H87: Message$ = "Insufficient memory pages in system."
  CASE &H88: Message$ = "Insufficient memory pages available."
  CASE &H89: Message$ = "Zero pages requested."
  CASE &H8A: Message$ = "Invalid logical page number encountered."
  CASE &H8B: Message$ = "Invalid physical page number encountered."
  CASE &H8C: Message$ = "Page-mapping hardware state save area is full."
  CASE &H8D: Message$ = "Save of mapping context failed."
  CASE &H8E: Message$ = "Restore of mapping context failed."
  CASE &H8F: Message$ = "Undefined subfunction."
  CASE &H90: Message$ = "Undefined attribute type."
  CASE &H91: Message$ = "Feature not supported."
  CASE &H92: Message$ = "Successful, but a portion of the source region has been overwritten."
  CASE &H93: Message$ = "Length of source or destination region exceeds length of region allocated to either source or destination handle."
  CASE &H94: Message$ = "Conventional and expanded memory regions overlap."
  CASE &H95: Message$ = "Offset within logical page exceeds size of logical page."
  CASE &H96: Message$ = "Region length exceeds 1 MB"
  CASE &H97: Message$ = "Source and destination EMS regions have same handle and overlap."
  CASE &H98: Message$ = "Memory source or destination type undefined."
  CASE &H9A: Message$ = "Specified alternate map register or DMA register set not supported."
  CASE &H9B: Message$ = "All alternate map register or DMA register sets currently allocated."
  CASE &H9C: Message$ = "Alternate map register or DMA register sets not supported."
  CASE &H9D: Message$ = "Undefined or unallocated alternate map register or DMA register set."
  CASE &H9E: Message$ = "Dedicated DMA channels not supported."
  CASE &H9F: Message$ = "Specified dedicated DMA channel not supported."
  CASE &HA0: Message$ = "No such handle name."
  CASE &HA1: Message$ = "A handle found had no name, or duplicate handle name."
  CASE &HA2: Message$ = "Attempted to wrap around 1 MB conventional address space."
  CASE &HA3: Message$ = "Source array corrupted."
  CASE &HA4: Message$ = "Operating system denied access."
  CASE ELSE: Message$ = "Undefined error: 0x" + HEX$(EMSErrorCode) + "."
 END SELECT

 EMSError$ = Message$
END FUNCTION

SUB EMSExchangeBaseWithBase (Length&, Segment, Offset, OtherSegment, OtherOffset)
DIM Registers AS RegTypeX

 ExchangeInformation$ = MKL$(Length&) + CHR$(&H0) + MKI$(&H0) + MKI$(Offset) + MKI$(Segment) + CHR$(&H0) + MKI$(&H0) + MKI$(OtherOffset) + MKI$(OtherSegment)

 Registers.ax = &H5701
 Registers.ds = VARSEG(ExchangeInformation$)
 Registers.si = SADD(ExchangeInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSExchangeBaseWithEMS (Length&, BaseSegment, BaseOffset, EMSHandle, EMSOffset, EMSPage)
DIM Registers AS RegTypeX

 ExchangeInformation$ = MKL$(Length&) + CHR$(&H0) + MKI$(&H0) + MKI$(BaseOffset) + MKI$(BaseSegment) + CHR$(&H1) + MKI$(EMSHandle) + MKI$(EMSOffset) + MKI$(EMSPage)

 Registers.ax = &H5701
 Registers.ds = VARSEG(ExchangeInformation$)
 Registers.si = SADD(ExchangeInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

SUB EMSExchangeEMSWithEMS (Length&, Handle, Offset, Page, OtherHandle, OtherOffset, OtherPage)
DIM Registers AS RegTypeX

 ExchangeInformation$ = MKL$(Length&) + CHR$(&H1) + MKI$(Handle) + MKI$(Offset) + MKI$(Page) + CHR$(&H1) + MKI$(OtherHandle) + MKI$(OtherOffset) + MKI$(OtherPage)

 Registers.ax = &H5701
 Registers.ds = VARSEG(ExchangeInformation$)
 Registers.si = SADD(ExchangeInformation$)
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
END SUB

FUNCTION EMSFreeHandles
DIM Registers AS RegTypeX

  Registers.ax = &H4B00
  InterruptX &H67, Registers, Registers
  UsedHandles = Registers.bx

  Registers.ax = &H5402
  InterruptX &H67, Registers, Registers
  EMSErrorCode = Registers.ax \ &H100
  TotalHandles = Registers.bx

  EMSFreeHandles = TotalHandles - UsedHandles
END FUNCTION

FUNCTION EMSFreePages
DIM Registers AS RegTypeX

 Registers.ax = &H4200
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSFreePages = Registers.bx
END FUNCTION

FUNCTION EMSInstalled
DIM Registers AS RegTypeX

 Registers.ax = &H3567
 InterruptX &H21, Registers, Registers

 DEF SEG = Registers.es
 FOR Position = &HA TO &H11
  EMM$ = EMM$ + CHR$(PEEK(Position))
 NEXT Position

 EMSInstalled = (EMM$ = "EMMXXXX0")
END FUNCTION

FUNCTION EMSPageFrameAddress
DIM Registers AS RegTypeX

 Registers.ax = &H4100
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSPageFrameAddress = Registers.bx
END FUNCTION

FUNCTION EMSTotalPages
DIM Registers AS RegTypeX

 Registers.ax = &H4200
 InterruptX &H67, Registers, Registers
 EMSErrorCode = Registers.ax \ &H100
 EMSTotalPages = Registers.dx
END FUNCTION

FUNCTION EMSVersion$
DIM Registers AS RegTypeX

 Registers.ax = &H4600
 InterruptX &H67, Registers, Registers

 EMSErrorCode = Registers.ax \ &H100
 Version = Registers.ax AND &HFF
 Major = (Version AND &HF0) \ &H10
 Minor = Version AND &HF
 EMSVersion$ = LTRIM$(STR$(Major)) + "." + LTRIM$(STR$(Minor))
END FUNCTION

