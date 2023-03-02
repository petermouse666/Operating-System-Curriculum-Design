%include	"pm.inc"

; =============================全局=================================

; =================================================================
; 页表部分
PageDirBase0	equ	200000h	; 页目录 0 开始地址:	2M
PageTblBase0	equ	201000h	; 页表 0 开始地址:		2M +  4K
PageDirBase1	equ	210000h	; 页目录 1 开始地址:	2M + 64K
PageTblBase1	equ	211000h	; 页表 1 开始地址:		2M + 64K + 4K
PageDirBase2	equ	220000h	; 页目录 2 开始地址:	
PageTblBase2	equ	221000h	; 页表 2 开始地址:		
PageDirBase3	equ	230000h	; 页目录 3 开始地址:	
PageTblBase3	equ	231000h	; 页表 3 开始地址:		
; =================================================================

org	0100h
	jmp	LABEL_BEGIN

; ==================================================================
; 中断描述符表IDT部分
[SECTION .idt]
ALIGN	32
[BITS	32]
LABEL_IDT:
; 门                          目标选择子,            偏移, DCount, 属性
%rep 32			
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate	
%endrep
.020h:			Gate	SelectorCode32,    ClockHandler,      0, DA_386IGate 			; 时钟中断
%rep 96			
				Gate	SelectorCode32, SpuriousHandler,      0, DA_386IGate	
%endrep
IdtLen		equ	$ - LABEL_IDT	; IDT 长度
IdtPtr		dw	IdtLen - 1		; IDT 段界限
			dd	0				; IDT 基地址, 待设置
; END of [SECTION .idt]
; ==================================================================

; ==================================================================
; 全局描述符表GDT部分
[SECTION .gdt]
;                                  段基址,            段界限,  属性
LABEL_GDT:			Descriptor	       0,                 0, 0								; 空描述符
LABEL_DESC_NORMAL:	Descriptor	       0,            0ffffh, DA_DRW							; Normal 描述符
LABEL_DESC_FLAT_C:	Descriptor         0,           0fffffh, DA_CR | DA_32 | DA_LIMIT_4K	; 0 ~ 4G
LABEL_DESC_FLAT_RW:	Descriptor         0,           0fffffh, DA_DRW | DA_LIMIT_4K			; 0 ~ 4G
LABEL_DESC_CODE32:	Descriptor	       0,  SegCode32Len - 1, DA_CR | DA_32					; 非一致代码段, 32 位, 基址待设置
LABEL_DESC_CODE16:	Descriptor	       0,            0ffffh, DA_C							; 非一致代码段, 16 位, 基址待设置
LABEL_DESC_DATA:	Descriptor	       0,		DataLen - 1, DA_DRW							; 数据段, 基址待设置
LABEL_DESC_STACK:	Descriptor	       0,        TopOfStack, DA_DRWA | DA_32				; 堆栈段, 32 位, 基址待设置
LABEL_DESC_VIDEO:	Descriptor	 0B8000h,            0ffffh, DA_DRW	| DA_DPL3					; 显存首地址

LABEL_DESC_LDT0:	Descriptor	       0,       LDTLen0 - 1, DA_LDT							; LDT0, 基址待设置
LABEL_DESC_TSS0:	Descriptor	       0,       TSSLen0 - 1, DA_386TSS						; TSS0, 基址待设置
LABEL_DESC_LDT1:	Descriptor	       0,       LDTLen1 - 1, DA_LDT							; LDT1, 基址待设置
LABEL_DESC_TSS1:	Descriptor	       0,       TSSLen1 - 1, DA_386TSS						; TSS1, 基址待设置
LABEL_DESC_LDT2:	Descriptor	       0,       LDTLen2 - 1, DA_LDT							; LDT2, 基址待设置
LABEL_DESC_TSS2:	Descriptor	       0,       TSSLen2 - 1, DA_386TSS						; TSS2, 基址待设置
LABEL_DESC_LDT3:	Descriptor	       0,       LDTLen3 - 1, DA_LDT							; LDT3, 基址待设置
LABEL_DESC_TSS3:	Descriptor	       0,       TSSLen3 - 1, DA_386TSS						; TSS3, 基址待设置

GdtLen		equ	$ - LABEL_GDT	; GDT 长度
GdtPtr		dw	GdtLen - 1		; GDT 界限
			dd	0				; GDT 基地址, 待设置

; GDT 选择子
SelectorNormal		equ	LABEL_DESC_NORMAL	- LABEL_GDT
SelectorFlatC		equ	LABEL_DESC_FLAT_C	- LABEL_GDT
SelectorFlatRW		equ	LABEL_DESC_FLAT_RW	- LABEL_GDT
SelectorCode32		equ	LABEL_DESC_CODE32	- LABEL_GDT
SelectorCode16		equ	LABEL_DESC_CODE16	- LABEL_GDT
SelectorData		equ	LABEL_DESC_DATA		- LABEL_GDT
SelectorStack		equ	LABEL_DESC_STACK	- LABEL_GDT
SelectorVideo		equ	LABEL_DESC_VIDEO	- LABEL_GDT

SelectorLDT0		equ	LABEL_DESC_LDT0		- LABEL_GDT				; LDT0 的选择子
SelectorTSS0		equ	LABEL_DESC_TSS0		- LABEL_GDT				; TSS0 的选择子
SelectorLDT1		equ	LABEL_DESC_LDT1		- LABEL_GDT				; LDT1 的选择子
SelectorTSS1		equ	LABEL_DESC_TSS1		- LABEL_GDT				; TSS1 的选择子
SelectorLDT2		equ	LABEL_DESC_LDT2		- LABEL_GDT				; LDT2 的选择子
SelectorTSS2		equ	LABEL_DESC_TSS2		- LABEL_GDT				; TSS2 的选择子
SelectorLDT3		equ	LABEL_DESC_LDT3		- LABEL_GDT				; LDT3 的选择子
SelectorTSS3		equ	LABEL_DESC_TSS3		- LABEL_GDT				; TSS3 的选择子



; END of [SECTION .gdt]
; ==================================================================

; ==================================================================
; 全局数据段部分（存放全局字符串与变量）
[SECTION .data_glob]
ALIGN	32
[BITS	32]
LABEL_DATA:
; 实模式下使用这些符号
; 字符串                                
_szPMMessage:		db	"Operating System Curriculum Design for 2023 QWQ       INFO:" , 0Ah
					db  "MADE BY Guanghao Zhao U202011998. Class CSE 2001. 2023-2-28", 0Ah,0Ah,0Ah, 0
_szMemChkTitle:		db  "Memory Info:",0Ah
					db	"BaseAddrL BaseAddrH LengthLow LengthHigh   Type  ", 0Ah, 0
_szRAMSize			db	"Can use RAM size:", 0
_szReturn			db	0Ah, 0
; 变量
_wSPValueInRealMode	dw	0
_dwMCRNumber:		dd	0					; Memory Check Result Number
_dwDispPos:			dd	(80 * 2 + 0) * 2	; 屏幕第 2 行, 第 0 列。
_dwMemSize:			dd	0
_ARDStruct:									; Address Range Descriptor Structure
	_dwBaseAddrLow:		dd	0
	_dwBaseAddrHigh:	dd	0
	_dwLengthLow:		dd	0
	_dwLengthHigh:		dd	0
	_dwType:			dd	0
_PageTableNumber	dd	0
_SavedIDTR:			dd	0					; 用于保存 IDTR
					dd	0
_SavedIMREG:		db	0					; 中断屏蔽寄存器值
_MemChkBuf:	times	256	db	0				; Memory Check Result Buffer

_PriorityTask0		dd	1024
_PriorityTask1		dd	512
_PriorityTask2		dd	246
_PriorityTask3		dd	128

_dwCurrentTask		dd	0
_dwCurrentcnt		dd	0
; 保护模式下使用这些符号
szPMMessage		equ	_szPMMessage	- $$
szMemChkTitle	equ	_szMemChkTitle	- $$
szRAMSize		equ	_szRAMSize		- $$
szReturn		equ	_szReturn		- $$
dwDispPos		equ	_dwDispPos		- $$
dwMemSize		equ	_dwMemSize		- $$
dwMCRNumber		equ	_dwMCRNumber	- $$
ARDStruct		equ	_ARDStruct		- $$
	dwBaseAddrLow	equ	_dwBaseAddrLow	- $$
	dwBaseAddrHigh	equ	_dwBaseAddrHigh	- $$
	dwLengthLow		equ	_dwLengthLow	- $$
	dwLengthHigh	equ	_dwLengthHigh	- $$
	dwType			equ	_dwType			- $$
MemChkBuf		equ	_MemChkBuf		- $$
SavedIDTR		equ	_SavedIDTR		- $$
SavedIMREG		equ	_SavedIMREG		- $$
PageTableNumber	equ	_PageTableNumber	- $$

PriorityTask0	equ	_PriorityTask0 - $$
PriorityTask1	equ	_PriorityTask1 - $$
PriorityTask2	equ	_PriorityTask2 - $$
PriorityTask3	equ	_PriorityTask3 - $$

dwCurrentTask	equ _dwCurrentTask	- $$
dwCurrentcnt	equ _dwCurrentcnt	- $$

INT_M_CTL	    equ	0x20
EOI				equ	0x20
DataLen			equ	$ - LABEL_DATA
; END of [SECTION .data_glob]
; ==================================================================

; ==================================================================
; 全局堆栈段部分
[SECTION .stack_glob]
ALIGN	32
[BITS	32]
LABEL_STACK:
	times 512 db 0
TopOfStack	equ	$ - LABEL_STACK - 1
; END of [SECTION .stack_glob]
; ==================================================================

; ==================================================================
; 16 位代码段, 程序的入口和出口.
[SECTION .s16]
[BITS	16]
; 程序入口
LABEL_BEGIN:
	mov		ax, cs
	mov		ds, ax
	mov		es, ax
	mov		ss, ax
	mov		sp, 0100h

	mov		[LABEL_GO_BACK_TO_REAL + 3], ax	; 设置跳回实模式的基址
	mov		[_wSPValueInRealMode], sp		; 保存实模式下的 sp 寄存器

	; 使用 15 号中断得到内存信息
	mov		ebx, 0
	mov		di, _MemChkBuf
.loop:
	mov		eax, 0E820h
	mov		ecx, 20
	mov		edx, 0534D4150h
	int		15h
	jc		LABEL_MEM_CHK_FAIL
	add		di, 20
	inc		dword [_dwMCRNumber]
	cmp		ebx, 0
	jne		.loop
	jmp		LABEL_MEM_CHK_OK
LABEL_MEM_CHK_FAIL:
	mov		dword [_dwMCRNumber], 0
LABEL_MEM_CHK_OK:
	; 初始化描述符基地址
	DescriptorBaseInit LABEL_SEG_CODE16,	LABEL_DESC_CODE16			; 初始化 16 位代码段描述符
	DescriptorBaseInit LABEL_SEG_CODE32,	LABEL_DESC_CODE32			; 初始化 32 位代码段描述符
	DescriptorBaseInit LABEL_DATA,			LABEL_DESC_DATA				; 初始化数据段描述符
	DescriptorBaseInit LABEL_STACK, 		LABEL_DESC_STACK			; 初始化堆栈段描述符
	DescriptorBaseInit LABEL_LDT0,			LABEL_DESC_LDT0				; 初始化 LDT0 描述符
	DescriptorBaseInit LABEL_T0_DATA,		LABEL_DESC_TASK0_DATA		; 初始化 LDT0 中 DATA 的描述符
	DescriptorBaseInit LABEL_T0_CODE,		LABEL_DESC_TASK0_CODE		; 初始化 LDT0 中 CODE 的描述符
	DescriptorBaseInit LABEL_T0_CODE3,		LABEL_DESC_TASK0_CODE3		; 初始化 LDT0 中 CODE3	的描述符
	DescriptorBaseInit LABEL_T0_STACK0,		LABEL_DESC_TASK0_STACK0		; 初始化 LDT0 中 STACK0 的描述符
	DescriptorBaseInit LABEL_T0_STACK3,		LABEL_DESC_TASK0_STACK3		; 初始化 LDT0 中 STACK3 的描述符
	DescriptorBaseInit LABEL_TSS0,			LABEL_DESC_TSS0				; 初始化 TSS0 描述符
	DescriptorBaseInit LABEL_LDT1,			LABEL_DESC_LDT1				; 初始化 LDT1 描述符
	DescriptorBaseInit LABEL_T1_DATA, 		LABEL_DESC_TASK1_DATA		; 初始化 LDT1 中 DATA 的描述符
	DescriptorBaseInit LABEL_T1_CODE,		LABEL_DESC_TASK1_CODE		; 初始化 LDT1 中 CODE 的描述符
	DescriptorBaseInit LABEL_T1_CODE3,		LABEL_DESC_TASK1_CODE3		; 初始化 LDT1 中 CODE3	的描述符
	DescriptorBaseInit LABEL_T1_STACK0,		LABEL_DESC_TASK1_STACK0		; 初始化 LDT1 中 STACK0 的描述符
	DescriptorBaseInit LABEL_T1_STACK3,		LABEL_DESC_TASK1_STACK3		; 初始化 LDT1 中 STACK3 的描述符
	DescriptorBaseInit LABEL_TSS1,			LABEL_DESC_TSS1				; 初始化 TSS1 描述符
	DescriptorBaseInit LABEL_LDT2,			LABEL_DESC_LDT2				; 初始化 LDT2 描述符
	DescriptorBaseInit LABEL_T2_DATA, 		LABEL_DESC_TASK2_DATA		; 初始化 LDT2 中 DATA 的描述符
	DescriptorBaseInit LABEL_T2_CODE,		LABEL_DESC_TASK2_CODE		; 初始化 LDT2 中 CODE 的描述符
	DescriptorBaseInit LABEL_T2_CODE3,		LABEL_DESC_TASK2_CODE3		; 初始化 LDT2 中 CODE3	的描述符
	DescriptorBaseInit LABEL_T2_STACK0,		LABEL_DESC_TASK2_STACK0		; 初始化 LDT2 中 STACK0 的描述符
	DescriptorBaseInit LABEL_T2_STACK3,		LABEL_DESC_TASK2_STACK3		; 初始化 LDT2 中 STACK3 的描述符
	DescriptorBaseInit LABEL_TSS2,			LABEL_DESC_TSS2				; 初始化 TSS2 描述符
	DescriptorBaseInit LABEL_LDT3,			LABEL_DESC_LDT3				; 初始化 LDT3 描述符
	DescriptorBaseInit LABEL_T3_DATA, 		LABEL_DESC_TASK3_DATA		; 初始化 LDT3 中 DATA 的描述符
	DescriptorBaseInit LABEL_T3_CODE,		LABEL_DESC_TASK3_CODE		; 初始化 LDT3 中 CODE 的描述符
	DescriptorBaseInit LABEL_T3_CODE3,		LABEL_DESC_TASK3_CODE3		; 初始化 LDT3 中 CODE3	的描述符
	DescriptorBaseInit LABEL_T3_STACK0,		LABEL_DESC_TASK3_STACK0		; 初始化 LDT3 中 STACK0 的描述符
	DescriptorBaseInit LABEL_T3_STACK3,		LABEL_DESC_TASK3_STACK3		; 初始化 LDT3 中 STACK3 的描述符
	DescriptorBaseInit LABEL_TSS3,			LABEL_DESC_TSS3				; 初始化 TSS3 描述符
	
	; 为加载 GDTR 作准备
	DTRLoad		LABEL_GDT,		GdtPtr

	; 为加载 IDTR 作准备
	DTRLoad		LABEL_IDT,		IdtPtr

	sidt	[_SavedIDTR]				; 保存 IDTR

	in		al, 21h						; ┳ 保存中断屏蔽寄存器(IMREG)值
	mov		[_SavedIMREG], al			; ┛

	lgdt	[GdtPtr]					; 加载 GDTR

	cli									; 关中断

	lidt	[IdtPtr]					; 加载 IDTR

	in		al, 92h						; ┓
	or		al, 00000010b				; ┣ 打开地址线 A20
	out		92h, al						; ┛

	mov		eax, cr0					; ┓
	or		eax, 1						; ┣ 准备切换到保护模式
	mov		cr0, eax					; ┛

	jmp		dword SelectorCode32:0		; 真正进入保护模式
; ==================================================================

; ==================================================================
; 从保护模式跳回到实模式就到了这里, 程序出口
LABEL_REAL_ENTRY:		
	mov		ax, cs
	mov		ds, ax
	mov		es, ax
	mov		ss, ax

	mov		sp, [_wSPValueInRealMode]	; 恢复 sp 寄存器

	lidt	[_SavedIDTR]				; 恢复 IDTR 的原值

	mov		al, [_SavedIMREG]			; ┳ 恢复中断屏蔽寄存器 IMREG 的原值
	out		21h, al						; ┛

	in		al, 92h						; ┓
	and		al, 11111101b				; ┣ 关闭 A20 地址线
	out		92h, al						; ┛

	sti									; 开中断

	mov		ax, 4c00h					; ┳ 回到 DOS
	int		21h							; ┛
; END of [SECTION .s16]
; ==================================================================

; ==================================================================
; 32 位代码段, 由实模式跳入.
[SECTION .s32]
[BITS	32]
LABEL_SEG_CODE32:
	mov		ax, SelectorData
	mov		ds, ax				; DS <- 数据段选择子
	mov		es, ax				; ES <- 数据段选择子
	mov		ax, SelectorVideo
	mov		gs, ax				; GS <- 视频段选择子
	mov		ax, SelectorStack
	mov		ss, ax				; SS <-堆栈段选择子
	mov		esp, TopOfStack		; ESP <- 栈顶指针

	call	Init8253A			; 设置定时芯片
	call	Init8259A			; 初始化中断
	call	ClearScreen			; 清屏，便于显示

	push	szPMMessage			; ┓
	mov		ah,52H				; ┃
	call	DispStr				; ┣ 输出信息
	add		esp, 4				; ┛

	push	szMemChkTitle		; ┓
	mov		ah,43H				; ┃
	call	DispStr				; ┣ 显示内存信息
	add		esp, 4				; ┃
	call	DispMemSize			; ┛

	; 根据内存大小计算应初始化多少 PDE 以及多少页表
	xor		edx, edx
	mov		eax, [dwMemSize]
	mov		ebx, 400000h			; 400000h = 4M = 4096 * 1024, 一个页表对应的内存大小
	div		ebx
	mov		ecx, eax				; 此时 ecx 为页表的个数，也即 PDE 应该的个数
	test	edx, edx
	jz		.no_remainder
	inc		ecx						; 如果余数不为 0 就需增加一个页表
.no_remainder:
	mov		[PageTableNumber], ecx	; 暂存页表个数

	;xchg bx,bx
	call	SetupPaging0		; 初始化任务 0 的页表
	;xchg bx,bx
	call	SetupPaging1		; 初始化任务 1 的页表
	;xchg bx,bx
	call	SetupPaging2		; 初始化任务 2 的页表
	;xchg bx,bx
	call	SetupPaging3		; 初始化任务 3 的页表
	sti							; 打开中断


	mov		ax, SelectorTSS0	; ┳ 加载 TSS
	ltr		ax					; ┛

	mov		ax, SelectorLDT0	; ┳ 加载 LDT
	lldt	ax					; ┛

	mov		eax, PageDirBase0	; ┳ 加载 CR3
	mov		cr3, eax			; ┛
	mov		eax, cr0			; ┓
	or		eax, 80000000h		; ┣ 打开分页
	mov		cr0, eax			; ┃
	jmp		short .1			; ┛
.1:
	nop


	; 使用 iretd 进行任务切换, 切换至任务 0
	push	SelectorTask0Stack0	; SS
	push	TopOfTask0Stack0	; ESP
	pushfd						; EFLAGS
	pop		eax					; ┓
	or		eax, 0x200			; ┣ 将 EFLAGS 中的 IF 位置 1, 即开启中断
	push	eax					; ┛
	push	SelectorTask0Code	; CS
	push	0					; EIP
	iretd

	call	SetRealmode8259A	; 恢复 8259A 以顺利返回实模式, （未执行）
	jmp		SelectorCode16:0	; 返回实模式, （未执行）
; ==================================================================



; ==================================================================
; 32位下的功能函数
; ClearScreen ------------------------------------------------------------------
ClearScreen:
	push	eax
	push	ebx
	push	ecx

	mov		ah, 00000000b			; 0000: 黑底    0000: 黑字
	mov		al, 0
	mov		ebx, 0
	mov		ecx, 4000
.1:
	mov		[gs:ebx], ax
	add		ebx, 2
	loop 	.1

	pop		ecx
	pop		ebx
	pop		eax

	ret
; END of ClearScreen -----------------------------------------------------------

; Init8259A --------------------------------------------------------------------
Init8259A:
	mov		al, 011h
	out		020h, al	; 主8259, ICW1.
	call	io_delay

	out		0A0h, al	; 从8259, ICW1.
	call	io_delay

	mov		al, 020h	; IRQ0 对应中断向量 0x20
	out		021h, al	; 主8259, ICW2.
	call	io_delay

	mov		al, 028h	; IRQ8 对应中断向量 0x28
	out		0A1h, al	; 从8259, ICW2.
	call	io_delay

	mov		al, 004h	; IR2 对应从8259
	out		021h, al	; 主8259, ICW3.
	call	io_delay

	mov		al, 002h	; 对应主8259的 IR2
	out		0A1h, al	; 从8259, ICW3.
	call	io_delay

	mov		al, 001h
	out		021h, al	; 主8259, ICW4.
	call	io_delay

	out		0A1h, al	; 从8259, ICW4.
	call	io_delay

	mov		al, 11111110b	; 仅仅开启定时器中断
	; mov		al, 11111111b	; 屏蔽主8259所有中断
	out		021h, al	; 主8259, OCW1.
	call	io_delay

	mov		al, 11111111b	; 屏蔽从8259所有中断
	out		0A1h, al	; 从8259, OCW1.
	call	io_delay

	ret
; END of Init8259A -------------------------------------------------------------

; SetRealmode8259A -------------------------------------------------------------
SetRealmode8259A:
	mov		ax, SelectorData
	mov		fs, ax

	mov		al, 017h
	out		020h, al	; 主8259, ICW1.
	call	io_delay

	mov		al, 008h	; IRQ0 对应中断向量 0x8
	out		021h, al	; 主8259, ICW2.
	call	io_delay

	mov		al, 001h
	out		021h, al	; 主8259, ICW4.
	call	io_delay

	mov		al, [fs:SavedIMREG]	; ┓恢复中断屏蔽寄存器 IMREG 的原值
	out		021h, al			; ┛
	call	io_delay

	ret
; END of SetRealmode8259A ------------------------------------------------------

; Init8253A --------------------------------------------------------------------
Init8253A:
	mov		al, 00110110b		; 通道 0 的 CONTROL 字节
	out		043h, al			; 设置 8253A 芯片, 2 字节计数值, 模式 3, 二进制计数
	call	io_delay

	mov		ax, 23863			; 频率 50 Hz, 设置 COUNT 为 1193180 / 50 = 23863
	out		040h, al			; 将 COUNT 的低位写入通道 0
	call	io_delay

	mov		al, ah
	out		040h, al			; 将 COUNT 的高位写入通道 0
	call	io_delay

	ret
; END of Init8253A -------------------------------------------------------------

; io_delay ---------------------------------------------------------------------
io_delay:
	nop
	nop
	nop
	nop
	ret
; END of io_delay --------------------------------------------------------------

; int handler ------------------------------------------------------------------
_ClockHandler:
ClockHandler	equ	_ClockHandler - $$
	push	ebx							; ┓
	push	ds							; ┣ 保存原寄存器值
	pusha								; ┛

	mov		eax, SelectorData			; ┳ 设置 DS 以读取 dwCurrentTask
	mov		ds, ax						; ┛

	mov		al, EOI						; ┳ 发送 EOI
	out		INT_M_CTL, al				; ┛


	inc		dword [dwCurrentcnt]		; 增加当前任务的计数值

	cmp		dword [dwCurrentTask], 0	;
	je		.0							; ┃
	cmp		dword [dwCurrentTask], 1	; ┃
	je		.1							; ┣	判定当前任务
	cmp		dword [dwCurrentTask], 2	; ┃
	je		.2							; ┛

	mov		ebx, dword [PriorityTask3]
	cmp		dword [dwCurrentcnt], ebx
	jnz		exit
	mov		dword [dwCurrentTask], 0	
	mov		dword [dwCurrentcnt], 0
	jmp		SelectorTSS0:0
	jmp		exit
.0:
	mov		ebx, dword [PriorityTask0]
	cmp		dword [dwCurrentcnt], ebx
	jnz		exit
	mov		dword [dwCurrentTask], 1
	mov		dword [dwCurrentcnt], 0
	jmp		SelectorTSS1:0
	jmp		exit
.1:
	mov		ebx, dword [PriorityTask1]
	cmp		dword [dwCurrentcnt], ebx
	jnz		exit
	mov		dword [dwCurrentTask], 2
	mov		dword [dwCurrentcnt], 0
	jmp		SelectorTSS2:0
	jmp		exit
.2:
	mov		ebx, dword [PriorityTask2]
	cmp		dword [dwCurrentcnt], ebx
	jnz		exit
	mov		dword [dwCurrentTask], 3
	mov		dword [dwCurrentcnt], 0
	jmp		SelectorTSS3:0
exit:
	popa								; ┓	
	pop		ds							; ┣ 恢复原寄存器值
	pop		ebx							; ┛
	iretd



_SpuriousHandler:
SpuriousHandler	equ	_SpuriousHandler - $$
	; mov		ah, 0Ch							; 0000: 黑底    1100: 红字
	; mov		al, '!'
	; mov		[gs:((80 * 0 + 75) * 2)], ax	; 屏幕第 0 行, 第 75 列。
	iretd
; END of int handler -----------------------------------------------------------

; SetupPaging0 -----------------------------------------------------------------
SetupPaging0:
	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.
	; 首先初始化页目录
	mov		ax, SelectorFlatRW
	mov		es, ax
	mov		edi, PageDirBase0		; 此段首地址为 PageDirBase0
	xor		eax, eax
	mov		eax, PageTblBase0 | PG_P  | PG_USU | PG_RWW
	mov		ecx, [PageTableNumber]
.1:
	stosd							; [EAX] -> [EDI], EDI += 4
	add		eax, 4096				; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	mov		eax, [PageTableNumber]	; 页表个数
	mov		ebx, 1024				; 每个页表 1024 个 PTE
	mul		ebx
	mov		ecx, eax				; PTE个数 = 页表个数 * 1024
	mov		edi, PageTblBase0		; 此段首地址为 PageTblBase0
	xor		eax, eax
	mov		eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add		eax, 4096				; 每一页指向 4K 的空间
	loop	.2

	ret
; End of SetupPaging0 ----------------------------------------------------------

; SetupPaging1 -----------------------------------------------------------------
SetupPaging1:
	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.
	; 首先初始化页目录
	mov		ax, SelectorFlatRW
	mov		es, ax
	mov		edi, PageDirBase1		; 此段首地址为 PageDirBase1
	xor		eax, eax
	mov		eax, PageTblBase1 | PG_P  | PG_USU | PG_RWW
	mov		ecx, [PageTableNumber]
	;xchg bx,bx
.1:
	stosd							; [EAX] -> [EDI], EDI += 4
	add		eax, 4096				; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	;xchg bx,bx
	mov		eax, [PageTableNumber]	; 页表个数
	mov		ebx, 1024				; 每个页表 1024 个 PTE
	mul		ebx
	mov		ecx, eax				; PTE个数 = 页表个数 * 1024
	mov		edi, PageTblBase1		; 此段首地址为 PageTblBase1
	xor		eax, eax
	mov		eax, PG_P  | PG_USU | PG_RWW
	;xchg bx,bx
.2:
	stosd
	add		eax, 4096				; 每一页指向 4K 的空间
	loop	.2

	;xchg bx,bx
	ret
; End of SetupPaging1 ----------------------------------------------------------

; SetupPaging2 -----------------------------------------------------------------
SetupPaging2:
	; 为简化处理, 所有线性地址对应相等的物理地址. 并且不考虑内存空洞.
	; 首先初始化页目录
	mov		ax, SelectorFlatRW
	mov		es, ax
	mov		edi, PageDirBase2		; 此段首地址为 PageDirBase2
	xor		eax, eax
	mov		eax, PageTblBase2 | PG_P  | PG_USU | PG_RWW
	mov		ecx, [PageTableNumber]
.1:
	stosd							; [EAX] -> [EDI], EDI += 4
	add		eax, 4096				; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	mov		eax, [PageTableNumber]	; 页表个数
	mov		ebx, 1024				; 每个页表 1024 个 PTE
	mul		ebx
	mov		ecx, eax				; PTE个数 = 页表个数 * 1024
	mov		edi, PageTblBase2		; 此段首地址为 PageTblBase2
	xor		eax, eax
	mov		eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add		eax, 4096				; 每一页指向 4K 的空间
	loop	.2

	ret
; End of SetupPaging2 ----------------------------------------------------------

; SetupPaging3 -----------------------------------------------------------------
SetupPaging3:
	; 初始化页目录
	mov		ax, SelectorFlatRW
	mov		es, ax
	mov		edi, PageDirBase3		; 此段首地址为 PageDirBase3
	xor		eax, eax
	mov		eax, PageTblBase3 | PG_P  | PG_USU | PG_RWW
	mov		ecx, [PageTableNumber]
.1:
	stosd
	add		eax, 4096				; 为了简化, 所有页表在内存中是连续的.
	loop	.1

	; 再初始化所有页表
	mov		eax, [PageTableNumber]	; 页表个数
	mov		ebx, 1024				; 每个页表 1024 个 PTE
	mul		ebx
	mov		ecx, eax				; PTE个数 = 页表个数 * 1024
	mov		edi, PageTblBase3		; 此段首地址为 PageTblBase3
	xor		eax, eax
	mov		eax, PG_P  | PG_USU | PG_RWW
.2:
	stosd
	add		eax, 4096				; 每一页指向 4K 的空间
	loop	.2

	ret
; End of SetupPaging3 ----------------------------------------------------------

; DispMemSize ------------------------------------------------------------------
DispMemSize:
	push	esi
	push	edi
	push	ecx

	mov		esi, MemChkBuf
	mov		ecx, [dwMCRNumber]		;for(int i = 0; i < [MCRNumber]; i++) // 每次得到一个ARDS(Address Range Descriptor Structure)结构
.loop:								;{
	mov		edx, 5					;	for(int j = 0; j < 5; j++)	// 每次得到一个ARDS中的成员，共5个成员
	mov		edi, ARDStruct			;	{
.1:									;		// 依次显示：BaseAddrLow，BaseAddrHigh，LengthLow，LengthHigh，Type
	push	dword [esi]				;
	call	DispInt					;		DispInt(MemChkBuf[j * 4]); // 显示一个成员
	pop		eax						;
	stosd							;		ARDStruct[j * 4] = MemChkBuf[j * 4];
	add		esi, 4					;
	dec		edx						;
	cmp		edx, 0					;
	jnz		.1						;	}
	call	DispReturn				;	printf("\n");
	cmp		dword [dwType], 1		;	if(Type == AddressRangeMemory) // AddressRangeMemory : 1, AddressRangeReserved : 2
	jne		.2						;	{
	mov		eax, [dwBaseAddrLow]	;
	add		eax, [dwLengthLow]		;
	cmp		eax, [dwMemSize]		;		if(BaseAddrLow + LengthLow > MemSize)
	jb		.2						;
	mov		[dwMemSize], eax		;			MemSize = BaseAddrLow + LengthLow;
.2:									;	}
	loop	.loop					;}
									;
	call	DispReturn				;printf("\n");
	push	szRAMSize				;
	mov		ah,63h
	call	DispStr					;printf("RAM size:");
	add		esp, 4					;
									;
	push	dword [dwMemSize]		;
	call	DispInt					;DispInt(MemSize);
	add		esp, 4					;

	pop		ecx
	pop		edi
	pop		esi
	ret
; End of DispMemSize -----------------------------------------------------------

%include	"lib.inc"	; 库函数
SegCode32Len	equ	$ - LABEL_SEG_CODE32
; END of [SECTION .s32]


; 16 位代码段, 由 32 位代码段跳入, 跳出后到实模式.
[SECTION .s16code]
ALIGN	32
[BITS	16]
LABEL_SEG_CODE16:
	; 跳回实模式:
	mov		ax, SelectorNormal
	mov		ds, ax
	mov		es, ax
	mov		fs, ax
	mov		gs, ax
	mov		ss, ax

	mov		eax, cr0
	; and		al, 11111110b	; 仅切换到实模式
	and		eax, 7ffffffeh		; 切换到实模式并关闭分页
	mov		cr0, eax

LABEL_GO_BACK_TO_REAL:
	jmp		0:LABEL_REAL_ENTRY	; 段地址会在程序开始处被设置成正确的值

Code16Len	equ	$ - LABEL_SEG_CODE16
; END of [SECTION .s16code]





; ==============================任务0================================
; 功能：显示字符串Peter
; ==================================================================
; 任务0的LDT部分
[SECTION .ldt0]
ALIGN	32
LABEL_LDT0:
;                                          段基址,            段界限,  属性
LABEL_DESC_TASK0_DATA:		Descriptor	       0, Task0DataLen - 1, DA_DRWA				 		; 数据段, 32位, 基址待设置
LABEL_DESC_TASK0_CODE:		Descriptor	       0, Task0CodeLen - 1, DA_C + DA_32				; 代码段, 32位, 基址待设置
LABEL_DESC_TASK0_CODE3:		Descriptor	       0, Task0Code3Len - 1, DA_C + DA_32 + DA_DPL3		; 代码段, 32位, ring3, 基址待设置
LABEL_DESC_TASK0_STACK0:	Descriptor	       0, TopOfTask0Stack0, DA_DRWA + DA_32			 	; 堆栈段, 32位，ring0, 基址待设置
LABEL_DESC_TASK0_STACK3:	Descriptor	       0, TopOfTask0Stack3, DA_DRWA + DA_32 + DA_DPL3	; 堆栈段, 32位，ring3, 基址待设置
; 门                                            目标选择子,       偏移, DCount, 属性
LABEL_DESC_CALL_GATE0:		Gate		SelectorTask0Code,	LABEL_T0_CODE_END - LABEL_T0_CODE,	0,	DA_386CGate + DA_DPL3
LDTLen0	equ	$ - LABEL_LDT0		; LDT 的大小
; LDT 选择子
SelectorTask0Data		equ LABEL_DESC_TASK0_DATA	- LABEL_LDT0 + SA_TIL				; 数据段选择子
SelectorTask0Code		equ LABEL_DESC_TASK0_CODE	- LABEL_LDT0 + SA_TIL				; 代码段选择子
SelectorTask0Code3		equ LABEL_DESC_TASK0_CODE3	- LABEL_LDT0 + SA_TIL + SA_RPL3		; ring3 代码段选择子
SelectorTask0Stack0		equ	LABEL_DESC_TASK0_STACK0	- LABEL_LDT0 + SA_TIL				; ring0 堆栈段选择子
SelectorTask0Stack3		equ	LABEL_DESC_TASK0_STACK3	- LABEL_LDT0 + SA_TIL + SA_RPL3 	; ring3 堆栈段选择子
SelectorCallGate0		equ	LABEL_DESC_CALL_GATE0	- LABEL_LDT0 + SA_TIL + SA_RPL3		; 调用门选择子
; END of [SECTION .ldt0]
; ==================================================================
; 任务0的TSS部分
[SECTION .tss0]
ALIGN	32
[BITS	32]
LABEL_TSS0:
		DD	0               	; Back
		DD	TopOfTask0Stack0	; 0 级堆栈
		DD	SelectorTask0Stack0	; 
		DD	0					; 1 级堆栈
		DD	0					; 
		DD	0					; 2 级堆栈
		DD	0					;
		DD	PageDirBase0		; CR3
		DD	0 					; EIP
		DD	0x200				; EFLAGS
		DD	0					; EAX
		DD	0					; ECX
		DD	0					; EDX
		DD	0					; EBX
		DD	TopOfTask0Stack0	; ESP
		DD	0					; EBP
		DD	0					; ESI
		DD	0					; EDI
		DD	0					; ES
		DD	SelectorTask0Code	; CS
		DD	SelectorTask0Stack0	; SS
		DD	SelectorTask0Data	; DS
		DD	0					; FS
		DD	SelectorVideo		; GS
		DD	SelectorLDT0		; LDT
		DW	0					; 调试陷阱标志
		DW	$ - LABEL_TSS0 + 2	; I/O位图基址
		DB	0ffh				; I/O位图结束标志
TSSLen0		equ	$ - LABEL_TSS0	; TSS0 的大小
; END of [SECTION .tss0]
; ==================================================================
; 任务0的数据段部分
[SECTION .t0data]
ALIGN	32
[BITS	32]
LABEL_T0_DATA:
_szTask0Message:  db	0Ah,0Ah,0Ah
szTask0Message	equ _szTask0Message - $$
Task0DataLen	equ	$ - LABEL_T0_DATA
; END of [SECTION .t0data]
; ==================================================================
; 任务0的ring0堆栈段部分
[SECTION .t0r0s]
ALIGN	32
[BITS	32]
LABEL_T0_STACK0:
	times 512 db 0
TopOfTask0Stack0	equ	$ - LABEL_T0_STACK0 - 1
; END of [SECTION .t0r0s]
; ==================================================================
; 任务0的ring3堆栈段部分
[SECTION .t0r3s]
ALIGN	32
[BITS	32]
LABEL_T0_STACK3:
	times 512 db 0
TopOfTask0Stack3	equ	$ - LABEL_T0_STACK3 - 1
; END of [SECTION .t0r3s]
; ==================================================================
; 任务0的ring0代码段
[SECTION .t0c32]
ALIGN	32
[BITS	32]
LABEL_T0_CODE:
	push	SelectorTask0Stack3
	push	TopOfTask0Stack3
	push	SelectorTask0Code3
	push	0
	retf	
LABEL_T0_CODE_END:
	mov 	ecx, 0xFFFF
.delay0:
	loop	.delay0
	jmp		LABEL_T0_CODE
Task0CodeLen	equ	$ - LABEL_T0_CODE
; END of [SECTION .t0c32]
; ==================================================================
; 任务0的Ring3代码段
[SECTION .t0c32_3]
ALIGN	32
[BITS	32]
LABEL_T0_CODE3:
	push	eax
	mov		ax, SelectorVideo
	mov		gs, ax
	mov		ah, 24h							; 绿底红字
	mov		al, 'P'
	mov		[gs:((80 * 20 + 0) * 2)], ax	; 屏幕第 20 行, 第 0 列。
	mov		al, 'e'
	mov		[gs:((80 * 20 + 1) * 2)], ax	; 屏幕第 20 行, 第 1 列。
	mov		al, 't'
	mov		[gs:((80 * 20 + 2) * 2)], ax	; 屏幕第 20 行, 第 2 列。
	mov		al, 'e'
	mov		[gs:((80 * 20 + 3) * 2)], ax	; 屏幕第 20 行, 第 3 列。
	mov		al, 'r'
	mov		[gs:((80 * 20 + 4) * 2)], ax	; 屏幕第 20 行, 第 4 列。	
	pop		eax
	call	SelectorCallGate0:0
	jmp		$
Task0Code3Len	equ	$ - LABEL_T0_CODE3
; END of [SECTION .t0c32_3]
; ==================================================================



; ==============================任务1================================
; 功能：显示字符串Mouse
; ==================================================================
; 任务1的LDT部分
[SECTION .ldt1]
ALIGN	32
LABEL_LDT1:
;                                          段基址,            段界限,  属性
LABEL_DESC_TASK1_DATA:		Descriptor	       0, Task1DataLen - 1, DA_DRWA				 		; 数据段, 32位, 基址待设置
LABEL_DESC_TASK1_CODE:		Descriptor	       0, Task1CodeLen - 1, DA_C + DA_32				; 代码段, 32位, 基址待设置
LABEL_DESC_TASK1_CODE3:		Descriptor	       0, Task1Code3Len - 1, DA_C + DA_32 + DA_DPL3		; 代码段, 32位, ring3, 基址待设置
LABEL_DESC_TASK1_STACK0:	Descriptor	       0, TopOfTask1Stack0, DA_DRWA + DA_32			 	; 堆栈段, 32位，ring0, 基址待设置
LABEL_DESC_TASK1_STACK3:	Descriptor	       0, TopOfTask1Stack3, DA_DRWA + DA_32 + DA_DPL3	; 堆栈段, 32位，ring3, 基址待设置
; 门                                            目标选择子,       偏移, DCount, 属性
LABEL_DESC_CALL_GATE1:		Gate		SelectorTask1Code,	LABEL_T1_CODE_END - LABEL_T1_CODE,	0,	DA_386CGate + DA_DPL3	; 任务 0 的调用门

LDTLen1	equ	$ - LABEL_LDT1           ; LDT1 的大小
; LDT 选择子
SelectorTask1Data		equ LABEL_DESC_TASK1_DATA	- LABEL_LDT1 + SA_TIL				; 数据段选择子
SelectorTask1Code		equ LABEL_DESC_TASK1_CODE	- LABEL_LDT1 + SA_TIL				; 代码段选择子
SelectorTask1Code3		equ LABEL_DESC_TASK1_CODE3	- LABEL_LDT1 + SA_TIL + SA_RPL3		; ring3 代码段选择子
SelectorTask1Stack0		equ	LABEL_DESC_TASK1_STACK0	- LABEL_LDT1 + SA_TIL				; ring0 堆栈段选择子
SelectorTask1Stack3		equ	LABEL_DESC_TASK1_STACK3	- LABEL_LDT1 + SA_TIL + SA_RPL3 	; ring3 堆栈段选择子
SelectorCallGate1		equ	LABEL_DESC_CALL_GATE1	- LABEL_LDT1 + SA_TIL + SA_RPL3		; 调用门选择子
; END of [SECTION .ldt1]
; ==================================================================
; 任务1的TSS部分
[SECTION .tss1]
ALIGN	32
[BITS	32]
LABEL_TSS1:
		DD	0               	; Back
		DD	TopOfTask1Stack0	; 0 级堆栈
		DD	SelectorTask1Stack0	; 
		DD	0					; 1 级堆栈
		DD	0					; 
		DD	0					; 2 级堆栈
		DD	0					;
		DD	PageDirBase1		; CR3
		DD	0					; EIP
		DD	0x200				; EFLAGS
		DD	0					; EAX
		DD	0					; ECX
		DD	0					; EDX
		DD	0					; EBX
		DD	TopOfTask1Stack0	; ESP
		DD	0					; EBP
		DD	0					; ESI
		DD	0					; EDI
		DD	0					; ES
		DD	SelectorTask1Code	; CS
		DD	SelectorTask1Stack0	; SS
		DD	SelectorTask1Data	; DS
		DD	0					; FS
		DD	SelectorVideo		; GS
		DD	SelectorLDT1		; LDT
		DW	0					; 调试陷阱标志
		DW	$ - LABEL_TSS1 + 2	; I/O位图基址
		DB	0ffh				; I/O位图结束标志
TSSLen1		equ	$ - LABEL_TSS1	; TSS1 的大小
; END of [SECTION .tss1]
; ==================================================================
; 任务1的数据段部分
[SECTION .t1data]
ALIGN	32
[BITS	32]
LABEL_T1_DATA:
_szTask1Message:	db 0Ah,0Ah,0Ah
szTask1Message	equ _szTask1Message - $$
Task1DataLen	equ	$ - LABEL_T1_DATA
; END of [SECTION .t1data]
; ==================================================================
; 任务1的ring0堆栈段部分
[SECTION .t1r0s]
ALIGN	32
[BITS	32]
LABEL_T1_STACK0:
	times 512 db 0
TopOfTask1Stack0	equ	$ - LABEL_T1_STACK0 - 1
; END of [SECTION .t1r0s]
; ==================================================================
; 任务1的ring3堆栈段部分
[SECTION .t1r3s]
ALIGN	32
[BITS	32]
LABEL_T1_STACK3:
	times 512 db 0
TopOfTask1Stack3	equ	$ - LABEL_T1_STACK3 - 1
; END of [SECTION .t1r3s]
; ==================================================================
; 任务1的ring0代码段部分
[SECTION .t1c32]
ALIGN	32
[BITS	32]
LABEL_T1_CODE:
	; xchg	bx,bx
	push	SelectorTask1Stack3
	push	TopOfTask1Stack3
	push	SelectorTask1Code3
	; xchg	bx,bx
	push	0
	retf
LABEL_T1_CODE_END:	
	mov 	ecx, 0xFFF
.delay1:
	loop	.delay1
	jmp		LABEL_T1_CODE
Task1CodeLen	equ	$ - LABEL_T1_CODE
; END of [SECTION .t1c32]
; ==================================================================
; 任务1的ring3代码段部分
[SECTION .t1c32_3]
ALIGN	32
[BITS	32]
LABEL_T1_CODE3:
	push	eax
	; xchg	bx,bx
	mov		ax, SelectorVideo
	mov		gs, ax
	mov		ah, 71h							; 白底蓝字
	mov		al, 'M'
	mov		[gs:((80 * 20 + 0) * 2)], ax	; 屏幕第 20 行, 第 0 列。
	mov		al, 'o'
	mov		[gs:((80 * 20 + 1) * 2)], ax	; 屏幕第 20 行, 第 1 列。
	mov		al, 'u'
	mov		[gs:((80 * 20 + 2) * 2)], ax	; 屏幕第 20 行, 第 2 列。
	mov		al, 's'
	mov		[gs:((80 * 20 + 3) * 2)], ax	; 屏幕第 20 行, 第 3 列。
	mov		al, 'e'
	mov		[gs:((80 * 20 + 4) * 2)], ax	; 屏幕第 20 行, 第 4 列。
	pop		eax
	; xchg 	bx,bx
	call	SelectorCallGate1:0
	jmp		$
Task1Code3Len	equ	$ - LABEL_T1_CODE3
; END of [SECTION .t1c32_3]
; ==================================================================


; ==============================任务2================================
; 功能：显示字符串Loves
; ==================================================================
; 任务2的LDT部分
[SECTION .ldt2]
ALIGN	32
LABEL_LDT2:
;                                          段基址,            段界限,  属性
LABEL_DESC_TASK2_DATA:		Descriptor	       0, Task2DataLen - 1, DA_DRWA				 		; 数据段, 32位, 基址待设置
LABEL_DESC_TASK2_CODE:		Descriptor	       0, Task2CodeLen - 1, DA_C + DA_32				; 代码段, 32位, 基址待设置
LABEL_DESC_TASK2_CODE3:		Descriptor	       0, Task2Code3Len - 1, DA_C + DA_32 + DA_DPL3		; 代码段, 32位, ring3, 基址待设置
LABEL_DESC_TASK2_STACK0:	Descriptor	       0, TopOfTask2Stack0, DA_DRWA + DA_32			 	; 堆栈段, 32位，ring0, 基址待设置
LABEL_DESC_TASK2_STACK3:	Descriptor	       0, TopOfTask2Stack3, DA_DRWA + DA_32 + DA_DPL3	; 堆栈段, 32位，ring3, 基址待设置
; 门                                            目标选择子,       偏移, DCount, 属性
LABEL_DESC_CALL_GATE2:		Gate		SelectorTask2Code,	LABEL_T2_CODE_END - LABEL_T2_CODE,	0,	DA_386CGate + DA_DPL3	; 任务 0 的调用门
LDTLen2	equ	$ - LABEL_LDT2           ; LDT2 的大小
; LDT 选择子
SelectorTask2Data		equ LABEL_DESC_TASK2_DATA	- LABEL_LDT2 + SA_TIL				; 数据段选择子
SelectorTask2Code		equ LABEL_DESC_TASK2_CODE	- LABEL_LDT2 + SA_TIL				; 代码段选择子
SelectorTask2Code3		equ LABEL_DESC_TASK2_CODE3	- LABEL_LDT2 + SA_TIL + SA_RPL3		; ring3 代码段选择子
SelectorTask2Stack0		equ	LABEL_DESC_TASK2_STACK0	- LABEL_LDT2 + SA_TIL				; ring0 堆栈段选择子
SelectorTask2Stack3		equ	LABEL_DESC_TASK2_STACK3	- LABEL_LDT2 + SA_TIL + SA_RPL3 	; ring3 堆栈段选择子
SelectorCallGate2		equ	LABEL_DESC_CALL_GATE2	- LABEL_LDT2 + SA_TIL + SA_RPL3		; 调用门选择子
; END of [SECTION .ldt2]
; ==================================================================
; 任务2的TSS部分
[SECTION .tss2]
ALIGN	32
[BITS	32]
LABEL_TSS2:
		DD	0               	; Back
		DD	TopOfTask2Stack0	; 0 级堆栈
		DD	SelectorTask2Stack0	; 
		DD	0					; 1 级堆栈
		DD	0					; 
		DD	0					; 2 级堆栈
		DD	0					;
		DD	PageDirBase2		; CR3
		DD	0					; EIP
		DD	0x200				; EFLAGS
		DD	0					; EAX
		DD	0					; ECX
		DD	0					; EDX
		DD	0					; EBX
		DD	TopOfTask2Stack0	; ESP
		DD	0					; EBP
		DD	0					; ESI
		DD	0					; EDI
		DD	0					; ES
		DD	SelectorTask2Code	; CS
		DD	SelectorTask2Stack0	; SS
		DD	SelectorTask2Data	; DS
		DD	0					; FS
		DD	SelectorVideo		; GS
		DD	SelectorLDT2		; LDT
		DW	0					; 调试陷阱标志
		DW	$ - LABEL_TSS2 + 2	; I/O位图基址
		DB	0ffh				; I/O位图结束标志
TSSLen2		equ	$ - LABEL_TSS2	; TSS2 的大小
; END of [SECTION .tss2]
; ==================================================================
; 任务2的数据段部分
[SECTION .t2data]
ALIGN	32
[BITS	32]
LABEL_T2_DATA:
_szTask2Message:	db	0Ah,0Ah,0Ah
szTask2Message	equ _szTask2Message - $$
Task2DataLen	equ	$ - LABEL_T2_DATA
; END of [SECTION .t2data]
; ==================================================================
; 任务2的ring0堆栈段部分
[SECTION .t2r0s]
ALIGN	32
[BITS	32]
LABEL_T2_STACK0:
	times 512 db 0
TopOfTask2Stack0	equ	$ - LABEL_T2_STACK0 - 1
; END of [SECTION .t2r0s]
; ==================================================================
; 任务2的ring3堆栈段部分
[SECTION .t2r3s]
ALIGN	32
[BITS	32]
LABEL_T2_STACK3:
	times 512 db 0
TopOfTask2Stack3	equ	$ - LABEL_T2_STACK3 - 1
; END of [SECTION .t2r3s]
; ==================================================================
; 任务2的ring0代码段部分
[SECTION .t2c32]
ALIGN	32
[BITS	32]
LABEL_T2_CODE:
	push	SelectorTask2Stack3
	push	TopOfTask2Stack3
	push	SelectorTask2Code3
	push	0
	retf
LABEL_T2_CODE_END:
	mov 	ecx, 0xFFF
.delay2:
	loop	.delay2
	jmp		LABEL_T2_CODE
Task2CodeLen	equ	$ - LABEL_T2_CODE
; END of [SECTION .t2c32]
; ==================================================================
; 任务2的ring3代码段部分
[SECTION .t2c32_3]
ALIGN	32
[BITS	32]
LABEL_T2_CODE3:
	push	eax
	mov		ax, SelectorVideo
	mov		gs, ax
	mov		ah, 4ah							; 红底绿字
	mov		al, 'L'
	mov		[gs:((80 * 20 + 0) * 2)], ax	; 屏幕第 20 行, 第 0 列。
	mov		al, 'o'
	mov		[gs:((80 * 20 + 1) * 2)], ax	; 屏幕第 20 行, 第 1 列。
	mov		al, 'v'
	mov		[gs:((80 * 20 + 2) * 2)], ax	; 屏幕第 20 行, 第 2 列。
	mov		al, 'e'
	mov		[gs:((80 * 20 + 3) * 2)], ax	; 屏幕第 20 行, 第 3 列。
	mov		al, 's'
	mov		[gs:((80 * 20 + 4) * 2)], ax	; 屏幕第 20 行, 第 4 列。
	pop		eax
	call	SelectorCallGate2:0
	jmp		$
Task2Code3Len	equ	$ - LABEL_T2_CODE3
; END of [SECTION .t2c32_3]
; ==================================================================


; ==============================任务3================================
; 功能：显示字符串Study
; ==================================================================
; 任务3的LDT部分
[SECTION .ldt3]
ALIGN	32
LABEL_LDT3:
;                                          段基址,            段界限,  属性
LABEL_DESC_TASK3_DATA:		Descriptor	       0, Task3DataLen - 1, DA_DRWA				 		; 数据段, 32位, 基址待设置
LABEL_DESC_TASK3_CODE:		Descriptor	       0, Task3CodeLen - 1, DA_C + DA_32				; 代码段, 32位, 基址待设置
LABEL_DESC_TASK3_CODE3:		Descriptor	       0, Task3Code3Len - 1, DA_C + DA_32 + DA_DPL3		; 代码段, 32位, ring3, 基址待设置
LABEL_DESC_TASK3_STACK0:	Descriptor	       0, TopOfTask3Stack0, DA_DRWA + DA_32			 	; 堆栈段, 32位，ring0, 基址待设置
LABEL_DESC_TASK3_STACK3:	Descriptor	       0, TopOfTask3Stack3, DA_DRWA + DA_32 + DA_DPL3	; 堆栈段, 32位，ring3, 基址待设置
; 门                                            目标选择子,       偏移, DCount, 属性
LABEL_DESC_CALL_GATE3:		Gate		SelectorTask3Code,	LABEL_T3_CODE_END - LABEL_T3_CODE,	0,	DA_386CGate + DA_DPL3	; 任务 0 的调用门
LDTLen3 equ	$ - LABEL_LDT3           ; LDT3 的大小
; LDT 选择子
SelectorTask3Data		equ LABEL_DESC_TASK3_DATA	- LABEL_LDT3 + SA_TIL				; 数据段选择子
SelectorTask3Code		equ LABEL_DESC_TASK3_CODE	- LABEL_LDT3 + SA_TIL				; 代码段选择子
SelectorTask3Code3		equ LABEL_DESC_TASK3_CODE3	- LABEL_LDT3 + SA_TIL + SA_RPL3		; ring3 代码段选择子
SelectorTask3Stack0		equ	LABEL_DESC_TASK3_STACK0	- LABEL_LDT3 + SA_TIL				; ring0 堆栈段选择子
SelectorTask3Stack3		equ	LABEL_DESC_TASK3_STACK3	- LABEL_LDT3 + SA_TIL + SA_RPL3 	; ring3 堆栈段选择子
SelectorCallGate3		equ	LABEL_DESC_CALL_GATE3	- LABEL_LDT3 + SA_TIL + SA_RPL3		; 调用门选择子
; END of [SECTION .ldt3]
; ==================================================================
; 任务3的TSS部分
[SECTION .tss3]
ALIGN	32
[BITS	32]
LABEL_TSS3:
		DD	0               	; Back
		DD	TopOfTask3Stack0	; 0 级堆栈
		DD	SelectorTask3Stack0	; 
		DD	0					; 1 级堆栈
		DD	0					; 
		DD	0					; 2 级堆栈
		DD	0					;
		DD	PageDirBase3		; CR3
		DD	0					; EIP
		DD	0x200				; EFLAGS
		DD	0					; EAX
		DD	0					; ECX
		DD	0					; EDX
		DD	0					; EBX
		DD	TopOfTask3Stack0	; ESP
		DD	0					; EBP
		DD	0					; ESI
		DD	0					; EDI
		DD	0					; ES
		DD	SelectorTask3Code	; CS
		DD	SelectorTask3Stack0	; SS
		DD	SelectorTask3Data	; DS
		DD	0					; FS
		DD	SelectorVideo		; GS
		DD	SelectorLDT3		; LDT
		DW	0					; 调试陷阱标志
		DW	$ - LABEL_TSS3 + 2	; I/O位图基址
		DB	0ffh				; I/O位图结束标志
TSSLen3		equ	$ - LABEL_TSS3	; TSS3 的大小
; END of [SECTION .tss3]
; ==================================================================
; 任务3的数据段部分
[SECTION .t3data]
ALIGN	32
[BITS	32]
LABEL_T3_DATA:
_szTask3Message:	db	0Ah,0Ah,0Ah
szTask3Message	equ _szTask3Message - $$
Task3DataLen	equ	$ - LABEL_T3_DATA
; END of [SECTION .t3data]
; ==================================================================
; 任务3的ring0堆栈段部分
[SECTION .t3r0s]
ALIGN	32
[BITS	32]
LABEL_T3_STACK0:
	times 512 db 0
TopOfTask3Stack0	equ	$ - LABEL_T3_STACK0 - 1
; END of [SECTION .t3r0s]
; ==================================================================
; 任务3的ring3堆栈段部分
[SECTION .t3r3s]
ALIGN	32
[BITS	32]
LABEL_T3_STACK3:
	times 512 db 0
TopOfTask3Stack3	equ	$ - LABEL_T3_STACK3 - 1
; END of [SECTION .t3r3s]
; ==================================================================
; 任务3的ring0代码段部分
[SECTION .t3c32]
ALIGN	32
[BITS	32]
LABEL_T3_CODE:
	push	SelectorTask0Stack3
	push	TopOfTask0Stack3
	push	SelectorTask0Code3
	push	0
	retf
LABEL_T3_CODE_END:
	mov 	ecx, 0xFFF
.delay3:
	loop	.delay3
	jmp		LABEL_T3_CODE
Task3CodeLen	equ	$ - LABEL_T3_CODE
; END of [SECTION .t3c32]
; ==================================================================
; 任务3的ring3代码段部分
[SECTION .t3c32_3]
ALIGN	32
[BITS	32]
LABEL_T3_CODE3:
	push	eax
	mov		ax, SelectorVideo
	mov		gs, ax
	mov		ah, 36h							; 青底棕字
	mov		al, 'S'
	mov		[gs:((80 * 20 + 0) * 2)], ax	; 屏幕第 20 行, 第 0 列。
	mov		al, 't'
	mov		[gs:((80 * 20 + 1) * 2)], ax	; 屏幕第 20 行, 第 1 列。
	mov		al, 'u'
	mov		[gs:((80 * 20 + 2) * 2)], ax	; 屏幕第 20 行, 第 2 列。
	mov		al, 'd'
	mov		[gs:((80 * 20 + 3) * 2)], ax	; 屏幕第 20 行, 第 3 列。
	mov		al, 'y'
	mov		[gs:((80 * 20 + 4) * 2)], ax	; 屏幕第 20 行, 第 4 列。
	pop		eax
	call	SelectorCallGate3:0
	jmp		$
Task3Code3Len	equ	$ - LABEL_T3_CODE3
; END of [SECTION .t3c32_3]
; ==================================================================

