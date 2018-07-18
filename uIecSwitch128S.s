; -----------------------------------------------------------
; uIecSwitchS1: switch disk images on a uIEC (GUI, event handlers)
; revision history:	
;		1.0, initial release
;		1.1, fix to run on sd2iec
;		1.2, bug fix for uIEC detection, usability fixes
;
; Written by Glenn Holmer (a.k.a "Shadow", a.k.a "Cenbe")
; GEOS 128 support added by Alex Burger
; -----------------------------------------------------------
	.forceimport	__STARTUP__
	.export		_main
	.include	"uIecSwitch.inc"
	.include	"cbm.inc"
	.include	"geosmac.inc"
	.include	"geossym.inc"
	.include	"geossym2.inc"
	.include	"jumptab.inc"
	.include	"const.inc"
; -----------------------------------------------------------

.segment	"STARTUP"

.proc	_main: near

.segment	"STARTUP"

start:	
	jsr Check128
	bmi @3
	LoadW r0,GEOS128Dlg	; GEOS 128 not detected, whine 'n' bail
	jsr	DoDlgBox
	jmp	EnterDeskTop
@3:	
	lda	#2	;50% stipple
	jsr	SetPattern
	LoadB	r2L,0
	LoadB	r2H,199
	LoadW	r3,0
	LoadW	r4,(319|DOUBLE_W)
	jsr	Rectangle	;clear screen
	jsr	findUiec
	bcs	@10				; no uIEC present
	bit graphMode		; Check if 40 or 80 columns
	bpl @5				; 40 column mode
	; 80 column mode
	MoveW mainMenu80+4, menuWidthW
	LoadW	r0,mainMenu80
	jmp @6
@5:	; 40 column mode
	MoveW mainMenu+4, menuWidthW
	LoadW	r0,mainMenu
@6:
	lda	#0	;cursor on item #0
	jsr	DoMenu
	jsr	titleBar
	jsr	readDir
	jsr	doGui
	rts		;to MainLoop
@10:	
	LoadW	r0,uIecDlg	;no uIEC present, whine 'n' bail
	jsr	DoDlgBox
	lda	savDrive
	sta	curDrive	;restore original drive
	jsr	OpenDisk
	jmp	EnterDeskTop
	
.endproc
	
; -----------------------------------------------------------
; draw title bar
; -----------------------------------------------------------
titleBar:	
	; ----------------------
	; Main title bar box
	; ----------------------
	lda	#9	;horizontal stripes
	jsr	SetPattern
	LoadB	r2L,0
	LoadB	r2H,14
	MoveW	menuWidthW,r3
	AddVW 1, r3
	LoadW	r4,(319|DOUBLE_W)
	jsr	Rectangle
	; -------------------------------------------
	; Clear title text area based on size of text
	; Top / bottom of rectangle already set above (r2L, r2H)
	; -------------------------------------------
	LoadW	r0,title
	jsr	strWidth	;returns string width in a0
	LoadB	r1H,9

	bit graphMode		; Check if 40 or 80 columns
	bpl @5				; 40 column mode
	; 80 column mode:
	LoadW	r11,640		; (320|DOUBLE_W) does not work for some reason....
	jmp @6
@5:
	LoadW	r11,(320|DOUBLE_W)	; Need DOUBLE_W for 40 column mode to work!
@6:

	SubW	menuWidthW,r11	;320 - menu width
	SubW	a0,r11	;minus string width
	
	; Divide width by 2
	clc
	lda r11H
	asl
	ror	r11H
	ror	r11L	;divided by two

	AddW	menuWidthW,r11	;plus menu width (i.e. right edge)
	PushW	r11	;trashed by Rectangle
	lda	#0	;clear
	jsr	SetPattern
	LoadB	r2L,1
	LoadB	r2H,13
	MoveW	r11,r3
	lda	r3L
	clc
	adc	a0L
	sta	r4L
	lda	r3H
	adc	a0H
	sta	r4H
	jsr	Rectangle	;clear title text area		r2L top, r2H bot, r3 left, r4 right
	; ----------------
	; Write title text
	; ----------------
	PopW	r11						; x position for string.  0-319
	PushW	rightMargin
	LoadW	rightMargin,(318|DOUBLE_W)
	PushB	windowTop
	LoadB	windowTop,0
	jsr	PutString
	PopB	windowTop
	PopW	rightMargin
	rts
; -----------------------------------------------------------
doInfo:	
	jsr	GotoFirstMenu
	LoadW	r0,infoDlg	;geos/info menu handler
	jsr	DoDlgBox
	rts
; -----------------------------------------------------------
doQuit:	
	jsr	GotoFirstMenu	;file/quit menu handler
	LoadW	r0,gp
	jsr	sendCmd
	ldx	msg	;byte 0 (partition type)
	dex		;translate to GEOS type
	txa
	ldy	drvUiec
	cmp	driveType-8,y
	beq	@10
	LoadW	r0,typeDlg	;wrong image type for device
	jsr	DoDlgBox
	lda	r0L
	cmp	#YES	;confirm exit?
	beq	@10
	rts		;no, back to MainLoop
@10:
	lda	savDrive
	sta	curDrive	;restore original drive
	jsr	OpenDisk
	jmp	EnterDeskTop
; -----------------------------------------------------------
; Handler for click on "Parent" button.
; -----------------------------------------------------------
doParent:	LoadW	r0,cdParent
	jsr	sendCmd
	lda	msg
	cmp	#'2'
	bcc	@30
	cmp	#'6'
	bne	@10
	lda	msg+1
	cmp	#'2'	;62, file not found?
	bne	@10
	LoadW	errorMsg, noParent
	bra	@20
@10:
	LoadW	errorMsg,msg
@20:
	LoadW	r0,errorDlg
	jsr	DoDlgBox	;fall thru, try to re-read directory
@30:
	jsr	readDir
	jsr	fillList
	rts
; -----------------------------------------------------------
; Handler for click on "Open" button.
; -----------------------------------------------------------
doOpen:	
	lda	selName
	bpl	@10	;anything selected?
	LoadW	errorMsg,noSelect
	LoadW	r0,errorDlg
	jsr	DoDlgBox
	rts
@10:
	clc
	adc	topName
	sta	a8L
	lda	#0
	adc	topName+1
	sta	a8H
	jsr	a8Name
	ldy	#0
	lda	(a8),y
	iny
	cmp	#$86	;subdirectory?
	beq	@20
	jsr	isImage
	bcs	@15
	LoadW	errorMsg,noEntry
	LoadW	r0,errorDlg
	jsr	DoDlgBox
	rts
@15:
	LoadW	a9,cdImage+2
	bra	@30
@20:
	LoadW	a9,chDir+2
@30:
	lda	(a8),y
	cmp	#$a0	;first $a0 is end of filename
	beq	@40
	sta	(a9),y
	iny
	cpy	#17
	bne	@30
@40:
	lda	#0
	sta	(a9),y
	lda	a9L
	sec
	sbc	#2
	sta	r0L
	lda	a9H
	sbc	#0
	sta	r0H
	jsr	sendCmd
	lda	msg	
	cmp	#'0'
	beq	@50
	LoadW	errorMsg,msg
	LoadW	r0,errorDlg
	jsr	DoDlgBox
	rts	
@50:
	jsr	readDir
	jsr	fillList
	rts
; -----------------------------------------------------------
; Down button dispatch routine.
; -----------------------------------------------------------
doDown:	lda	topName	;can bottom name increment?
	clc
	adc	#MAX_LIST
	sta	a0L
	lda	topName+1
	adc	#0
	sta	a0H
	CmpW	a0L,fCount
	bcc	@10
	jsr	beep
	rts
@10:
	inc	topName
	bne	@20
	inc	topName+1
@20:
	jsr	fillList
	lda	selName	;anything selected?
	bmi	@40
	beq	@30	;first item?
	dec	selName	;no, follow item in list
	lda	selName
@30:
	jsr	invList
@40:
	rts
; -----------------------------------------------------------
; Up button dispatch routine.
; -----------------------------------------------------------
doUp:
	lda	topName+1	;already at top?
	bne	@10
	lda	topName
	bne	@10
	jsr	beep
	rts
@10:
	lda	topName
	sec
	sbc	#1
	sta	topName
	lda	topName+1
	sbc	#0
	sta	topName+1
	jsr	fillList
	lda	selName	;anything selected?
	bmi	@30
	cmp	#MAX_LIST-1	;last item?
	beq	@20
	inc	selName	;no, follow item in list
	lda	selName
@20:
	jsr	invList
@30:
	rts
; -----------------------------------------------------------
; Page down routine (called through otherPressVector).
; -----------------------------------------------------------
pageDown:	
	lda	topName
	clc
	adc	#MAX_LIST
	sta	a0L
	lda	topName+1
	adc	#0
	sta	a0H
	CmpW	a0L,fCount	;can page down at all?
	bcc	@10
	jsr	beep
	rts
@10:
	AddVW	MAX_LIST-2,a0L
	CmpW	a0L,fCount	;can go down full page?
	bcc	@20
	lda	fCount	;no, go to bottom
	sec
	sbc	#MAX_LIST
	sta	topName
	lda	fCount+1
	sbc	#0
	sta	topName+1
	bra	@30
@20:
	AddVW	MAX_LIST-1,topName ;yes, top item becomes bottom
@30:
	jsr	fillList
	lda	selName
	bmi	@40
	jsr	invList	;restore list selection display
@40:
	rts
; -----------------------------------------------------------
; Page up routine (called through otherPressVector).
; -----------------------------------------------------------
pageUp:	
	lda	topName+1	;can page up at all?
	bne	@10
	lda	topName
	bne	@10
	jsr	beep
	rts
@10:
	CmpWI	topName,MAX_LIST-1 ;can go up full page?
	bcc	@20
	lda	topName	;yes,bottom item becomes top
	sec
	sbc	#MAX_LIST-1
	sta	topName
	lda	topName+1
	sbc	#0
	sta	topName+1
	bra	@30
@20:	lda	#0	;no, go to top
	sta	topName
	sta	topName+1
@30:	jsr	fillList
	lda	selName
	bmi	@40
	jsr	invList	;restore list selection display
@40:	rts
; -----------------------------------------------------------
; draw GUI and fill listbox
; -----------------------------------------------------------
doGui:	lda	#1	;solid (shadow box)
	jsr	SetPattern
	LoadB	r2L,MAIN_T+8
	LoadB	r2H,MAIN_B+8
	LoadW	r3,((MAIN_L+8)|DOUBLE_W)
	LoadW	r4,((MAIN_R+8)|DOUBLE_W)
	jsr	Rectangle
;	------------------------------------------------
	lda	#0	;clear
	jsr	SetPattern
	LoadB	r2L,MAIN_T+1
	LoadB	r2H,MAIN_B-1
	LoadW	r3,((MAIN_L+1)|DOUBLE_W)
	LoadW	r4,((MAIN_R-1)|DOUBLE_W)
	jsr	Rectangle
	LoadB	r2L,MAIN_T
	LoadB	r2H,MAIN_B
	LoadW	r3,((MAIN_L)|DOUBLE_W)
	LoadW	r4,((MAIN_R)|DOUBLE_W)
	lda	#$ff	;solid line
	jsr	FrameRectangle
;	------------------------------------------------
	LoadB	r2L,LIST_T	;listbox for filenames
	LoadB	r2H,LIST_B
	LoadW	r3,(LIST_L|DOUBLE_W)
	LoadW	r4,(LIST_R|DOUBLE_W)
	lda	#$ff	;solid line
	jsr	FrameRectangle
;	------------------------------------------------
	LoadB	r3L,LIST_T
	LoadB	r3H,LIST_B
	LoadW	r4,(SCROLL_B|DOUBLE_W)
	lda	#$ff	;solid line
	jsr	VerticalLine	;scrollbar boundary
;	------------------------------------------------
	LoadB	r1H,107
	LoadW	r11,(200|DOUBLE_W)
	LoadW	r0,legend
	jsr	PutString
	ldx	#0
	stx	topName
	stx	topName+1
	dex
	stx	selName
	lda	fCount+1
	bne	@10
	lda	fCount
	beq	@20
@10:
	jsr	fillList
@20:
	LoadW	r0,icons
	jsr	DoIcons
	LoadW	otherPressVec,chkMouse
	rts
; -----------------------------------------------------------
; Fill listbox from list of filenames.
;	pass:	topName, index of first filename to show
;	destroyed:	a0L, a1H, a2, a8, a9 (from call to a8Name)
; -----------------------------------------------------------
fillList:
	CmpW	fNames,fEnd	;empty list?
	bne	@10
	lda	#0	;clear
	jsr	SetPattern
	LoadB	r2L,LIST_T+1
	LoadB	r2H,LIST_B-1
	LoadW	r3,((LIST_L+1)|DOUBLE_W)
	LoadW	r4,((SCROLL_B-1)|DOUBLE_W)
	jsr	Rectangle	;clear listbox
	jsr	doThumb
	rts
@10:
	LoadB	a1H,LIST_T+9	;first baseline
	LoadB	a0L,0	;counter
	MoveW	topName,a8
	jsr	a8Name	;text pointer to a8
	lda	#LIST_T+1
	sta	r2L
	sta	a2L	;top of list item
	lda	#LIST_T+11
	sta	r2H
	sta	a2H	;bottom of list item
@20:
	LoadW	r3,((LIST_L+1)|DOUBLE_W)
	LoadW	r4,((SCROLL_B-1)|DOUBLE_W)
	lda	#0	;clear
	jsr	SetPattern
	jsr	Rectangle
	CmpW	a8L,fEnd	;at end of list?
	bcs	@30
	jsr	addName
@30:
	inc	a0L
	lda	a0L
	cmp	#MAX_LIST
	bcc	@40
	jsr	doThumb
	rts
@40:
	AddVW	17,a8L	;get next filename
	lda	a2L
	clc
	adc	#10	;height of list item
	sta	a2L
	sta	r2L
	lda	a2H
	clc
	adc	#10
	sta	a2H
	sta	r2H
	AddVB	10,a1H	;baseline
	bra	@20
; -----------------------------------------------------------
; Add filename to listbox. Images in bold, directories in italics.
; -----------------------------------------------------------
addName:	ldy	#0
	lda	(a8),y	;file type
	cmp	#$86	;subdirectory?
	bne	@10
	lda	#ITALICON
	bne	@30
@10:
	jsr	isImage
	bcc	@20
	lda	#BOLDON
	bne	@30
@20:
	lda	#PLAINTEXT
@30:
	sta	tmpEntry
	iny
@40:
	lda	(a8),y	;filename
	beq	@60
	cmp	#$ff
	bne	@50
	lda	#'~'	;icky 8.3 FAT filename
@50:
	sta	tmpEntry,y
	iny
	cpy	#17
	bne	@40
@60:
	lda	tmpEntry
	cmp	#PLAINTEXT
	beq	@70
	lda	#PLAINTEXT
	sta	tmpEntry,y
	iny
@70:
	lda	#0
	sta	tmpEntry,y
	LoadW	r0,tmpEntry
	MoveB	a1H,r1H	;baseline
	LoadW	r11,(LIST_L+2|DOUBLE_W)
	jsr	PutString
	rts
; -----------------------------------------------------------
; Draw thumb for scrollbar.
; -----------------------------------------------------------
doThumb:
	jsr	thumbPos	;sets thumb height and position
	sta	thumbSav
	lda	#0	;clear
	jsr	SetPattern
	LoadB	r2L,LIST_T+8
	LoadB	r2H,LIST_B-8
	LoadW	r3,((SCROLL_B+1)|DOUBLE_W)
	LoadW	r4,((LIST_R-1)|DOUBLE_W)
	jsr	Rectangle	;clear scroll area
	lda	#1	;solid
	jsr	SetPattern
	lda	thumbSav	;already calculated
	sta	r2L
	clc
	adc	thumbHi
	sta	r2H
	LoadW	r3,(THUMB_L|DOUBLE_W)
	LoadW	r4,((THUMB_L+(THUMB_WD-1))|DOUBLE_W)
	jsr	Rectangle
	rts
; -----------------------------------------------------------
; Calculate top position of thumb for scrollbar.
; Solve for offset from THUMBTOP:
; offset / (lowest thumb pos. - THUMBTOP) = topName / max. top item
; (topName * (lowest thumb pos. - THUMBTOP)) / max. top item = offset
; -----------------------------------------------------------
thumbPos:
	lda	fCount+1	;all files fit in listbox?
	bne	@10
	lda	fCount
	cmp	#MAX_LIST
	bcs	@10
	lda	#THUMBMAX
	sta	thumbHi
	lda	#THUMBTOP
	rts
@10:
	jsr	thumbSiz	;sets thumbHi
	lda	#LIST_B
	sec
	sbc	#8	;for arrow icon
	sbc	thumbHi	;lowest possible thumb pos.
	sbc	#THUMBTOP
	sta	a9L	;src (byte)
	lda	topName
	sta	a8L	;dest (word)
	lda	topName+1
	sta	a8H
	ldx	#a8	;dest
	ldy	#a9	;src
	jsr	BMult	;topName * (lowest thumb pos. - THUMBTOP)
	lda	fCount
	sec
	sbc	#MAX_LIST
	sta	a9L	;max. top item
	lda	fCount+1
	sbc	#0
	sta	a9H
	ldx	#a8	;dest
	ldy	#a9	;src
	jsr	Ddiv	;divide result by max. top item
	lda	#THUMBTOP
	clc
	adc	a8L	;and add to THUMBTOP
	rts
; -----------------------------------------------------------
; Calculate height of thumb for scrollbar.
; Solve for thumb height:
; thumbHi / THUMBMAX = MAX_LIST / fCount
; (MAX_LIST * THUMBMAX) / fCount = thumbHi
; -----------------------------------------------------------
thumbSiz:
	lda	#THUMBMAX
	sta	a9L
	lda	#MAX_LIST
	sta	a8L
	lda	#0
	sta	a8H
	ldx	#a8	;dest
	ldy	#a9	;src
	jsr	BBMult	;MAX_LIST * THUMBMAX
	lda	fCount
	sta	a9L
	lda	fCount+1
	sta	a9H
	ldx	#a8	;dest
	ldy	#a9	;src
	jsr	Ddiv	;(MAX_LIST * THUMBMAX) / fCount
	lda	a8L
	cmp	#THUMBMIN
	bcs	@10
	lda	#THUMBMIN
	bne	@30
@10:
	lda	#THUMBMAX
	cmp	a8L
	bcs	@20
	lda	#THUMBMAX
	bne	@30
@20:
	lda	a8L
@30:
	sta	thumbHi
	rts
; -----------------------------------------------------------
; Check for mouse click over listbox/scroll bar.
;	destroyed: a0
; -----------------------------------------------------------
chkMouse:	lda	mouseData
	bpl	@20
@10:
	rts		;ignore releases
@20:
	CmpW	fCount,fEnd	;empty list?
	beq	@10
	LoadB	r2L,LIST_T+1
	LoadB	r2H,LIST_T+11
	LoadW	r3,(LIST_L+1|DOUBLE_W)
	LoadW	r4,((SCROLL_B-1)|DOUBLE_W)
	ldy	#0
	php
	sei
@30:
	jsr	IsMseInRegion
	cmp	#$ff
	beq	@50	;hit
	iny
	cpy	#MAX_LIST
	bne	@40
	plp		;click wasnt over listbox
	bra	ckScroll
@40:
	AddVB	10,r2L	;height of list item
	AddVB	10,r2H
	bra	@30
;	------------------------------------------------
;	click over filename: invert and check for double-click
;	------------------------------------------------
@50:
	LoadB	dblClickCount, CLICK_COUNT
	plp
	tya		;list index
	clc
	adc	topName
	sta	a0L
	lda	topName+1
	adc	#0
	sta	a0H
	CmpW	a0L,fCount	;click over empty area?
	bcs	@90	;yes, exit
	cpy	selName	;already selected?
	beq	@70	;yes, check for double-click
	tya
	ldx	selName	;anything selected?
	bmi	@60	;no
	pha		;save list index
	txa
	jsr	invList	;deselect previous
	pla
@60:
	sta	selName
	jsr	invList	;select new one
@70:
	lda	dblClickCount	;check for double-click
	beq	@90	;double-click timed out?
	lda	mouseData
	bpl	@70	;still pressed?
@80:
	lda	dblClickCount	;no, released
	beq	@90	;double-click timed out?
	lda	mouseData	;no, check for 2nd click
	bmi	@80	;still released?
	jmp	doOpen	;double-click, open the file
@90:
	rts
ckScroll:	LoadB	r2L,LIST_T+8	;click above scroll thumb?
	ldx	thumbSav
	dex
	stx	r2H
	LoadW	r3,((SCROLL_B+1)|DOUBLE_W)
	LoadW	r4,((LIST_R+1)|DOUBLE_W)
	php
	sei
	jsr	IsMseInRegion
	plp
	cmp	#$ff
	bne	@10
	jsr	pageUp
	rts
@10:
	lda	thumbSav	;click below scroll thumb?
	clc
	adc	thumbHi
	sta	r2L
	LoadB	r2H,LIST_B-8
	LoadW	r3,((SCROLL_B+1)|DOUBLE_W)
	LoadW	r4,((LIST_R-1)|DOUBLE_W)
	php
	sei
	jsr	IsMseInRegion
	plp
	cmp	#$ff
	bne	@20
	jsr	pageDown
@20:
	rts
; -----------------------------------------------------------
; Invert listbox selection.
;	pass:	.A, index of item in list to invert
; -----------------------------------------------------------
invList:
	sta	a8L	;dest
	LoadB	a9L,10	;src
	ldx	#a8L
	ldy	#a9L
	jsr	BBMult
	lda	#LIST_T+1
	clc
	adc	a8L
	sta	r2L
	lda	#LIST_T+11
	clc
	adc	a8L
	sta	r2H
	LoadW	r3,((LIST_L+1)|DOUBLE_W)
	LoadW	r4,((SCROLL_B-1)|DOUBLE_W)
	jsr	InvertRectangle
	rts
; -----------------------------------------------------------
; Get pointer to filename at given index.
;	pass:	a8, index into filename list
;	return:	a8, pointer to filename
;	destroyed:	a9
; -----------------------------------------------------------
a8Name:
	LoadW	a9L,17	;length of fNames entry
	ldx	#a8	;dest (word)
	ldy	#a9	;src (byte)
	jsr	BMult
	lda	a8L
	clc
	adc	#<fNames
	sta	a8L
	lda	a8H
	adc	#>fNames
	sta	a8H
	rts
	
; uIecSwitch2
; -----------------------------------------------------------
; Verify that there is a uIEC or sd2iec on the system. Send "X?", 
; which is specific to these devices, and check for device name 
; in output. Assume that only one of these devices is present.
;	return:	carry clear if uIEC found, set otherwise
; -----------------------------------------------------------
findUiec:
	lda	curDrive
	sta	savDrive
	ldy	#8
@10:
	tya
	pha
	sta	drvUiec
	sta	curDrive
	jsr	ExitTurbo
	jsr	PurgeTurbo
	jsr	ckUiec
	pla
	tay
	bcs	@20	;not found here
	clc		;take first one (in drvUiec)
	rts
@20:
	iny
	cpy	#11	;try drives, A, B, C
	bne	@10
	sec		;no uIEC found
	rts
; -----------------------------------------------------------
; Check if current device is a uIEC or sd2iec.
;	pass:	curDrive, the drive to check
;	return:	carry clear if found, set otherwise
; -----------------------------------------------------------
ckUiec:
	LoadW	r0,xq
	jsr	sendCmd	;calls InitForIO/DoneWithIO
	bcs	@60	;no drive at this address
	ldx	#0
	ldy	#3
@10:
	lda	uIec,x
	beq	@70
	cmp	msg,y
	bne	@20
	inx
	iny
	bne	@10
;	------------------------------------------------
@20:
	ldx	#0
	ldy	#3
@30:
	lda	sd2iec,x
	beq	@70
	cmp	msg,y
	bne	@40
	inx
	iny
	bne	@30
;	------------------------------------------------
;	gh 1.2: some uIECs respond "00, OK,00,00" here!
;	------------------------------------------------
@40:
	ldx	#0
	ldy	#3
@50:
	lda	ok,x
	beq	@70
	cmp	msg,y
	bne	@60
	inx
	iny
	bne	@50
@60:
	sec		;not found
	rts
@70:
	clc		;found
	rts
; -----------------------------------------------------------
; Read current directory; store names of non-deleted files at fNames.
; -----------------------------------------------------------
readDir:
	jsr	InitForIO
	jsr	openDir
	ldx	#2
	jsr	C_CHKIN
	ldy	#254	;skip header block
@5:
	jsr	C_CHRIN
	dey
	bne	@5
@10:
	LoadW	a0,fNames
	lda	#0
	sta	fCount	;number of files added to list
	sta	fCount+1
	sta	blkCount	;entry counter for this block
@20:
	ldy	#0
	jsr	C_CHRIN	;file type
	sta	tmpEntry,y
	iny
	jsr	C_CHRIN	;skip track & sector pointer
	jsr	C_CHRIN
@30:
	jsr	C_CHRIN	;filename
	sta	tmpEntry,y
	iny
	cpy	#17
	bcc	@30
	jsr	addEntry	;use this entry? (increments fCount)
	ldy	#0
@40:
	jsr	C_CHRIN	;discard remainder of entry
	lda	$90
	and	#$40	;EOF?
	bne	dirDone
	iny
	lda	blkCount
	cmp	#7	;every 8th entry is shorter
	beq	@50
	cpy	#13
	bcc	@40
	bcs	@60
@50:
	cpy	#11
	bcc	@40
@60:
	ldx	blkCount
	cpx	#7
	bcc	@70
	LoadB	blkCount,0
	beq	@80
@70:
	inc	blkCount
@80:
	bra	@20
dirDone:
	lda	#2
	jsr	C_CLOSE
	jsr	DoneWithIO
	MoveW	a0L,fEnd	;end-of-list marker
	ldx	#0
	stx	topName
	stx	topName+1
	dex
	stx	selName
	rts
; -----------------------------------------------------------
; open directory as a sequential file
; -----------------------------------------------------------
openDir:	
	lda	#1
	ldx	#<dollar
	ldy	#>dollar
	jsr	C_SETNAM

;	This should work but it does not..  C_SETBNK - $FF68 is not mapped in,
;     so we do it manually below.
;	lda #$00			; C128 - bank 1 to load into
;	ldx #$01			; C128 - fn im bank 0
;	jsr C_SETBNK		; C128 - SET bank for filename and data

	; Set C128 bank 1 for filename location
	lda #$01
	sta $c7

	lda	#2
	ldx	curDrive
	ldy	#2
	jsr	C_SETLFS
	
	jsr	C_OPEN
	rts
; -----------------------------------------------------------
; Add filename to list (unless the file is deleted).
; -----------------------------------------------------------
addEntry:
	lda	tmpEntry
	beq	@20	;deleted file
	ldy	#16	;add entry to list
@10:
	lda	tmpEntry,y
	sta	(a0),y
	dey
	bpl	@10
	AddVW	17,a0
	inc	fCount
	bne	@20
	inc	fCount+1
@20:
	rts
; -----------------------------------------------------------
; Determine whether filename represents a disk image.
;	pass:	pointer to filename in a8
;	return:	carry set if image, clear otherwise
;	preserved:	.Y
;	destroyed:	a9L
; -----------------------------------------------------------
isImage:
	tya
	pha
	ldy	#1	;past file type
@10:
	lda	(a8),y
	cmp	#$a0	;first $a0 is end of filename
	beq	@20
	iny
	cpy	#17
	bne	@10
@20:
	dey		;back up from end of filename
	sty	a9L
	ldx	#3
@30:
	lda	(a8),y
	ora	#$20	;case-insensitive
	cmp	d64Ext,x
	bne	@40
	dey
	dex
	bpl	@30
	bmi	@110
@40:
	ldy	a9L
	ldx	#3
@50:
	lda	(a8),y
	ora	#$20	;case-insensitive
	cmp	d71Ext,x
	bne	@60
	dey
	dex
	bpl	@50
	bmi	@110
@60:
	ldy	a9L
	ldx	#3
@70:
	lda	(a8),y
	ora	#$20	;case-insensitive
	cmp	d81Ext,x
	bne	@80
	dey
	dex
	bpl	@70
	bmi	@110
@80:
	ldy	a9L
	ldx	#3
@90:
	lda	(a8),y
	ora	#$20	;case-insensitive
	cmp	dnpExt,x
	bne	@100
	dey
	dex
	bpl	@90
	bmi	@110
@100:
	clc		;not an image
	bcc	@120
@110:
	sec		;is an image
@120:
	pla
	tay
	rts
; -----------------------------------------------------------
; Send disk command and read response (does not open a file).
; -----------------------------------------------------------
sendCmd:	jsr	InitForIO
	LoadB	C_STATUS,0
	lda	drvUiec
	jsr	C_LISTEN
	bit	C_STATUS
	bmi	@10
	lda	#$6f
	jsr	C_SECOND
	bit	C_STATUS
	bpl	@20
@10:
	jsr	C_UNLSN
	jsr	DoneWithIO
	sec
	rts
@20:
	ldy	#0
@30:
	lda	(r0),y
	beq	@40
	jsr	C_CIOUT
	iny
	bne	@30
@40:
	jsr	C_UNLSN
	lda	drvUiec
	jsr	C_TALK
	bit	C_STATUS
	bmi	@50
	lda	#$6f
	jsr	C_TKSA
	bit	C_STATUS
	bpl	@60
@50:
	jsr	C_UNTLK
	jsr	DoneWithIO
	sec
	rts
@60:
	ldy	#0
@70:
	jsr	C_ACPTR
	cmp	#$0d
	bne	@80
	lda	#0
@80:
	sta	msg,y
	iny
	bit	C_STATUS	;EOF (#$40)?
	bvc	@70
	lda	#0
	sta	msg,y
	jsr	C_UNTLK
	jsr	DoneWithIO
	clc
	rts
; -----------------------------------------------------------
;	Get string width in pixels (must be < 256 chars).
;	pass:	string address in r0
;	return:	string length in a0
;	destroyed: a1L
; -----------------------------------------------------------
strWidth:
	ldy	#0
	sty	a0L
	sty	a0H
@10:
	lda	(r0),y
	beq	@20
	sty	a1L
	jsr	GetCharWidth
	clc
	adc	a0L
	sta	a0L
	lda	#0
	adc	a0H
	sta	a0H
	ldy	a1L
	iny
	bne	@10	;string must be < 256 chars.
@20:
	rts
; -----------------------------------------------------------
; Generic beep.
; -----------------------------------------------------------
beep:	php
	sei
	lda	$01
	pha
	and	#$f8
	ora	#$05
	sta	$01
	LoadB	$d400,$31	;voice 1 frequency low
	LoadB	$d401,$1c	;voice 1 frequency high
	LoadB	$d405,$00	;voice 1 attack/decay
	LoadB	$d406,$f9	;voice 1 sustain/release
	LoadB	$d418,$0c	;no filters, volume 15
	LoadB	$d404,$11	;gate on triangle, voice 1
	LoadB	$d404,$10	;gate off voice 1
	pla
	sta	$01
	plp
	rts
; -----------------------------------------------------------
; Check for GEOS 128
; -----------------------------------------------------------
Check128:
	lda #$12
	cmp version
	bpl @10
	lda c128Flag
@10:
	rts
; -----------------------------------------------------------
; main menu 40 columns
; -----------------------------------------------------------
mainMenu:	
	.byte	0,14							; top, bottom of menu
	.word	0,(48|DOUBLE_W)					; left, right of menu
	.byte	HORIZONTAL | 2	
	.word	geosText
	.byte	SUB_MENU
	.word	geosMenu
	.word	fileText
	.byte	SUB_MENU
	.word	fileMenu
geosText:	.byte	"geos",0
fileText:	.byte	"file",0
; -----------------------------------------------------------
geosMenu:	
	.byte	15,30							; top, bottom of menu
	.word	0,(24|DOUBLE_W)					; left, right of menu
	.byte	VERTICAL | CONSTRAINED | 1
	.word	infoText
	.byte	MENU_ACTION
	.word	doInfo
infoText:	.byte	"info",0
; -----------------------------------------------------------
fileMenu:	
	.byte	15,30							; top, bottom of menu
	.word	(28|DOUBLE_W)	,(52|DOUBLE_W)								; left, right of menu
	.byte	VERTICAL | CONSTRAINED | 1
	.word	quitText
	.byte	MENU_ACTION
	.word	doQuit
quitText:	.byte	"quit",0
; -----------------------------------------------------------
; main menu 80 columns - text defined above
; Use xx|DOUBLE_W for width as title bar depends on it.
; -----------------------------------------------------------
mainMenu80:	
	.byte	0,14							; top, bottom of menu
	.word	0,75							; left, right of menu
	.byte	HORIZONTAL | 2	
	.word	geosText
	.byte	SUB_MENU
	.word	geosMenu80
	.word	fileText
	.byte	SUB_MENU
	.word	fileMenu80
; -----------------------------------------------------------
geosMenu80:	
	.byte	15,30							; top, bottom of menu
	.word	0,35							; left, right of menu
	.byte	VERTICAL | CONSTRAINED | 1
	.word	infoText
	.byte	MENU_ACTION
	.word	doInfo
; -----------------------------------------------------------
fileMenu80:	
	.byte	15,30							; top, bottom of menu
	.word	36,75							; left, right of menu
	.byte	VERTICAL | CONSTRAINED | 1
	.word	quitText
	.byte	MENU_ACTION
	.word	doQuit
; -----------------------------------------------------------
icons:	
	.byte	4	;no. icons
	.word	(18|DOUBLE_W)	;X position to leave mouse
	.byte	5	;Y position to leave mouse
;	------------------------------------------------
	.word	upArrow	;pointer to icon image
	.byte	(23|DOUBLE_B)	;X position in cards
	.byte	48	;Y position in pixels
	.byte	(1|DOUBLE_B),7	;width (cards), height (pixels) (8px x 7px)
	.word	doUp	;dispatch routine
;	------------------------------------------------
	.word	dnArrow	;pointer to icon image
	.byte	(23|DOUBLE_B)	;X position in cards
	.byte	144	;Y position in pixels
	.byte	(1|DOUBLE_B),7	;width (cards), height (pixels) (8px x 7px)
	.word	doDown	;dispatch routine
;	------------------------------------------------
	.word	open	;pointer to icon image
	.byte	(25|DOUBLE_B)	;X position in cards
	.byte	48	;Y position in pixels
	.byte	(6|DOUBLE_B),16	;width (cards), height (pixels) (48px x 16px)
	.word	doOpen	;dispatch routine
;	------------------------------------------------
	.word	parent	;pointer to icon image
	.byte	(25|DOUBLE_B)	;X position in cards
	.byte	72	;Y position in pixels
	.byte	(6|DOUBLE_B),16	;width (cards), height (pixels)	(48px x 16px)
	.word	doParent	;dispatch routine
; -----------------------------------------------------------
upArrow:
; This file was generated by sp65 2.17 - Git cc5c093 from
; icon-up.pcx (8x7, 2 colors, indexed)
;
        .byte   $87,$FF,$80,$88,$9C,$BE,$80,$FF
	
dnArrow:
;
; This file was generated by sp65 2.17 - Git cc5c093 from
; icon-down.pcx (8x7, 2 colors, indexed)
;
        .byte   $87,$FF,$80,$BE,$9C,$88,$80,$FF

open:
;
; This file was generated by sp65 2.17 - Git cc5c093 from
; icon-open.pcx (48x16, 2 colors, indexed)
;
        .byte   $05,$FF,$82,$FE,$80,$04,$00,$83,$03,$80,$78,$03,$00,$83,$03,$80
        .byte   $84,$03,$00,$8F,$03,$81,$02,$5C,$3C,$5C,$03,$81,$02,$62,$42,$62
        .byte   $03,$81,$02,$03,$42,$AB,$03,$81,$02,$42,$7E,$42,$03,$81,$02,$42
        .byte   $40,$42,$03,$80,$84,$62,$40,$42,$03,$80,$78,$5C,$3E,$42,$03,$80
        .byte   $00,$40,$00,$00,$03,$80,$00,$40,$00,$00,$03,$80,$00,$40,$00,$00
        .byte   $03,$06,$FF,$81,$7F,$05,$FF

parent:
;
; This file was generated by sp65 2.17 - Git cc5c093 from
; icon-parent.pcx (48x16, 2 colors, indexed)
;
        .byte   $05,$FF,$82,$FE,$80,$04,$00,$82,$03,$8F,$04,$00,$83,$03,$88,$80
        .byte   $03,$00,$83,$43,$88,$80,$03,$00,$AC,$43,$88,$8F,$16,$78,$B9,$F3
        .byte   $88,$90,$98,$84,$C4,$43,$8F,$00,$90,$84,$84,$43,$88,$0F,$90,$FC
        .byte   $84,$43,$88,$10,$90,$80,$84,$43,$88,$11,$90,$80,$84,$43,$88,$0E
        .byte   $90,$7C,$84,$33,$80,$04,$00,$82,$03,$80,$04,$00,$81,$03,$06,$FF
        .byte   $81,$7F,$05,$FF

; -----------------------------------------------------------
; info dialog
; -----------------------------------------------------------
infoDlg:
	.byte	DEF_DB_POS | 1
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_1_Y
	.word	infoName
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_2_Y
	.word	code
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_3_Y
	.word	c128
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_4_Y
	.word	qa
	.byte	OK,DBI_X_2,DBI_Y_2
	.byte	0	;end of dialog definition
infoName:	.byte	BOLDON,"uIEC Switch 1.2",PLAINTEXT,0
code:	.byte	"code: ShadowM",0
c128: .byte	"GEOS 128 support: Alex Burger",0
qa:	.byte	"QA: Wizard the Cat",0
; -----------------------------------------------------------
; generic error dialog
; -----------------------------------------------------------
errorDlg:	.byte	DEF_DB_POS | 1
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_1_Y
errorMsg:	.word	0	;filled in by caller
	.byte	OK,DBI_X_2,DBI_Y_2
	.byte	0	;end of dialog definition
noSelect:	.byte	"Nothing selected.",0
noParent:	.byte	"No parent directory.",0
noEntry:	.byte	"Not a disk image or subdirectory.",0
; -----------------------------------------------------------
uIecDlg:
	.byte	DEF_DB_POS | 1
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_1_Y
	.word	uIecErr
	.byte	OK,DBI_X_2,DBI_Y_2
	.byte	0
uIecErr:	.byte	"No uIEC or sd2iec found.",0
; -----------------------------------------------------------
typeDlg:
	.byte	DEF_DB_POS | 1
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_1_Y
	.word	typeErr1
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_2_Y
	.word	typeErr2
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_3_Y
	.word	typeErr3
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_4_Y
	.word	typeErr4
	.byte	YES,DBI_X_1,DBI_Y_2
	.byte	NO,DBI_X_2,DBI_Y_2
	.byte	0
typeErr1:	.byte	"The partition or disk image selected",0
typeErr2:	.byte	"on your uIEC/sd2iec does not match",0
typeErr3:	.byte	"the GEOS drive type for that device.",0
typeErr4:	.byte	"Are you sure you want to exit?",0
; -----------------------------------------------------------
GEOS128Dlg:	
	.byte	DEF_DB_POS | 1
	.byte	DBTXTSTR,TXT_LN_X,TXT_LN_1_Y
	.word	GEOS128Err
	.byte	OK,DBI_X_2,DBI_Y_2
	.byte	0
GEOS128Err:	.byte	"This version requires GEOS 128.",0
; -----------------------------------------------------------
legend:
	.byte	ULINEON,"legend",PLAINTEXT,":"
	.byte	GOTOXY
	.word	(200|DOUBLE_W)
	.byte	117
	.byte	BOLDON,"DISK IMG.",PLAINTEXT
	.byte	GOTOXY
	.word	(200|DOUBLE_W)
	.byte	127
	.byte	ITALICON,"DIRECTORY",PLAINTEXT
	.byte	GOTOXY
	.word	(200|DOUBLE_W)
	.byte	137
	.byte	"OTHER FILES",0
; -----------------------------------------------------------
uIec:	.byte	"UIEC",0
sd2iec:	.byte	"SD2IEC",0
ok:	.byte	" OK",0
xq:	.byte	"X?",0
dollar:	.byte	"$",0
cdImage:	.byte	"CD:"
	.res	17
chDir:	.byte	"CD/"
	.res	17
cdParent:	.byte	"CD",95,0	;95 is back-arrow
gp:	.byte	"G-P",255,0
savDrive:	.byte	0
drvUiec:	.byte	0
d64Ext:	.byte	".d64",0
d71Ext:	.byte	".d71",0
d81Ext:	.byte	".d81",0
dnpExt:	.byte	".dnp",0
title:	.byte	" uIecSwitch 1.2 ",0
fCount:	.word	0
fEnd:	.word	0
blkCount:	.byte	0
topName:	.word	0	;top item (index into filenames)
selName:	.byte	0	;selected item (index into listbox)
thumbSav:	.byte	$ff	;last top thumb position
thumbHi:	.byte	0	;calculated thumb height
menuWidthB:	.word 	0	; Expected to be <=255
menuWidthW:	.word	0
; -----------------------------------------------------------
;.ramsect
tmpEntry:	.res	19
msg:	.res	64
fNames:	.res	1024

	