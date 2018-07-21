# uIecSwitch128
Written by Glenn Holmer (a.k.a "Shadow", a.k.a "Cenbe").
https://www.lyonlabs.org/commodore/onrequest/geos/index.html#uIecSwitch

GEOS 128 support added by Alex Burger.

July 20th, 2018


## Overview

uIecSwitch128 is a GEOS 128 application for switching between disk images on a uIEC or sd2iec drive.


## Installing

Inside the bin folder is a cc65 compiled version in GEOS CONVERT format which can be copied directly to a .d64, d71 or .d81 image file using DirMaster or another Commodore disk imaging program.  When using DirMaster, the file will be automatically converted to GEOS format but with other utilities you may have to manually convert to GEOS format using CONVERT 2.5.


## Compiling

Compiling requires the cc65 6502 compiler which is available from https://cc65.github.io/cc65/.

The following libraries are required from the GOES 2.0 source code at https://github.com/mist64/geos/tree/master/inc

	geosmac.inc
	geossym.inc
	jumptab.inc

The following libary is required from the cc65 project, which can be found at https://github.com/cc65/cc65/tree/master/libsrc/geos-cbm

	geossym2.inc

The button bitmap data is contained inside the main source file uIecSwitch128.S.  If the .png/.pcx source images are changed, they need to be recompiled using sp65 and then manually added to uIecSwitch128.S.  If needed, use Gimp to convert from .png to .pcx.

	sp65 -v -r button-open.pcx -c geos-bitmap -w button-open.s,format=asm
	sp65 -v -r button-parent.pcx -c geos-bitmap -w button-parent.s,format=asm
	sp65 -v -r button-up.pcx -c geos-bitmap -w button-up.s,format=asm
	sp65 -v -r button-down.pcx -c geos-bitmap -w button-down.s,format=asm

The program icon is contained in icon-program.bin.  If the .png/.pcx source image is changed, it needs to be recompiled using sp65.

	sp65 -v -r icon-program.pcx -c geos-icon -w icon-program.bin,format=bin

The following will compile, resulting in a regular program file which can then be converted using Convert in GEOS.

	ca65 -t geos-cbm uIecSwitch128S.s
	grc65 -t geos-cbm uIecSwitchG.grc
	ca65 -t geos-cbm uIecSwitchG.s
	ld65 -t geos-cbm -o uIecSwitch128.cvt uIecSwitch128S.o uIecSwitchG.o geos-cbm.lib
