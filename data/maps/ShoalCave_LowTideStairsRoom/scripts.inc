ShoalCave_LowTideStairsRoom_MapScripts::
	map_script MAP_SCRIPT_ON_LOAD, ShoalCave_LowTideStairsRoom_OnLoad
	.byte 0

ShoalCave_LowTideStairsRoom_OnLoad:
	call ShoalCave_LowTideStairsRoom_EventScript_SetShoalItemMetatiles
	end

ShoalCave_LowTideStairsRoom_EventScript_SetShoalItemMetatiles::
	goto_if_set FLAG_RECEIVED_SHOAL_SALT_3, ShoalCave_LowTideStairsRoom_EventScript_SetShoalItemMetatilesEnd
	setmetatile 11, 11, METATILE_Cave_ShoalCave_DirtPile_Large, 1
	return

ShoalCave_LowTideStairsRoom_EventScript_SetShoalItemMetatilesEnd::
	return

ShoalCave_LowTideStairsRoom_EventScript_ShoalSalt3::
	lockall
	goto_if_set FLAG_RECEIVED_SHOAL_SALT_3, ShoalCave_LowTideStairsRoom_EventScript_ReceivedShoalSalt
	giveitem ITEM_SHOAL_SALT
	compare VAR_RESULT, FALSE
	goto_if_eq Common_EventScript_ShowBagIsFull
	setmetatile 11, 11, METATILE_Cave_ShoalCave_DirtPile_Small, 0
	special DrawWholeMapView
	setflag FLAG_RECEIVED_SHOAL_SALT_3
	releaseall
	end

ShoalCave_LowTideStairsRoom_EventScript_ReceivedShoalSalt::
	msgbox ShoalCave_Text_WasShoalSaltNowNothing, MSGBOX_DEFAULT
	releaseall
	end
