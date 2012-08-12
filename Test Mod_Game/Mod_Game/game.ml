#open "bimedia";;
#open "sound";;


type GameStyleSheet = {mutable gSpritesDir:string; mutable gSoundsDir:string; mutable gSpeed:float; mutable gFps:int};;

let MakeGameStyleSheet spritesDir soundsDir speed fps =
	{gSpritesDir=spritesDir; gSoundsDir=soundsDir; gSpeed=float_of_int speed; gFps=fps};;

let SPRITES_DIR_DEFAULT = "./Media/Sprites/";;
let SOUNDS_DIR_DEFAULT = "./Media/Sounds/";;
let SPEED_DEFAULT = 150;;
let FPS_DEFAULT = 100;;

let mainStyleSheet = ref (MakeGameStyleSheet SPRITES_DIR_DEFAULT SOUNDS_DIR_DEFAULT SPEED_DEFAULT FPS_DEFAULT);;
let GetSpritesDir() =
	(!mainStyleSheet).gSpritesDir;;
let GetSoundsDir() =
	(!mainStyleSheet).gSoundsDir;;
let GetSpeed() =
	(!mainStyleSheet).gSpeed;;
let GetFPS() =
	(!mainStyleSheet).gFps;;

let voidImage = ref (VoidSurf());;
let gumImage = ref (VoidSurf());;
let bGumImage = ref (VoidSurf());;
let GetVoidImg() =
	if (not SurfOK (!voidImage)) then voidImage := LoadImage ((GetSpritesDir())^"void.bmp");
	(!voidImage);;
let GetGumImg() =
	if (not SurfOK (!gumImage)) then gumImage := LoadImage ((GetSpritesDir())^"gum.png");
	(!gumImage);;
let GetBGumImg() =
	if (not SurfOK (!bGumImage)) then bGumImage := LoadImage ((GetSpritesDir())^"bgum.bmp");
	(!bGumImage);;

let chompSound = ref (VoidFSound());;
let initSound = ref (VoidFSound());;
let dieSound = ref (VoidFSound());;
let deadGhostSound = ref (VoidFSound());;
let intermissionSound = ref (VoidFSound());;

let GetChompSound() =
	if (not FSoundOK (!chompSound)) then chompSound := LoadFSound ((GetSoundsDir())^"chomp.wav") SAMPLE;
	(!chompSound);;
let GetInitSound() =
	if (not FSoundOK (!initSound)) then initSound := LoadFSound ((GetSoundsDir())^"init.wav") SAMPLE;
	(!initSound);;
let GetDieSound() =
	if (not FSoundOK (!dieSound)) then dieSound := LoadFSound ((GetSoundsDir())^"die.wav") SAMPLE;
	(!dieSound);;
let GetDeadGhostSound() =
	if (not FSoundOK (!deadGhostSound)) then deadGhostSound := LoadFSound ((GetSoundsDir())^"deadGhost.wav") SAMPLE;
	(!deadGhostSound);;
let GetIntermissionSound() =
	if (not FSoundOK (!intermissionSound)) then intermissionSound := LoadFSound ((GetSoundsDir())^"intermission.wav") SAMPLE;
	(!intermissionSound);;

let DeleteAllMedias() =
	if (SurfOK (!voidImage)) then DeleteSurf (!voidImage);
	if (SurfOK (!gumImage)) then DeleteSurf (!gumImage);
	if (SurfOK (!bGumImage)) then DeleteSurf (!bGumImage);
	voidImage := VoidSurf();
	gumImage := VoidSurf();
	bGumImage := VoidSurf();

	if (FSoundOK (!chompSound)) then DeleteFSound (!chompSound);
	if (FSoundOK (!initSound)) then DeleteFSound (!initSound);
	if (FSoundOK (!dieSound)) then DeleteFSound (!dieSound);
	if (FSoundOK (!deadGhostSound)) then DeleteFSound (!deadGhostSound);
	if (FSoundOK (!intermissionSound)) then DeleteFSound (!intermissionSound);
	chompSound := VoidFSound();
	initSound := VoidFSound();
	dieSound := VoidFSound();
	deadGhostSound := VoidFSound();
	intermissionSound := VoidFSound();;

let SetGameStyleSheet spritesDir soundsDir speed fps =
	mainStyleSheet := MakeGameStyleSheet spritesDir soundsDir speed fps;
	DeleteAllMedias();;


let DIM = 30;;

type Map = {mutable map:Case vect vect; mutable pmInitPos:float*float; mutable ghInitPos:(float*float) vect; mutable mapMatrix:(int*(int*int)) vect vect; mutable gumCount:int; mutable mapName:string; mutable mapFile:string}
and Case == int;;

type Campaign = {mapList:Map vect; campaignName:string};;

type Direction = UP | DOWN | RIGHT | LEFT;;

type PacMan = {mutable pmAnims:Anim vect; mutable pmDirection:Direction; mutable pmNDir:Direction; mutable pmTime:float; mutable pmPosition:float*float; mutable points:int; mutable nbLives:int};;

type Ghost = {mutable gAnims:Anim vect; mutable gDirection:Direction; mutable gTime:float; mutable gPosition:float*float; mutable gState:GhostState; mutable gType:GhostType ; mutable gScaredTime:int}
and GhostState = GHOST_HUNGRY | GHOST_SCARED | GHOST_DEAD
and GhostType = GHOST_RED | GHOST_BLUE | GHOST_ORANGE | GHOST_PINK;;

let OppositeDir dir =
	match dir with
	| LEFT -> RIGHT
	| RIGHT -> LEFT
	| UP -> DOWN
	| DOWN -> UP;;

let rec modulo x n =
	if (x >= n) then modulo (x-n) n
	else if (x < 0) then modulo (x+n) n
	else x;;

let rec mod_float x m =
	let mf = float_of_int m in
	if (x >=. mf) then mod_float (x -. mf) m
	else if (x <. 0.) then mod_float (x +. mf) m
	else x;;



(*Maps and Campaigns*)

let LoadDefaultMap() =
	let map =
	[|
		[| -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 |] ;
		[| -2 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ; 32 ; -1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  1 ; -1 ; -1 ; 32 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ; -1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  1 ; -1 ; -1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  0 ; -1 ;  0 ; -1 ; -1 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -2 |] ;
		[| -2 ; -2 ; -2 ; -2 ; -1 ;  1 ; -1 ;  0 ;  0 ;  0 ;  0 ;  0 ;  0 ;  0 ; -1 ;  1 ; -1 ; -2 ; -2 ; -2 ; -2 |] ;
		[| -1 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  0 ; -1 ; -1 ;  0 ; -1 ; -1 ;  0 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -1 |] ;
		[|  0 ;  0 ;  0 ;  0 ;  0 ;  1 ;  0 ;  0 ; -1 ;  0 ;  0 ;  0 ; -1 ;  0 ;  0 ;  1 ;  0 ;  0 ;  0 ;  0 ;  0 |] ;
		[| -1 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  0 ; -1 ; -1 ; -1 ; -1 ; -1 ;  0 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -1 |] ;
		[| -2 ; -2 ; -2 ; -2 ; -1 ;  1 ; -1 ;  0 ;  0 ;  0 ;  0 ;  0 ;  0 ;  0 ; -1 ;  1 ; -1 ; -2 ; -2 ; -2 ; -2 |] ;
		[| -2 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  0 ; -1 ; -1 ; -1 ; -1 ; -1 ;  0 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ; -1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -1 ;  1 ; -1 ; -1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ; 32 ;  1 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  0 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ;  1 ; 32 ; -1 ; -2 |] ;
		[| -2 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ; -1 ;  1 ;  1 ;  1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ;  1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ;  1 ; -1 ; -2 |] ;
		[| -2 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -1 ; -2 |] ;
		[| -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 ; -2 |]
	|] in

	let m = vect_length map and n = vect_length (map.(0)) in
	{map=map; pmInitPos=(270.,480.); ghInitPos=[| (270.,300.) ; (240.,300.) ; (300.,300.) ; (270.,270.) |]; mapMatrix=make_matrix m n (m*n+1,(0,0)); gumCount=0; mapName="Initiation"; mapFile="./Files/Maps/System"};;

let LoadVoidMap() =
	let map = make_matrix 12 12 0 in
	{map=map; pmInitPos=(0.,0.); ghInitPos=[| (30.,0.) ; (60.,0.) ; (90.,0.) ; (120.,0.) |]; mapMatrix=make_matrix 12 12 (145,(0,0)); gumCount=0; mapName="Sans nom"; mapFile="./Files/Maps/Perso"};;


let CASE_WALL = -1;;
let CASE_HOLE = -2;;
let CASE_VOID = 0;;
let CASE_GUM = 1;;
let CASE_BGUM = 32;;

let IsCaseOK case =
	case >= 0;;

let IsCaseWall case =
	case = -1;;

let IsCaseHole case =
	case = -2;;

let IsCaseVoid case =
	case = 0;;

let HasCaseGum case =
	case > 0 && case < 32;;

let HasCaseBGum case =
	case = 32;;

let CaseNumber case =
	match case with
	| -2 -> 2
	| -1 -> 1
	| _ -> 0;;


let GetMap map =
	map.map;;

let GetMapPacManInitPosition map =
	map.pmInitPos;;

let GetMapGhostsInitPositions map =
	map.ghInitPos;;

let GetMapDim mapStruct =
	let map = GetMap mapStruct in
	let n = vect_length map -2 in
	if (n<0) then failwith "Map not loaded yet"
	else (n, vect_length (map.(0)) -2);;

let GetMapCase mapStruct (i,j) =
	let (m,n) = GetMapDim mapStruct and map = GetMap mapStruct in
	map.(modulo i (m+2)).(modulo j (n+2));;

let GetMapMatrix map =
	map.mapMatrix;;

let GetMapGumCount map =
	map.gumCount;;

let GetMapName map =
	map.mapName;;

let GetMapFile map =
	map.mapFile;;

let GetCampaignMaps cmp =
	cmp.mapList;;

let GetCampaignName cmp =
	cmp.campaignName;;

let ResetMap mapStruct =
	let (m,n) = GetMapDim mapStruct and tab = GetMapMatrix mapStruct in
	for i=0 to (m+1) do
		for j=0 to (n+1) do
			tab.(i).(j) <- ((m+2)*(n+2)+1,(0,0))
		done
	done;;

let SetMap mapStruct v =
	mapStruct.map <- v;;

let SetMapCase mapStruct i j v =
	let (m,n) = GetMapDim mapStruct and map = GetMap mapStruct in
	map.(modulo i (m+2)).(modulo j (n+2)) <- v;;

let SetMapGumCount map count =
	map.gumCount <- count;;

let SetMapPacManInitPosition map (x,y) =
	map.pmInitPos <- (x,y);;

let SetMapGhostsInitPositions map vPos =
	map.ghInitPos <- vPos;;

let SetMapDim mapStruct (m,n) =
	let (m0,n0) = GetMapDim mapStruct in
	let map2 = make_matrix (m+2) (n+2) 0 in
	let map = GetMap mapStruct in
	
	for i=1 to m do
		for j=1 to n do
			if (i <= m0 && j <= n0) then map2.(i).(j) <- (if (map.(i).(j) = -2) then 0 else map.(i).(j))
		done
	done;

	SetMap mapStruct map2;;

let SetMapName map name =
	map.mapName <- name;;

let SetMapFile map file =
	map.mapFile <- file;;

let PrintMap mapStruct =
	let map = GetMap mapStruct in
	let (m,n) = GetMapDim mapStruct in

	for i=0 to m+1 do
		for j=0 to n+1 do
			if (map.(i).(j) >= 0 && map.(i).(j) < 10) then print_string " ";
			print_string ((string_of_int (map.(i).(j)))^" ");
		done;
		print_newline();
	done;;

let DeleteMap map =
	SetMap map [||];;
	
let CleanMap mapStruct =
	let (m,n) = GetMapDim mapStruct in
	let map = GetMap mapStruct in

	for i=0 to m+1 do
		for j=0 to n+1 do
			if (map.(i).(j) = -2) then map.(i).(j) <- 0
		done
	done;;

let rec CorrectMap mapStruct =
	let (n,m) = GetMapDim mapStruct and map = GetMap mapStruct in
	let count = ref 0. and corrected = ref false in
	for i=1 to n do
		for j=1 to m do
			if (IsCaseOK (map.(i).(j)) && (IsCaseHole (map.(i).(j-1)) || IsCaseHole (map.(i).(j+1)) || IsCaseHole (map.(i+1).(j)) || IsCaseHole (map.(i-1).(j))) ) then
			begin
				map.(i).(j) <- -2;
				corrected := true
			end;

			if (HasCaseGum (map.(i).(j))) then
			begin
				map.(i).(j) <- 16;
				count := (!count) +. 1.;

				if (HasCaseGum (map.(i-1).(j))) then map.(i).(j) <- map.(i).(j) lor 8;
				if (HasCaseGum (map.(i).(j+1))) then map.(i).(j) <- map.(i).(j) lor 4;
				if (HasCaseGum (map.(i+1).(j))) then map.(i).(j) <- map.(i).(j) lor 2;
				if (HasCaseGum (map.(i).(j-1))) then map.(i).(j) <- map.(i).(j) lor 1;

				if (map.(i).(j) land 8 > 0) then count := (!count) +. 0.5;
				if (map.(i).(j) land 4 > 0) then count := (!count) +. 0.5;
				if (map.(i).(j) land 2 > 0) then count := (!count) +. 0.5;
				if (map.(i).(j) land 1 > 0) then count := (!count) +. 0.5
			end
			else if (HasCaseBGum (map.(i).(j))) then count := (!count) +. 1.
		done
	done;

	if (!corrected) then CorrectMap mapStruct else
	begin
		SetMapGumCount mapStruct (int_of_float (!count));

		for i=0 to n+1 do
			map.(i).(m+1) <- map.(i).(0)
		done;

		for j=0 to m+1 do
			map.(n+1).(j) <- map.(0).(j)
		done;

		mapStruct.mapMatrix <- make_matrix (n+2) (m+2) ((n+2)*(m+2)+1,(0,0))
	end;;


let CreateCaseSurf mapStruct i2 j2 =
	let map = GetMap mapStruct and (n,m) = GetMapDim mapStruct in
	let i = modulo (i2-1) n +1 and j = modulo (j2-1) m +1 in
	let surf0 = NewSurf DIM DIM in

	(
		match map.(i).(j) with
		| -2 -> BlitSurf surf0 (GetVoidImg()) (MakeRect 0 0 0 0) (MakeRect 0 0 0 0)
		| 32 -> BlitSurf surf0 (GetBGumImg()) (MakeRect 0 0 0 0) (MakeRect 0 0 0 0)
		| -1 ->
			let surf1 = LoadImage ((GetSpritesDir())^(string_of_int (CaseNumber (map.(i-1).(j-1))))^(string_of_int (CaseNumber (map.(i-1).(j))))^(string_of_int (CaseNumber (map.(i).(j-1))))^".bmp") in
			let surf2 = LoadImage ((GetSpritesDir())^(string_of_int (CaseNumber (map.(i-1).(j+1))))^(string_of_int (CaseNumber (map.(i).(j+1))))^(string_of_int (CaseNumber (map.(i-1).(j))))^".bmp") in
			let surf3 = LoadImage ((GetSpritesDir())^(string_of_int (CaseNumber (map.(i+1).(j+1))))^(string_of_int (CaseNumber (map.(i+1).(j))))^(string_of_int (CaseNumber (map.(i).(j+1))))^".bmp") in
			let surf4 = LoadImage ((GetSpritesDir())^(string_of_int (CaseNumber (map.(i+1).(j-1))))^(string_of_int (CaseNumber (map.(i).(j-1))))^(string_of_int (CaseNumber (map.(i+1).(j))))^".bmp") in

			let rSurf2 = RotateSurfFast 1 surf2 in
			let rSurf3 = RotateSurfFast 2 surf3 in
			let rSurf4 = RotateSurfFast 3 surf4 in

			BlitSurf surf0 surf1  (MakeRect 0       0       0 0) (MakeRect 0 0 0 0);
			BlitSurf surf0 rSurf2 (MakeRect (DIM/2) 0       0 0) (MakeRect 0 0 0 0);
			BlitSurf surf0 rSurf3 (MakeRect (DIM/2) (DIM/2) 0 0) (MakeRect 0 0 0 0);
			BlitSurf surf0 rSurf4 (MakeRect 0       (DIM/2) 0 0) (MakeRect 0 0 0 0);

			DeleteSurf surf1;
			DeleteSurf surf2;
			DeleteSurf surf3;
			DeleteSurf surf4;

			DeleteSurf rSurf2;
			DeleteSurf rSurf3;
			DeleteSurf rSurf4;

		| _ ->
			BlitSurf surf0 (GetVoidImg()) (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);

			let gum = GetGumImg() in
			let (w,h) = DimSurf gum in
		
			if (map.(i).(j) land 16 = 16) then BlitSurf surf0 gum (MakeRect ((DIM-w)/2) ((DIM-h)/2) 0 0) (MakeRect 0 0 0 0);
			if (map.(i).(j) land 8 = 8)   then BlitSurf surf0 gum (MakeRect ((DIM-w)/2) (-h/2)      0 0) (MakeRect 0 0 0 0);
			if (map.(i).(j) land 4 = 4)   then BlitSurf surf0 gum (MakeRect (DIM-(w/2)) ((DIM-h)/2) 0 0) (MakeRect 0 0 0 0);
			if (map.(i).(j) land 2 = 2)   then BlitSurf surf0 gum (MakeRect ((DIM-w)/2) (DIM-(h/2)) 0 0) (MakeRect 0 0 0 0);
			if (map.(i).(j) land 1 = 1)   then BlitSurf surf0 gum (MakeRect (-w/2)      ((DIM-h)/2) 0 0) (MakeRect 0 0 0 0)
	);
	
	surf0;;
		
	

let CreateMapSurf mapStruct =
	CorrectMap mapStruct;
	let map = GetMap mapStruct in
	let (n,m) = (fst (GetMapDim mapStruct) +2, snd (GetMapDim mapStruct) +2) in
	let surf = NewSurf ((m-2)*DIM) ((n-2)*DIM) in

	for i=1 to (n-2) do
		for j=1 to (m-2) do
			let case = CreateCaseSurf mapStruct i j in
			BlitSurf surf case (MakeRect ((j-1)*DIM) ((i-1)*DIM) 0 0) (MakeRect 0 0 0 0);
			DeleteSurf case
		done
	done;

	surf;;


type FileVersion = {versionNumber:int; versionDate:int*int*int};;
let currentVersion = {versionNumber=1; versionDate=(17,12,2011)};;
let GetVersionNumber version =
	version.versionNumber;;

let SaveMap map =
	let folder = GetMapFile map in
	let fileName = if (nth_char folder (string_length folder -1) = `/`) then folder^(GetMapName map)^".map" else folder^"/"^(GetMapName map)^".map" in
	try
		let file = open_out_bin fileName in
		output_value file currentVersion;
		output_value file map;
		close_out file
	with _ -> failwith "Unable to save Map";;


let LoadMapV1 file =
	(input_value file : Map);;

let mapLoadingFunctions =
[|
	LoadMapV1;
	LoadMapV1
|] ;;

let LoadMap fileName =
	try
		let file = open_in_bin fileName in
		let version = (input_value file : FileVersion) in
		let map = mapLoadingFunctions.(GetVersionNumber version) file in
		close_in file;
		map
	with _ -> failwith "Unable to load Map";;


let LoadDefaultCampaign() =
	let mapList =
	[|
		LoadDefaultMap() ;
		LoadMap "./Files/Maps/System/Maya.map" ;
		LoadMap "./Files/Maps/System/Spiral.map" ;
		LoadMap "./Files/Maps/System/Google.map" ;
		LoadMap "./Files/Maps/System/Championship.map" ;
		LoadMap "./Files/Maps/System/World_Domination.map" ;
		LoadMap "./Files/Maps/System/Championship_2.map"
	|] in
	{mapList=mapList; campaignName="Official"};;


(*PacMan*)

let LoadPacManDefault() =
	let anim = LoadAnim [(GetSpritesDir())^"pm1.png" ; (GetSpritesDir())^"pm2.png" ; (GetSpritesDir())^"pm3.png" ; (GetSpritesDir())^"pm4.png" ; (GetSpritesDir())^"pm5.png" ; (GetSpritesDir())^"pm6.png"] 100 in
	PlayAnim anim;
	let v = make_vect 4 anim in
	v.(1) <- ApplyFunctionToAnim (RotateSurfFast 1) anim;
	v.(2) <- ApplyFunctionToAnim (RotateSurfFast 2) anim;
	v.(3) <- ApplyFunctionToAnim (RotateSurfFast 3) anim;

	{pmAnims=v ; pmDirection=LEFT ; pmNDir=LEFT ; pmTime=float_of_int (SDLTime()) /. 1000. ; pmPosition=(0.,0.) ; points=0 ; nbLives=3};;

let GetPacManAnims pm =
	pm.pmAnims;;

let GetPacManDirection pm =
	pm.pmDirection;;

let DeletePacMan pm =
	let n = vect_length pm.pmAnims in
	for i=0 to (n-1) do
		DeleteAnim (pm.pmAnims.(i))
	done;
	pm.pmAnims <- [||];;

let ResetPacMan pm =
	do_vect ResetAnim (GetPacManAnims pm);;

let GetPacManSurface pm =
	let pma = GetPacManAnims pm in
	if (vect_length pma < 4) then failwith "GetPacManSurface : PacMan not loaded yet";
	
	match GetPacManDirection pm with
	| RIGHT -> GetAnimSurf (pma.(0))
	| DOWN -> GetAnimSurf (pma.(1))
	| LEFT -> GetAnimSurf (pma.(2))
	| UP -> GetAnimSurf (pma.(3));;

let GetPacManTime pm =
	pm.pmTime;;

let GetPacManNextDirection pm =
	pm.pmNDir;;

let GetPacManPosition pm =
	pm.pmPosition;;

let GetPacManPoints pm =
	pm.points;;

let GetPacManLives pm =
	pm.nbLives;;

let SetPacManDirection pm dir =
	pm.pmDirection <- dir;;

let SetPacManNextDirection pm dir =
	pm.pmNDir <- dir;;

let SetPacManPosition pm coord =
	pm.pmPosition <- coord;;

let SetPacManTime pm time =
	pm.pmTime <- time;;

let SetPacManPoints pm points =
	pm.points <- points;;

let SetPacManLives pm lives =
	pm.nbLives <- lives;;

let AddPacManPoints pm pts =
	let points = GetPacManPoints pm in
	if (points/10000 < (points+pts)/10000) then SetPacManLives pm (GetPacManLives pm +1);
	SetPacManPoints pm (points+pts);;

let XToLeftColumn x =
	int_of_float ( x /. (float_of_int DIM) +. 1. );;

let XToRightColumn x =
	int_of_float ( (x +. (float_of_int DIM) -. 1.) /. (float_of_int DIM) +. 1. );;

let YToUpLine y =
	int_of_float ( y /. (float_of_int DIM) +. 1. );;

let YToDownLine y =
	int_of_float ( (y +. (float_of_int DIM) -. 1.) /. (float_of_int DIM) +. 1. );;

let ColumnToX c =
	float_of_int ((c-1)*DIM);;

let LineToY l =
	float_of_int ((l-1)*DIM);;

let GetPacManCoord pm mapStruct =
	let (n,m) = GetMapDim mapStruct in
	let (x,y) =  GetPacManPosition pm and dir = GetPacManDirection pm in
	match dir with
		| LEFT -> (modulo (YToUpLine y -1) n +1, modulo (XToLeftColumn x -1) m +1)
		| RIGHT -> (modulo (YToUpLine y -1) n +1, modulo (XToRightColumn x -1) m +1)
		| UP -> (modulo (YToUpLine y -1) n +1, modulo (XToLeftColumn x -1) m +1)
		| DOWN -> (modulo (YToDownLine y -1) n +1, modulo (XToLeftColumn x -1) m +1);;

	

let BlitSprite spr coord surf =
	let (x,y) = coord and (w,h) = DimSurf surf in
	let ix = int_of_float x and iy = int_of_float y in
	BlitSurf surf spr (MakeRect ix iy 0 0) (MakeRect 0 0 0 0);
	if (x >. (float_of_int (w-DIM))) then BlitSurf surf spr (MakeRect (ix-w) iy 0 0) (MakeRect 0 0 0 0);
	if (y >. (float_of_int (h-DIM))) then BlitSurf surf spr (MakeRect ix (iy-h) 0 0) (MakeRect 0 0 0 0);;

let BlitPacMan pm surf =
	BlitSprite (GetPacManSurface pm) (GetPacManPosition pm) surf;;
	
let UnblitPacMan pm map surf =
	let dir = GetPacManDirection pm and (i1,j1) = GetPacManCoord pm map in
	SetPacManDirection pm (OppositeDir dir);
	let (i2,j2) = GetPacManCoord pm map in
	SetPacManDirection pm dir;
	
	let spr1 = CreateCaseSurf map i1 j1 in
	let spr2 = CreateCaseSurf map i2 j2 in
	
	BlitSprite spr1 (ColumnToX j1, LineToY i1) surf;
	BlitSprite spr2 (ColumnToX j2, LineToY i2) surf;
	
	DeleteSurf spr1;
	DeleteSurf spr2;;





(*Ghosts*)

let LoadGhostDefault typ =
	let name =
		match typ with
		| GHOST_BLUE -> "bghost"
		| GHOST_RED -> "rghost"
		| GHOST_ORANGE -> "oghost"
		| GHOST_PINK -> "pghost"
	in
	
	let v =
	[|
		LoadAnim [(GetSpritesDir())^name^"right1.png" ; (GetSpritesDir())^name^"right2.png"] 100 ;
		LoadAnim [(GetSpritesDir())^name^"down1.png" ; (GetSpritesDir())^name^"down2.png"] 100 ;
		LoadAnim [(GetSpritesDir())^name^"left1.png" ; (GetSpritesDir())^name^"left2.png"] 100 ;
		LoadAnim [(GetSpritesDir())^name^"up1.png" ; (GetSpritesDir())^name^"up2.png"] 100;
		LoadAnim [(GetSpritesDir())^"sghost1.png" ; (GetSpritesDir())^"sghost2.png"] 100;
		LoadAnim [(GetSpritesDir())^"sghost1.png" ; (GetSpritesDir())^"sghost2.png" ; (GetSpritesDir())^"sghost1.png" ; (GetSpritesDir())^"sghost2.png" ;
				(GetSpritesDir())^"swghost1.png" ; (GetSpritesDir())^"swghost2.png" ; (GetSpritesDir())^"swghost1.png" ; (GetSpritesDir())^"swghost2.png"] 100;
		LoadAnim [(GetSpritesDir())^"ghosteyesright.png"] 100 ;
		LoadAnim [(GetSpritesDir())^"ghosteyesdown.png"] 100 ;
		LoadAnim [(GetSpritesDir())^"ghosteyesleft.png"] 100 ;
		LoadAnim [(GetSpritesDir())^"ghosteyesup.png"] 100
	|] in
		
	PlayAnim (v.(0)); PlayAnim (v.(1));
	PlayAnim (v.(2)); PlayAnim (v.(3));
	PlayAnim (v.(4)); PlayAnim (v.(5));
	
	{gAnims=v ; gDirection=LEFT ; gTime=float_of_int (SDLTime()) /. 1000. ; gPosition=(0.,0.) ; gState=GHOST_HUNGRY ; gType = typ ; gScaredTime = 0};;

let LoadAllGhostsDefault() =
	[| LoadGhostDefault GHOST_BLUE ; LoadGhostDefault GHOST_RED ; LoadGhostDefault GHOST_ORANGE ; LoadGhostDefault GHOST_PINK |];;

let DeleteGhost gh =
	let n = vect_length (gh.gAnims) in
	for i=0 to (n-1) do
		DeleteAnim (gh.gAnims.(i))
	done;
	gh.gAnims <- [||];;

let GetGhostScaredTime gh =
	gh.gScaredTime;;

let GetGhostState gh =
	gh.gState;;

let GetGhostSurface gh =
	if (vect_length (gh.gAnims) < 10) then failwith "GetGhostSurface : Ghost not loaded yet";
	
	match GetGhostState gh with
	| GHOST_HUNGRY ->
		(match gh.gDirection with
		| RIGHT -> GetAnimSurf (gh.gAnims.(0))
		| DOWN -> GetAnimSurf (gh.gAnims.(1))
		| LEFT -> GetAnimSurf (gh.gAnims.(2))
		| UP -> GetAnimSurf (gh.gAnims.(3));  )
	| GHOST_SCARED ->
		if ((SDLTime()) - (GetGhostScaredTime gh) <= 5000) then GetAnimSurf (gh.gAnims.(4)) else GetAnimSurf (gh.gAnims.(5))
	| GHOST_DEAD ->
		match gh.gDirection with
		| RIGHT -> GetAnimSurf (gh.gAnims.(6))
		| DOWN -> GetAnimSurf (gh.gAnims.(7))
		| LEFT -> GetAnimSurf (gh.gAnims.(8))
		| UP -> GetAnimSurf (gh.gAnims.(9));;

let GetGhostTime gh =
	gh.gTime;;

let GetGhostDirection gh =
	gh.gDirection;;

let GetGhostPosition gh =
	gh.gPosition;;

let GetGhostType gh =
	gh.gType;;

let SetGhostTime gh time =
	gh.gTime <- time;;

let SetGhostDirection gh dir =
	gh.gDirection <- dir;;

let SetGhostPosition gh pos =
	gh.gPosition <- pos;;

let SetGhostState gh state=
	gh.gState <- state;;

let SetGhostType gh typ=
	gh.gType <- typ;;

let SetGhostScaredTime gh t =
	gh.gScaredTime <- t;;

let GetGhostCoord gh mapStruct =
	let (n,m) = GetMapDim mapStruct in
	let (x,y) =  GetGhostPosition gh and dir = GetGhostDirection gh in
	match dir with
		| LEFT -> (modulo (YToUpLine y -1) n +1, modulo (XToLeftColumn x -1) m +1)
		| RIGHT -> (modulo (YToUpLine y -1) n +1, modulo (XToRightColumn x -1) m +1)
		| UP -> (modulo (YToUpLine y -1) n +1, modulo (XToLeftColumn x -1) m +1)
		| DOWN -> (modulo (YToDownLine y -1) n +1, modulo (XToLeftColumn x -1) m +1);;

let SetGhostsPositions vGh vPos =
	let n = max (vect_length vGh) (vect_length vPos) in
	for i=0 to (n-1)
	do
		SetGhostPosition (vGh.(i)) (vPos.(i))
	done;;

let BlitGhosts vGh surf =
	let n = vect_length vGh in
	for i=0 to (n-1)
	do
		BlitSprite (GetGhostSurface (vGh.(i))) (GetGhostPosition (vGh.(i))) surf
	done;;
	
let UnblitGhosts vGh map surf =
	let n = vect_length vGh in
	for i=0 to (n-1)
	do
		let dir = GetGhostDirection (vGh.(i)) and (i1,j1) = GetGhostCoord (vGh.(i)) map in
		SetGhostDirection (vGh.(i)) (OppositeDir dir);
		let (i2,j2) = GetGhostCoord (vGh.(i)) map in
		SetGhostDirection (vGh.(i)) dir;
	
		let spr1 = CreateCaseSurf map i1 j1 in
		let spr2 = CreateCaseSurf map i2 j2 in
	
		BlitSprite spr1 (ColumnToX j1, LineToY i1) surf;
		BlitSprite spr2 (ColumnToX j2, LineToY i2) surf;
	
		DeleteSurf spr1;
		DeleteSurf spr2
	done;;
	

let ChangeGhostDirection gh mapStruct dir =
	let dir0 = GetGhostDirection gh and map = GetMap mapStruct in
	let (x,y) = GetGhostPosition gh and (l,c) = GetGhostCoord gh mapStruct in

	match (dir,dir0) with
	| (UP,DOWN) -> SetGhostDirection gh UP
	| (DOWN,UP) -> SetGhostDirection gh DOWN
	| (RIGHT,LEFT) -> SetGhostDirection gh RIGHT
	| (LEFT,RIGHT) -> SetGhostDirection gh LEFT
	| (UP,_) -> if (modulo (int_of_float x) DIM = 0 && IsCaseOK (map.(l-1).(c))) then SetGhostDirection gh UP
	| (DOWN,_) -> if (modulo (int_of_float x) DIM = 0 && IsCaseOK (map.(l+1).(c))) then SetGhostDirection gh DOWN
	| (RIGHT,_) -> if (modulo (int_of_float x) DIM = 0 && IsCaseOK (map.(l).(c+1))) then SetGhostDirection gh RIGHT
	| (LEFT,_) -> if (modulo (int_of_float x) DIM = 0 && IsCaseOK (map.(l).(c-1))) then SetGhostDirection gh LEFT;;


let ScareGhost gh =
	if (GetGhostState gh <> GHOST_DEAD) then
	begin
		SetGhostState gh GHOST_SCARED;
		SetGhostScaredTime gh (SDLTime())
	end;;



(*PacMan moves*)
let PointCount pm vGh mapStruct =
	let map = GetMap mapStruct in
	let (x,y) = GetPacManPosition pm and dir = GetPacManDirection pm and ndir = GetPacManNextDirection pm in
	let (i,j) = GetPacManCoord pm mapStruct in

	let prevCase = map.(i).(j) in
	if (modulo (int_of_float x) (DIM/2) = 0 && modulo (int_of_float y) (DIM/2) = 0) then
	begin
		if (modulo (int_of_float x) DIM = 0 && modulo (int_of_float y) DIM = 0) then
		begin
			match (ndir,dir) with
			| (LEFT,_) when (IsCaseOK (map.(i).(j-1))) ->
				if (map.(i).(j) land 1 = 1) then map.(i).(j) <- map.(i).(j) - 1;
		 		if (map.(i).(j-1) land 4 = 4) then map.(i).(j-1) <- map.(i).(j-1) - 4
			| (DOWN,_) when (IsCaseOK (map.(i+1).(j))) ->
				if (map.(i).(j) land 2 = 2) then map.(i).(j) <- map.(i).(j) - 2;
				if (map.(i+1).(j) land 8 = 8) then map.(i+1).(j) <- map.(i+1).(j) - 8
			| (RIGHT,_) when (IsCaseOK (map.(i).(j+1))) ->
				if (map.(i).(j) land 4 = 4) then map.(i).(j) <- map.(i).(j) - 4;
				if (map.(i).(j+1) land 1 = 1) then map.(i).(j+1) <- map.(i).(j+1) - 1
			| (UP,_) when (IsCaseOK (map.(i-1).(j))) ->
				if (map.(i).(j) land 8 = 8) then map.(i).(j) <- map.(i).(j) - 8;
				if (map.(i-1).(j) land 2 = 2) then map.(i-1).(j) <- map.(i-1).(j) - 2
			| (_,LEFT) ->
				if (map.(i).(j) land 1 = 1) then map.(i).(j) <- map.(i).(j) - 1;
		 		if (IsCaseOK (map.(i).(j-1)) && map.(i).(j-1) land 4 = 4) then map.(i).(j-1) <- map.(i).(j-1) - 4
			| (_,DOWN) ->
				if (map.(i).(j) land 2 = 2) then map.(i).(j) <- map.(i).(j) - 2;
				if (IsCaseOK (map.(i+1).(j)) && map.(i+1).(j) land 8 = 8) then map.(i+1).(j) <- map.(i+1).(j) - 8
			| (_,RIGHT) ->
				if (map.(i).(j) land 4 = 4) then map.(i).(j) <- map.(i).(j) - 4;
				if (IsCaseOK (map.(i).(j+1)) && map.(i).(j+1) land 1 = 1) then map.(i).(j+1) <- map.(i).(j+1) - 1
			| (_,UP) ->
				if (map.(i).(j) land 8 = 8) then map.(i).(j) <- map.(i).(j) - 8;
				if (IsCaseOK (map.(i-1).(j)) && map.(i-1).(j) land 2 = 2) then map.(i-1).(j) <- map.(i-1).(j) - 2
		end
		else
		begin
			if (dir <> OppositeDir ndir && map.(i).(j) land 16 = 16) then map.(i).(j) <- map.(i).(j) - 16;
			if (dir <> OppositeDir ndir && map.(i).(j) land 32 = 32) then
			begin
				map.(i).(j) <- map.(i).(j) - 32;
				do_vect ScareGhost vGh
			end
		end
	end;

	if (prevCase <> map.(i).(j)) then
	begin
		PlayFSound (GetChompSound());
		let points = GetPacManPoints pm in
		AddPacManPoints pm 10;
		SetMapGumCount mapStruct (GetMapGumCount mapStruct -1)
	end;;
		

let MovePacMan pm vGh mapStruct =
	let map = GetMap mapStruct in
	let rec MPM_rec move =
	
		PointCount pm vGh mapStruct;
		if (move <. 1.) then ()
		else
			let (x0,y0) = GetPacManPosition pm and (n,m) = GetMapDim mapStruct in
			let x = mod_float x0 (m*DIM) and y = mod_float y0 (n*DIM) in
			let dir = GetPacManDirection pm and ndir = GetPacManNextDirection pm in
			let move2 = ref 0. in

			match dir with
			| LEFT ->
				let c = XToLeftColumn (x -. 1.) and l = YToUpLine y in
				let nx2 = ColumnToX c in
				let nx = if (nx2 +. float_of_int (DIM/2) <. x) then nx2 +. float_of_int (DIM/2) else nx2 in
				let x2 = max (x -. move) nx in

				if (IsCaseOK (map.(l).(c))) then
				begin
					SetPacManPosition pm (x2,y);
					move2 := x -. x2
				end;
				
				let (x3,_) = GetPacManPosition pm in
				let c2 = XToLeftColumn x3 in
				if (modulo (int_of_float x3) DIM = 0 && ((IsCaseOK (map.(l+1).(c2)) && ndir = DOWN) || (IsCaseOK (map.(l-1).(c2)) && ndir = UP))) then
					SetPacManDirection pm ndir
				else
				begin
					if ((!move2) = 0.) then move2 := move
				end;

				MPM_rec (move -. (!move2))

			| RIGHT ->
				let c = XToRightColumn (x +. 1.) and l = YToUpLine y in
				let nx2 = ColumnToX c in
				let nx = if (nx2 -. float_of_int (DIM/2) >. x) then nx2 -. float_of_int (DIM/2) else nx2 in
				let x2 = min (x +. move) nx in
		
				if (IsCaseOK (map.(l).(c))) then
				begin
					SetPacManPosition pm (x2,y);
					move2 := x2 -. x
				end;
		
				let (x3,_) = GetPacManPosition pm in
				let c2 = XToRightColumn x3 in
				if (modulo (int_of_float x3) DIM = 0 && ((IsCaseOK (map.(l+1).(c2)) && ndir = DOWN) || (IsCaseOK (map.(l-1).(c2)) && ndir = UP))) then
					SetPacManDirection pm ndir
				else
				begin
					if ((!move2) = 0.) then move2 := move
				end;

				MPM_rec (move -. (!move2))

			| UP ->
				let l = YToUpLine (y -. 1.) and c = XToLeftColumn x in
				let ny2 = LineToY l in
				let ny = if (ny2 +. float_of_int (DIM/2) <. y) then ny2 +. float_of_int (DIM/2) else ny2 in
				let y2 = max (y -. move) ny in
		
				if (IsCaseOK (map.(l).(c))) then
				begin
					SetPacManPosition pm (x,y2);
					move2 := y -. y2
				end;
		
				let (_,y3) = GetPacManPosition pm in
				let l2 = YToUpLine y3 in
				if (modulo (int_of_float y3) DIM = 0 && ((IsCaseOK (map.(l2).(c+1)) && ndir = RIGHT) || (IsCaseOK (map.(l2).(c-1)) && ndir = LEFT))) then
					SetPacManDirection pm ndir
				else
				begin
					if ((!move2) = 0.) then move2 := move
				end;

				MPM_rec (move -. (!move2))

			| DOWN ->
				let l = YToDownLine (y +. 1.) and c = XToLeftColumn x in
				let ny2 = LineToY l in
				let ny = if (ny2 -. float_of_int (DIM/2) >. y) then ny2 -. float_of_int (DIM/2) else ny2 in
				let y2 = min (y +. move) ny in
		
				if (IsCaseOK (map.(l).(c))) then
				begin
					SetPacManPosition pm (x,y2);
					move2 := y2 -. y
				end;
		
				let (_,y3) = GetPacManPosition pm in
				let l2 = YToDownLine y3 in
				if (modulo (int_of_float y3) DIM = 0 && ((IsCaseOK (map.(l2).(c+1)) && ndir = RIGHT) || (IsCaseOK (map.(l2).(c-1)) && ndir = LEFT))) then
					SetPacManDirection pm ndir
				else
				begin
					if ((!move2) = 0.) then move2 := move
				end;

				MPM_rec (move -. (!move2))
	in

	let t = GetPacManTime pm and t2 = (float_of_int (SDLTime())) /. 1000. in
	let move = max ((GetSpeed()) *. (t2 -. t)) 1. in
	MPM_rec move;
	SetPacManTime pm t2;;
	

let ChangePacManDirection pm dir =
	let dir0 = GetPacManDirection pm in
	(match (dir,dir0) with
	| (UP,DOWN) -> SetPacManDirection pm UP
	| (DOWN,UP) -> SetPacManDirection pm DOWN
	| (RIGHT,LEFT) -> SetPacManDirection pm RIGHT
	| (LEFT,RIGHT) -> SetPacManDirection pm LEFT
	| _ -> () );

	SetPacManNextDirection pm dir;;


(*IA*)

let GetMapCaseDir map (l,c) dir =
	match dir with
	| UP -> GetMapCase map (l-1,c)
	| DOWN -> GetMapCase map (l+1,c)
	| RIGHT -> GetMapCase map (l,c+1)
	| LEFT -> GetMapCase map (l,c-1);;


let FindWay start finish mapStruct dir weightFn =
	let (m,n) = GetMapDim mapStruct in
	let tab = GetMapMatrix mapStruct and max = (m+2)*(n+2)+1 in
	let map = GetMap mapStruct in
	
	let rec FW_rec pt =
		if (pt = finish) then ()
		else
			let (l,c) = pt in
			
			if (IsCaseOK (map.(l+1).(c)) && l+1 <= m) then
			begin
				let d = weightFn pt (l+1,c) tab start finish in
				if (fst (tab.(l+1).(c)) > d) then tab.(l+1).(c) <- (d,pt)
			end;
			if (IsCaseOK (map.(l-1).(c)) && l-1 > 0) then
			begin
				let d = weightFn pt (l-1,c) tab start finish in
				if (fst (tab.(l-1).(c)) > d) then tab.(l-1).(c) <- (d,pt)
			end;
			if (IsCaseOK (map.(l).(c+1)) && c+1 <= n) then
			begin
				let d = weightFn pt (l,c+1) tab start finish in
				if (fst (tab.(l).(c+1)) > d) then tab.(l).(c+1) <- (d,pt)
			end;
			if (IsCaseOK (map.(l).(c-1)) && c-1 > 0) then
			begin
				let d = weightFn pt (l,c-1) tab start finish in
				if (fst (tab.(l).(c-1)) > d) then tab.(l).(c-1) <- (d,pt)
			end;

			let (_,prevPt) = tab.(l).(c) in
			tab.(l).(c) <- (-1,prevPt);

			let min = ref max and ptMin = ref (1,1) in
			for i=1 to m do
				for j=1 to n do
					if (fst (tab.(i).(j)) >= 0 && (!min) > fst (tab.(i).(j))) then
					begin
						min := fst (tab.(i).(j));
						ptMin := (i,j)
					end;
					if (fst (tab.(i).(j)) = -2) then
						let (_,ppt) = tab.(i).(j) in
						tab.(i).(j) <- (max,ppt)
				done
			done;

			if ((!min) >= max) then failwith "No path found !";

			FW_rec (!ptMin);
	in

	let rec BuildWay (l,c) =
		if (l = fst start && c = snd start) then [(l,c)]
		else ( BuildWay (snd (tab.(l).(c))) )@[(l,c)];
	in

	try
		ResetMap mapStruct;

		let (l,c) = start in
		tab.(l).(c) <- (0,(0,0));
		
		(match dir with
		| DOWN -> tab.(l-1).(c) <- (-2,(0,0))
		| UP -> tab.(l+1).(c) <- (-2,(0,0))
		| LEFT -> tab.(l).(c+1) <- (-2,(0,0))
		| RIGHT -> tab.(l).(c-1) <- (-2,(0,0)) );

		try
			FW_rec start;
			BuildWay finish
		with Failure _ ->
		
		ResetMap mapStruct;
		tab.(l).(c) <- (0,(0,0));
		
		FW_rec start;
		BuildWay finish

	with Failure _ -> [];;
		
			
			
let SwitchVect v i j =
	let x = v.(i) in
	v.(i) <- v.(j);
	v.(j) <- x;;

let GetPacManNextCoord pm map =
	let (m,n) = GetMapDim map in
	let rec GPMNC_rec prevCoord (l,c) =
		if (not IsCaseOK (GetMapCase map (l,c))) then prevCoord
		else
			let cList = ref [] in
			let l2 = modulo l m +1 in
			if (IsCaseOK (GetMapCase map (l2,c)) && (l2,c) <> prevCoord) then cList := (l2,c)::(!cList);
			let l2 = modulo (l-2) m +1 in
			if (IsCaseOK (GetMapCase map (l2,c)) && (l2,c) <> prevCoord) then cList := (l2,c)::(!cList);
			let c2 = modulo c n +1 in
			if (IsCaseOK (GetMapCase map (l,c2)) && (l,c2) <> prevCoord) then cList := (l,c2)::(!cList);
			let c2 = modulo (c-2) n +1 in
			if (IsCaseOK (GetMapCase map (l,c2)) && (l,c2) <> prevCoord) then cList := (l,c2)::(!cList);
			
			match (!cList) with
			| [] -> (l,c)
			| [x] -> GPMNC_rec (l,c) x
			| _ -> (l,c);
	in
	
	let (l,c) = GetPacManCoord pm map in
	let lup = modulo (l-2) m +1 and ldown = modulo l m +1 in
	let cleft = modulo (c-2) n +1 and cright = modulo c n +1 in
	match GetPacManDirection pm with
	| UP -> if (IsCaseOK (GetMapCase map (lup,c))) then GPMNC_rec (l,c) (lup,c) else GPMNC_rec (ldown,c) (l,c)
	| DOWN -> if (IsCaseOK (GetMapCase map (ldown,c))) then GPMNC_rec (l,c) (ldown,c) else GPMNC_rec (lup,c) (l,c)
	| RIGHT -> if (IsCaseOK (GetMapCase map (l,cright))) then GPMNC_rec (l,c) (l,cright) else GPMNC_rec (l,cleft) (l,c)
	| LEFT -> if (IsCaseOK (GetMapCase map (l,cleft))) then GPMNC_rec (l,c) (l,cleft) else GPMNC_rec (l,cright) (l,c);;

let GetPacManPrevCoord pm map =
	let dir = GetPacManDirection pm in
	SetPacManDirection pm (OppositeDir dir);
	let coord = GetPacManNextCoord pm map in
	SetPacManDirection pm dir;
	coord;;

let IsIntersection (l,c) map =
	(IsCaseOK (GetMapCase map (l+1,c)) || IsCaseOK (GetMapCase map (l-1,c))) && (IsCaseOK (GetMapCase map (l,c+1)) || IsCaseOK (GetMapCase map (l,c-1)));;
	

let ManageGhosts map pm vGhost =
	let ghost = vGhost.(0) in
	let (gl,gc) = GetGhostCoord ghost map and gState = GetGhostState ghost in

	let (tx,ty) = (GetMapGhostsInitPositions map).(0) in
	if ( (SDLTime() - GetGhostScaredTime ghost > 7500 && gState = GHOST_SCARED) || ((gl,gc) = (YToUpLine ty, XToLeftColumn tx) && gState = GHOST_DEAD) ) then SetGhostState ghost GHOST_HUNGRY;
		
	match gState with
	| GHOST_HUNGRY ->
	(
		match GetGhostType ghost with
		| GHOST_RED ->
			let (pml,pmc) =
				let pmnc = GetPacManNextCoord pm map in
				if (pmnc = (gl,gc)) then GetPacManCoord pm map else pmnc
			in
			let weightFn pt (l,c) tab start (fl,fc) =
				(l-fl)*(l-fl) + (c-fc)*(c-fc);
			in
			(match FindWay (gl,gc) (pml,pmc) map (GetGhostDirection ghost) weightFn with
			| _::(l,c)::_ ->
				if (l > gl) then ChangeGhostDirection ghost map DOWN
				else if (l < gl) then ChangeGhostDirection ghost map UP
				else if (c > gc) then ChangeGhostDirection ghost map RIGHT
				else ChangeGhostDirection ghost map LEFT
			| _ -> ()  );

		| GHOST_BLUE ->
			let (pml,pmc) =
				let pmnc = GetPacManNextCoord pm map in
				if (pmnc = (gl,gc)) then GetPacManCoord pm map else pmnc
			in
			let weightFn pt (l,c) tab start (fl,fc) =
				(l-fl)*(l-fl) + (c-fc)*(c-fc);
			in
			(match FindWay (gl,gc) (pml,pmc) map (GetGhostDirection ghost) weightFn with
			| _::(l,c)::_ ->
				if (l > gl) then ChangeGhostDirection ghost map DOWN
				else if (l < gl) then ChangeGhostDirection ghost map UP
				else if (c > gc) then ChangeGhostDirection ghost map RIGHT
				else ChangeGhostDirection ghost map LEFT
			| _ -> ()  );

			
		| GHOST_ORANGE ->
			let (pml,pmc) =
				let pmnc = GetPacManPrevCoord pm map in
				if (pmnc = (gl,gc)) then GetPacManCoord pm map else pmnc
			in
			let weightFn (l,c) pt tab start finish =
				fst (tab.(l).(c)) +1;
			in
			(match FindWay (gl,gc) (pml,pmc) map (GetGhostDirection ghost) weightFn with
			| _::(l,c)::_ ->
				if (l > gl) then ChangeGhostDirection ghost map DOWN
				else if (l < gl) then ChangeGhostDirection ghost map UP
				else if (c > gc) then ChangeGhostDirection ghost map RIGHT
				else ChangeGhostDirection ghost map LEFT
			| _ -> ()  );

		| GHOST_PINK ->
			let (pml,pmc) =
				let pmnc = GetPacManPrevCoord pm map in
				if (pmnc = (gl,gc)) then GetPacManCoord pm map else pmnc
			in
			let weightFn (l,c) pt tab start finish =
				fst (tab.(l).(c)) +1;
			in
			(match FindWay (gl,gc) (pml,pmc) map (GetGhostDirection ghost) weightFn with
			| _::(l,c)::_ ->
				if (l > gl) then ChangeGhostDirection ghost map DOWN
				else if (l < gl) then ChangeGhostDirection ghost map UP
				else if (c > gc) then ChangeGhostDirection ghost map RIGHT
				else ChangeGhostDirection ghost map LEFT
			| _ -> ()  );
	);

	| GHOST_SCARED ->
		let (pml,pmc) = GetPacManCoord pm map and v = [| DOWN ; RIGHT ; UP ; LEFT |] in
		if (gl < pml) then SwitchVect v 0 2;
		if (gc < pmc) then SwitchVect v 1 3;
		if (gc = pmc) then
		begin
			SwitchVect v 0 1;
			SwitchVect v 2 3
		end;

		let dir = OppositeDir (GetGhostDirection ghost) in
		if (v.(0) = dir) then SwitchVect v 0 1;
		if (v.(1) = dir) then SwitchVect v 1 2;
		if (v.(2) = dir) then SwitchVect v 2 3;
		
		if (IsCaseOK (GetMapCaseDir map (gl,gc) (v.(0)))) then ChangeGhostDirection ghost map (v.(0))
		else if (IsCaseOK (GetMapCaseDir map (gl,gc) (v.(1)))) then ChangeGhostDirection ghost map (v.(1))
		else if (IsCaseOK (GetMapCaseDir map (gl,gc) (v.(2)))) then ChangeGhostDirection ghost map (v.(2))
		else if (IsCaseOK (GetMapCaseDir map (gl,gc) (v.(3)))) then ChangeGhostDirection ghost map (v.(3));

	| GHOST_DEAD ->
		let (x,y) = (GetMapGhostsInitPositions map).(0) in
		let (l,c) = (YToUpLine y, XToLeftColumn x) in
		let weightFn (l,c) pt tab start finish =
				fst (tab.(l).(c)) +1;
		in
		match FindWay (gl,gc) (l,c) map (GetGhostDirection ghost) weightFn with
		| _::(l,c)::_ ->
			if (l > gl) then ChangeGhostDirection ghost map DOWN
			else if (l < gl) then ChangeGhostDirection ghost map UP
			else if (c > gc) then ChangeGhostDirection ghost map RIGHT
			else ChangeGhostDirection ghost map LEFT
		| _ -> ();;



let MoveGhosts vGh mapStruct pm =
	let map = GetMap mapStruct in
	let rec MPM_rec move v =
		let gh = v.(0) in
		
		if (move <. 1.) then ()
		else
			let (x0,y0) = GetGhostPosition gh and (n,m) = GetMapDim mapStruct in
			let x = mod_float x0 (m*DIM) and y = mod_float y0 (n*DIM) in
			let dir = GetGhostDirection gh in
			let move2 = ref 0. in

			match dir with
			| LEFT ->
				let c = XToLeftColumn (x -. 1.) and l = YToUpLine y in
				let nx = ColumnToX c in
				let x2 = max (x -. move) nx in

				if (IsCaseOK (map.(l).(c))) then
				begin
					SetGhostPosition gh (x2,y);
					move2 := x -. x2
				end;
				
				let (x3,_) = GetGhostPosition gh and (gl,gc) = GetGhostCoord gh mapStruct in
				if (modulo (int_of_float x3) DIM = 0 && (IsIntersection (gl,gc) mapStruct || (!move2) = 0.)) then ManageGhosts mapStruct pm v;
				
				if (GetGhostDirection gh = LEFT && (!move2) = 0.) then move2 := move;

				MPM_rec (move -. (!move2)) v

			| RIGHT ->
				let c = XToRightColumn (x +. 1.) and l = YToUpLine y in
				let nx = ColumnToX c in
				let x2 = min (x +. move) nx in
		
				if (IsCaseOK (map.(l).(c))) then
				begin
					SetGhostPosition gh (x2,y);
					move2 := x2 -. x
				end;
		
				let (x3,_) = GetGhostPosition gh and (gl,gc) = GetGhostCoord gh mapStruct in
				if (modulo (int_of_float x3) DIM = 0 && (IsIntersection (gl,gc) mapStruct || (!move2) = 0.)) then ManageGhosts mapStruct pm v;

				if (GetGhostDirection gh = RIGHT && (!move2) = 0.) then move2 := move;


				MPM_rec (move -. (!move2)) v

			| UP ->
				let l = YToUpLine (y -. 1.) and c = XToLeftColumn x in
				let ny = LineToY l in
				let y2 = max (y -. move) ny in
		
				if (IsCaseOK (map.(l).(c))) then
				begin
					SetGhostPosition gh (x,y2);
					move2 := y -. y2
				end;
		
				let (_,y3) = GetGhostPosition gh and (gl,gc) = GetGhostCoord gh mapStruct in
				if (modulo (int_of_float y3) DIM = 0 && (IsIntersection (gl,gc) mapStruct || (!move2) = 0.)) then ManageGhosts mapStruct pm v;

				if (GetGhostDirection gh = UP && (!move2) = 0.) then move2 := move;

				MPM_rec (move -. (!move2)) v

			| DOWN ->
				let l = YToDownLine (y +. 1.) and c = XToLeftColumn x in
				let ny = LineToY l in
				let y2 = min (y +. move) ny in
		
				if (IsCaseOK (map.(l).(c))) then
				begin
					SetGhostPosition gh (x,y2);
					move2 := y2 -. y
				end;
		
				let (_,y3) = GetGhostPosition gh and (gl,gc) = GetGhostCoord gh mapStruct in
				if (modulo (int_of_float y3) DIM = 0 && (IsIntersection (gl,gc) mapStruct || (!move2) = 0.)) then ManageGhosts mapStruct pm v;
				
				if (GetGhostDirection gh = DOWN && (!move2) = 0.) then move2 := move;

				MPM_rec (move -. (!move2)) v
	in

	let n = vect_length vGh in
	for i=0 to (n-1)
	do
		let t = GetGhostTime (vGh.(i)) and t2 = (float_of_int (SDLTime())) /. 1000. in
		let rec MakeList j r =
			if (r <= 0) then [] else (vGh.(j mod n))::(MakeList (j+1) (r-1));
		in
			
		let dist = if (GetGhostState (vGh.(i)) <> GHOST_SCARED) then ((GetSpeed()) *. (t2 -. t)) else ((GetSpeed()) *. (t2 -. t)) /. 2. in
		MPM_rec (max dist 1.) (vect_of_list (MakeList i n));
		SetGhostTime (vGh.(i)) t2
	done;;
	







(*Main functions !!*)
exception Over;;
exception Win;;

let PlayFSoundWait sound =
	PlayFSound sound;
	while (GetFSoundStatus sound = PLAY)
	do
		SDLSleep 10
	done;;

let TestLiveLost pm vGh map screen blitFn =
	let n = vect_length vGh and (pmx,pmy) = GetPacManPosition pm in
	
	
	try
		for i=0 to (n-1) do
			let (gx,gy) = GetGhostPosition (vGh.(i)) in
			if (abs_float (gx-.pmx) +. abs_float (gy-.pmy) <. float_of_int DIM /. 2.) then
			begin
				if (GetGhostState (vGh.(i)) = GHOST_HUNGRY) then raise Exit
				else if (GetGhostState (vGh.(i)) = GHOST_SCARED) then
				begin
					SetGhostState (vGh.(i)) GHOST_DEAD;
					AddPacManPoints pm 200;
					PlayFSoundWait (GetDeadGhostSound());
					SetPacManTime pm ((float_of_int (SDLTime())) /. 1000.);
					let resetFn x = SetGhostTime x (float_of_int (SDLTime()) /. 1000.) in
					do_vect resetFn vGh
				end
			end
		done
	with Exit ->
	
	UnblitPacMan pm map screen;
	UnblitGhosts vGh map screen;
	SetPacManLives pm (GetPacManLives pm -1);
	PlayFSoundWait (GetDieSound());
	SetPacManPosition pm (GetMapPacManInitPosition map);
	SetGhostsPositions vGh (GetMapGhostsInitPositions map);
	do_vect (function x -> SetGhostState x GHOST_HUNGRY) vGh;
	SetPacManDirection pm LEFT;
	SetPacManNextDirection pm LEFT;
		
	if (GetPacManLives pm > 0) then
	begin
		ResetPacMan pm;
		BlitPacMan pm screen;
		BlitGhosts vGh screen;
		blitFn screen;
		SDLSleep 1000;
		SetPacManTime pm ((float_of_int (SDLTime())) /. 1000.);
		let resetFn x =
			SetGhostTime x (float_of_int (SDLTime()) /. 1000.);
			SetGhostDirection x LEFT
		in
		do_vect resetFn vGh
	end;;

	
let WaitForEnter() =
	try
		while true do
			let e = WaitEvent false in
			let (t,k,_,_) = GetEventComp e in
			if (t = SDL_KEYDOWN && (k = SDLK_ENTER || k = SDLK_RETURN)) then raise Exit;
			DeleteEvent e
		done
	with Exit -> ();;

let RestartTheGame map pm screen blitFn menuFn =
	let font = LoadFont ((GetSpritesDir())^"default.ttf") 20 in
	let formalFont = LoadFont ((GetSpritesDir())^"formal.ttf") 12 in
	let img = CreateMapSurf map in
	let (wm,hm) = DimSurf img and (ws,hs) = DimSurf screen in
	
	let vGh = LoadAllGhostsDefault() in
	let t0 = ref (SDLTime()) and fps = ref 0 in
	
	let textSc = NewText "Score :" font (MakeColor 255 255 255) in
	let (wsc,hsc) = DimSurf textSc in
	let xsc = 10 and ysc = hm+10 in
	
	let textLi = NewText "Vies :" font (MakeColor 255 255 255) in
	let (wli,hli) = DimSurf textLi in
	let xli = xsc+3*wsc and yli = ysc in
	
	let textF = NewText "FPS :" formalFont (MakeColor 255 255 255) in
	let (wfps,hfps) = DimSurf textF in
	let xfps = ws-(2*wfps) and yfps = hs-hfps-5 in

	SetPacManPosition pm (GetMapPacManInitPosition map);
	SetGhostsPositions vGh (GetMapGhostsInitPositions map);
	
	BlitSurf screen img (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	BlitSurf screen textSc (MakeRect xsc ysc 0 0) (MakeRect 0 0 0 0);
	BlitSurf screen textLi (MakeRect xli yli 0 0) (MakeRect 0 0 0 0);
	BlitSurf screen textF (MakeRect xfps yfps 0 0) (MakeRect 0 0 0 0);
	ResetPacMan pm;
	BlitPacMan pm screen;
	BlitGhosts vGh screen;
	blitFn screen;

	PlayFSoundWait (GetInitSound());
	
	SetPacManTime pm ((float_of_int (SDLTime())) /. 1000.);
	let resetFn x = SetGhostTime x (float_of_int (SDLTime()) /. 1000.) in
	do_vect resetFn vGh;
	
	let r =
		try
			while true
			do
				let e = WaitEvent true in
				if (IsExitEvent e) then
				begin
					DeleteEvent e;
					raise Exit
				end;
				
				UnblitPacMan pm map screen;
				UnblitGhosts vGh map screen;

				MovePacMan pm vGh map;
				MoveGhosts vGh map pm;
			
				let (t,k,x,y) = GetEventComp e in
				if (t = SDL_KEYDOWN) then
				begin
					if (k = SDLK_UP) then ChangePacManDirection pm UP;
					if (k = SDLK_DOWN) then ChangePacManDirection pm DOWN;
					if (k = SDLK_RIGHT) then ChangePacManDirection pm RIGHT;
					if (k = SDLK_LEFT) then ChangePacManDirection pm LEFT
				end;
				
				BlitPacMan pm screen;
				BlitGhosts vGh screen;

				FillRect screen (MakeColor 0 0 0) (MakeRect 0 hm ws hs);
				BlitSurf screen textSc (MakeRect xsc ysc 0 0) (MakeRect 0 0 0 0);
				BlitSurf screen textLi (MakeRect xli yli 0 0) (MakeRect 0 0 0 0);
				BlitSurf screen textF (MakeRect xfps yfps 0 0) (MakeRect 0 0 0 0);

				let textScore = NewText (string_of_int (GetPacManPoints pm)) font (MakeColor 255 255 255) in
				BlitSurf screen textScore (MakeRect (xsc+wsc+5) ysc 0 0) (MakeRect 0 0 0 0);
				DeleteSurf textScore;
						
				let textLives = NewText (string_of_int (GetPacManLives pm)) font (MakeColor 255 255 255) in
				BlitSurf screen textLives (MakeRect (xli+wli+5) yli 0 0) (MakeRect 0 0 0 0);
				DeleteSurf textLives;
					
				let textFps = NewText (string_of_int (!fps)) formalFont (MakeColor 255 255 255) in
				BlitSurf screen textFps (MakeRect (xfps+wfps+5) yfps 0 0) (MakeRect 0 0 0 0);
				DeleteSurf textFps;

				if (IsExitEvent e) then raise Exit;
				if ((IsKeyPushed e `q`) || (IsKeyPushed e `Q`)) then raise Over;
				if (t = SDL_KEYDOWN && k = SDLK_ESCAPE) then
				begin
					let (w,h) = DimSurf screen in
					let surf = NewSurf w h in
					BlitSurf surf screen (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
						
					match menuFn() with
					| GAME_EXIT -> DeleteEvent e; raise Exit
					| GAME_OVER -> DeleteEvent e; raise Over
					| _ ->
						SetPacManTime pm ((float_of_int (SDLTime())) /. 1000.);
						do_vect resetFn vGh;
						BlitSurf screen surf (MakeRect 0 0 0 0) (MakeRect 0 0 0 0)
				end;

				DeleteEvent e;
				blitFn screen;

				TestLiveLost pm vGh map screen blitFn;
	
				if (GetPacManLives pm <= 0) then raise Over;
				if (GetMapGumCount map <= 0) then raise Win;
					
				let t = SDLTime() in
				if ( t-(!t0) < ( 1000/(GetFPS()) ) ) then SDLSleep (( 1000/(GetFPS()) ) - (t-(!t0)));
				
				fps := 1000 / (SDLTime()-(!t0));
				t0 := SDLTime();
			
			done;
			raise Exit

		with Exit -> GAME_EXIT
		| ex when (ex = Win || ex = Over) ->
			let bigFont = LoadFont ((GetSpritesDir())^"default.ttf") 35 in
			let surf = NewSurf ws hs and text = NewText (if (ex = Win) then "Good Game !" else "Game Over") bigFont (MakeColor 255 255 255) in
			let (wt,ht) = DimSurf text in
			SetSurfAlphaLevel surf 200;
			BlitSurf screen surf (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			BlitSurf screen text (MakeRect ((ws-wt)/2) ((hs-ht)/2) 0 0) (MakeRect 0 0 0 0);
			blitFn screen;
			if (ex = Win) then PlayFSoundWait (GetIntermissionSound());
			DeleteSurf surf; DeleteSurf text;
			DeleteFont bigFont;
			WaitForEnter();
			if (ex = Win) then GAME_OK else GAME_OVER
	in

	DeleteSurf img; DeleteSurf textSc; DeleteSurf textF;
	DeleteMap map;
	DeleteFont font;
	DeleteAllMedias();
	do_vect DeleteGhost vGh;
	r;;

let LaunchTheGame map screen blitFn menuFn =
	let pm = LoadPacManDefault() in
	let r = RestartTheGame map pm screen blitFn menuFn in
	DeletePacMan pm;
	r;;

let GetDimNeeded map =
	let (m,n) = GetMapDim map in
	(n*DIM, m*DIM+60);;


let LaunchTheCampaign cmp redimFn blitFn menuFn saveFn =
	let vMaps = GetCampaignMaps cmp in
	let n = vect_length vMaps in
	let font = LoadFont ((GetSpritesDir())^"default.ttf") 30 in
	let pm = LoadPacManDefault() in

	try
		for i=0 to (n-1) do
			let (ws,hs) = GetDimNeeded (vMaps.(i)) in
			let screen = redimFn ws hs in
			let text1 = NewText ("Mission "^(string_of_int (i+1))) font (MakeColor 255 255 255) in
			let text2 = NewText (GetMapName (vMaps.(i))) font (MakeColor 255 255 255) in
			let (w1,h1) = DimSurf text1 and (w2,h2) = DimSurf text2 in
			BlitSurf screen text1 (MakeRect ((ws-w1)/2) ((hs-(2*h1+h2))/2) 0 0) (MakeRect 0 0 0 0);
			BlitSurf screen text2 (MakeRect ((ws-w2)/2) ((hs-(2*h1+h2))/2 +2*h1) 0 0) (MakeRect 0 0 0 0);
			blitFn screen;

			WaitForEnter();
			DeleteSurf text1;
			DeleteSurf text2;
				
			SetPacManDirection pm LEFT;
			SetPacManNextDirection pm LEFT;
			match RestartTheGame (vMaps.(i)) pm screen blitFn menuFn with
			| z when (z = GAME_EXIT || z = GAME_OVER) -> saveFn (GetPacManPoints pm) (i+1); raise Exit
			| _ -> if (i = n-1) then saveFn (GetPacManPoints pm) n;
		done;
		raise Exit

	with Exit ->
	
	DeleteFont font;
	DeletePacMan pm;;


let CreateWholeMapSurf map =
	let img = CreateMapSurf map in
	let pm = LoadPacManDefault() and vGh = LoadAllGhostsDefault() in
	SetPacManPosition pm (GetMapPacManInitPosition map);
	SetGhostsPositions vGh (GetMapGhostsInitPositions map);
	BlitPacMan pm img;
	BlitGhosts vGh img;
	DeletePacMan pm;
	do_vect DeleteGhost vGh;
	img;;

	


