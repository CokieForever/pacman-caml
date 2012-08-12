#open "bimedia";;
#open "menus";;
#open "sound";;
#open "game";;
#open "interface";;
#open "editor";;
#open "unix";;

let DIMX = 570 and DIMY = 660;;

type Action = NEW_GAME | QUIT | CONFIG | LOAD_GAME | BUILDER | HIGH_SCORES | CREDITS | NO_ACTION;;

type Profile = {profileName:string; profileScore:int; profileMission:int};;

let MakeProfile name score mission =
	{profileName=name; profileScore=score; profileMission=mission};;

let GetProfileComp profile =
	(profile.profileName, profile.profileScore, profile.profileMission);;


type FileVersion = {versionNumber:int; versionDate:int*int*int};;
let currentVersion = {versionNumber=1; versionDate=(18,12,2011)};;
let GetVersionNumber version =
	version.versionNumber;;

let LoadProfilesV1 file =
	(input_value file : Profile list);;

let profilesLoadingFunctions =
[|
	LoadProfilesV1 ;
	LoadProfilesV1
|];;

let LoadProfiles() =
	try
		let file = open_in_bin "./Files/main.profile" in
		let version = (input_value file : FileVersion) in
		let profilesList = profilesLoadingFunctions.(GetVersionNumber version) file in
		close_in file;
		profilesList
	with _ -> [];;

let SaveProfile profile =
	try
		let profilesList = LoadProfiles() in
		let file = open_out_bin "./Files/main.profile" in
		output_value file currentVersion;
		output_value file (profile::profilesList);
		close_out file
	with _ -> failwith "Unable to save profile";;
	


exception Out;;

let ConfigParameters() =
	let screen = GetScreen() in
	let (w,h) = DimSurf screen in
	let bkg = LoadImage "Media/Images/paramBkg.jpg" in
	let cdSurf = LoadImage "Media/Images/cdSurf.png" in
	let (wcds,hcds) = DimSurf cdSurf in
	let font = LoadFont "Media/Fonts/paramFont.ttf" 20 in
	let whiteColor = MakeColor 255 255 255 in

	let textEndButton = NewText "OK" font whiteColor in
	let (web,heb) = DimSurf textEndButton in
	let xeb = DIMX-web-10 and yeb = DIMY-heb-10 in
	let endButton = NewSurfButton textEndButton (xeb,yeb) in

	let textVideoDir = NewText "Repertoire video :" font whiteColor in
	let (wvd,hvd) = DimSurf textVideoDir in
	let xvd = 20 and yvd = 120 in

	let textAudioDir = NewText "Repertoire audio :" font whiteColor in
	let (wad,had) = DimSurf textAudioDir in
	let xad = xvd and yad = yvd+hvd+3 in

	let textSpeed = NewText "Vitesse de jeu :" font whiteColor in
	let (wsp,hsptmp) = DimSurf textSpeed in
	let xsp = xvd and ysptmp = yad+had+20 in
	let xcdsp = xsp+wsp+10 and (ysp,ycdsp) = if (hcds > hsptmp) then (ysptmp+((hcds-hsptmp)/2),ysptmp) else (ysptmp,ysptmp+((hsptmp-hcds)/2)) in
	let hsp = if (hcds > hsptmp) then hsptmp+((hcds-hsptmp)/2) else hsptmp in
	let wcdsp = min (w-xcdsp-wcds-10) 200 in

	let PositionToSpeed x = int_of_float (float_of_int (( x -xcdsp)*(300-50)) /. (float_of_int wcdsp) +.50.) in
	let SpeedToPosition sp = int_of_float (float_of_int (( sp -50)*wcdsp) /. (float_of_int (300-50)) +. (float_of_int xcdsp)) in
	let cdSpeed = NewSurfCD cdSurf (SpeedToPosition (int_of_float (GetSpeed())), ycdsp) (MakeRect xcdsp ycdsp wcdsp 0) in
	
	let textFps = NewText "FPS maximal :" font whiteColor in
	let (wfps,hfpstmp) = DimSurf textFps in
	let xfps = xsp and yfpstmp = ysp+hsp+10 in
	let xcdfps = xfps+wfps+10 and (yfps,ycdfps) = if (hcds > hfpstmp) then (yfpstmp+((hcds-hfpstmp)/2),yfpstmp) else (yfpstmp,yfpstmp+((hfpstmp-hcds)/2)) in
	let hfps = if (hcds > hfpstmp) then hfpstmp+((hcds-hfpstmp)/2) else hfpstmp in
	let wcdfps = min (w-xcdfps-wcds-10) 200 in

	let PositionToFPS x = int_of_float (float_of_int (( x -xcdfps)*(200-25)) /. (float_of_int wcdfps) +.25.) in
	let FPSToPosition fps = int_of_float (float_of_int (( fps -25)*wcdfps) /. (float_of_int (200-25)) +. (float_of_int xcdfps)) in
	let cdFps = NewSurfCD cdSurf (FPSToPosition (GetFPS()), ycdfps) (MakeRect xcdfps ycdfps wcdfps 0) in

	let textDefaultButton = NewText "Valeurs par defaut" font (MakeColor 200 0 0) in
	let (wdb,hdb) = DimSurf textDefaultButton in
	let xdb = xfps and ydb = yfps+hfps+50 in
	let defaultButton = NewSurfButton textDefaultButton (xdb,ydb) in

	BlitSurf bkg textEndButton (MakeRect xeb yeb 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textVideoDir (MakeRect xvd yvd 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textAudioDir (MakeRect xad yad 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textSpeed (MakeRect xsp ysp 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textFps (MakeRect xfps yfps 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textDefaultButton (MakeRect xdb ydb 0 0) (MakeRect 0 0 0 0);
	BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	BlitSurf screen cdSurf (MakeRect xcdsp ycdsp 0 0) (MakeRect 0 0 0 0);
	BlitSurf screen cdSurf (MakeRect xcdfps ycdfps 0 0) (MakeRect 0 0 0 0);
			
	let editionVideoDir = NewTB screen 100 (MakeRect (xvd+wvd+10) yvd (w-(xvd+wvd+10)-10) hvd) font whiteColor [TB_STYLE_NORMAL] in
	let tmp = ref (SetTBText editionVideoDir (GetSpritesDir())) in
	
	let editionAudioDir = NewTB screen 100 (MakeRect (xad+wad+10) yad (w-(xad+wad+10)-10) had) font whiteColor [TB_STYLE_NORMAL] in
	tmp := SetTBText editionAudioDir (GetSoundsDir());
		
	try
		try
			while true
			do
				let e = WaitEvent false in
				let (t,k,_,_) = GetEventComp e in
			
				if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then
				begin
					DeleteEvent e;
					raise Out
				end;

				BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			
				(match HoldButton e defaultButton with
				| BUTTON_CLICKED ->
					tmp := SetTBText editionVideoDir SPRITES_DIR_DEFAULT;
					tmp := SetTBText editionAudioDir SOUNDS_DIR_DEFAULT;
					let (_,y) = GetCDPosition cdSpeed and (_,y2) = GetCDPosition cdFps in
					SetCDPosition (SpeedToPosition SPEED_DEFAULT, y) cdSpeed;
					SetCDPosition (FPSToPosition FPS_DEFAULT, y2) cdFps
				| x when (x = BUTTON_HOT || x = BUTTON_PUSHED) -> DrawRect screen (MakeColor 0 150 0) (MakeRect (xdb-5) (ydb-2) (wdb+10) (hdb+4))
				| _ -> () );

				HoldTB editionVideoDir e;
				DisplayTB editionVideoDir;

				HoldTB editionAudioDir e;
				DisplayTB editionAudioDir;

				(match HoldButton e endButton with
				| BUTTON_CLICKED -> DeleteEvent e; raise Exit
				| x when (x = BUTTON_HOT || x = BUTTON_PUSHED) -> DrawRect screen (MakeColor 200 0 0) (MakeRect (xeb-5) (yeb-2) (web+10) (heb+4))
				| _ -> () );
			
				HoldAllCDs e;

				let (x,y) = GetCDPosition cdSpeed in
				let t1 = NewText (string_of_int (PositionToSpeed x)) font whiteColor in
				BlitSurf screen cdSurf (MakeRect x y 0 0) (MakeRect 0 0 0 0);
				BlitSurf screen t1 (MakeRect (xcdsp+wcdsp+10+wcds) ysp 0 0) (MakeRect 0 0 0 0);
				DeleteSurf t1;
			
				let (x2,y2) = GetCDPosition cdFps in
				let t2 = NewText (string_of_int (PositionToFPS x2)) font whiteColor in
				BlitSurf screen cdSurf (MakeRect x2 y2 0 0) (MakeRect 0 0 0 0);
				BlitSurf screen t2 (MakeRect (xcdfps+wcdfps+10+wcds) yfps 0 0) (MakeRect 0 0 0 0);
				DeleteSurf t2;
			
				DeleteEvent e;
				Flip screen
			done
		with Exit ->

		let (x,_) = GetCDPosition cdSpeed and (x2,_) = GetCDPosition cdFps in
		SetGameStyleSheet (GetTBText editionVideoDir) (GetTBText editionAudioDir) (PositionToSpeed x) (PositionToFPS x2);
		raise Out

	with Out ->

	DeleteSurf bkg; DeleteSurf textEndButton; DeleteSurf textVideoDir; DeleteSurf textAudioDir; DeleteSurf cdSurf;
	DeleteFont font;
	DeleteButton endButton;
	DeleteTB editionVideoDir; DeleteTB editionAudioDir;
	DeleteCD cdSpeed;;

let GetDirList dirName =
	let rec GDL_rec dir =
		try
			let fileName = readdir dir in
			let l = GDL_rec dir in
			try
				let d = opendir fileName in
				closedir d;
				l;
			with _ -> fileName::l
		with End_of_file -> []
	in
	let dir = opendir dirName in
	rewinddir dir;
	let l = GDL_rec dir in
	closedir dir;
	l;;

let LoadingPage() =
	let screen = GetScreen() in
	let bkg = LoadImage "Media/Images/loadBkg.jpg" in
	let font = LoadFont "Media/Fonts/loadFont.ttf" 20 in
	let whiteColor = MakeColor 255 255 255 in
	let mapName = ref "none" in
	
	let systemDirName = "./Files/Maps/System/" in
	let systemDirList = "Initiation (defaut)"::(GetDirList systemDirName) in
	let mapSystemName = ref (hd systemDirList) in
	let xtms = 20 and ytms = 120 in
	let textMapSystem = NewText "Carte du jeu :" font whiteColor in
	let textDlButtonMapSystem = ref (NewText ((!mapSystemName)^"  ") font whiteColor) in
	let xdlbms = xtms+20 + fst (DimSurf textMapSystem) and ydlbms = ytms and (wdlbms,hdlbms) = DimSurf (!textDlButtonMapSystem) in
	let dlButtonMapSystem = NewSurfButton (!textDlButtonMapSystem) (xdlbms,ydlbms) in
	let xdlms = xdlbms and ydlms = ydlbms + snd (DimSurf (!textDlButtonMapSystem)) in
	let dlMapSystem = NewSurfDL (!textDlButtonMapSystem) (xdlms,ydlms) systemDirList font whiteColor (MakeColor 10 10 10) dlButtonMapSystem in
	let textButtonMapSystem = NewText "Charger" font whiteColor in
	let xbms = xtms and ybms = ytms+10 + snd (DimSurf textMapSystem) in
	let buttonMapSystem = NewSurfButton textButtonMapSystem (xbms,ybms) in

	BlitSurf bkg textMapSystem (MakeRect xtms ytms 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textButtonMapSystem (MakeRect xbms ybms 0 0) (MakeRect 0 0 0 0);
	
	let persoDirName = "./Files/Maps/Perso/" in
	let persoDirList = "(aucune)         "::(GetDirList persoDirName) in
	let mapPersoName = ref (hd persoDirList) in
	let xtmp = xbms and ytmp = ybms+30 + snd (DimSurf textButtonMapSystem) in
	let textMapPerso = NewText "Carte perso :" font whiteColor in
	let textDlButtonMapPerso = ref (NewText ((!mapPersoName)^"  ") font whiteColor) in
	let xdlbmp = xtmp+20 + fst (DimSurf textMapPerso) and ydlbmp = ytmp and (wdlbmp,hdlbmp) = DimSurf (!textDlButtonMapPerso) in
	let dlButtonMapPerso = NewSurfButton (!textDlButtonMapPerso) (xdlbmp,ydlbmp) in
	let xdlmp = xdlbmp and ydlmp = ydlbmp + snd (DimSurf (!textDlButtonMapPerso)) in
	let dlMapPerso = NewSurfDL (!textDlButtonMapPerso) (xdlmp,ydlmp) persoDirList font whiteColor (MakeColor 10 10 10) dlButtonMapPerso in
	let textButtonMapPerso = NewText "Charger" font whiteColor in
	let xbmp = xtmp and ybmp = ytmp+10 + snd (DimSurf textMapPerso) in
	let buttonMapPerso = NewSurfButton textButtonMapPerso (xbmp,ybmp) in

	BlitSurf bkg textMapPerso (MakeRect xtmp ytmp 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textButtonMapPerso (MakeRect xbmp ybmp 0 0) (MakeRect 0 0 0 0);
	
	let textEndButton = NewText "Retour" font whiteColor in
	let (web,heb) = DimSurf textEndButton in
	let xeb = DIMX-web-10 and yeb = DIMY-heb-10 in
	let endButton = NewSurfButton textEndButton (xeb,yeb) in
	
	BlitSurf bkg textEndButton (MakeRect xeb yeb 0 0) (MakeRect 0 0 0 0);
	
	try 
		while true do
			let e = WaitEvent false in
			let (t,k,_,_) = GetEventComp e in
			
			if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then
			begin
				DeleteEvent e;
				mapName := "none";
				raise Exit
			end;

			(match HoldDL e dlMapSystem with
			| (n,DL_SELECTED) ->
				DeleteSurf (!textDlButtonMapSystem);
				mapSystemName := (vect_of_list systemDirList).(n);
				textDlButtonMapSystem := NewText (!mapSystemName) font whiteColor
			| _ -> () );

			(match HoldDL e dlMapPerso with
			| (n,DL_SELECTED) ->
				DeleteSurf (!textDlButtonMapPerso);
				mapPersoName := (vect_of_list persoDirList).(n);
				textDlButtonMapPerso := NewText (!mapPersoName) font whiteColor
			| _ -> () );

			BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			BlitSurf screen (!textDlButtonMapSystem) (MakeRect xdlbms ydlbms 0 0) (MakeRect 0 0 0 0);
			BlitSurf screen (!textDlButtonMapPerso) (MakeRect xdlbmp ydlbmp 0 0) (MakeRect 0 0 0 0);

			(match GetButtonState dlButtonMapSystem with
			| z when ((z = BUTTON_HOT || z = BUTTON_PUSHED || z = BUTTON_CLICKED) && snd (GetDLState dlMapSystem) <> DL_EXTENDED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xdlbms-5) (ydlbms-2) (wdlbms+10) (hdlbms+4))
			| _ -> () );
			
			(match GetButtonState dlButtonMapPerso with
			| z when ((z = BUTTON_HOT || z = BUTTON_PUSHED || z = BUTTON_CLICKED) && snd (GetDLState dlMapPerso) <> DL_EXTENDED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xdlbmp-5) (ydlbmp-2) (wdlbmp+10) (hdlbmp+4))
			| _ -> () );

			(match HoldButton e buttonMapSystem with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				let (w,h) = DimSurf textButtonMapSystem in
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xbms-5) (ybms-2) (w+10) (h+4))
			| BUTTON_CLICKED ->
				mapName := if ((!mapSystemName) = hd systemDirList) then "default" else (systemDirName^(!mapSystemName));
				DeleteEvent e;
				raise Exit
			| _ -> () );

			(match HoldButton e buttonMapPerso with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				let (w,h) = DimSurf textButtonMapPerso in
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xbmp-5) (ybmp-2) (w+10) (h+4))
			| BUTTON_CLICKED ->
				if ((!mapPersoName) <> hd persoDirList) then
				begin
					mapName := persoDirName^(!mapPersoName);
					DeleteEvent e;
					raise Exit
				end
			| _ -> () );

			DisplayDL screen dlMapSystem;
			DisplayDL screen dlMapPerso;

			(match HoldButton e endButton with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xeb-5) (yeb-2) (web+10) (heb+4))
			| BUTTON_CLICKED -> DeleteEvent e; mapName := "none"; raise Exit
			| _ -> () );
			
			DeleteEvent e;
			Flip screen
		done;
		raise Exit
	with Exit ->
			
	DeleteSurf textMapSystem; DeleteSurf textButtonMapSystem; DeleteSurf (!textDlButtonMapSystem);
	DeleteButton buttonMapSystem;
	DeleteDL dlMapSystem;
	DeleteSurf textMapPerso; DeleteSurf textButtonMapPerso; DeleteSurf (!textDlButtonMapPerso);
	DeleteButton buttonMapPerso;
	DeleteDL dlMapPerso;
	DeleteSurf bkg; DeleteFont font;
	
	(!mapName);;


let BuilderPage() =
	let screen = GetScreen() in
	let bkg = LoadImage "Media/Images/builderBkg.jpg" in
	let font = LoadFont "Media/Fonts/builderFont.ttf" 20 in
	let whiteColor = MakeColor 255 255 255 in
	let mapName = ref "none" in
	
	let persoDirName = "./Files/Maps/Perso/" in
	let persoDirList = "(aucune)       "::(GetDirList persoDirName) in
	let mapPersoName = ref (hd persoDirList) in
	let xtmp = 20 and ytmp = 120 in
	let textMapPerso = NewText "Carte perso :" font whiteColor in
	let textDlButtonMapPerso = ref (NewText ((!mapPersoName)^"  ") font whiteColor) in
	let xdlbmp = xtmp+20 + fst (DimSurf textMapPerso) and ydlbmp = ytmp and (wdlbmp,hdlbmp) = DimSurf (!textDlButtonMapPerso) in
	let dlButtonMapPerso = NewSurfButton (!textDlButtonMapPerso) (xdlbmp,ydlbmp) in
	let xdlmp = xdlbmp and ydlmp = ydlbmp + snd (DimSurf (!textDlButtonMapPerso)) in
	let dlMapPerso = NewSurfDL (!textDlButtonMapPerso) (xdlmp,ydlmp) persoDirList font whiteColor (MakeColor 10 10 10) dlButtonMapPerso in
	let textButtonMapPerso = NewText "Charger" font whiteColor in
	let xbmp = xtmp and ybmp = ytmp+10 + snd (DimSurf textMapPerso) in
	let buttonMapPerso = NewSurfButton textButtonMapPerso (xbmp,ybmp) in

	BlitSurf bkg textMapPerso (MakeRect xtmp ytmp 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg textButtonMapPerso (MakeRect xbmp ybmp 0 0) (MakeRect 0 0 0 0);
	
	let textButtonNewMap = NewText "Nouvelle carte" font whiteColor in
	let xbnm = xbmp and ybnm = ybmp+30 + snd (DimSurf textMapPerso) in
	let buttonNewMap = NewSurfButton textButtonNewMap (xbnm,ybnm) in
	
	BlitSurf bkg textButtonNewMap (MakeRect xbnm ybnm 0 0) (MakeRect 0 0 0 0);

	let textEndButton = NewText "Retour" font whiteColor in
	let (web,heb) = DimSurf textEndButton in
	let xeb = DIMX-web-10 and yeb = DIMY-heb-10 in
	let endButton = NewSurfButton textEndButton (xeb,yeb) in
	
	BlitSurf bkg textEndButton (MakeRect xeb yeb 0 0) (MakeRect 0 0 0 0);
	
	try 
		while true do
			let e = WaitEvent false in
			let (t,k,_,_) = GetEventComp e in
			
			if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then
			begin
				DeleteEvent e;
				mapName := "none";
				raise Exit
			end;

			(match HoldDL e dlMapPerso with
			| (n,DL_SELECTED) ->
				DeleteSurf (!textDlButtonMapPerso);
				mapPersoName := (vect_of_list persoDirList).(n);
				textDlButtonMapPerso := NewText (!mapPersoName) font whiteColor
			| _ -> () );

			BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			BlitSurf screen (!textDlButtonMapPerso) (MakeRect xdlbmp ydlbmp 0 0) (MakeRect 0 0 0 0);

			(match GetButtonState dlButtonMapPerso with
			| z when ((z = BUTTON_HOT || z = BUTTON_PUSHED || z = BUTTON_CLICKED) && snd (GetDLState dlMapPerso) <> DL_EXTENDED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xdlbmp-5) (ydlbmp-2) (wdlbmp+10) (hdlbmp+4))
			| _ -> () );

			(match HoldButton e buttonMapPerso with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				let (w,h) = DimSurf textButtonMapPerso in
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xbmp-5) (ybmp-2) (w+10) (h+4))
			| BUTTON_CLICKED ->
				if ((!mapPersoName) <> hd persoDirList) then
				begin
					mapName := persoDirName^(!mapPersoName);
					DeleteEvent e;
					raise Exit
				end
			| _ -> () );

			(match HoldButton e buttonNewMap with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				let (w,h) = DimSurf textButtonNewMap in
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xbnm-5) (ybnm-2) (w+10) (h+4))
			| BUTTON_CLICKED ->
				mapName := "empty";
				DeleteEvent e;
				raise Exit
			| _ -> () );

			DisplayDL screen dlMapPerso;
			
			(match HoldButton e endButton with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xeb-5) (yeb-2) (web+10) (heb+4))
			| BUTTON_CLICKED -> DeleteEvent e; mapName := "none"; raise Exit
			| _ -> () );
			
			DeleteEvent e;
			Flip screen
		done;
		raise Exit
	with Exit ->
			
	DeleteSurf textMapPerso; DeleteSurf textButtonMapPerso; DeleteSurf (!textDlButtonMapPerso);
	DeleteButton buttonMapPerso;
	DeleteDL dlMapPerso;
	DeleteSurf textButtonNewMap;
	DeleteButton buttonNewMap;
	DeleteSurf bkg; DeleteFont font;
	
	(!mapName);;


let DisplayHighScores() =
	let screen = GetScreen() in
	let (ws,hs) = DimSurf screen in
	let bkg = LoadImage "./Media/Images/scoresBkg.jpg" in
	let font = LoadFont "./Media/Fonts/default.ttf" 20 in

	let textEndButton = NewText "Retour" font (MakeColor 255 255 255) in
	let (web,heb) = DimSurf textEndButton in
	let xeb = ws-web-10 and yeb = hs-heb-10 in
	let endButton = NewSurfButton textEndButton (xeb,yeb) in
	
	BlitSurf bkg textEndButton (MakeRect xeb yeb 0 0) (MakeRect 0 0 0 0);

	BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	let textBox = NewTB screen 100000 (MakeRect 20 120 (ws-40) (yeb-140)) font (MakeColor 255 255 255) [TB_STYLE_MULTILINE ; TB_STYLE_HSCROLL ; TB_STYLE_VSCROLL ; TB_STYLE_JUSTDISPLAY ] in
	
	let rec DHS_rec l i =
		match l with
		| [] -> ""
		| profile::tail ->
			let (name,score,mission) = GetProfileComp profile in
			( (string_of_int i)^". "^name^" : "^(string_of_int score)^" pts (Mission "^(string_of_int mission)^")\n")^(DHS_rec tail (i+1))
	in

	let predicat prf1 prf2 =
		let (_,score1,_) = GetProfileComp prf1 and (_,score2,_) = GetProfileComp prf2 in
		score1 > score2
	in

	let str = DHS_rec (sort__sort predicat (LoadProfiles())) 1 in
	
	let a = SetTBText textBox str in ();

	try
		while true do
			let e = WaitEvent false in
			let (t,k,_,_) = GetEventComp e in
			
			if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then
			begin
				DeleteEvent e;
				raise Exit
			end;
			
			BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			
			(match HoldButton e endButton with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xeb-5) (yeb-2) (web+10) (heb+4))
			| BUTTON_CLICKED -> DeleteEvent e; raise Exit
			| _ -> () );

			HoldTB textBox e;
			DisplayTB textBox;

			DeleteEvent e;
			Flip screen
		done
	with Exit ->

	DeleteSurf bkg;
	DeleteButton endButton;
	DeleteTB textBox;
	DeleteFont font;;

let DisplayCredits() =
	let screen = GetScreen() in
	let (ws,hs) = DimSurf screen in
	let bkg = LoadImage "./Media/Images/creditsBkg.jpg" in
	let font = LoadFont "./Media/Fonts/default.ttf" 18 in

	let textEndButton = NewText "Retour" font (MakeColor 255 255 255) in
	let (web,heb) = DimSurf textEndButton in
	let xeb = ws-web-10 and yeb = 550-heb-10 in
	let endButton = NewSurfButton textEndButton (xeb,yeb) in
	
	BlitSurf bkg textEndButton (MakeRect xeb yeb 0 0) (MakeRect 0 0 0 0);

	BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	let textBox = NewTB screen 100000 (MakeRect 20 120 (ws-40) (yeb-140)) font (MakeColor 255 255 255) [TB_STYLE_MULTILINE ; TB_STYLE_HSCROLL ; TB_STYLE_AUTOJUMP ; TB_STYLE_JUSTDISPLAY ] in
	
	let str =
"PacMan

Realise par Shuo Song et Quoc-Nam Dessoulles, eleves en premiere annee a Telecom Bretagne
Dans le cadre du projet CamL S1

Date de publication : 19 decembre 2011
Derniere mise a jour : 5 janvier 2012
Version : 0.19
Licence : Logiciel et code source distribues sous les termes de la licence Creative Commons BY-NC-SA"
	in

	let a = SetTBText textBox str in ();

	try
		while true do
			let e = WaitEvent false in
			let (t,k,_,_) = GetEventComp e in
			
			if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then
			begin
				DeleteEvent e;
				raise Exit
			end;
			
			BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			
			(match HoldButton e endButton with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xeb-5) (yeb-2) (web+10) (heb+4))
			| BUTTON_CLICKED -> DeleteEvent e; raise Exit
			| _ -> () );

			HoldTB textBox e;
			DisplayTB textBox;

			DeleteEvent e;
			Flip screen
		done
	with Exit ->

	DeleteSurf bkg;
	DeleteButton endButton;
	DeleteTB textBox;
	DeleteFont font;;
	


let saveFunction score missionNumber =
	let screen = GetScreen() in
	let (ws,hs) = DimSurf screen in
	let font = LoadFont "./Media/Fonts/default.ttf" 30 in
	let text = NewText "Entre ton nom !" font (MakeColor 255 255 255) in
	let (wt,ht) = DimSurf text in
	let xt = (ws-wt)/2 and yt = (hs-(ht*2+30))/2 in
	
	let saveSurf = NewSurf ws hs and bkg = NewSurf ws hs in
	BlitSurf saveSurf screen (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg text (MakeRect xt yt 0 0) (MakeRect 0 0 0 0);
	
	let textEndButton = NewText "OK" font (MakeColor 255 255 255) in
	let (web,heb) = DimSurf textEndButton in
	let xeb = ws-web-10 and yeb = hs-heb-10 in
	let endButton = NewSurfButton textEndButton (xeb,yeb) in
	
	BlitSurf bkg textEndButton (MakeRect xeb yeb 0 0) (MakeRect 0 0 0 0);
	let tbDim = MakeRect ((ws-300)/2) (yt+ht+30) 300 ht in
	DrawRect bkg (MakeColor 200 0 200) tbDim;
	
	BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	let textBox = NewTB screen 100 tbDim font (MakeColor 255 255 255) [TB_STYLE_NORMAL] in
	let a = SetTBText textBox "Anne Onyme" in ();
	SetTBFocus textBox true;
	Flip screen;

	try
		while true do
			let e = WaitEvent false in
			let (t,k,_,_) = GetEventComp e in
			
			if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then
			begin
				DeleteEvent e;
				let a = SetTBText textBox "" in ();
				raise Exit
			end;
			
			BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			
			(match HoldButton e endButton with
			| z when (z = BUTTON_HOT || z = BUTTON_PUSHED) ->
				DrawRect screen (MakeColor 200 0 0) (MakeRect (xeb-5) (yeb-2) (web+10) (heb+4))
			| BUTTON_CLICKED -> DeleteEvent e; raise Exit
			| _ -> () );

			if (t = SDL_KEYDOWN && (k = SDLK_ENTER || k = SDLK_RETURN)) then
			begin
				DeleteEvent e;
				raise Exit
			end;

			HoldTB textBox e;
			DisplayTB textBox;
			
			DeleteEvent e;
			Flip screen
		done
	with Exit ->

	let name = GetTBText textBox in
	
	BlitSurf screen saveSurf (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	DeleteSurf bkg; DeleteSurf saveSurf;
	DeleteButton endButton;
	DeleteTB textBox;
	DeleteFont font;

	if (name <> "") then SaveProfile (MakeProfile name score missionNumber);;
	
	

let menuFunction() =
	let menu = LoadMenu "Files/Menus/pause.txt" and screen = GetScreen() in
	let (w,h) = DimSurf screen in
	
	let screenCopy = NewSurf w h in
	BlitSurf screenCopy screen (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
		
	let nextAction = ref QUIT in
	let actionFn s =
		match s with
		| "RESUME" -> nextAction := NO_ACTION; MENU_QUIT
		| "EXIT" -> MENU_QUIT
		| _ -> nextAction := NO_ACTION; MENU_OK
	in

	let relayFn e =
		let (t,k,_,_) = GetEventComp e in
		if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then MENU_QUIT else MENU_OK
	in

	let surf = NewSurf w h in
	SetSurfAlphaLevel surf 200;
	BlitSurf screen surf (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	
	HandleMenu screen menu Flip relayFn actionFn;

	BlitSurf screen screenCopy (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	
	DeleteMenu menu;
	DeleteSurf surf; DeleteSurf screenCopy;
	
	match (!nextAction) with
	| QUIT -> GAME_EXIT
	| _ -> GAME_OK;;




let main() =

	InitSDL();
	InitFMOD();

	let screen = ref (VoidSurf()) in
	
	let menu = LoadMenu "Files/Menus/main.txt" in
	let arrowL = LoadImage "Media/Images/arrowL.png" in
	let arrowR = LoadImage "Media/Images/arrowR.png" in
	let font = LoadFont "Media/Fonts/default.ttf" 25 in
	SetDisplayParam arrowL arrowR font (MakeColor 255 255 255);

	let music = try LoadFSound "Media/Musics/ghostbusters.ogg" STREAM with Failure _ -> VoidFSound() in
		
	let nextAction = ref NO_ACTION in
	let actionFn s =
		match s with
		| "NEW_GAME" -> nextAction := NEW_GAME; MENU_QUIT
		| "CONFIG" -> nextAction := CONFIG; MENU_QUIT
		| "LOAD_GAME" -> nextAction := LOAD_GAME; MENU_QUIT
		| "BUILDER" -> nextAction := BUILDER; MENU_QUIT
		| "HIGH_SCORES" -> nextAction := HIGH_SCORES; MENU_QUIT
		| "CREDITS" -> nextAction := CREDITS; MENU_QUIT
		| _ -> print_string ("Action : "^s); print_newline(); MENU_OK;
	in
	
	let relayFn e =
		let (t,k,_,_) = GetEventComp e in
		if (IsExitEvent e || (t = SDL_KEYDOWN && k = SDLK_ESCAPE)) then MENU_QUIT else MENU_OK
	in

	try
		while true
		do
			screen := OpenGraph DIMX DIMY;
			SetCaption "PacMan";
			try
				nextAction := QUIT;
				if (FSoundOK music) then PlayFSound music;

				HandleMenu (!screen) menu Flip relayFn actionFn;
	
				match (!nextAction) with
				| NEW_GAME ->
					if (FSoundOK music) then StopFSound music;
					let cmp = LoadDefaultCampaign() in
					
					let a = LaunchTheCampaign cmp OpenGraph Flip menuFunction saveFunction in ()

				| QUIT -> raise Exit
				| CONFIG -> ConfigParameters()
				| LOAD_GAME ->
					let mapName = LoadingPage() in
					if (mapName <> "none") then
						let map = if (mapName = "default") then LoadDefaultMap() else LoadMap mapName in
						let (w,h) = GetDimNeeded map in
						screen := OpenGraph w h;
						if (FSoundOK music) then StopFSound music;
						let a = LaunchTheGame map (!screen) Flip menuFunction in ()
				| BUILDER ->
					let mapName = BuilderPage() in
					if (mapName <> "none") then
						let map = if (mapName = "empty") then LoadVoidMap() else LoadMap mapName in
						if (FSoundOK music) then StopFSound music;
						LaunchEditor map Flip OpenGraph
				| HIGH_SCORES -> DisplayHighScores()
				| CREDITS ->
					if (FSoundOK music) then StopFSound music;
					let hymne = try LoadFSound "Media/Musics/hymne_telecom.mp3" STREAM with Failure _ -> VoidFSound() in
					if (FSoundOK hymne) then PlayFSound hymne;
					DisplayCredits();
					if (FSoundOK hymne) then StopFSound hymne;
				| _ -> ()
			with Failure s -> print_string ("Unexpected error : "^s); print_newline();
			| x when (x = Exit || x = Out) -> raise Exit
			| ex -> print_string "Unexpected unknown error"; print_newline(); raise ex
		done
	with Exit ->
	
	if (FSoundOK music) then DeleteFSound music;
	DeleteMenu menu;
	DeleteSurf arrowL;
	DeleteSurf arrowR;
	DeleteFont font;
	QuitFMOD();
	QuitSDL() 

in

let exec() =
	try
		main();
		raise Exit
	with Failure s -> print_string ("Fatal error : "^s); print_newline();
	| Exit -> print_string "Well done !"; print_newline();

in printexc__f exec ();;





