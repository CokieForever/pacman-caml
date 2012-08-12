#open "bimedia";;
#open "interface";;
#open "game";;

let ICONS_DIR = "./Media/Icons/";;
let DIM = 30;;

type CustomButton = {button:Button; surf:Surface; pos:int*int; fn:Map->unit};;

let MakeCustomButton button surf pos fn =
	{button=button; surf=surf; pos=pos; fn=fn};;

let BlitCustomButton screen cButton =
	let (x,y) = cButton.pos in
	BlitSurf screen (cButton.surf) (MakeRect x y 0 0) (MakeRect 0 0 0 0);;

let ManageCustomButton event screen map cButton =
	match HoldButton event (cButton.button) with
	| z when (z = BUTTON_PUSHED || z = BUTTON_HOT || z = BUTTON_CLICKED) ->
		let (w,h) = DimSurf (cButton.surf) and (x,y) = cButton.pos in
		DrawRect screen (MakeColor 200 0 0) (MakeRect x y w h);
		if (z = BUTTON_CLICKED) then cButton.fn map;
	| _ -> ();;

let DeleteCustomButton cButton =
	DeleteSurf cButton.surf;
	DeleteButton cButton.button;;

let line = ref 1 and column = ref 1;;
let mapChanged = ref true;;

let fullButtonFunction map =
	let (m,n) = GetMapDim map in
	SetMapCase map (!line) (!column) CASE_WALL;
	mapChanged := true;;

let voidButtonFunction map =
	let (m,n) = GetMapDim map in
	SetMapCase map (!line) (!column) CASE_VOID;
	mapChanged := true;;

let gumButtonFunction map =
	let (m,n) = GetMapDim map in
	SetMapCase map (!line) (!column) CASE_GUM;
	mapChanged := true;;

let bGumButtonFunction map =
	let (m,n) = GetMapDim map in
	SetMapCase map (!line) (!column) CASE_BGUM;
	mapChanged := true;;

let pacManButtonFunction map =
	let (m,n) = GetMapDim map in
	SetMapPacManInitPosition map (ColumnToX (!column) , LineToY (!line));
	mapChanged := true;;
let blueGhostButtonFunction map =
	let (m,n) = GetMapDim map in
	let vGh = GetMapGhostsInitPositions map in
	SetMapGhostsInitPositions map [| (ColumnToX (!column) , LineToY (!line)) ; vGh.(1) ; vGh.(2) ; vGh.(3) |];
	mapChanged := true;;

let redGhostButtonFunction map =
	let (m,n) = GetMapDim map in
	let vGh = GetMapGhostsInitPositions map in
	SetMapGhostsInitPositions map [| vGh.(0) ; (ColumnToX (!column) , LineToY (!line)) ; vGh.(2) ; vGh.(3) |];
	mapChanged := true;;

let orangeGhostButtonFunction map =
	let (m,n) = GetMapDim map in
	let vGh = GetMapGhostsInitPositions map in
	SetMapGhostsInitPositions map [| vGh.(0) ; vGh.(1) ; (ColumnToX (!column) , LineToY (!line)) ; vGh.(3) |];
	mapChanged := true;;

let pinkGhostButtonFunction map =
	let (m,n) = GetMapDim map in
	let vGh = GetMapGhostsInitPositions map in
	SetMapGhostsInitPositions map [| vGh.(0) ; vGh.(1) ; vGh.(2) ; (ColumnToX (!column) , LineToY (!line)) |];
	mapChanged := true;;

let widthPlusButtonFunction map =
	let (m,n) = GetMapDim map in
	mapChanged := true;
	SetMapDim map (m,n+1);;

let widthMinusButtonFunction map =
	let (m,n) = GetMapDim map in
	if (n>10) then
	begin
		mapChanged := true;
		SetMapDim map (m,n-1)
	end;;

let heightPlusButtonFunction map =
	let (m,n) = GetMapDim map in
	mapChanged := true;
	SetMapDim map (m+1,n);;

let heightMinusButtonFunction map =
	let (m,n) = GetMapDim map in
	if (m>10) then
	begin
		mapChanged := true;
		SetMapDim map (m-1,n)
	end;;


let DIMX = 450;;
let DIMY = 450;;

let LaunchEditor map blitFn redimFn =
	let screen = ref (redimFn DIMX DIMY) in
	let bkg = NewSurf DIMX DIMY in
	SetCaption "PacMan Map Builder";

	let font = LoadFont (ICONS_DIR^"default.ttf") 20 in
	let titleSurf = NewText "Nom de la carte :" font (MakeColor 255 255 255) in
	let saveSurf = NewText "Sauvegarder :" font (MakeColor 255 255 255) in
	let quitSurf = NewText "Quitter" font (MakeColor 255 255 255) in
	
	let xts = 10 and yts = 10 in
	let titleEdition = NewTB (!screen) 100 (MakeRect (xts+10+ fst (DimSurf titleSurf)) yts 250 0) font (MakeColor 255 255 255) [TB_STYLE_NORMAL] in
	let xss = xts and yss = yts+10+ snd (DimSurf titleSurf) in
	let saveEdition = NewTB (!screen) 100 (MakeRect (xss+10+ fst (DimSurf saveSurf)) yss 250 0) font (MakeColor 255 255 255) [TB_STYLE_NORMAL] in
	let saveButton = NewSurfButton saveSurf (xss,yss) in

	let a = SetTBText titleEdition (GetMapName map) in ();
	let a = SetTBText saveEdition (GetMapFile map) in ();
	
	let fullIcon = LoadImage (ICONS_DIR^"full.bmp") in
	let voidIcon = LoadImage (ICONS_DIR^"void.bmp") in
	let gumIcon = LoadImage (ICONS_DIR^"gum.bmp") in
	let bGumIcon = LoadImage (ICONS_DIR^"bgum.bmp") in
	
	let pacManIcon = LoadImage (ICONS_DIR^"pacman.bmp") in
	let blueGhostIcon = LoadImage (ICONS_DIR^"blueghost.bmp") in
	let redGhostIcon = LoadImage (ICONS_DIR^"redghost.bmp") in
	let pinkGhostIcon = LoadImage (ICONS_DIR^"pinkghost.bmp") in
	let orangeGhostIcon = LoadImage (ICONS_DIR^"orangeghost.bmp") in

	let xfb = xss and yfb = yss+20+ snd (DimSurf saveSurf) in
	let fullButton = NewSurfButton fullIcon (xfb,yfb) in
	let xvb = xfb and yvb = yfb+DIM+5 in
	let voidButton = NewSurfButton voidIcon (xvb,yvb) in
	let xgb = xvb and ygb = yvb+DIM+5 in
	let gumButton = NewSurfButton gumIcon (xgb,ygb) in
	let xbgumb = xvb and ybgumb = ygb+DIM+5 in
	let bGumButton = NewSurfButton bGumIcon (xbgumb,ybgumb) in
	
	let xpmb = xfb+DIM+5 and ypmb = yfb in
	let pacManButton = NewSurfButton pacManIcon (xpmb,ypmb) in
	let xbgb = xpmb and ybgb =ypmb+DIM+5 in
	let blueGhostButton = NewSurfButton blueGhostIcon (xbgb,ybgb) in
	let xrgb = xbgb and yrgb = ybgb+DIM+5 in
	let redGhostButton = NewSurfButton redGhostIcon (xrgb,yrgb) in
	let xpgb = xrgb and ypgb = yrgb+DIM+5 in
	let pinkGhostButton = NewSurfButton pinkGhostIcon (xpgb,ypgb) in
	let xogb = xpgb and yogb = ypgb+DIM+5 in
	let orangeGhostButton = NewSurfButton orangeGhostIcon (xogb,yogb) in
	
	
	let widthPlusIcon = LoadImage (ICONS_DIR^"widthplus.bmp") in
	let widthMinusIcon = LoadImage (ICONS_DIR^"widthminus.bmp") in
	let heightPlusIcon = LoadImage (ICONS_DIR^"heightplus.bmp") in
	let heightMinusIcon = LoadImage (ICONS_DIR^"heightminus.bmp") in

	let xwpb = xfb and ywpb = yogb+DIM+20 in
	let widthPlusButton = NewSurfButton widthPlusIcon (xwpb,ywpb) in
	let xwmb = xfb+DIM+5 and ywmb = ywpb in
	let widthMinusButton = NewSurfButton widthMinusIcon (xwmb,ywmb) in
	let xhpb = xwpb and yhpb = ywpb+DIM+5 in
	let heightPlusButton = NewSurfButton heightPlusIcon (xhpb,yhpb) in
	let xhmb = xhpb+DIM+5 and yhmb = yhpb in
	let heightMinusButton = NewSurfButton heightMinusIcon (xhmb,yhmb) in
	
	let allButtons =
	[|
		MakeCustomButton fullButton fullIcon (xfb,yfb) fullButtonFunction ;
		MakeCustomButton voidButton voidIcon (xvb,yvb) voidButtonFunction ;
		MakeCustomButton gumButton gumIcon (xgb,ygb) gumButtonFunction ;
		MakeCustomButton bGumButton bGumIcon (xbgumb,ybgumb) bGumButtonFunction ;
		MakeCustomButton pacManButton pacManIcon (xpmb,ypmb) pacManButtonFunction ;
		MakeCustomButton blueGhostButton blueGhostIcon (xbgb,ybgb) blueGhostButtonFunction ;
		MakeCustomButton redGhostButton redGhostIcon (xrgb,yrgb) redGhostButtonFunction ;
		MakeCustomButton orangeGhostButton orangeGhostIcon (xogb,yogb) orangeGhostButtonFunction ;
		MakeCustomButton pinkGhostButton pinkGhostIcon (xpgb,ypgb) pinkGhostButtonFunction ;
		MakeCustomButton widthPlusButton  widthPlusIcon (xwpb,ywpb) widthPlusButtonFunction ;
		MakeCustomButton widthMinusButton  widthMinusIcon (xwmb,ywmb) widthMinusButtonFunction ;
		MakeCustomButton heightPlusButton  heightPlusIcon (xhpb,yhpb) heightPlusButtonFunction ;
		MakeCustomButton heightMinusButton  heightMinusIcon (xhmb,yhmb) heightMinusButtonFunction
	|] in

	let xqb = xhpb and yqb = yhpb+DIM+15 in
	let quitButton = NewSurfButton quitSurf (xqb,yqb) in

	do_vect (BlitCustomButton bkg) allButtons;
	BlitSurf bkg titleSurf (MakeRect xts yts 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg saveSurf (MakeRect xss yss 0 0) (MakeRect 0 0 0 0);
	BlitSurf bkg quitSurf (MakeRect xqb yqb 0 0) (MakeRect 0 0 0 0);

	CleanMap map;
	let img = ref (CreateWholeMapSurf map) in
	let xm = xpmb+DIM+30 and ym = ypmb+10 in
	let (wm,hm) = DimSurf (!img) in
	let ws = max (xm+wm) DIMX and hs = max (ym+hm) DIMY in
	screen := redimFn ws hs;
	BlitSurf (!screen) bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	blitFn (!screen);

	try
		while true do
			let e = WaitEvent false in

			(*let (xmouse,ymouse) = GetMousePos() in
			print_string ("( "^(string_of_int xmouse)^" ; "^(string_of_int ymouse)^" )"); print_newline();*)
			
			BlitSurf (!screen) bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			
			do_vect (ManageCustomButton e (!screen) map) allButtons;

			(match HoldButton e quitButton with
			| z when (z = BUTTON_PUSHED || z = BUTTON_HOT || z = BUTTON_CLICKED) ->
				let (w,h) = DimSurf quitSurf in
				DrawRect (!screen) (MakeColor 200 0 0) (MakeRect (xqb-5) (yqb-5) (w+10) (h+10));
				if (z = BUTTON_CLICKED) then
				begin
					DeleteEvent e;
					raise Exit
				end
			| _ -> () );

			HoldTB titleEdition e;
			HoldTB saveEdition e;

			DisplayTB titleEdition;
			DisplayTB saveEdition;

			(match HoldButton e saveButton with
			| z when (z = BUTTON_PUSHED || z = BUTTON_HOT) ->
				let (w,h) = DimSurf saveSurf in
				DrawRect (!screen) (MakeColor 200 0 0) (MakeRect (xss-5) (yss-5) (w+10) (h+10))
			| BUTTON_CLICKED ->
				let (w,h) = DimSurf saveSurf in
				DrawRect (!screen) (MakeColor 0 200 0) (MakeRect (xss-5) (yss-5) (w+10) (h+10));
				SetMapFile map (GetTBText saveEdition);
				SetMapName map (GetTBText titleEdition);
				(try SaveMap map with Failure _ ->
					let a = SetTBText saveEdition "" in
					DrawRect (!screen) (MakeColor 255 0 255) (MakeRect (xss-5) (yss-5) (w+10) (h+10)) )
			| _ -> () );

			if (IsExitEvent e) then
			begin
				DeleteEvent e;
				raise Exit
			end;

			CorrectMap map;

			let (m,n) = GetMapDim map in
			
			let (t,k,_,_) = GetEventComp e in
			if (t = SDL_KEYDOWN && k = SDLK_UP) then line := ((!line)+m-2) mod m +1;
			if (t = SDL_KEYDOWN && k = SDLK_DOWN) then line := ((!line)) mod m +1;
			if (t = SDL_KEYDOWN && k = SDLK_RIGHT) then column := ((!column)) mod n +1;
			if (t = SDL_KEYDOWN && k = SDLK_LEFT) then column := ((!column)+n-2) mod n +1;
			
			line := ((!line)+m-1) mod m +1;
			column := ((!column)+n-1) mod n +1;

			if ((!mapChanged)) then
			begin
				DeleteSurf (!img);
				img := CreateWholeMapSurf map;

				let (wm,hm) = DimSurf (!img) in
				let ws = max (xm+wm) DIMX and hs = max (ym+hm) DIMY in
				if ((ws,hs) <> DimSurf (!screen)) then
				begin
					screen := redimFn ws hs;
					BlitSurf (!screen) bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0)
				end;
				mapChanged := false
			end;

			let d = fst (DimSurf (!img)) / n in
			BlitSurf (!screen) (!img) (MakeRect xm ym 0 0) (MakeRect 0 0 0 0);
			DrawRect (!screen) (MakeColor 200 200 0) (MakeRect (xm+((!column)-1)*d) (ym+((!line)-1)*d) d d);
			
			DeleteEvent e;
			blitFn (!screen);
		done;
		raise Exit

	with Exit -> 
	
	do_vect DeleteCustomButton allButtons;
	DeleteSurf bkg; DeleteSurf titleSurf; DeleteSurf saveSurf; DeleteSurf quitSurf; DeleteSurf (!img);
	DeleteButton quitButton; DeleteButton saveButton;
	DeleteTB titleEdition; DeleteTB saveEdition;
	DeleteFont font;;


	
	 
