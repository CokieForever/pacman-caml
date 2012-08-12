#open "io";;
#open "bimedia";;

type Menu = {mutable title:string; mutable bkg:Surface; mutable data:Data}
and Data = Action of string | SubMenus of Menu list;;

exception Out;;

let GetMenuComp menu =
	(menu.title, menu.bkg, menu.data);;
let MakeMenu title bkg data =
	{title=title; bkg=bkg; data=data};;

let SetMenuBkg menu bkg =
	menu.bkg <- bkg;;
let SetMenuTitle menu title =
	menu.title <- title;;
let SetMenuData menu data =
	menu.data <- data;;

let VoidMenu() =
	MakeMenu "PacMan" (VoidSurf()) (Action "");;

let mainArrowLeft = ref (VoidSurf());;
let mainArrowRight = ref (VoidSurf());;
let mainFont = ref (VoidFont());;
let mainColor = ref (MakeColor 0 0 0);;

let GetDisplayParam() =
	((!mainArrowLeft), (!mainArrowRight), (!mainFont), (!mainColor));;

let SetDisplayParam aL aR f c =
	mainArrowLeft := aL;
	mainArrowRight := aR;
	mainFont := f;
	mainColor := c;;


let ReadString file =
	let buf = ref "" and n=ref 0 in
	try
		while (true) do
			buf := (try input_line file with _ -> failwith "ReadString : Unable to read a line");
			n := string_length (!buf);
	
			for i=0 to ((!n)-1) do
				if (nth_char (!buf) i) <> `\t` then 
				begin
					buf := (try sub_string (!buf) i ((!n)-i) with _ -> failwith "ReadString : Substring Error");
					raise Exit
				end
			done
		done;
		raise Exit
	with Exit -> (!buf);;



let LoadMenu filename =

	let rec RM_rec file title =

		let menu = VoidMenu() and line = ReadString file in
		if ((nth_char line 0) = `{`) then 
			let str1 = ReadString file in
			if (str1 = "(none)") then SetMenuBkg menu (VoidSurf())
			else SetMenuBkg menu (try LoadImage str1 with _ -> VoidSurf());
			
			SetMenuTitle menu title;

			try
				while (true) do
					let str2 = ReadString file in
					if ((nth_char str2 0) = `}`) then raise Exit;
		
					let (_,_,data) = GetMenuComp menu in
					match data with
					| SubMenus x -> SetMenuData menu (SubMenus (x@[RM_rec file str2]))
					| _ -> SetMenuData menu (SubMenus [RM_rec file str2])
				done;
				raise Exit
			with Exit -> menu;

		else MakeMenu title (VoidSurf()) (Action line);

	in

	let file = (try open_in filename with _ -> failwith ("ReadMenu : Unable to load the file "^filename)) in
	let menu = RM_rec file "PacMan" in
	close_in file;
	menu;;

let rec DeleteMenu menu =
	let (_,bkg,data) = GetMenuComp menu in
	if (SurfOK bkg) then DeleteSurf bkg;
	
	match data with
	| SubMenus l -> do_list DeleteMenu l
	| _ -> ();;
	

let BlitMenu surf menu pos =

	let (_,bkg,data) = GetMenuComp menu and (arrowL,arrowR,font,color) = GetDisplayParam() in
	match data with
	| Action _ -> failwith "AfficherMenu : Parametre menu incorrect"
	| SubMenus menuList ->

	let l = 30 and n = list_length menuList in
	let (w,h) = DimSurf surf and (wal,hal) = DimSurf arrowL and (war,har) = DimSurf arrowR in
	let y0 = (h-(l*n))/2 in
	
	BlitSurf surf bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	
	let rec BM_rec ml i =
		match ml with
		| head::tail ->
			let (title,_,_) = GetMenuComp head in
			let text = NewText title font color in
			let (w1,h1) = DimSurf text in
			let x = (w-w1)/2 and y = i*l+y0 in
			BlitSurf surf text (MakeRect x y 0 0) (MakeRect 0 0 0 0);
			DeleteSurf text;

			if (i=pos) then
			begin
				if (SurfOK arrowL) then BlitSurf surf arrowL (MakeRect (x-wal-5) ((h1-hal)/2+y) 0 0) (MakeRect 0 0 0 0);
				if (SurfOK arrowR) then BlitSurf surf arrowR (MakeRect (x+w1+5) ((h1-har)/2+y) 0 0) (MakeRect 0 0 0 0)
			end;
			
			BM_rec tail (i+1)

		| _ -> ();
	in
	BM_rec menuList 0;;




let IsKeyPressed event key =
	let (t,k,_,_) = GetEventComp event in
	t = SDL_KEYDOWN && k = key;;

let HandleMenu surf m blitFn relayFn actionFn =

	let rec HM_rec menu =
		let (_,_,data) = GetMenuComp menu in
		
		match data with
		| Action "QUIT" -> raise Out
		| Action "BACK" -> raise Exit
		| Action s -> 
			(match (actionFn s) with
			| MENU_QUIT -> raise Out
			| MENU_BACK -> raise Exit
			| _ -> ())

		| SubMenus menuList ->
			let l = list_length menuList and out = ref false in
			let pos = ref 0 in

			try
				while true
				do
					BlitMenu surf menu (!pos);
					blitFn surf;
				
					let e = WaitEvent false in
					match (relayFn e) with
					| MENU_QUIT -> DeleteEvent e; raise Out
					| MENU_BACK -> DeleteEvent e; raise Exit
					| MENU_DONE -> DeleteEvent e
					| MENU_OK ->
						if (IsKeyPressed e SDLK_UP) then pos := (!pos)-1;
						if (IsKeyPressed e SDLK_DOWN) then pos := (!pos)+1;

						if ((IsKeyPressed e SDLK_SPACE) || (IsKeyPressed e SDLK_ENTER) || (IsKeyPressed e SDLK_RETURN)) then 
							HM_rec ((vect_of_list menuList).(!pos));

						if ((!pos)<0) then pos := l-1;
						if ((!pos)>=l) then pos := 0;

						DeleteEvent e
				done;
			with Exit -> ();
	in
	
	let (_,bkg,_) = GetMenuComp m and r = ref false in
	if (not (SurfOK bkg)) then
	begin
		let (w,h) = DimSurf surf in
		let s = NewSurf w h in
		BlitSurf s surf (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
		SetMenuBkg m s;
		r := true
	end;

	(try HM_rec m with Out -> ());
	
	if (!r) then
		let (_,bkg2,_) = GetMenuComp m in
		DeleteSurf bkg2;
		SetMenuBkg m (VoidSurf());;

