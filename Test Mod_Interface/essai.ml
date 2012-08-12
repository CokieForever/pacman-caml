#open "bimedia";;
#open "interface";;

let IsExitEvent e =
	let (t,_,_,_) = GetEventComp e in
	t = SDL_QUIT;;

let main() =
	InitSDL();
	let screen = OpenGraph 700 438 in

	SetCaption "Pacman : testing module interface";

	let font = LoadFont "Fonts/default.ttf" 20 in
	let textButton = NewText "Click me !" font (MakeColor 0 0 0) and textCD = NewText "Move me !" font (MakeColor 0 0 0) in
	let bkg = LoadImage "Images/bkg.jpg" in
	let (w,h) = DimSurf textButton and (w2,h2) = DimSurf textCD in
	let x = 20 and y = 20 in
	let button = NewSurfButton textButton (x,y) in
	let (ws,hs) = DimSurf screen in
	let cd = NewSurfCD textCD (x,y+20) (MakeRect 0 0 (ws-w2) (hs-h2)) in
	
	let textDL = NewText "DropList" font (MakeColor 0 0 0) in
	let dlButton = NewSurfButton textDL (x+400,y) in
	let dl = NewSurfDL textDL (x+400,y+20) ["Choix 1";"Choix 2";"Choix 3";"Choix 4";"Choix 5"] font (MakeColor 0 0 0) (MakeColor 255 255 255) dlButton in

	BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
	let tb = NewTB screen 100 (MakeRect x (y+50) 300 100) font (MakeColor 0 0 0) [TB_STYLE_MULTILINE;TB_STYLE_VSCROLL;TB_STYLE_AUTOJUMP] in
	
	let tmp = SetTBText tb "Edit me !" in

	try
		while true do
			let e = WaitEvent false in
			if (IsExitEvent e) then
			begin
				DeleteEvent e;
				raise Exit
			end;

			BlitSurf screen bkg (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			BlitSurf screen textButton (MakeRect x y 0 0) (MakeRect 0 0 0 0);

			(match HoldButton e button with
			| BUTTON_PUSHED -> DrawRect screen (MakeColor 255 0 0) (MakeRect x y w h)
			| BUTTON_HOT -> DrawRect screen (MakeColor 255 255 0) (MakeRect x y w h)
			| BUTTON_CLICKED -> print_string "Button clicked !"; print_newline(); DrawRect screen (MakeColor 255 255 0) (MakeRect x y w h)
			| _ -> ());

			HoldAllCDs e;
			let state = GetCDState cd in
			let (xcd,ycd) = GetCDPosition cd in
			BlitSurf screen textCD (MakeRect xcd ycd 0 0) (MakeRect 0 0 0 0);

			(match state with
			| CD_DROPPED -> print_string "Text Dropped !"; print_newline()
			| _ -> ());

			(match HoldDL e dl with
			| (x,DL_SELECTED) -> print_string ("Choice "^(string_of_int (x+1))^" selected !"); print_newline()
			| _ -> ());
			BlitSurf screen textDL (MakeRect (x+400) y 0 0) (MakeRect 0 0 0 0);
			DisplayDL screen dl;
			
			HoldTB tb e;
			DisplayTB tb;
		
			DeleteEvent e;
			Flip screen
		done
	with Exit ->
	
	DeleteFont font;
	DeleteSurf textButton; DeleteSurf textCD; DeleteSurf textDL;
	DeleteSurf bkg;
	DeleteButton button; DeleteButton dlButton;
	DeleteCD cd;
	DeleteDL dl;
	DeleteTB tb;
	
	QuitSDL();;



try
	main();
	raise Exit
with Failure s -> print_string ("Fatal error : "^s); print_newline(); QuitSDL()
| Exit -> print_string "Well done !"; print_newline();;
		
		
		
