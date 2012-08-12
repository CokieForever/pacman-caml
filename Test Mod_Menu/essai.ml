#open "bimedia";;
#open "menus";;
#open "sound";;


let main() =
	try
		InitSDL();
		InitFMOD();
		
		let screen = OpenGraph 700 438 in
		SetCaption "PacMan : testing module menu";
		let menu = LoadMenu "Menus/cfg.txt" in

		let arrowL = LoadImage "Images/arrowL.png" in
		let arrowR = LoadImage "Images/arrowR.png" in
		let font = LoadFont "Fonts/default.ttf" 25 in
		SetDisplayParam arrowL arrowR font (MakeColor 0 0 0);

		let music = try LoadFSound "Musics/ghostbusters.ogg" STREAM with Failure _ -> VoidFSound() in
		if (FSoundOK music) then PlayFSound music;
		let actionFn s = 
			print_string ("Action : "^s);
			print_newline();
			MENU_OK;
		in
		HandleMenu screen menu Flip (function _ -> MENU_OK) actionFn;

		if (FSoundOK music) then 
		begin
			StopFSound music;
			DeleteFSound music
		end;
		
		DeleteMenu menu;
		DeleteSurf arrowL;
		DeleteSurf arrowR;
		DeleteFont font;
		QuitFMOD();
		QuitSDL();
		raise Exit
	with Failure s -> print_string "Fatal error : "; print_string s
	| Exit -> print_string "Well done !";;

main();;
print_newline();;
