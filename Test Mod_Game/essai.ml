#open "bimedia";;
#open "game";;
#open "sound";;


let main() =
	InitSDL();
	InitFMOD();

	let cmp = LoadDefaultCampaign() in
	
	SetCaption "Pacman";

	let a = LaunchTheCampaign cmp OpenGraph Flip (function _ -> GAME_OK) (function _ -> function _ -> ()) in
	QuitSDL();
	QuitFMOD();

in

let exec() =

	try 
		main();
		raise Exit

	with Failure s -> print_string ("Fatal error : "^s); print_newline()
	| Exit -> print_string "Well done !"; print_newline()

in printexc__f exec ();;
