#open "bimedia";;
#open "game";;
#open "editor";;

let main() =
	InitSDL();

	let map = LoadVoidMap() in
	LaunchEditor map Flip OpenGraph;

	QuitSDL();

in

let exec() =

	try 
		main();
		raise Exit

	with Failure s -> print_string ("Fatal error : "^s); print_newline()
	| Exit -> print_string "Well done !"; print_newline()

in printexc__f exec ();;
