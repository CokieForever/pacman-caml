#open "bimedia";;


let main() =
let screen = OpenGraph 700 438 and tmp = ref false in

let img = LoadImage "Images/bkg.jpg" in
let icon = LoadImage "Images/icone.png" in
let icon2 = RotateSurf 30 icon in
let anim = LoadAnim ["Images/anim0.png"; "Images/anim1.png"; "Images/anim2.png"; "Images/anim3.png"; "Images/anim4.png"; "Images/anim5.png"; "Images/anim6.png"; "Images/anim7.png"] 150 in
PlayAnim anim;

SetCaption "SDL test !";

try
	while true
	do
		let e = WaitEvent true in
		if ((IsExitEvent e) || (IsKeyPushed e `q`) || (IsKeyPushed e `Q`)) then
		begin
			DeleteEvent e;
			raise Exit
		end;
		
		BlitSurf screen img (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
		BlitSurf screen icon (MakeRect 50 50 0 0) (MakeRect 0 0 0 0);
		BlitSurf screen icon2 (MakeRect 200 50 0 0) (MakeRect 0 0 0 0);
		BlitAnim screen anim (MakeRect 50 250 0 0) (MakeRect 0 0 0 0);
		
		DeleteEvent e;

		SDLSleep 20;
		Flip screen;
	done
with Exit ->

DeleteSurf img; DeleteSurf icon; DeleteSurf icon2;
QuitSDL();;



try 
	main();
	raise Exit
with Failure s -> print_string "Error : "; print_string s
| Exit -> print_string "Well done !";;
print_newline();;
