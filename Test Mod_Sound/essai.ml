#open "sound";;
#open "graphics";;

let GetTickCount() = int_of_float (sys__time() *. 1000.);;

let main() =
	InitFMOD();
	let sound = LoadFSound "Sounds/attack.mp3" STREAM and t = ref (GetTickCount()) in
	
	PlayFSound sound;
	try
		while true
		do
			let sp = FSoundSpectrum(sound) in
			set_color black;
			fill_rect 0 0 (size_x()) (size_y());
			for i=0 to 50 do
				let h = ref (int_of_float (sp.(i) *. 500.)) and j = ref 5 in
				if ((!h)>300) then h := 300 else ();
				
				while ((!j) <= (!h)) do
					let v = (255 - int_of_float ((float_of_int (!j)) /. 300. *. 255.)) in
					if (v < 0) then
						set_color (rgb 255 0 0)
					else set_color (rgb 255 v 0);
					fill_rect (i*10) ((!j)-5) 10 5;
					j := (!j) + 5
				done;
			done;

			let e = wait_next_event [Poll] in
			if (e.keypressed && (e.key = `Q` || e.key = `q`)) then raise Exit;
			
			if (GetTickCount() - (!t) < 40) then Sleep(40 - (GetTickCount() - (!t)));
			t := GetTickCount()
		done;
	with Exit -> close_graph();

	StopFSound(sound);
	DeleteFSound(sound);

	QuitFMOD();;

open_graph " 500x300";;
try
	main();
	raise Exit
with Failure s -> print_string "Error : "; print_string s
|Exit -> print_string "Well done !";;
print_newline();;


