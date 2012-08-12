#open "sound_c";;

type FSound = {mutable fsId:int; mutable fsType:FSoundType};;

let MakeFSound id typ =
	{fsId=id; fsType=typ};;

let GetFSoundComp fs =
	(fs.fsId, fs.fsType);;

let SetFSoundId fs id =
	fs.fsId <- id;;

let VoidFSound() =
	MakeFSound 0 STREAM;;

let FSoundOK sound =
	fst (GetFSoundComp sound) <> 0;;


let InitFMOD() =
	if (InitFMOD_c() = 0) then failwith "Unable to initialize FMOD" else ();;

let QuitFMOD() =
	if (QuitFMOD_c() = 0) then failwith "Unable to quit FMOD" else ();;


let LoadFSound nom typ =
	let sound =
		match typ with
		| SAMPLE -> MakeFSound (LoadFSound_c nom) SAMPLE
		| _ -> MakeFSound (LoadFStream_c nom) STREAM
	in
	if (not (FSoundOK sound)) then failwith "Unable to load the sound" else sound;;

let GetFSoundType sound =
	snd (GetFSoundComp sound);;

let GetFSoundLength sound =
	if (not (FSoundOK sound)) then failwith "Error : tried to get length of void sound";
	let l = GetFSoundLength_c (fst (GetFSoundComp sound)) in
	if (l < 0) then failwith "Unable to get sound length" else l;;

let GetFSoundStatus sound =
	if (not (FSoundOK sound)) then failwith "Error : tried to get status of void sound";
	match GetFSoundStatus_c (fst (GetFSoundComp sound)) with
	| 0 -> STOP
	| 1 -> PLAY
	| 2 -> PAUSE
	| _ -> UNLOAD;;

let DeleteFSound sound =
	if (not (FSoundOK sound)) then
	begin
		print_string "Warning : tried to free void sound";
		print_newline()
	end
	else
	begin
		if (DeleteFSound_c (fst (GetFSoundComp sound)) = 0) then
		begin
			print_string "Unable to delete the sound";
			print_newline()
		end
		else SetFSoundId sound 0
	end;;

let PlayFSound sound =
	if (not (FSoundOK sound)) then failwith "Error : tried to play void sound";
	if (PlayFSound_c (fst (GetFSoundComp sound)) = 0) then failwith "Unable to play the sound" else ();;

let StopFSound sound =
	if (not (FSoundOK sound)) then failwith "Error : tried to stop void sound";
	if (StopFSound_c (fst (GetFSoundComp sound)) = 0) then failwith "Unable to stop the sound" else ();;

let FSoundSpectrum sound =
	if (not (FSoundOK sound)) then failwith "Error : tried to get spectrum of void sound";
	FSoundSpectrum_c (fst (GetFSoundComp sound));;

let Sleep ms =
	let r = Sleep_c ms in ();;




