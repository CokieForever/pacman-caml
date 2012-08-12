value LoadFSound_c : string -> int = 1 "LoadFSound_Env"
and LoadFStream_c : string -> int = 1 "LoadFStream_Env"
and DeleteFSound_c : int -> int = 1 "DeleteFSound_Env"
and PlayFSound_c : int -> int = 1 "PlayFSound_Env"
and StopFSound_c : int -> int = 1 "StopFSound_Env"
and InitFMOD_c : unit -> int = 1 "InitFMOD_Env"
and QuitFMOD_c : unit -> int = 1 "QuitFMOD_Env"
and FSoundSpectrum_c : int -> float vect = 1 "FSoundSpectrum_Env"
and Sleep_c : int -> int = 1 "Sleep_Env";;

value GetFSoundLength_c : int -> int = 1 "GetFSoundLength_Env"
and GetFSoundStatus_c : int -> int = 1 "GetFSoundStatus_Env";;
