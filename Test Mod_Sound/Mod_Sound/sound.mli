type FSound
and FSoundType = STREAM | SAMPLE
and FSoundStatus = PLAY | STOP | PAUSE | UNLOAD;;

value LoadFSound : string -> FSoundType -> FSound
and DeleteFSound : FSound -> unit
and PlayFSound : FSound -> unit
and StopFSound : FSound -> unit
and InitFMOD : unit -> unit
and QuitFMOD : unit -> unit
and FSoundSpectrum : FSound -> float vect
and Sleep : int -> unit
and FSoundOK : FSound -> bool
and VoidFSound : unit -> FSound;;

value GetFSoundType : FSound -> FSoundType
and GetFSoundStatus : FSound -> FSoundStatus
and GetFSoundLength : FSound -> int;;
