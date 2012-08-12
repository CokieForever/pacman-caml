value NewTB_c : int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int -> int = 11 "NewTB_Env"
and DeleteTB_c : int -> int = 1 "DeleteTB_Env"
and UpdateTB_c : int -> int = 1 "UpdateTB_Env"
and DisplayTB_c : int -> int = 1 "DisplayTB_Env"
and HoldTB_c : int -> int -> int  = 2 "HoldTB_Env";;

value SetTBText_c : int -> string -> int = 2 "SetTBText_Env"
and GetTBText_c : int -> string = 1 "GetTBText_Env";;

value SetTBFocus_c : int -> int -> int = 2 "SetTBFocus_Env"
and GetTBFocus_c : int -> int = 1 "GetTBFocus_Env";;

value GetTBStyleNormalId_c : unit -> int = 1 "GetTBStyleNormalId_Env"
and GetTBStyleMultilineId_c : unit -> int = 1 "GetTBStyleMultilineId_Env"
and GetTBStyleVScrollId_c : unit -> int = 1 "GetTBStyleVScrollId_Env"
and GetTBStyleHScrollId_c : unit -> int = 1 "GetTBStyleHScrollId_Env"
and GetTBStyleAutojumpId_c : unit -> int = 1 "GetTBStyleAutojumpId_Env"
and GetTBStyleReadOnlyId_c : unit -> int = 1 "GetTBStyleReadOnlyId_Env"
and GetTBStyleJustDisplayId_c : unit -> int = 1 "GetTBStyleJustDisplayId_Env";;
