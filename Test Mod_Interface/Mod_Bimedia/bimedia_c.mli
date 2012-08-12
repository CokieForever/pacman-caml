value InitSDL_c : unit -> int = 1 "InitSDL_Env"
(*and InitSDLMixer_c : unit -> int = 1 "InitSDLMixer_Env"*)
and QuitSDL_c : unit -> int = 1 "QuitSDL_Env"
(*and QuitSDLMixer_c : unit -> int = 1 "QuitSDLMixer_Env"*)
and OpenGraph_c : int -> int -> int = 2 "OpenGraph_Env"
and GetScreen_c : unit -> int = 1 "GetScreen_Env"
and WaitEvent_c : int -> int vect = 1 "WaitEvent_Env"
and DeleteEvent_c : int -> int = 1 "DeleteEvent_Env"
and SDLSleep_c : int -> int = 1 "SDLSleep_Env"
and SDLTime_c : unit -> int = 1 "SDLTime_Env"
and SetCaption_c : string -> int = 1 "SetCaption_Env"
and NewSurf_c : int -> int -> int = 2 "NewSurf_Env"
and DeleteSurf_c : int -> int = 1 "DeleteSurf_Env"
and SetSurfAlphaLevel_c : int -> int -> int = 2 "SetSurfAlphaLevel_Env"
and GetTransparencyLevel_c : unit -> int = 1 "GetTransparencyLevel_Env"
and GetOpacityLevel_c : unit -> int = 1 "GetOpacityLevel_Env"
and FillRect_c : int -> int -> int -> int -> int -> int -> int -> int -> int = 8 "FillRect_Env"
and DrawRect_c : int -> int -> int -> int -> int -> int -> int -> int -> int = 8 "DrawRect_Env"
and BlitSurface_c : int -> int -> int -> int -> int -> int -> int -> int -> int = 8 "BlitSurface_Env"
and Flip_c : int -> int = 1 "Flip_Env"
and WSurf_c : int -> int = 1 "WSurf_Env"
and HSurf_c : int -> int = 1 "HSurf_Env";;

value LoadImage_c : string -> int = 1 "LoadImage_Env";;

value RotateSurf_c : int -> int -> int = 2 "RotateSurf_Env"
and RotateSurfFast_c : int -> int -> int = 2 "RotateSurfFast_Env"
and GetPixel_c : int -> int -> int -> int vect = 3 "GetPixel_Env"
and SetPixel_c : int -> int -> int -> int -> int -> int -> int = 6 "SetPixel_Env";;

value GetKeyDownId_c : unit -> int = 1 "GetKeyDownId_Env"
and GetKeyUpId_c : unit -> int = 1 "GetKeyUpId_Env"
and GetMouseMotionId_c : unit -> int = 1 "GetMouseMotionId_Env"
and GetMouseButtonDownId_c : unit -> int = 1 "GetMouseButtonDownId_Env"
and GetMouseButtonUpId_c : unit -> int = 1 "GetMouseButtonUpId_Env"
and GetButtonLeftId_c : unit -> int = 1 "GetButtonLeftId_Env"
and GetButtonRightId_c : unit -> int = 1 "GetButtonRightId_Env"
and GetExitId_c : unit -> int = 1 "GetExitId_Env"
and GetActiveEventId_c : unit -> int = 1 "GetActiveEventId_Env"
and GetEscapeId_c : unit -> int = 1 "GetEscapeId_Env"
and GetUpId_c : unit -> int = 1 "GetUpId_Env"
and GetDownId_c : unit -> int = 1 "GetDownId_Env"
and GetLeftId_c : unit -> int = 1 "GetLeftId_Env"
and GetRightId_c : unit -> int = 1 "GetRightId_Env"
and GetReturnId_c : unit -> int = 1 "GetReturnId_Env"
and GetEnterId_c : unit -> int = 1 "GetEnterId_Env"
and GetSpaceId_c : unit -> int = 1 "GetSpaceId_Env";;

value IsMouseButtonDown_c : int -> int = 1 "IsMouseButtonDown_Env"
and IsKeyDown_c : int -> int = 1 "IsKeyDown_Env"
and GetMousePos_c : unit -> int vect = 1 "GetMousePos_Env";;

(*value LoadMusic_c : string -> int = 1 "LoadMusic_Env"
and FreeMusic_c : int -> int = 1 "FreeMusic_Env"
and LoadSound_c : string -> int = 1 "LoadSound_Env"
and FreeSound_c : int -> int = 1 "FreeSound_Env"
and PlayMusic_c : int -> int -> int = 2 "PlayMusic_Env"
and PlaySound_c : int -> int -> int = 2 "PlaySound_Env"
and PauseMusic_c : unit -> int = 1 "PauseMusic_Env"
and ResumeMusic_c : unit -> int = 1 "ResumeMusic_Env"
and StopMusic_c : unit -> int = 1 "StopMusic_Env";;*)

value LoadFont_c : string -> int -> int = 2 "LoadFont_Env"
and NewText_c : string -> int -> int -> int -> int -> int = 5 "NewText_Env"
and DeleteFont_c : int -> int = 1 "DeleteFont_Env";;
