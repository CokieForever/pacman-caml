type Surface;;
type Event;;
type Rect;;
type Color;;
type EventType;;
type Key;;
type Anim;;

value SDL_KEYDOWN : EventType
and SDL_KEYUP : EventType
and SDL_MOUSEMOTION : EventType
and SDL_MOUSEBUTTONDOWN : EventType
and SDL_MOUSEBUTTONUP : EventType
and SDL_BUTTON_LEFT : Key
and SDL_BUTTON_RIGHT : Key
and SDL_QUIT : EventType
and SDL_ACTIVEEVENT : EventType
and SDLK_ESCAPE : Key
and SDLK_DOWN : Key
and SDLK_UP : Key
and SDLK_LEFT : Key
and SDLK_RIGHT : Key
and SDLK_RETURN : Key
and SDLK_ENTER : Key
and SDLK_SPACE : Key;;

value GetRectComp : Rect -> int*int*int*int
and GetColorComp : Color -> int*int*int
and GetEventComp : Event -> EventType*Key*int*int
and GetIdEvent : Event -> int;;

value MakeRect : int -> int -> int -> int -> Rect
and MakeColor : int -> int -> int -> Color
and KeyFromChar : char -> Key;;

value IsKeyPushed : Event -> char -> bool
and IsExitEvent : Event -> bool;;

value InitSDL : unit -> unit
(*and InitSDLMixer : unit -> unit*)
and QuitSDL : unit -> unit
(*and QuitSDLMixer : unit -> unit*);;

value OpenGraph : int -> int -> Surface
and SetCaption : string -> unit
and GetScreen : unit -> Surface;;

value NewSurf : int -> int -> Surface
and DeleteSurf : Surface -> unit;;

value SetSurfAlphaLevel : Surface -> int -> unit
and GetTransparencyLevel : unit -> int
and GetOpacityLevel : unit -> int;;

value FillRect : Surface -> Color -> Rect -> unit
and DrawRect : Surface -> Color -> Rect -> unit
and BlitSurf : Surface -> Surface -> Rect -> Rect -> unit
and Flip : Surface -> unit;;

value LoadImage : string -> Surface;;

value LoadAnim : string list -> int -> Anim
and DeleteAnim : Anim -> unit
and PlayAnim : Anim -> unit
and StopAnim : Anim -> unit
and ResetAnim : Anim -> unit
and GetAnimSurf : Anim -> Surface
and BlitAnim : Surface -> Anim -> Rect -> Rect -> unit
and ApplyFunctionToAnim : (Surface -> Surface) -> Anim -> Anim;;

value RotateSurf : int -> Surface -> Surface
and RotateSurfFast : int -> Surface -> Surface;;

value GetPixel : Surface -> int -> int -> Color
and SetPixel : Surface -> Color -> int -> int -> unit;;

value WaitEvent : bool -> Event
and DeleteEvent : Event -> unit;;

value SDLSleep : int -> unit
and SDLTime : unit -> int;;

value SurfOK : Surface -> bool
and DimSurf : Surface -> int*int
and VoidSurf : unit -> Surface
and GetSurfId : Surface -> int;;

value IsMouseButtonDown : Key -> bool
and IsKeyDown : Key -> bool
and GetMousePos : unit -> int*int;;


(*type Music;;
type Sound;;


value LoadMusic : string -> Music
and FreeMusic : Music -> unit;;

value LoadSound : string -> Sound
and FreeSound : Sound -> unit;;

value PlayMusic : Music -> int -> unit
and PauseMusic : unit -> unit
and ResumeMusic : unit -> unit
and StopMusic : unit -> unit;;

value PlaySound : Sound -> int -> unit;;

value MusicOK : Music -> bool
and SoundOK : Sound -> bool
and VoidMusic : unit -> Music
and VoidSound : unit -> Sound;;*)



type Font;;

value LoadFont : string -> int -> Font
and DeleteFont : Font -> unit
and FontOK : Font -> bool
and VoidFont : unit -> Font
and GetFontId : Font -> int;;

value NewText : string -> Font -> Color -> Surface;;
