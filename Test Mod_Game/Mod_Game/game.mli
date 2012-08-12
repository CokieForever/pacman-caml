#open "bimedia";;

value SPRITES_DIR_DEFAULT : string
and SOUNDS_DIR_DEFAULT : string
and SPEED_DEFAULT : int
and FPS_DEFAULT : int;;

value GetSpritesDir : unit -> string
and GetSoundsDir : unit -> string
and GetSpeed : unit -> float
and GetFPS : unit -> int;;

value SetGameStyleSheet : string -> string -> int -> int -> unit;;


type GameStatus = GAME_EXIT | GAME_DONE | GAME_OK | GAME_OVER | GAME_SAVE;;
type Campaign
and Map
and Case;;

value CASE_WALL : Case
and CASE_HOLE : Case
and CASE_VOID : Case
and CASE_GUM : Case
and CASE_BGUM : Case;;

value ColumnToX : int -> float
and LineToY : int -> float;;

value CorrectMap : Map -> unit
and PrintMap : Map -> unit
and CleanMap : Map -> unit;;

value GetMapPacManInitPosition : Map -> float*float
and GetMapGhostsInitPositions : Map -> (float*float) vect
and GetMapDim : Map -> int*int
and GetMapName : Map -> string
and GetMapFile : Map -> string;;

value SetMapCase : Map -> int -> int -> Case -> unit
and SetMapPacManInitPosition : Map -> float*float -> unit
and SetMapGhostsInitPositions : Map -> (float*float) vect -> unit
and SetMapDim : Map -> int*int -> unit
and SetMapName : Map -> string -> unit
and SetMapFile : Map -> string -> unit;;

value LaunchTheGame : Map -> Surface -> (Surface -> unit) -> (unit -> GameStatus) -> GameStatus
and LaunchTheCampaign : Campaign -> (int -> int -> Surface) -> (Surface -> unit) -> (unit -> GameStatus) -> (int -> int -> unit) -> unit;;

value LoadDefaultMap : unit -> Map
and LoadVoidMap : unit -> Map
and LoadDefaultCampaign : unit -> Campaign
and GetDimNeeded : Map -> int*int;;

value CreateWholeMapSurf : Map -> Surface;;

value LoadMap : string -> Map
and SaveMap : Map -> unit;;
