#open "bimedia";;

type Id
and Shape = RECT | CIRCLE;;

type Button
and ButtonState = BUTTON_CLICKED | BUTTON_PUSHED | BUTTON_HOT | BUTTON_COLD;;

type ClicDrop
and CDState = CD_CAUGHT | CD_DROPPED | CD_HOT | CD_COLD;;

type DropList
and DLState = DL_EXTENDED | DL_SELECTED | DL_COLD;;

type ItemList
and ILState = IL_COLD;;

type TextBox
and TBStyle;;

value NewButton : Rect -> Shape -> Button
and NewSurfButton : Surface -> int*int -> Button
and DeleteButton : Button -> unit;;

value GetButtonState : Button -> ButtonState
and HoldButton : Event -> Button -> ButtonState
and HoldAllButtons : Event -> unit;;

value NewCD : Rect -> Shape -> Rect -> ClicDrop
and NewSurfCD : Surface -> int*int -> Rect -> ClicDrop
and DeleteCD : ClicDrop -> unit;;

value GetCDPosition : ClicDrop -> int*int
and SetCDPosition :  int*int -> ClicDrop-> unit;;

value GetCDState : ClicDrop -> CDState
and HoldCD : Event -> ClicDrop -> CDState
and HoldAllCDs : Event -> unit;;

value NewDL : Rect -> string list -> Font -> Color -> Color -> Button -> DropList
and NewSurfDL : Surface -> int*int -> string list -> Font -> Color -> Color -> Button -> DropList
and DeleteDL : DropList -> unit;;

value GetDLState : DropList -> int*DLState
and SetDLNumSelected : DropList -> int -> unit
and HoldDL : Event -> DropList -> int*DLState
and HoldAllDLs : Event -> unit;;

value DisplayDL : Surface -> DropList -> unit
and DisplayAllDLs : Surface -> unit;;


value NewTB : Surface -> int -> Rect -> Font -> Color -> TBStyle list -> TextBox
and DeleteTB : TextBox -> unit
and UpdateTB : TextBox -> unit
and DisplayTB : TextBox -> unit
and HoldTB : TextBox -> Event -> unit;;

value SetTBText : TextBox -> string -> int
and GetTBText : TextBox -> string;;

value SetTBFocus : TextBox -> bool -> unit
and GetTBFocus : TextBox -> bool;;

value TB_STYLE_NORMAL : TBStyle
and TB_STYLE_MULTILINE : TBStyle
and TB_STYLE_VSCROLL : TBStyle
and TB_STYLE_HSCROLL : TBStyle
and TB_STYLE_AUTOJUMP : TBStyle
and TB_STYLE_READONLY : TBStyle
and TB_STYLE_JUSTDISPLAY : TBStyle;;
