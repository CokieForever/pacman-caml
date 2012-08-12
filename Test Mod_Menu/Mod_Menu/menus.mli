#open "bimedia";;

type Menu;;

type MenuStatus = MENU_QUIT | MENU_BACK | MENU_OK | MENU_DONE;;

value SetDisplayParam : Surface -> Surface -> Font -> Color -> unit;;

value VoidMenu : unit -> Menu
and LoadMenu : string -> Menu
and DeleteMenu : Menu -> unit
and BlitMenu : Surface -> Menu -> int ->  unit
and HandleMenu : Surface -> Menu -> (Surface -> unit) -> (Event -> MenuStatus) -> (string -> MenuStatus) -> unit;;
