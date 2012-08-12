#open "bimedia_c";;

type Surface = {mutable sId:int};;
type Event = {mutable typ:EventType; mutable key:Key; mutable mx:int; mutable my:int; mutable id:int}
and EventType == int
and Key == int;;

type Rect = {mutable x:int; mutable y:int; mutable w:int; mutable h:int};;
type Color = {mutable r:int; mutable g:int; mutable b:int};;
type Anim = {mutable imgList:Surface list; mutable time:int; mutable chrono:int; mutable state:AnimState}
and AnimState = ANIM_PLAY | ANIM_STOP;;


let SDL_KEYDOWN = GetKeyDownId_c();;
let SDL_KEYUP = GetKeyUpId_c();;
let SDL_MOUSEMOTION = GetMouseMotionId_c();;
let SDL_MOUSEBUTTONDOWN = GetMouseButtonDownId_c();;
let SDL_MOUSEBUTTONUP = GetMouseButtonUpId_c();;
let SDL_BUTTON_LEFT = GetButtonLeftId_c();;
let SDL_BUTTON_RIGHT = GetButtonRightId_c();;
let SDL_QUIT = GetExitId_c();;
let SDL_ACTIVEEVENT = GetActiveEventId_c();;
let SDLK_ESCAPE = GetEscapeId_c();;
let SDLK_UP = GetUpId_c();;
let SDLK_DOWN = GetDownId_c();;
let SDLK_LEFT = GetLeftId_c();;
let SDLK_RIGHT = GetRightId_c();;
let SDLK_RETURN = GetReturnId_c();;
let SDLK_ENTER = GetEnterId_c();;
let SDLK_SPACE = GetSpaceId_c();;

let rec GetListElement l n =
	match (l,n) with
	| (h::t,0) -> h
	| (h::t,_) -> if (n>0) then GetListElement t (n-1) else failwith "Unable to get list element, incorret indice specified"
	| _ -> failwith "Unable to get list element, indice is too high";;

let GetRectComp rect =
	(rect.x, rect.y, rect.w, rect.h);;
let GetColorComp color =
	(color.r, color.g, color.b);;
let GetEventComp ev =
	(ev.typ, ev.key, ev.mx, ev.my);;
let GetIdEvent ev =
	ev.id;;
let GetSurfId surf =
	surf.sId;;

let MakeRect x y w h =
	{x=x; y=y; w=w; h=h};;
let MakeColor r g b =
	{r=r; g=g; b=b};;
let MakeEvent t k mx my =
	{typ=t; key=k; mx=mx; my=my; id=0};;
let MakeSurf id =
	{sId=id};;
let KeyFromChar c =
	int_of_char c;;
let SetEventId ev id =
	ev.id <- id;;
let SetSurfId surf id =
	surf.sId <- id;;

let IsKeyPushed e c =
	let (t,k,x,y) = GetEventComp e in
	t = SDL_KEYDOWN && k = KeyFromChar c;;
let IsExitEvent e =
	let (t,k,x,y) = GetEventComp e in
	t = SDL_QUIT;;

let MakeAnim imgList time chrono state =
	{imgList=imgList; time=time; chrono=chrono; state=state};;
let GetAnimComp anim =
	(anim.imgList, anim.time, anim.chrono, anim.state);;
let SetAnimState anim state =
	anim.state <- state;;
let SetAnimChrono anim chrono =
	anim.chrono <- chrono;;
let SetAnimImgList anim imgList =
	anim.imgList <- imgList;;

let SurfOK surf =
	GetSurfId surf <> 0;;
let DimSurf surf =
	((WSurf_c (GetSurfId surf)) , (HSurf_c (GetSurfId surf)));;
let VoidSurf() =
	MakeSurf 0;;

let SafeOperation fn surf =
	if (SurfOK surf) then fn (GetSurfId surf)
	else failwith "Error : tried to make operation on void surface";;

let SafeOperationSilent fn surf fail =
	if (SurfOK surf) then fn (GetSurfId surf)
	else
	begin
		print_string "Warning : tried to make operation on void surface";
		print_newline();
		fail
	end;;

let InitSDL() =
	if (InitSDL_c() = 0) then failwith "Unable to initialize SDL" else ();;
(*let InitSDLMixer() =
	if (InitSDLMixer_c() = 0) then failwith "Unable to initialize SDL_mixer" else ();;*)
let QuitSDL() =
	let a = QuitSDL_c() in ();;
(*let QuitSDLMixer() =
	let a = QuitSDLMixer_c() in ();;*)

let OpenGraph x y = 
	let graph = MakeSurf (OpenGraph_c x y) in
	if (not (SurfOK graph)) then failwith "Unable to open new graph" else graph;;

let SetCaption caption =
	let a = SetCaption_c caption in ();;

let GetScreen() =
	MakeSurf (GetScreen_c());;

let NewSurf w h = 
	let surf = MakeSurf (NewSurf_c w h) in
	if (not (SurfOK surf)) then failwith "Unable to create surface" else surf;;

let DeleteSurf surf =
	if (SafeOperationSilent DeleteSurf_c surf 0 = 0) then
	begin
		print_string "Unable to delete surface";
		print_newline()
	end
	else SetSurfId surf 0;;

let SetSurfAlphaLevel surf alpha =
	let fn s = SetSurfAlphaLevel_c s alpha in
	if (SafeOperationSilent fn surf 0 <> 1) then
	begin
		print_string "Unable to set alpha level";
		print_newline()
	end;;

let GetTransparencyLevel() =
	GetTransparencyLevel_c();;

let GetOpacityLevel() =
	GetOpacityLevel_c();;

let FillRect surf color rect =
	let (r,g,b) = GetColorComp color in
	let (x,y,w,h) = GetRectComp rect in
	let fn s = FillRect_c s r g b x y w h in
	if (SafeOperationSilent fn surf 0 = 0) then
	begin
		print_string "Unable to fill rect";
		print_newline()
	end;;

let DrawRect surf color rect =
	let (r,g,b) = GetColorComp color in
	let (x,y,w,h) = GetRectComp rect in
	let fn s = DrawRect_c s r g b x y w h in
	if (SafeOperationSilent fn surf 0 = 0) then
	begin
		print_string "Unable to draw rect";
		print_newline()
	end;;


let BlitSurf tSurf sSurf tRect sRect =
	if ((not (SurfOK tSurf)) || (not (SurfOK sSurf))) then
	begin
		print_string "Warning : tried to blit with void surface(s)";
		print_newline()
	end
	else
		let id1 = GetSurfId tSurf and id2 = GetSurfId sSurf in
		let (xt, yt, wt, ht) = GetRectComp tRect in
		let (xs, ys, ws, hs) = GetRectComp sRect in
		if ((BlitSurface_c id1 id2 xt yt xs ys ws hs) = 0) then failwith "Unable to blit surface" else ();;

let Flip surf =
	let a = SafeOperation Flip_c surf in ();;

let LoadImage name =
	let img = MakeSurf (LoadImage_c name) in
	if (not (SurfOK img)) then failwith "Unable to load image" else img;;


let LoadAnim nameList time =
	let imgList = map LoadImage nameList in
	MakeAnim imgList time (SDLTime_c()) ANIM_STOP;;

let DeleteAnim anim =
	let (imgList,_,_,_) = GetAnimComp anim in
	do_list DeleteSurf imgList;
	SetAnimImgList anim [];;

let PlayAnim anim =
	SetAnimState anim ANIM_PLAY;;

let StopAnim anim =
	SetAnimState anim ANIM_STOP;;

let ResetAnim anim =
	SetAnimChrono anim (SDLTime_c());;

let GetAnimSurf anim =
	let t = SDLTime_c() and (imgList,time,chrono,state) = GetAnimComp anim in
	if (imgList = [] || time <= 0) then failwith "Unable to get anim surface, incorrect anim specified";
	if (state <> ANIM_PLAY) then hd imgList
	else
		let length = list_length imgList in
		let n = ((t-chrono)/time) mod length in
		GetListElement imgList n;;

let BlitAnim tSurf anim tRect sRect =
	BlitSurf tSurf (GetAnimSurf anim) tRect sRect;;

let ApplyFunctionToAnim fn anim =
	let (imgList,time,chrono,state) = GetAnimComp anim in
	MakeAnim (map fn imgList) time chrono state;;
		
	
	

let RotateSurf angle surf =
	let fn s = MakeSurf (RotateSurf_c s angle) in
	let surf2 = SafeOperation fn surf in
	if (not (SurfOK surf2)) then failwith "Unable to rotate surface" else surf2;;

let RotateSurfFast rot surf =
	let fn s = MakeSurf (RotateSurfFast_c s rot) in
	let surf2 = SafeOperation fn surf in
	if (not (SurfOK surf2)) then failwith "Unable to fast-rotate surface" else surf2;;

let GetPixel surf x y =
	let fn s = GetPixel_c s x y in
	let tab = SafeOperation fn surf in
	MakeColor (tab.(0)) (tab.(1)) (tab.(2));;

let SetPixel surf color x y =
	let (r,g,b) = GetColorComp color in
	let fn s = SetPixel_c s r g b x y in
	if (SafeOperationSilent fn surf 0 = 0) then
	begin
		print_string "Unable to set pixel color";
		print_newline()
	end;;


let WaitEvent poll =
	let p = ref 0 in
	if (poll) then p := 1;
	let v = WaitEvent_c (!p) in
	let e = MakeEvent (v.(0)) (v.(1)) (v.(2)) (v.(3)) in
	SetEventId e (v.(4));
	e;;

let DeleteEvent e =
	let x = DeleteEvent_c (GetIdEvent e) in ();;

let SDLSleep t =
	let x = SDLSleep_c t in ();;

let SDLTime() =
	SDLTime_c();;

let IsMouseButtonDown button =
	IsMouseButtonDown_c button = 1;;

let IsKeyDown key =
	let r = IsKeyDown_c key in
	if (r = -1) then failwith "Incorrect key specified";
	r = 1;;

let GetMousePos() =
	let t = GetMousePos_c() in
	(t.(0), t.(1));;




(*type Music == int;;
type Sound == int;;

let MusicOK music =
	music <> 0;;
let SoundOK sound =
	sound <> 0;;

let VoidMusic() =
	0;;
let VoidSound() =
	0;;

let LoadMusic name =
	let music = LoadMusic_c name in
	if (not (MusicOK music)) then failwith "Unable to load music" else music;;

let FreeMusic music =
	let a = FreeMusic_c music in ();;

let LoadSound name =
	let sound = LoadSound_c name in
	if (not (SoundOK sound)) then failwith "Unable to load sound" else sound;;

let FreeSound sound =
	let a = FreeSound_c sound in ();;

let PlayMusic music loops =
	if ((PlayMusic_c music loops) = 0) then failwith "Unable to play music" else ();;
let PlaySound sound loops =
	if ((PlaySound_c sound loops) = 0) then failwith "Unable to play sound" else ();;

let PauseMusic() =
	let a = PauseMusic_c() in ();;
let ResumeMusic() =
	let a = ResumeMusic_c() in ();;
let StopMusic() =
	let a = StopMusic_c() in ();;*)



type Font = {mutable fId:int};;

let MakeFont id =
	{fId=id};;
let GetFontId font =
	font.fId;;
let SetFontId font id =
	font.fId <- id;;

let FontOK font =
	GetFontId font <> 0;;
let VoidFont() =
	MakeFont 0;;

let LoadFont name size =
	let font = MakeFont (LoadFont_c name size) in
	if (not (FontOK font)) then failwith "Unable to load font" else font;;

let DeleteFont font =
	if (FontOK font) then let a = DeleteFont_c (GetFontId font) in ()
	else
	begin
		print_string "Warning : tried to delete void font";
		print_newline()
	end;;

let NewText text font color =
	if (not (FontOK font)) then failwith "Error : tried to use void font";
	let (r,g,b) = GetColorComp color in
	let surf = MakeSurf (NewText_c text (GetFontId font) r g b) in
	if (not (SurfOK surf)) then failwith "Unable to create text surface" else surf;;


