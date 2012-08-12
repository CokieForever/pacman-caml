#open "bimedia";;
#open "tecaml";;

type Id == int;;

type Button = {mutable bRect:Rect; mutable bId:Id; mutable bShape:Shape; mutable bState:ButtonState};;
let buttonList = ref [];;

type ClicDrop = {mutable cdRect:Rect; mutable cdId:Id; mutable cdShape:Shape; mutable cdState:CDState; mutable anchor:int*int; mutable framework:Rect};;
let cdList = ref [];;

type DropList = {mutable dlRect:Rect; mutable dlId:Id; mutable dropButton:Button; mutable dlState:DLState; mutable buttonList:Button list; mutable surfBG:Surface; mutable dlNumSelected:int; mutable color:Color};;
let dlList = ref [];;

type ItemList = {mutable ilRect:Rect; mutable ilId:Id; mutable columns:(int*string) list; mutable ilState:ILState; mutable itemList:(string list) list; mutable ilNumSelected:int};;
let ilList = ref [];;

type TextBox == int
and TBStyle == int;;


(*indoor procedures*)

(*global*)
let IsInRect coord rect =
	let (cx,cy) = coord and (x,y,w,h) = GetRectComp rect in
	cx >= x && cx <= (x+w) && cy >= y && cy <= (y+h);;

let IsInCircle coord rect =
	let (cx,cy) = coord and (x,y,r,_) = GetRectComp rect in
	(cx-x)*(cx-x)+(cy-y)*(cy-y) <= r*r;;

let GetFreeId l2 fn =
	let rec GFI_rec l m =
		match l with
		| [] -> m+1
		| h::t ->
			let id = fn h in
			if (id > m) then GFI_rec t id else GFI_rec t m;
	in
	GFI_rec l2 0;;

let GetObject l2 fn id =
	let rec GO_rec l =
		match l with
		| [] -> failwith "GetObject : object not found"
		| h::t ->
			let id2 = fn h in
			if (id2 = id) then h else GO_rec t;
	in
	GO_rec l2;;

let SaveObject l obj =
	l := obj::(!l);;

let DeleteObject obj l2 fn fn2 =
	let id = fn obj in
	let rec DO_rec l =
		match l with
		| [] -> failwith "DeleteObject : object not found, maybe already deleted"
		| h::t ->
			let id2 = fn h in
			if (id2 = id) then t else h::(DO_rec t);
	in
	l2 := DO_rec (!l2);
	fn2 obj (-1);;


(*Buttons*)
let MakeButton rect id shape =
	{bRect=rect; bId=id; bShape=shape; bState=BUTTON_COLD};;

let SetButtonState button state =
	button.bState <- state;;

let SetButtonId button id =
	button.bId <- id;;

let GetButtonComp button =
	(button.bRect, button.bId, button.bShape, button.bState);;

let GetButtonId button =
	let (_,id,_,_) = GetButtonComp button in
	id;;

let IsInButton coord button =
	let (r,_,t,_) = GetButtonComp button in
	match t with
	| RECT -> IsInRect coord r
	| CIRCLE -> IsInCircle coord r;;

let GetFreeId_Button() =
	GetFreeId (!buttonList) GetButtonId;;

let GetButton id =
	GetObject (!buttonList) GetButtonId id;;

let SaveButton button =
	SaveObject buttonList button;;


(*"Clic and Drop" (called "CD")*)
let MakeCD rect id shape fw =
	{cdRect=rect; cdId=id; cdShape=shape; cdState=CD_COLD; anchor=(0,0); framework=fw};;

let GetCDComp cd =
	(cd.cdRect, cd.cdId, cd.cdShape, cd.cdState);;

let GetCDAnchor cd =
	cd.anchor;;

let GetCDFramework cd =
	cd.framework;;

let GetCDId cd =
	let (_,id,_,_) = GetCDComp cd in
	id;;

let SetCDState cd state =
	cd.cdState <- state;;

let SetCDPosition coord cd =
	let (x,y) = coord and (rect,_,_,_) = GetCDComp cd in
	let (_,_,w,h) = GetRectComp rect in
	cd.cdRect <- MakeRect x y w h;;

let SetCDAnchor coord cd =
	cd.anchor <- coord;;

let SetCDId cd id =
	cd.cdId <- id;;

let IsInCD coord cd =
	let (r,_,sh,_) = GetCDComp cd in
	match sh with
	| RECT -> IsInRect coord r
	| CIRCLE -> IsInCircle coord r;;

let GetFreeId_CD() =
	GetFreeId (!cdList) GetCDId;;

let GetCD id =
	GetObject (!cdList) GetCDId id;;

let SaveCD cd =
	SaveObject cdList cd;;


(*Drop Lists (called "DL")*)

let MakeDL rect id db =
	{dlRect=rect; dlId=id; dropButton=db; dlState=DL_COLD; buttonList=[]; surfBG=VoidSurf(); dlNumSelected=(-1); color=MakeColor 0 0 0};;

let GetDLComp dl =
	(dl.dlRect, dl.dlId, dl.dropButton, dl.dlState);;

let GetDLId dl =
	let (_,id,_,_) = GetDLComp dl in
	id;;

let GetDLButtonList dl =
	dl.buttonList;;

let GetDLSurfBG dl =
	dl.surfBG;;

let GetDLNumSelected dl =
	dl.dlNumSelected;;

let GetDLColor dl =
	dl.color;;

let SetDLState dl state =
	dl.dlState <- state;;

let SetDLId dl id =
	dl.dlId <- id;;

let SetDLButtonList dl bList =
	dl.buttonList <- bList;;

let SetDLSurfBG dl surfBG =
	dl.surfBG <- surfBG;;

let SetDLColor dl color =
	dl.color <- color;;

let GetFreeId_DL() =
	GetFreeId (!dlList) GetDLId;;

let GetDL id =
	GetObject (!dlList) GetDLId id;;

let SaveDL dl =
	SaveObject dlList dl;;

(*Item lists (called "IL") *)
(*let MakeIL rect id columns =
	{ilRect=rect; ilId=id; columns=columns; ilState=IL_COLD};;

let SetILState il state =
	il.ilState <- state;;

let SetILId il id =
	il.ilId <- id;;

let GetILComp il =
	(il.ilRect, il.ilId, il.columns, il.ilState);;

let GetILId il =
	let (_,id,_,_) = GetILComp il in
	id;;

let GetFreeId_IL() =
	GetFreeId (!ilList) GetILId;;

let GetIL id =
	GetObject (!ilList) GetILId id;;

let SaveIL il =
	SaveObject ilList il;;*)



(*Text boxes (called "TB") *)

(*None*)


	

(*outdoor procedures*)

(*Buttons*)
let NewButton rect shape =
	let id = GetFreeId_Button() in
	let button = MakeButton rect id shape in
	SaveButton button;
	button;;

let NewSurfButton surf coord =
	let (x,y) = coord and (w,h) = DimSurf surf in
	NewButton (MakeRect x y w h) RECT;;

let DeleteButton button =
	DeleteObject button buttonList GetButtonId SetButtonId;;

let GetButtonState button =
	let (_,_,_,state) = GetButtonComp button in
	state;;

let HoldButton event button =
	let HB_aux() =
		let (t,k,_,_) = GetEventComp event and (x,y) = GetMousePos() in
		let (rect,_,_,s) = GetButtonComp button in
		match t with
		| z when z = SDL_MOUSEMOTION ->
			if ((IsInRect (x,y) rect)) then
			begin
				if (s = BUTTON_PUSHED) then BUTTON_PUSHED else
				begin
					if (IsMouseButtonDown SDL_BUTTON_LEFT) then BUTTON_COLD else BUTTON_HOT
				end
			end
			else BUTTON_COLD
		| z when z = SDL_MOUSEBUTTONDOWN ->
			if (IsInRect (x,y) rect) then
			begin
				if (k = SDL_BUTTON_LEFT) then BUTTON_PUSHED else BUTTON_HOT
			end
			else BUTTON_COLD
		| z when z = SDL_MOUSEBUTTONUP ->
			if (IsInRect (x,y) rect) then
			begin
				if (k = SDL_BUTTON_LEFT && s = BUTTON_PUSHED) then BUTTON_CLICKED else BUTTON_HOT
			end
			else BUTTON_COLD
		| _ -> if (s = BUTTON_CLICKED) then BUTTON_HOT else s;
	in
	let state = HB_aux() in
	SetButtonState button state;
	state;;

let HoldAllButtons event =
	do_list (function b -> let s = HoldButton event b in ()) (!buttonList);;


(*CDs*)
let NewCD rect shape fw =
	let id = GetFreeId_CD() in
	let cd = MakeCD rect id shape fw in
	SaveCD cd;
	cd;;

let NewSurfCD surf coord fw =
	let (x,y) = coord and (w,h) = DimSurf surf in
	NewCD (MakeRect x y w h) RECT fw;;

let GetCDPosition cd =
	let (rect,_,_,_) = GetCDComp cd in
	let (x,y,_,_) = GetRectComp rect in
	(x,y);;
	
let DeleteCD cd =
	DeleteObject cd cdList GetCDId SetCDId;;

let GetCDState cd =
	let (_,_,_,state) = GetCDComp cd in
	state;;

let HoldCD event cd =
	let HCD_aux() =
		let (t,k,_,_) = GetEventComp event and (x,y) = GetMousePos() in
		let (rect,_,_,s) = GetCDComp cd in
		match t with
		| z when z = SDL_MOUSEMOTION ->
			if (s = CD_CAUGHT) then
				let (ax,ay) = GetCDAnchor cd and (fx,fy,fw,fh) = GetRectComp (GetCDFramework cd) in
				let x2 = ref (x-ax) and y2 = ref (y-ay) in
				if ((!x2) < fx) then x2 := fx;
				if ((!x2) > fx+fw) then x2 := fx+fw;
				if ((!y2) < fy) then y2 := fy;
				if ((!y2) > fy+fh) then y2 := fy+fh;
				SetCDPosition ((!x2),(!y2)) cd;
				CD_CAUGHT;
			else
			begin
				if (IsInRect (x,y) rect) then CD_HOT else CD_COLD
			end
		| z when z = SDL_MOUSEBUTTONDOWN ->
			if (IsInRect (x,y) rect) then
			begin
				if (k = SDL_BUTTON_LEFT) then
				begin
					let (cdx,cdy,_,_) = GetRectComp rect in
					SetCDAnchor (x-cdx,y-cdy) cd;
					CD_CAUGHT
				end
				else CD_HOT
			end
			else CD_COLD
		| z when z = SDL_MOUSEBUTTONUP ->
			if (IsInRect (x,y) rect) then
			begin
				if (k = SDL_BUTTON_LEFT && s = CD_CAUGHT) then CD_DROPPED else CD_HOT
			end
			else CD_COLD
		| _ -> if (s = CD_DROPPED) then CD_HOT else s;
	in
	let state = HCD_aux() in
	SetCDState cd state;
	state;;

let HoldAllCDs event =
	do_list (function b -> let s = HoldCD event b in ()) (!cdList);;


(*DLs*)

let NewDL rect tl0 font color colorBG db =
	let id = GetFreeId_DL() in
	let dl = MakeDL rect id db in
	let n = list_length tl0 and (x,y0,w,h0) = GetRectComp rect in
	let surfBG = NewSurf w ((h0+3)*n+1) in
	FillRect surfBG colorBG (MakeRect 0 0 0 0);
			
	let rec NB_rec y tl =
		match tl with
		| [] -> []
		| h::t ->
			let text = NewText h font color in
			let surf = NewSurf (w-4) h0 in
			FillRect surf colorBG (MakeRect 0 0 0 0);
			BlitSurf surf text (MakeRect 0 0 0 0) (MakeRect 0 0 0 0);
			DeleteSurf text;
			
			let button = NewButton (MakeRect x y w (h0+3)) RECT in
			BlitSurf surfBG surf (MakeRect 2 (y-y0+2) 0 0) (MakeRect 0 0 0 0);
			DeleteSurf surf;
			
			button::(NB_rec (y+h0+3) t);
	in
	
	SetDLButtonList dl (NB_rec y0 tl0);
	DrawRect surfBG color (MakeRect 0 0 w ((h0+3)*n+1));
	SetDLSurfBG dl surfBG;
	SetDLColor dl color;
	SaveDL dl;
	dl;;

let NewSurfDL surf coord tl font color colorBG db =
	let (x,y) = coord and (w,h) = DimSurf surf in
	NewDL (MakeRect x y w h) tl font color colorBG db;;

let DeleteDL dl =
	DeleteObject dl dlList GetDLId SetDLId;
	do_list DeleteButton (GetDLButtonList dl);
	DeleteSurf (GetDLSurfBG dl);;

let GetDLState dl =
	let (_,_,_,state) = GetDLComp dl in
	(GetDLNumSelected dl, state);;

let SetDLNumSelected dl num =
	dl.dlNumSelected <- num;;

let HoldDL event dl =
	let HDL_aux() =
		let (t,k,_,_) = GetEventComp event in
		let (rect,_,button,state) = GetDLComp dl and n = list_length (GetDLButtonList dl) in
		let surfBG = GetDLSurfBG dl and num = GetDLNumSelected dl in
		let (_,_,w,h0) = GetRectComp rect in
		let colorBG = GetPixel surfBG 1 1 and color = GetDLColor dl in

		DrawRect surfBG colorBG (MakeRect 0 (num*(h0+3)) w (h0+4));
		DrawRect surfBG color (MakeRect 0 0 w ((h0+3)*n+1));

		let bState = HoldButton event button in
	
		if (state = DL_EXTENDED) then
			let rec HB_rec l i =
				match l with
				| [] -> (-1)
				| h::t ->
					let s = HoldButton event h in
					if (s <> BUTTON_COLD) then
					begin
						DrawRect surfBG (MakeColor 200 0 0) (MakeRect 0 (i*(h0+3)) w (h0+4));
						if (s = BUTTON_CLICKED) then SetDLState dl DL_SELECTED;
						i;
					end
					else HB_rec t (i+1);
			in

			let i = HB_rec (GetDLButtonList dl) 0 in

			if ((bState = BUTTON_CLICKED) || (i < 0 && t = SDL_MOUSEBUTTONUP && k = SDL_BUTTON_LEFT)) then
				SetDLState dl DL_COLD;
			i;
		else
		begin
			if (bState = BUTTON_CLICKED) then SetDLState dl DL_EXTENDED else SetDLState dl DL_COLD;
			-1;
		end;
	in
	let num = HDL_aux() in
	SetDLNumSelected dl num;
	GetDLState dl;;

let HoldAllDLs event =
	do_list (function b -> let s = HoldDL event b in ()) (!dlList);;

let DisplayDL screen dl =
	let (rect,_,_,state) = GetDLComp dl and surfBG = GetDLSurfBG dl in

	if (state = DL_EXTENDED) then BlitSurf screen surfBG rect (MakeRect 0 0 0 0) else ();;
	
let DisplayAllDLs screen =
	do_list (DisplayDL screen) (!dlList);;


(*ILs*)
(*let NewIL rect columns iList font headFont color colorBG headColor headColorBG =
	let id = GetFreeId_IL() in
	let il = MakeIL rect id columns in
	let n = list_length iList and (x0,y0,w0,h0) = GetRectComp rect in
	let surfBG = NewSurf w0 h0 in
	FillRect surfBG colorBG (MakeRect 0 0 0 0);
			
	let rec DrawHead x l =
		match l with
		| [] -> 0
		| (name,size)::tail ->
			let text = NewText name headFont headColor in
			BlitSurf surfBG text (MakeRect x 1 0 0) (MakeRect 0 0 0 0);
			
			let (_,h) = DimSurf text and h2 = DrawHead (x+size+3) tail in
			DeleteSurf text;
			if (h>h2) then h else h2;
	in

	let rec DrawLine x y col l =
		match (l,col) with
		| (str::tail,(_,size)::cTail) ->
			let text = NewText str font color in
			BlitSurf surfBG text (MakeRect x y 0 0) (MakeRect 0 0 0 0);
			
			let (_,h) = DimSurf text and h2 = DrawLine (x+size+3) y cTail tail in
			DeleteSurf text;
			if (h>h2) then h else h2
		| _ -> 0
	in

	let rec DrawBody x y l =
		match l with
		| [] -> ()
		| l1::tail ->
			let h = DrawLine x y columns l1 in
			DrawBody tail (y+3);
	in

	let h = DrawHead 2 columns in
	DrawBody 2 (h+3) iList;
	
	SetDLButtonList dl (NB_rec y0 tl0);
	DrawRect surfBG color (MakeRect 0 0 w ((h0+3)*n+1));
	SetDLSurfBG dl surfBG;
	SetDLColor dl color;
	SaveDL dl;
	dl;;*)


(*TBs*)

let TB_STYLE_NORMAL = GetTBStyleNormalId_c();;
let TB_STYLE_MULTILINE = GetTBStyleMultilineId_c();;
let TB_STYLE_VSCROLL = GetTBStyleVScrollId_c();;
let TB_STYLE_HSCROLL = GetTBStyleHScrollId_c();;
let TB_STYLE_AUTOJUMP = GetTBStyleAutojumpId_c();;
let TB_STYLE_READONLY = GetTBStyleReadOnlyId_c();;
let TB_STYLE_JUSTDISPLAY = GetTBStyleJustDisplayId_c();;

let NewTB surf length pos font color style =
	let rec NTB_aux l =
		match l with
		| [] -> TB_STYLE_NORMAL
		| h::t -> h lor (NTB_aux t);
	in
	
	let (r,g,b) = GetColorComp color and (x,y,w,h) = GetRectComp pos in
	let tb = NewTB_c (GetSurfId surf) length x y w h (GetFontId font) r g b (NTB_aux style) in

	if (tb = 0) then failwith "Unable te create new Text Box" else tb;;

let DeleteTB tb =
	let x = DeleteTB_c tb in ();;

let UpdateTB tb =
	if ((UpdateTB_c tb) = 0) then failwith "Unable to update Text Box" else ();;

let DisplayTB tb =
	if ((DisplayTB_c tb) = 0) then failwith "Unable to display Text Box" else ();;

let HoldTB tb e =
	if ((HoldTB_c tb (GetIdEvent e)) = 0) then failwith "Unable to manage Text Box" else ();;

let SetTBText tb text =
	let n = SetTBText_c tb text in
	if (n<0) then failwith "Unable to set Text Box text" else n;;

let GetTBText tb =
	GetTBText_c tb;;

let SetTBFocus tb focus =
	if (SetTBFocus_c tb (if (focus) then 1 else 0) = 0) then failwith "Unable to set TextBox focus" else ();;

let GetTBFocus tb =
	let focus = GetTBFocus_c tb in
	if (focus = -1) then failwith "Unable to get Text Box focus" else focus=1;;
	



