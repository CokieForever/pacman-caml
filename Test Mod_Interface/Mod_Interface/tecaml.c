#include "textedition.c"

#include <alloc.h>
#include <memory.h>
#include <mlvalues.h>

#define FontOfVal(s)  		((TTF_Font*)(s))
#define TEOfVal(s)    		((TextEdition*)(s))
#define EventOfVal(s) 		((SDL_Event*)(s))
#define SurfOfVal(s)		((SDL_Surface*)(s))
#define ValOfPointer(s) 	( s==0 ? (Val_int(0)) : ((value)(s)) )


value NewTB_Env (value *argv, value argn)
{
	value surf = argv[0],
	      length = argv[1],
	      x = argv[2],
	      y = argv[3],
	      w = argv[4],
	      h = argv[5],
	      font = argv[6],
	      r = argv[7],
	      g = argv[8],
	      b = argv[9],
	      style = argv[10];

	TextEdition *te = malloc(sizeof(TextEdition));
	SDL_Rect pos;
	SDL_Color color;

	if (!te)
	{
		printf("Error while creating Text Edition : Unable to allocate memory space.\n");
		return Val_int(0);
	}
	
	memset(te, 0, sizeof(TextEdition));

	pos.x = Int_val(x);
	pos.y = Int_val(y);
	pos.w = Int_val(w);
	pos.h = Int_val(h);

	color.r = Int_val(r);
	color.g = Int_val(g);
	color.b = Int_val(b);

	te->colorBG = color;
	te->blitSurf = SurfOfVal(surf);
	if (TE_NewTextEdition(te, Int_val(length), pos, FontOfVal(font), color, Int_val(style)))
		return ValOfPointer(te);
	printf("Error while creating Text Edition...\n");
	return Val_int(0);
}

value DeleteTB_Env (value te)
{
	if (TEOfVal(te))
	{
		TE_DeleteTextEdition(TEOfVal(te));
		free(TEOfVal(te));
	}
	return Val_int(1);
}

value UpdateTB_Env (value te)
{
	if (TEOfVal(te) && TE_UpdateTextEdition(TEOfVal(te), 0))
		return Val_int(1);
	printf("Error while updating Text Edition %x\n", (int)TEOfVal(te));
	return Val_int(0);
}

value DisplayTB_Env (value te)
{
	if (TEOfVal(te) && TE_DisplayTextEdition(TEOfVal(te)))
		return Val_int(1);
	printf("Error while displaying Text Edition %x\n", (int)TEOfVal(te));
	return Val_int(0);
}

value HoldTB_Env (value te, value event)
{
	if (TEOfVal(te) && EventOfVal(event) && TE_HoldTextEdition(TEOfVal(te), *EventOfVal(event)))
		return Val_int(1);
	printf("Error while holding Text Edition %x\n", (int)TEOfVal(te));
	return Val_int(0);
}

value SetTBText_Env (value te, value text)
{
	char *t = String_val(text), buf[20] = "";
	int n = TEOfVal(te) ? TE_SetEditionText(TEOfVal(te), t) : -1;

	if (n<0)
	{
		strncpy(buf, t, 15);
		if (strlen(buf) > 14)
			strcpy(buf+12, "...");
		printf("Error while setting text \"%s\" of Text Edition %x\n", buf, (int)TEOfVal(te));
	}

	return Val_int(n);
}

value GetTBText_Env (value te)
{
	if (!TEOfVal(te))
	{
		printf("Warning : tried to get text of void Text Edition\n");
		return copy_string("");
	}
	return copy_string(TEOfVal(te)->text);
}

value SetTBFocus_Env (value te, value f)
{
	if (!TEOfVal(te))
	{
		printf("Warning : tried to set focus of void Text Edition\n");
		return Val_int(0);
	}
	
	if (Int_val(f))
		TEOfVal(te)->focus = 1;
	else TEOfVal(te)->focus = 0;

	return Val_int(1);
}

value GetTBFocus_Env (value te)
{
	if (!TEOfVal(te))
	{
		printf("Warning : tried to get focus of void Text Edition\n");
		return Val_int(-1);
	}

	return TEOfVal(te)->focus;
}




value GetTBStyleNormalId_Env(unit)
{
	return Val_int(TE_STYLE_NORMAL);
}

value GetTBStyleMultilineId_Env(unit)
{
	return Val_int(TE_STYLE_MULTILINE);
}

value GetTBStyleVScrollId_Env(unit)
{
	return Val_int(TE_STYLE_VSCROLL);
}

value GetTBStyleHScrollId_Env(unit)
{
	return Val_int(TE_STYLE_HSCROLL);
}

value GetTBStyleAutojumpId_Env(unit)
{
	return Val_int(TE_STYLE_AUTOJUMP);
}

value GetTBStyleReadOnlyId_Env(unit)
{
	return Val_int(TE_STYLE_READONLY);
}

value GetTBStyleJustDisplayId_Env(unit)
{
	return Val_int(TE_STYLE_JUSTDISPLAY);
}
