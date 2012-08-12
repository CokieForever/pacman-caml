#include <stdio.h>
#include <math.h>
#include "SDL.h"
#include "SDL_image.h"
/*#include "SDL_mixer.h"*/
#include "SDL_ttf.h"

#include <alloc.h>
#include <memory.h>
#include <mlvalues.h>

typedef struct
{
	SDL_Surface *surf;
	int allocated;
} Surface;

#define WSurf(s) 		(s->w)
#define HSurf(s) 		(s->h)
#define ValOfPointer(s) 	( s==0 ? (Val_int(0)) : ((value)(s)) )
#define SurfOfVal(s) 		((SDL_Surface*)(s))
#define FontOfVal(s) 		((TTF_Font*)(s))
#define EventOfVal(s) 		((SDL_Event*)(s))

#define PI 3.1415926535897932384626433832795028841971693993751058209

#if SDL_BYTEORDER == SDL_BIG_ENDIAN

#define RED_MASK   0xFF000000
#define GREEN_MASK 0x00FF0000
#define BLUE_MASK  0x0000FF00
#define ALPHA_MASK 0x000000FF

#else

#define RED_MASK   0x000000FF
#define GREEN_MASK 0x0000FF00
#define BLUE_MASK  0x00FF0000
#define ALPHA_MASK 0xFF000000

#endif


SDL_Surface* RotateSurface(SDL_Surface *surface, double angle);
SDL_Surface* RotateSurface_Fast(SDL_Surface *surface, int rotation);
Uint32 GetPixel(SDL_Surface *surface,int x,int y);
int SetPixel(SDL_Surface *surface, Uint32 pixel, int x, int y);
double Arg(double re, double im);
int CalculateRotCoord(double *x, double *y, double angle);
int CalculateNewDim(int *pw, int *ph, double tcos, double tsin);


Uint32 GetPixel(SDL_Surface *surface, int x, int y)
{
	int bpp = surface->format->BytesPerPixel;
	/* Here p is the address to the pixel we want to retrieve */
	Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;
	
	if (!surface || x<0 || x>=surface->w || y<0 || y>=surface->h)
		return 0;

	switch(bpp) 
	{
		case 1:
			return *p;

		case 2:
			return *(Uint16 *)p;

		case 3:
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
				return p[0] << 16 | p[1] << 8 | p[2];
			else return p[0] | p[1] << 8 | p[2] << 16;

		case 4:
			return *(Uint32 *)p;

		default:
			return SDL_MapRGBA(surface->format, 0,0,0,SDL_ALPHA_TRANSPARENT);       /* shouldn't happen, but avoids warnings */
	}
}

int SetPixel(SDL_Surface *surface, Uint32 pixel, int x, int y)
{
	int bpp = surface->format->BytesPerPixel;
	/* Here p is the address to the pixel we want to set */
	Uint8 *p = (Uint8 *)surface->pixels + y * surface->pitch + x * bpp;

	if (!surface || x<0 || x>=surface->w || y<0 || y>=surface->h)
		return 0;

	switch(bpp)
	{
		case 1:
			*p = pixel;
			break;

		case 2:
			*(Uint16 *)p = pixel;
			break;

		case 3:
			if(SDL_BYTEORDER == SDL_BIG_ENDIAN)
			{
				p[0] = (pixel >> 16) & 0xff;
				p[1] = (pixel >> 8) & 0xff;
				p[2] = pixel & 0xff;
			}
			else
			{
				p[0] = pixel & 0xff;
				p[1] = (pixel >> 8) & 0xff;
				p[2] = (pixel >> 16) & 0xff;
			}
			break;

		case 4:
			*(Uint32 *)p = pixel;
			break;
		default:
			return 0;
	}

	return 1;
}

SDL_Surface* RotateSurface_Fast(SDL_Surface *surface, int rotation)
{
	int i,j,w,h;
	SDL_Surface *copySurf, *tmpSurf;
	Uint32 pixel;
	if (!surface)
		return NULL;
	
	w = surface->w;
	h = surface->h;

	if (SDL_LockSurface(surface) < 0)
		return NULL;

	if (rotation != 1 && rotation != 3)
		copySurf = SDL_CreateRGBSurface(SDL_HWSURFACE, w, h, 32,0,0,0,0);
	else copySurf = SDL_CreateRGBSurface(SDL_HWSURFACE, h, w, 32,0,0,0,0);

	if (!copySurf)
	{
		SDL_UnlockSurface(surface);
		return NULL;
	}

	tmpSurf = SDL_ConvertSurface(copySurf,surface->format,SDL_HWSURFACE);
	SDL_FreeSurface(copySurf);
	copySurf = tmpSurf;

	if (!copySurf)
	{
		SDL_UnlockSurface(surface);
		return NULL;
	}

	if (SDL_LockSurface(copySurf) < 0)
	{
		SDL_FreeSurface(copySurf);
		SDL_UnlockSurface(surface);
		return NULL;
	}

	for (i=0 ; i<h ; i++)
	{
		for (j=0 ; j<w ; j++)
		{
			pixel = GetPixel(surface,i,j);
			switch (rotation)
			{
				case 3:
					SetPixel(copySurf, pixel, j, w-i-1);
					break;
				case 2:
					SetPixel(copySurf, pixel, w-i-1, w-j-1);
					break;
				case 1:
					SetPixel(copySurf, pixel, w-j-1, i);
					break;
				default:
					SetPixel(copySurf, pixel, i, j);
					break;
			}
		}
	}

	SDL_UnlockSurface(surface);
	SDL_UnlockSurface(copySurf);
	return copySurf;
}

/*double Arg(double re, double im)
{
	double arg;
	    
	if (!re)
	{
		if (im>0)
			return PI/2;
		else return -PI/2;
	}

	arg = atan(im/re);

	if (re<0 && im>=0)
		return arg+PI;
	else if (re<0 && im<=0)
		return arg-PI;
	else return arg;
}

int CalculateRotCoord(double *x, double *y, double angle)
{
	double r,t;

	if (!x || !y)
		return 0;

	if (*x == 0 && *y == 0)
		return 1;

	r = sqrt( ((*x) * (*x)) + ((*y) * (*y)) ),
	t = Arg(*x,*y);

	
	*x = r*cos(t+angle);
	*y = r*sin(t+angle);

	return 1;
}*/

int CalculateNewDim(int *pw, int *ph, double tcos, double tsin)
{
	double x,y,x2,y2,
	       tx,ty,tx2,ty2;
	int w,h,w2,h2;

	if (!pw || !ph)
		return 0;

	w = *pw;
	h = *ph;
	tsin = -tsin;
	
	x = -w/2.0; x2 = w/2.0;
	y = -h/2.0; y2 = h/2.0;

	tx = tcos * x + tsin * y;
   	ty = -tsin * x + tcos * y;
	tx2 = tcos * x2 + tsin * y2;
   	ty2 = -tsin * x2 + tcos * y2;

	w2 = fabs(tx-tx2);
	h2 = fabs(ty-ty2);


	x = w/2.0; x2 = -w/2.0;
	y = -h/2.0; y2 = h/2.0;

	tx = tcos * x + tsin * y;
   	ty = -tsin * x + tcos * y;
	tx2 = tcos * x2 + tsin * y2;
   	ty2 = -tsin * x2 + tcos * y2;

	if (fabs(tx-tx2) > w2)
		w2 = fabs(tx-tx2);
	if (fabs(ty-ty2) > h2)
		h2 = fabs(ty-ty2);

	*pw = w2;
	*ph = h2;

	return 1;
}	
	

SDL_Surface* RotateSurface(SDL_Surface *surface, double angle)
{
	int i,j,w,h,w2,h2;
	double x,y,x2,y2,
	       tcos,tsin;
	SDL_Surface *copySurf;
	Uint32 pixel;
	if (!surface)
		return NULL;
	
	w = surface->w;
	h = surface->h;
	
	if (SDL_LockSurface(surface) < 0)
		return NULL;

	w2 = w; h2 = h;
	tcos = cos(angle);
	tsin = -sin(angle);

	CalculateNewDim(&w2,&h2,tcos,tsin);
	
	if (!(copySurf = SDL_CreateRGBSurface(SDL_HWSURFACE, w2, h2, 32, RED_MASK, GREEN_MASK, BLUE_MASK, ALPHA_MASK)))
	{
		SDL_UnlockSurface(surface);
		return NULL;
	}
	SDL_FillRect(copySurf, NULL, SDL_MapRGBA(copySurf->format, 0, 0, 0, SDL_ALPHA_TRANSPARENT));

	if (SDL_LockSurface(copySurf) < 0)
	{
		SDL_FreeSurface(copySurf);
		SDL_UnlockSurface(surface);
		return NULL;
	}

	for (i=0 ; i<h2 ; i++)
	{
		for (j=0 ; j<w2 ; j++)
		{
			x = j-w2/2.0;
			y = i-h2/2.0;
			
			x2 = tcos * x + tsin * y;
   			y2 = -tsin * x + tcos * y;

			pixel = GetPixel(surface,y2+h/2.0,x2+w/2.0);
			SetPixel(copySurf, pixel, i, j);
		}
	}

	SDL_UnlockSurface(surface);
	SDL_UnlockSurface(copySurf);
	return copySurf;
}




/* envelopes for CamL functions */

value InitSDL_Env(unit)
{
	int flags;
	if (SDL_Init(SDL_INIT_VIDEO))
	{
		printf("Error while initializing SDL\n\t%s\n", SDL_GetError());
		return Val_int(0);
	}

	if (TTF_Init())
	{
		printf("Error while initializing SDL_ttf\n\t%s\n", TTF_GetError());
		return Val_int(0);
	}

	flags = IMG_INIT_JPG | IMG_INIT_PNG;
	if ((flags&IMG_Init(flags)) != flags)
	{
		printf("Error while initializing SDL_image\n\t%s\n", IMG_GetError());
		return Val_int(0);
	}

	return Val_int(1);
}

/*value InitSDLMixer_Env(unit)
{
	int flags;
	if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_AUDIO))
	{
		printf("Error while initializing SDL\n\t%s\n", SDL_GetError());
		return Val_int(0);
	}

	if (TTF_Init())
	{
		printf("Error while initializing SDL_ttf\n\t%s\n", TTF_GetError());
		return Val_int(0);
	}

	flags = IMG_INIT_JPG | IMG_INIT_PNG;
	if ((flags&IMG_Init(flags)) != flags)
	{
		printf("Error while initializing SDL_image\n\t%s\n", IMG_GetError());
		return Val_int(0);
	}

	flags = MIX_INIT_FLAC | MIX_INIT_MOD | MIX_INIT_MP3 | MIX_INIT_OGG;
	if ((flags&Mix_Init(flags)) != flags)
	{
		printf("Error while initializing SDL_mixer\n\t%s\n", Mix_GetError());
		return Val_int(0);
	}

	if (Mix_OpenAudio(44100, AUDIO_S16SYS, 2, 4096))
	{
		printf("Error while opening audio module\n\t%s\n", Mix_GetError());
		return Val_int(0);
	}

	return Val_int(1);
}*/


value QuitSDL_Env(unit)
{
	TTF_Quit();
	IMG_Quit();
	SDL_Quit();
	return Val_int(1);
}

/*value QuitSDLMixer_Env(unit)
{
	Mix_CloseAudio();
	Mix_Quit();
	TTF_Quit();
	IMG_Quit();
	SDL_Quit();
	return Val_int(1);
}*/

value OpenGraph_Env(value w, value h)
{
	SDL_Surface *surf = SDL_SetVideoMode(Int_val(w), Int_val(h), 32, SDL_HWSURFACE | SDL_DOUBLEBUF);
	if (!surf)
		printf("Error while initializing video mode (%d x %d)\n\t%s\n", Int_val(w), Int_val(h), SDL_GetError());
	return ValOfPointer(surf);
}

value GetScreen_Env(unit)
{
	return ValOfPointer(SDL_GetVideoSurface());
}

value WaitEvent_Env(value poll)
{
	SDL_Event *event = malloc(sizeof(SDL_Event));
	value tab = alloc_tuple(5);
	int i;

	for (i=0 ; i<5 ; i++)
		Field(tab, i) = Val_int(0);
	
	if (!event)
	{
		printf("Error while waiting event : Unable to allocate memory space.\n");
		return tab;
	}
	
	Field(tab, 4) = ValOfPointer(event);
	if ( (Int_val(poll) && SDL_PollEvent(event)) || (!Int_val(poll) && SDL_WaitEvent(event)) )
	{
		Field(tab, 0) = Val_int(event->type);

		if (event->type == SDL_KEYDOWN || event->type == SDL_KEYUP)
			Field(tab, 1) = Val_int(event->key.keysym.sym);
		
		if (event->type == SDL_MOUSEMOTION || event->type == SDL_MOUSEBUTTONDOWN || event->type == SDL_MOUSEBUTTONUP)
		{
			Field(tab, 1) = Val_int(event->button.button);
			Field(tab, 2) = Val_int(event->button.x);
			Field(tab, 3) = Val_int(event->button.y);
		}
	}

	return tab;
}

value DeleteEvent_Env(value event)
{
	if (EventOfVal(event))
		free(EventOfVal(event));
	else printf("Warning : tried to free void event\n");
	return Val_int(1);
}

value SDLSleep_Env(value ms)
{
	if (Int_val(ms)>0)
		SDL_Delay(Int_val(ms));
	return Val_int(1);
}

value SDLTime_Env(unit)
{
	return Val_int(SDL_GetTicks());
}

value SetCaption_Env(value title)
{
	SDL_WM_SetCaption(String_val(title), NULL);
	return Val_int(1);
}

value NewSurf_Env(value w, value h)
{
	SDL_Surface *surf = SDL_CreateRGBSurface(SDL_HWSURFACE, Int_val(w), Int_val(h), 32, 0, 0, 0, 0);
	if (!surf)
		printf("Error while creating new surface (%d x %d)\n\t%s\n", Int_val(w), Int_val(h), SDL_GetError());
	return ValOfPointer(surf);
}

value DeleteSurf_Env(value s)
{
	SDL_Surface *surf = SurfOfVal(s);
	if (!surf)
		printf("Warning : tried to free void surface\n");
	else SDL_FreeSurface(surf);
	return Val_int(1);
}

value SetSurfAlphaLevel_Env(value s, value a)
{
	SDL_Surface *surf = SurfOfVal(s);
	if (!surf)
		printf("Warning : tried to set alpha level of void surface\n");
	else
	{
		if (!SDL_SetAlpha(surf, SDL_SRCALPHA, Int_val(a)))
			return Val_int(1);
		else printf("Error while setting alpha level of surface %x at level %d\n\t%s\n", (int)surf, Int_val(a), SDL_GetError());
	}
	return Val_int(0);
}

value GetTransparencyLevel_Env(unit)
{
	return Val_int(SDL_ALPHA_TRANSPARENT);
}

value GetOpacityLevel_Env(unit)
{
	return Val_int(SDL_ALPHA_OPAQUE);
}

value FillRect_Env(value *argv, value argn)
{
	value s = argv[0],
	      r = argv[1],
	      g = argv[2],
	      b = argv[3],
	      x = argv[4],
	      y = argv[5],
	      w = argv[6],
	      h = argv[7];	

	SDL_Rect rect;
	SDL_Surface *surf = SurfOfVal(s);
	
	if (!surf)
	{
		printf("Warning : tried to fill void surface\n");
		return Val_int(-1);
	}

	rect.x = Int_val(x);
	rect.y = Int_val(y);
	rect.w = Int_val(w);
	rect.h = Int_val(h);

	if (rect.w <= 0)
		rect.w = WSurf(surf);
	if (rect.h <= 0)
		rect.h = HSurf(surf);

	if (SDL_FillRect(surf, &rect, SDL_MapRGB(surf->format, Int_val(r), Int_val(g), Int_val(b))) )
	{
		printf("Error while filling rect (%d x %d) (%d,%d,%d) on surface %x\n\t%s\n", Int_val(w), Int_val(h), Int_val(r), Int_val(g), Int_val(b), (int)(surf), SDL_GetError());
		return Val_int(0);
	}
	else return Val_int(1);
}

value DrawRect_Env(value *argv, value argn)
{
	value s = argv[0],
	      r = argv[1],
	      g = argv[2],
	      b = argv[3],
	      x = argv[4],
	      y = argv[5],
	      w = argv[6],
	      h = argv[7];	

	SDL_Rect rect;
	SDL_Surface *surf = SurfOfVal(s),
		    *surfRect1, *surfRect2;
	int r2 = 0, g2 = 0, b2 = 0;

	if (!surf || Int_val(h)<=0 || Int_val(w)<=0)
	{
		printf("Warning : tried to draw rect with invalid surface or parameter(s)\n");
		return Val_int(0);
	}

	if (!Int_val(r) && !Int_val(g) && !Int_val(b))
		r2 = 255;

	surfRect1 = SDL_CreateRGBSurface(SDL_HWSURFACE, Int_val(w), Int_val(h), 32, 0, 0, 0, 0);
	if (!surfRect1)
	{
		printf("Error while creating new surface (%d x %d)\n\t%s\n", Int_val(w), Int_val(h), SDL_GetError());
		return Val_int(0);
	}
	SDL_FillRect(surfRect1, NULL, SDL_MapRGB(surfRect1->format, Int_val(r), Int_val(g), Int_val(b)));
	SDL_SetColorKey(surfRect1, SDL_SRCCOLORKEY, SDL_MapRGB(surfRect1->format, r2, g2, b2));

	if (Int_val(w)>2 && Int_val(h)>2)
	{
		surfRect2 = SDL_CreateRGBSurface(SDL_HWSURFACE, Int_val(w)-2, Int_val(h)-2, 32, 0, 0, 0, 0);
		SDL_FillRect(surfRect2, NULL, SDL_MapRGB(surfRect1->format, r2, g2, b2));
		if (!surfRect2)
		{
			SDL_FreeSurface(surfRect1);
			printf("Error while creating new surface (%d x %d)\n\t%s\n", Int_val(w)-2, Int_val(h)-2, SDL_GetError());
			return Val_int(0);
		}

		rect.x = 1;
		rect.y = 1;
		SDL_BlitSurface(surfRect2, NULL, surfRect1, &rect);
		SDL_FreeSurface(surfRect2);
	}
	
	rect.x = Int_val(x);
	rect.y = Int_val(y);
	
	SDL_BlitSurface(surfRect1, NULL, surf, &rect);
	SDL_FreeSurface(surfRect1);
	return Val_int(1);
}

value BlitSurface_Env(value *argv, value argn)
{
	value tgt = argv[0],
	      src = argv[1],
	      xt = argv[2],
	      yt = argv[3],
	      xs = argv[4],
	      ys = argv[5],
	      ws = argv[6],
	      hs = argv[7];

	SDL_Rect tRect, sRect;
	SDL_Surface *tSurf = SurfOfVal(tgt),
		    *sSurf = SurfOfVal(src);

	if (!tSurf || !sSurf)
	{
		printf("Warning : tried to blit with vois surface(s)\n");
		return Val_int(0);
	}

	tRect.x = Int_val(xt);
	tRect.y = Int_val(yt);
	sRect.x = Int_val(xs);
	sRect.y = Int_val(ys);
	sRect.w = Int_val(ws);
	sRect.h = Int_val(hs);

	if (sRect.x < 0)
		sRect.x = 0;
	if (sRect.y < 0)
		sRect.y = 0;
	
	if (sRect.w <= 0)
		sRect.w = WSurf(sSurf);
	if (sRect.h <= 0)
		sRect.h = HSurf(sSurf);
	
	if (SDL_BlitSurface(sSurf, &sRect, tSurf, &tRect) )
	{
		printf("Error while blitting surface %x (%d,%d,%d,%d) on surface %x (%d,%d)\n\t%s\n", (int)(sSurf), sRect.x, sRect.y, sRect.w, sRect.h, (int)tSurf, tRect.x, tRect.y, SDL_GetError());
		return Val_int(0);
	}
	else return Val_int(1);
}

value Flip_Env(value s)
{
	if (!SurfOfVal(s))
	{
		printf("Warning : tried to flip void surface\n");
		return Val_int(0);
	}
	SDL_Flip(SurfOfVal(s));
	return Val_int(1);
}

value WSurf_Env(value s)
{
	if (!SurfOfVal(s))
	{
		printf("Warning : tried to get width of void surface\n");
		return Val_int(-1);
	}
	else return Val_int(WSurf(SurfOfVal(s)));
}

value HSurf_Env(value s)
{
	if (!SurfOfVal(s))
	{
		printf("Warning : tried to get height of void surface\n");
		return Val_int(-1);
	}
	else return Val_int(HSurf(SurfOfVal(s)));
}
		
value LoadImage_Env(value nom)
{
	SDL_Surface *img = IMG_Load(String_val(nom));
	if (!img)
		printf("Error while loading image : \"%s\"\n\t%s\n", String_val(nom), IMG_GetError());
	return ValOfPointer(img);
}

value RotateSurf_Env(value surf, value angle)
{
	SDL_Surface *surf2;

	if (!SurfOfVal(surf))
	{
		printf("Warning : tried to rotate void surface\n");
		return Val_int(0);
	}

	if (!(surf2 = RotateSurface(SurfOfVal(surf), Int_val(angle)*PI/180.0)))
	{
		printf("Error while rotating surface %x of angle %d\n",(int) SurfOfVal(surf), Int_val(angle));
		return Val_int(0);
	}
	return ValOfPointer(surf2);
}

value RotateSurfFast_Env(value surf, value rotation)
{
	SDL_Surface *surf2;

	if (!SurfOfVal(surf))
	{
		printf("Warning : tried to fast-rotate void surface\n");
		return Val_int(0);
	}

	if (!(surf2 = RotateSurface_Fast(SurfOfVal(surf), Int_val(rotation))))
	{
		printf("Error while fast-rotating surface %x of angle %d\n", (int)SurfOfVal(surf), Int_val(rotation)*90);
		return Val_int(0);
	}
	return ValOfPointer(surf2);
}

value GetPixel_Env(value surf, value x, value y)
{
	Uint32 px;
	int r,g,b,a,i;
	value tab = alloc_tuple(4);

	for (i=0 ; i<4 ; i++)
		Field(tab, i) = Val_int(0);
	
	if (!SurfOfVal(surf))
	{
		printf("Warning : tried to get pixel of void surface\n");
		return Val_int(0);
	}

	SDL_LockSurface(SurfOfVal(surf));
	px = GetPixel(SurfOfVal(surf), Int_val(x), Int_val(y));

	SDL_GetRGBA(px,SurfOfVal(surf)->format,(Uint8*)&r,(Uint8*)&g,(Uint8*)&b,(Uint8*)&a);
	Field(tab, 0) = Val_int(r);
	Field(tab, 1) = Val_int(g);
	Field(tab, 2) = Val_int(b);
	Field(tab, 3) = Val_int(a);

	SDL_UnlockSurface(SurfOfVal(surf));

	return tab;
}

value SetPixel_Env(value *argv, value argn)
{
	value surf = argv[0],
	      r = argv[1],
	      g = argv[2],
	      b = argv[3],
	      x = argv[4],
	      y = argv[5];

	if (!SurfOfVal(surf))
	{
		printf("Warning : tried to set pixel of void surface\n");
		return Val_int(0);
	}

	SDL_LockSurface(SurfOfVal(surf));
	if (!SetPixel(SurfOfVal(surf), SDL_MapRGB(SurfOfVal(surf)->format, r, g, b),Int_val(x),Int_val(y)))
	{
		printf("Error while setting pixel (%d,%d) of the surface %x at the color (%d,%d,%d)\n", Int_val(x), Int_val(y), (int)SurfOfVal(surf), Int_val(r), Int_val(g), Int_val(b));
		SDL_UnlockSurface(SurfOfVal(surf));
		return Val_int(0);
	}
	SDL_UnlockSurface(SurfOfVal(surf));
	
	return Val_int(1);
}


value GetKeyDownId_Env(unit)
{
	return Val_int(SDL_KEYDOWN);
}

value GetKeyUpId_Env(unit)
{
	return Val_int(SDL_KEYUP);
}

value GetMouseMotionId_Env(unit)
{
	return Val_int(SDL_MOUSEMOTION);
}

value GetMouseButtonDownId_Env(unit)
{
	return Val_int(SDL_MOUSEBUTTONDOWN);
}

value GetMouseButtonUpId_Env(unit)
{
	return Val_int(SDL_MOUSEBUTTONUP);
}

value GetButtonLeftId_Env(unit)
{
	return Val_int(SDL_BUTTON_LEFT);
}

value GetButtonRightId_Env(unit)
{
	return Val_int(SDL_BUTTON_RIGHT);
}

value GetExitId_Env(unit)
{
	return Val_int(SDL_QUIT);
}

value GetActiveEventId_Env(unit)
{
	return Val_int(SDL_ACTIVEEVENT);
}

value GetEscapeId_Env(unit)
{
	return Val_int(SDLK_ESCAPE);
}

value GetUpId_Env(unit)
{
	return Val_int(SDLK_UP);
}

value GetDownId_Env(unit)
{
	return Val_int(SDLK_DOWN);
}

value GetRightId_Env(unit)
{
	return Val_int(SDLK_RIGHT);
}

value GetLeftId_Env(unit)
{
	return Val_int(SDLK_LEFT);
}

value GetReturnId_Env(unit)
{
	return Val_int(SDLK_RETURN);
}

value GetEnterId_Env(unit)
{
	return Val_int(SDLK_KP_ENTER);
}

value GetSpaceId_Env(unit)
{
	return Val_int(SDLK_SPACE);
}

value IsMouseButtonDown_Env(value button)
{
	if (SDL_GetMouseState(NULL, NULL) & SDL_BUTTON((Int_val(button))))
		return Val_int(1);
	else return Val_int(0);
}

value IsKeyDown_Env(value key)
{
	int num;
	Uint8 *tab = SDL_GetKeyState(&num);

	if (Int_val(key) >= num)
		return Val_int(-1);
	else if (tab[Int_val(key)])
		return Val_int(1);
	else return Val_int(0);
}

value GetMousePos_Env(unit)
{
	int x, y;
	SDL_GetMouseState(&x, &y);
	
	value tab = alloc_tuple(2);
		
	Field(tab, 0) = Val_int(x);
	Field(tab, 1) = Val_int(y);

	return tab;
}




/*value LoadMusic_Env(value name)
{
	Mix_Music *music = Mix_LoadMUS(String_val(name));
	if (!music)
		printf("Error while loading music : \"%s\"\n\t%s\n", String_val(name), Mix_GetError());
	return Val_int((int)music);
}

value FreeMusic_Env(value music)
{
	if (!Int_val(music))
		printf("Warning : tried to free void music\n");
	else Mix_FreeMusic( (Mix_Music*)(Int_val(music)) );
	return Val_int(1);
}

value LoadSound_Env(value name)
{
	Mix_Chunk *sound = Mix_LoadWAV(String_val(name));
	if (!sound)
		printf("Error while loading sound : \"%s\"\n\t%s\n", String_val(name), Mix_GetError());
	return Val_int((int)sound);
}

value FreeSound_Env(value sound)
{
	if (!Int_val(sound))
		printf("Warning : tried to free void music\n");
	else Mix_FreeChunk( (Mix_Chunk*)(Int_val(sound)) );
	return Val_int(1);
}

value PlayMusic_Env(value music, value loops)
{
	if (Mix_PlayMusic((Mix_Music*)(Int_val(music)), Int_val(loops)) )
	{
		printf("Error while playing music %x\n\t%s\n", Int_val(music), SDL_GetError());
		return Val_int(0);
	}
	else return Val_int(1);
}

value PlaySound_Env(value sound, value loops)
{
	if (Mix_PlayChannel(-1, (Mix_Chunk*)(Int_val(sound)), Int_val(loops)) )
	{
		printf("Error while playing sound %x\n\t%s\n", Int_val(sound), SDL_GetError());
		return Val_int(0);
	}
	else return Val_int(1);
}

value PauseMusic_Env(unit)
{
	Mix_PauseMusic();
	return Val_int(1);
}

value ResumeMusic_Env(unit)
{
	Mix_ResumeMusic();
	return Val_int(1);
}

value StopMusic_Env(unit)
{
	Mix_HaltMusic();
	return Val_int(1);
}*/




value LoadFont_Env(value name, value size)
{
	TTF_Font *font = TTF_OpenFont(String_val(name), Int_val(size));
	if (!font)
		printf("Error while loading font : \"%s\"\n\t%s\n", String_val(name), TTF_GetError());
	return ValOfPointer(font);
}

value NewText_Env(value text, value font, value r, value g, value b)
{
	SDL_Color color;
	SDL_Surface *surf;

	color.r = Int_val(r);
	color.g = Int_val(g);
	color.b = Int_val(b);

	if (!FontOfVal(font))
	{
		printf("Warning : tried to draw text with void font\n");
		return Val_int(0);
	}

	surf = TTF_RenderText_Blended( FontOfVal(font), String_val(text), color );
	if (!surf)
		printf("Error while drawing text \"%s\" with font %x (%d,%d,%d)\n\t%s\n", String_val(text), Int_val(font), color.r, color.g, color.b, TTF_GetError());
	return ValOfPointer(surf);
}

value DeleteFont_Env(value font)
{
	if (!FontOfVal(font))
	{
		printf("Warning : tried to free void font\n");
		return Val_int(0);
	}

	TTF_CloseFont(FontOfVal(font));
	return Val_int(1);
}






	

	
