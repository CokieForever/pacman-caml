CC=camlc
SUBLIBS=Mod_Bimedia Mod_Sound
BDIR=Mod_Bimedia
GDIR=Mod_Game
SDIR=Mod_Sound
SDLDIR=SDL
FMODDIR=FMOD
INSTALLFILES=$(SDLDIR)/runtime/* $(FMOD)/runtime/*
LDFLAGS=-lunix -lX11 -lpthread -lm -ldl
CFLAGS=-c -g -I $(BDIR) -I $(GDIR) -I $(SDIR) -ccopt -I$(SDLDIR)/headers -ccopt -I$(FMODDIR)/headers
STATICLIB=$(SDLDIR)/static/libSDL.a $(SDLDIR)/static/libSDL_image.a $(SDLDIR)/static/libSDL_ttf.a $(FMODDIR)/static/libfmodex.a
EXEC=essai


all: sublibs $(EXEC)

$(EXEC): $(EXEC).zo $(BDIR)/bimedia.zo $(BDIR)/bimedia_c.o $(SDIR)/sound_c.o $(SDIR)/sound.zo $(GDIR)/game.zo
	$(CC) -custom -g -o $@ unix.zo $(BDIR)/bimedia.zo $(BDIR)/bimedia_c.o $(SDIR)/sound_c.o $(SDIR)/sound.zo $(GDIR)/game.zo $< $(LDFLAGS) $(STATICLIB)

$(GDIR)/game.zi: $(GDIR)/game.mli $(BDIR)/bimedia.zi $(SDIR)/sound.zi
	$(CC) $(CFLAGS) $<

$(GDIR)/game.zo: $(GDIR)/game.ml $(GDIR)/game.zi $(BDIR)/bimedia.zi $(SDIR)/sound.zi
	$(CC) $(CFLAGS) $<

$(EXEC).zo: $(EXEC).ml $(BDIR)/bimedia.zi $(SDIR)/sound.zi $(GDIR)/game.zi
	$(CC) $(CFLAGS) $<

sublibs:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) all); done
	for i in $(SUBLIBS); do (cp ../Test\ $$i/$$i/*.zi* ./$$i; cp ../Test\ $$i/$$i/*.*o ./$$i); done

clean:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) clean); done
	rm -f *.z? $(GDIR)/*.o $(GDIR)/*.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
