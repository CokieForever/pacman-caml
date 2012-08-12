CC=camlc
SUBLIBS=Mod_Bimedia Mod_Game Mod_Sound Mod_Interface
BDIR=Mod_Bimedia
GDIR=Mod_Game
SDIR=Mod_Sound
IDIR=Mod_Interface
EDIR=Mod_Editor
SDLDIR=SDL
FMODDIR=FMOD
INSTALLFILES=$(SDLDIR)/runtime/* $(FMODDIR)/runtime/*
LDFLAGS=-lunix -lX11 -lpthread -lm -ldl
CFLAGS=-c -g -I $(BDIR) -I $(GDIR) -I $(SDIR) -I $(IDIR) -I $(EDIR) -ccopt -I$(SDLDIR)/headers -ccopt -I$(FMODDIR)/headers
STATICLIB=$(SDLDIR)/static/libSDL.a $(SDLDIR)/static/libSDL_ttf.a $(SDLDIR)/static/libSDL_image.a $(FMODDIR)/static/libfmodex.a
EXEC=essai


all: sublibs $(EXEC)

$(EXEC): $(EXEC).zo $(BDIR)/bimedia_c.o $(SDIR)/sound_c.o $(IDIR)/tecaml.o $(BDIR)/bimedia.zo $(SDIR)/sound.zo $(GDIR)/game.zo $(IDIR)/interface.zo $(EDIR)/editor.zo
	$(CC) -custom -g -o $@ unix.zo $(BDIR)/bimedia_c.o $(SDIR)/sound_c.o $(IDIR)/tecaml.o $(BDIR)/bimedia.zo $(SDIR)/sound.zo $(GDIR)/game.zo $(IDIR)/interface.zo $(EDIR)/editor.zo $< $(LDFLAGS) $(STATICLIB)

$(EXEC).zo: $(EXEC).ml $(BDIR)/bimedia.zi $(GDIR)/game.zi $(EDIR)/editor.zi
	$(CC) $(CFLAGS) $<

$(EDIR)/editor.zo: $(EDIR)/editor.ml $(EDIR)/editor.zi $(BDIR)/bimedia.zi $(IDIR)/interface.zi $(GDIR)/game.zi
	$(CC) $(CFLAGS) $<

$(EDIR)/editor.zi: $(EDIR)/editor.mli $(BDIR)/bimedia.zi $(GDIR)/game.zi
	$(CC) $(CFLAGS) $<

sublibs:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) all); done
	for i in $(SUBLIBS); do (cp ../Test\ $$i/$$i/*.zi* ./$$i; cp ../Test\ $$i/$$i/*.*o ./$$i); done

clean:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) clean); done
	rm -f *.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
