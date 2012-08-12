CC=camlc
SUBLIBS=Mod_Sound Mod_Bimedia Mod_Game Mod_Menu Mod_Interface Mod_Editor
SUBLIBSDIR=./Mods
SDIR=$(SUBLIBSDIR)/Mod_Sound
BDIR=$(SUBLIBSDIR)/Mod_Bimedia
GDIR=$(SUBLIBSDIR)/Mod_Game
MDIR=$(SUBLIBSDIR)/Mod_Menu
IDIR=$(SUBLIBSDIR)/Mod_Interface
EDIR=$(SUBLIBSDIR)/Mod_Editor
SDLDIR=./Mods/SDL
FMODDIR=./Mods/FMOD
INSTALLFILES=$(SDLDIR)/runtime/* $(FMODDIR)/runtime/*
LDFLAGS=-lunix -lX11 -lpthread -lm -ldl
CFLAGS=-c -g -I $(BDIR) -I $(SDIR) -I $(GDIR) -I $(MDIR) -I $(IDIR) -I $(EDIR) -ccopt -I$(SDLDIR)/headers -ccopt -I$(FMODDIR)/headers
STATICLIB=$(SDLDIR)/static/libSDL.a $(SDLDIR)/static/libSDL_ttf.a $(SDLDIR)/static/libSDL_image.a $(FMODDIR)/static/libfmodex.a
EXEC=pacman


all: sublibs $(EXEC)

$(EXEC): $(EXEC).zo $(BDIR)/bimedia_c.o $(SDIR)/sound_c.o $(IDIR)/tecaml.o $(BDIR)/bimedia.zo $(SDIR)/sound.zo $(GDIR)/game.zo $(MDIR)/menus.zo $(IDIR)/interface.zo $(EDIR)/editor.zo
	$(CC) -custom -g -o $@ unix.zo $(BDIR)/bimedia_c.o $(SDIR)/sound_c.o $(IDIR)/tecaml.o $(BDIR)/bimedia.zo $(SDIR)/sound.zo $(GDIR)/game.zo $(MDIR)/menus.zo $(IDIR)/interface.zo $(EDIR)/editor.zo $< $(LDFLAGS) $(STATICLIB)

$(EXEC).zo: $(EXEC).ml $(BDIR)/bimedia.zi $(SDIR)/sound.zi $(GDIR)/game.zi $(MDIR)/menus.zi $(IDIR)/interface.zi $(EDIR)/editor.zo
	$(CC) $(CFLAGS) $<

sublibs:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) all); done
	for i in $(SUBLIBS); do (cp ../Test\ $$i/$$i/*.zi* $(SUBLIBSDIR)/$$i; cp ../Test\ $$i/$$i/*.*o $(SUBLIBSDIR)/$$i); done

clean:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) clean); done
	rm -f *.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
