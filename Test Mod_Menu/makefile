CC=camlc
SUBLIBS=Mod_Bimedia Mod_Sound
BDIR=Mod_Bimedia
MDIR=Mod_Menu
SDIR=Mod_Sound
SDLDIR=SDL
FMODDIR=FMOD
INSTALLFILES=$(SDLDIR)/runtime/* $(FMODDIR)/runtime/*
LDFLAGS=-lunix -lX11 -lpthread -lm -ldl
CFLAGS=-c -g -I $(BDIR) -I $(MDIR) -I $(SDIR)
STATICLIB=$(SDLDIR)/static/libSDL.a $(SDLDIR)/static/libSDL_ttf.a $(SDLDIR)/static/libSDL_image.a $(FMODDIR)/static/libfmodex.a
EXEC=essai


all: sublibs $(EXEC)

$(EXEC): $(EXEC).zo $(SDIR)/sound_c.o $(BDIR)/bimedia_c.o $(SDIR)/sound.zo $(BDIR)/bimedia.zo $(MDIR)/menus.zo
	$(CC) -custom -g -o $@ unix.zo $(SDIR)/sound_c.o $(BDIR)/bimedia_c.o $(SDIR)/sound.zo $(BDIR)/bimedia.zo $(MDIR)/menus.zo $< $(LDFLAGS) $(STATICLIB)

$(MDIR)/menus.zi: $(MDIR)/menus.mli $(BDIR)/bimedia.zi
	$(CC) $(CFLAGS) $<

$(MDIR)/menus.zo: $(MDIR)/menus.ml $(MDIR)/menus.zi $(BDIR)/bimedia.zi
	$(CC) $(CFLAGS) $<

$(EXEC).zo: $(EXEC).ml $(BDIR)/bimedia.zi $(MDIR)/menus.zi $(SDIR)/sound.zi
	$(CC) $(CFLAGS) $<

sublibs:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) all); done
	for i in $(SUBLIBS); do (cp ../Test\ $$i/$$i/*.zi* ./$$i; cp ../Test\ $$i/$$i/*.*o ./$$i); done

clean:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) clean); done
	rm -f *.z? $(MDIR)/*.o $(MDIR)/*.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
