CC=camlc
SUBLIBS=Mod_Bimedia
BDIR=Mod_Bimedia
IDIR=Mod_Interface
SDLDIR=SDL
INSTALLFILES=$(BDIR)/runtime/* $(IDIR)/runtime/*
LDFLAGS=-lunix -lX11 -lpthread -lm -ldl
CFLAGS=-c -g -I $(BDIR) -I $(IDIR) -ccopt -I$(SDLDIR)/headers
STATICLIB=$(SDLDIR)/static/libSDL.a $(SDLDIR)/static/libSDL_mixer.a $(SDLDIR)/static/libSDL_image.a $(SDLDIR)/static/libSDL_ttf.a
EXEC=essai


all: sublibs $(EXEC)

$(EXEC): $(EXEC).zo $(BDIR)/bimedia.zo $(BDIR)/bimedia.zi $(BDIR)/bimedia_c.o $(BDIR)/bimedia_c.zi $(IDIR)/tecaml.zi $(IDIR)/tecaml.o $(IDIR)/interface.zo $(IDIR)/interface.zi
	cd ../Test\ $(BDIR) ; make clean all
	cp ../Test\ $(BDIR)/$(BDIR)/* $(BDIR)/
	$(CC) -custom -g -o $@ unix.zo $(BDIR)/bimedia_c.o $(BDIR)/bimedia.zo $(IDIR)/tecaml.o $(IDIR)/interface.zo $< $(LDFLAGS) $(STATICLIB)

$(IDIR)/interface.zi: $(IDIR)/interface.mli $(BDIR)/bimedia.zi
	$(CC) $(CFLAGS) $<

$(IDIR)/interface.zo: $(IDIR)/interface.ml $(IDIR)/interface.zi $(BDIR)/bimedia.zi $(IDIR)/tecaml.zi
	$(CC) $(CFLAGS) $<

$(IDIR)/tecaml.zi: $(IDIR)/tecaml.mli
	$(CC) $(CFLAGS) $<

$(IDIR)/tecaml.o: $(IDIR)/tecaml.c $(IDIR)/textedition.c $(IDIR)/textedition.h
	$(CC) $(CFLAGS) $<
	mv tecaml.o $(IDIR)/

$(EXEC).zo: $(EXEC).ml $(BDIR)/bimedia.zi $(IDIR)/interface.zi
	$(CC) $(CFLAGS) $<

sublibs:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) all); done
	for i in $(SUBLIBS); do (cp ../Test\ $$i/$$i/*.zi* ./$$i; cp ../Test\ $$i/$$i/*.*o ./$$i); done

clean:
	for i in $(SUBLIBS); do (cd ../Test\ $$i; $(MAKE) clean); done
	rm -f *.z? $(IDIR)/*.o $(IDIR)/*.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
