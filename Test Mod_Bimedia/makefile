CC=camlc
BDIR=Mod_Bimedia
SDLDIR=SDL
INSTALLFILES=$(SDLDIR)/runtime/*
LDFLAGS=-lunix -lX11 -lpthread -lm -ldl
CFLAGS=-c -g -I $(BDIR) -ccopt -I$(SDLDIR)/headers
STATICLIB=$(SDLDIR)/static/libSDL.a $(SDLDIR)/static/libSDL_image.a $(SDLDIR)/static/libSDL_ttf.a
EXEC=essai


all: $(EXEC)

$(EXEC): $(EXEC).zo $(BDIR)/bimedia.zo $(BDIR)/bimedia.zi $(BDIR)/bimedia_c.o $(BDIR)/bimedia_c.zi
	$(CC) -custom -g -o $@ unix.zo $(BDIR)/bimedia_c.o $(BDIR)/bimedia.zo $< $(LDFLAGS) $(STATICLIB)

$(BDIR)/bimedia.zi: $(BDIR)/bimedia.mli
	$(CC) $(CFLAGS) $<

$(BDIR)/bimedia.zo: $(BDIR)/bimedia.ml $(BDIR)/bimedia.zi $(BDIR)/bimedia_c.zi
	$(CC) $(CFLAGS) $<

$(BDIR)/bimedia_c.zi: $(BDIR)/bimedia_c.mli
	$(CC) $(CFLAGS) $<

$(BDIR)/bimedia_c.o: $(BDIR)/bimedia_c.c
	$(CC) $(CFLAGS) $<
	mv bimedia_c.o $(BDIR)/

$(EXEC).zo: $(EXEC).ml $(BDIR)/bimedia.zi
	$(CC) $(CFLAGS) $<

clean:
	rm -f *.z? $(BDIR)/*.o $(BDIR)/*.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
