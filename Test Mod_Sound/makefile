CC=camlc
SDIR=Mod_Sound
FMODDIR=FMOD
INSTALLFILES=$(FMODDIR)/runtime/*
LDFLAGS=-lunix -lgraph -lX11
CFLAGS=-c -g -I $(SDIR) -ccopt -I$(FMODDIR)/headers
STATICLIB=$(FMODDIR)/static/libfmodex.a
EXEC=essai

all: $(EXEC)

$(EXEC): $(EXEC).zo $(SDIR)/sound.zo $(SDIR)/sound.zi $(SDIR)/sound_c.o $(SDIR)/sound_c.zi
	$(CC) -custom -g -o $@ unix.zo graphics.zo $(SDIR)/sound_c.o $(SDIR)/sound.zo $< $(LDFLAGS) $(STATICLIB)

$(SDIR)/sound.zi: $(SDIR)/sound.mli
	$(CC) $(CFLAGS) $<

$(SDIR)/sound.zo: $(SDIR)/sound.ml $(SDIR)/sound.zi $(SDIR)/sound_c.zi
	$(CC) $(CFLAGS) $<

$(SDIR)/sound_c.zi: $(SDIR)/sound_c.mli
	$(CC) $(CFLAGS) $<

$(SDIR)/sound_c.o: $(SDIR)/sound_c.c
	$(CC) $(CFLAGS) $<
	mv sound_c.o $(SDIR)/

$(EXEC).zo: $(EXEC).ml $(SDIR)/sound.zi
	$(CC) $(CFLAGS) $<

clean:
	rm -f *.z? $(SDIR)/*.o $(SDIR)/*.z?

install:
	sudo cp -i -P -t /usr/lib/ $(INSTALLFILES)
