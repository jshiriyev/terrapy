CC = g++

CFLAGS = -c -Wall

all: Allrun

Allrun: main.o resControl.o
	$(CC) main.o resControl.o -o Allrun

main.o: main.c
	$(CC) $(CFLAGS) main.c

resControl.o: resControl.c
	$(CC) $(CFLAGS) resControl.c

clean:
	rm -rf *o Allrun
