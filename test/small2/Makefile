# Makefile in small2

hola: hola.c
	gcc -g -Wall -o hola hola.c
	./hola

holabox: holabox.c
	gcc -g -Wall -o holabox -D_GNUCC -Dx86_LINUX -DBEFOREBOX \
		-I /home/scott/wrk/safec/cil/lib \
		./holabox.c \
		/home/scott/wrk/safec/cil/lib/../obj/safecdebuglib.a

