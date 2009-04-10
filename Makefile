# leave these lines alone
.SUFFIXES: .erl .beam

.erl.beam:
	erlc -I include -o ebin -W $<

ERL = erl -boot start_clean 

# Here's a list of the erlang modules you want compiling
# If the modules don't fit onto one line add a \ character 
# to the end of the line and continue on the next line

# Edit the lines below
MODS = src/yggdrasil \
	   src/yggdrasil_sup \
	   src/yggdrasil_receiver \
	   src/yggdrasil_listener

# The first target in any makefile is the default target.
# If you just type "make" then "make all" is assumed (because
#   "all" is the first target in this makefile)

all: compile

#compile: ${MODS:%=%.beam} subdirs
compile: ${MODS:%=%.beam}
	
## special compilation requirements are added here

#special1.beam: special1.erl    
#	${ERL} -Dflag1 -W0 special1.erl

## run an application from the makefile

run: compile
	scripts/yggdrasil.sh

nrun: compile
	${ERL} -pa ./ -s yggdrasil -noshell

# the subdirs target compiles any code in 
# sub-directories

#subdirs:
#	cd dir1; make
#	cd dir2; make
#	...

# remove all the code

clean:	
	rm -rf *.beam erl_crash.dump
#	cd dir1; make clean
#	cd dir2; make clean  

