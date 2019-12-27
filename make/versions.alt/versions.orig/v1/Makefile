
build:
	gcc -I. $(COMPILE_PARAMETERS) -o make.exe make.c libglob.a -lkstat -L/usr/local/lib -lkvm -lelf

build-st:
	cfe make.c
	il-st-2 make.c
	mv -f make.int.c make.st.int.c
	gcc -I. -I$(ARISTOTLE_DIR)/headers/ $(COMPILE_PARAMETERS) \
	-o make.st.int.exe make.st.int.c libglob.a -lkstat -L/usr/local/lib \
	-lkvm -lelf -L $(ARISTOTLE_DIR)/lib/sunos5/ -lIPF_st \
	-lglobalfunc -lm

build-bt:
	cfe make.c
	il-bt-2 make.c
	mv -f make.int.c make.bt.int.c
	gcc -I. -I$(ARISTOTLE_DIR)/headers/ $(COMPILE_PARAMETERS) \
	-o make.bt.int.exe make.bt.int.c libglob.a -lkstat -L/usr/local/lib \
	-lkvm -lelf -L $(ARISTOTLE_DIR)/lib/sunos5/ -lIPF_bt \
	-lglobalfunc -lm
