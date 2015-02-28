LIB=lib
TMP=temp
RE=./rebar
RE_SRC=$(TMP)/rebar
MYSQL_SRC=$(TMP)/erlang-mysql-driver
MYSQL_BIN=$(MYSQL_SRC)/ebin

all : depends clean compile

build : clean compile

compile :
	$(RE) compile

release :
	$(RE) generate

clean :
	$(RE) clean

depends : init_depends depend_rebar depend_mysql

init_depends : 
	-mkdir -p $(LIB)

depend_mysql : init_mysql
	-git clone https://github.com/dizzyd/erlang-mysql-driver $(MYSQL_SRC)
	-cp -rf rebar $(MYSQL_SRC)
	-cd $(MYSQL_SRC) && make
	-cp -rf $(MYSQL_BIN)/* $(LIB)
init_mysql :
	-rm -rf $(MYSQL_SRC)

depend_rebar : init_rebar
	-git clone git://github.com/rebar/rebar.git $(RE_SRC)
	-cd $(RE_SRC) && ./bootstrap
	-cp $(RE_SRC)/rebar ./
init_rebar :
	-rm -rf rebar
	-mkdir -p $(TMP)
	-rm -rf $(RE_SRC)
	
.PHONY : all build compile release clean depends

