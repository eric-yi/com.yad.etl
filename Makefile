BIN=bin
LIB=lib
TMP=temp
RE=./rebar
ETL_LIB=ebin
RE_SRC=$(TMP)/rebar
MYSQL_SRC=$(TMP)/erlang-mysql-driver
MYSQL_LIB=$(MYSQL_SRC)/ebin
LOG4ERL_SRC=$(TMP)/log4erl
LOG4ERL_LIB=$(LOG4ERL_SRC)/ebin

default : build

all : depends clean compile build

build : clean compile
	-cp -rf $(ETL_LIB)/* $(LIB)

compile :
	$(RE) compile

release :
	$(RE) generate

clean :
	$(RE) clean

depends : init_depends depend_rebar depend_mysql depend_log4erl

init_depends :
	-mkdir -p $(LIB)

depend_mysql : init_mysql
	-git clone https://github.com/dizzyd/erlang-mysql-driver $(MYSQL_SRC)
	-cp -rf rebar $(MYSQL_SRC)
	-cd $(MYSQL_SRC) && make
	-cp -rf $(MYSQL_LIB)/* $(LIB)
init_mysql :
	-rm -rf $(MYSQL_SRC)

depend_log4erl : init_log4erl
	-git clone https://github.com/ahmednawras/log4erl $(LOG4ERL_SRC)
	-cp -rf rebar $(LOG4ERL_SRC)
	-cd $(LOG4ERL_SRC)/src && erlc log4erl_lex.xrl && erlc log4erl_parser.yrl && make; cd ../
	-cp -rf $(LOG4ERL_LIB)/* $(LIB)
init_log4erl :
	-rm -rf $(LOG4ERL_SRC)

depend_rebar : init_rebar
	-git clone git://github.com/rebar/rebar.git $(RE_SRC)
	-cd $(RE_SRC) && ./bootstrap
	-cp $(RE_SRC)/rebar ./
init_rebar :
	-rm -rf rebar
	-mkdir -p $(TMP)
	-rm -rf $(RE_SRC)

.PHONY : all build compile release clean depends

