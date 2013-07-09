
SOURCE = CloudHaskellTalk PingPongUntyped PingPongTyped PingPongTypedBad PingPongUntypedBad PingPongUntypedBadNoCrash PingPongUntypedBadNoCrashReceiveWait PingPongUntypedBadSupervised PingPongUntypedSimple
HS_PROG = $(SOURCE)
GHCFLAGS=-Wall -Werror -fwarn-tabs -fwarn-incomplete-record-updates -fwarn-monomorphism-restriction -fwarn-unused-do-bind
# GHCFLAGS=
HC=ghc $(GHCFLAGS)

all: compile

clean:
	@rm -f *.o *.hi $(HS_PROG)

compile:
	@clear

	@$(HC) -threaded --make CloudHaskellTalk.hs

	@$(HC) -threaded --make PingPongUntyped.hs
	@$(HC) -threaded --make PingPongTyped.hs

	@$(HC) -threaded --make PingPongTypedBad.hs

	@$(HC) -threaded --make PingPongUntypedBad.hs
	@$(HC) -threaded --make PingPongUntypedBadNoCrash.hs

	@$(HC) -threaded --make PingPongUntypedBadNoCrashReceiveWait.hs

	@$(HC) -threaded --make PingPongUntypedBadSupervised.hs

	@$(HC) -threaded --make PingPongUntypedSimple.hs

# @$(HC) -threaded --make ${SOURCE}.hs


$(HS_PROG): compile

master: compile
	@./$(HS_PROG) master 8006

slave: compile
	@./$(HS_PROG) slave 8005

slaves: compile
	@./$(HS_PROG) slave 8000 &
	@./$(HS_PROG) slave 8001 &
	@./$(HS_PROG) slave 8002 &
	@./$(HS_PROG) slave 8003 &
	@sleep 2
	@./$(HS_PROG) master 8004

ping: compile
	@clear
	./$(HS_PROG) ping 2020

chat_server: compile
	@clear
	./$(HS_PROG) chat_server ${ARGS}

chat_client: compile
	@clear
	./$(HS_PROG) chat_client ${ARGS}

PingPongUntypedServer: compile
	@clear
	./PingPongUntyped master ${ARGS}

PingPongUntypedClient: compile
	@clear
	./PingPongUntyped slave ${ARGS}


PingPongUntypedBadServer: compile
	@clear
	./PingPongUntypedBad master ${ARGS}

PingPongUntypedBadClient: compile
	@clear
	./PingPongUntypedBad slave ${ARGS}


PingPongUntypedBadNoCrashServer: compile
	@clear
	./PingPongUntypedBadNoCrash master ${ARGS}

PingPongUntypedBadNoCrashClient: compile
	@clear
	./PingPongUntypedBadNoCrash slave ${ARGS}


PingPongUntypedBadNoCrashReceiveWaitServer: compile
	@clear
	./PingPongUntypedBadNoCrashReceiveWait master ${ARGS}

PingPongUntypedBadNoCrashReceiveWaitClient: compile
	@clear
	./PingPongUntypedBadNoCrashReceiveWait slave ${ARGS}


PingPongTypedServer: compile
	@clear
	./PingPongTyped master ${ARGS}

PingPongTypedClient: compile
	@clear
	./PingPongTyped slave ${ARGS}

PingPongTypedBadServer: compile
	@clear
	./PingPongTypedBad master ${ARGS}

PingPongTypedBadClient: compile
	@clear
	./PingPongTypedBad slave ${ARGS}

PingPongUntypedBadSupervisedServer: compile
	@clear
	./PingPongUntypedBadSupervised master ${ARGS}

PingPongUntypedBadSupervisedClient: compile
	@clear
	./PingPongUntypedBadSupervised slave ${ARGS}


PingPongUntypedSimpleServer: compile
	@clear
	./PingPongUntyped master ${ARGS}

PingPongUntypedSimpleClient: compile
	@clear
	./PingPongUntypedSimple slave ${ARGS}

