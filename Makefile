
SOURCE = CloudHaskellTalk
HS_PROG = $(SOURCE)
HC = ghc

all: compile

clean:
	@rm -f $(SOURCE).o $(SOURCE).hi $(HS_PROG)

compile:
	@clear
	@$(HC) -threaded --make ${SOURCE}.hs

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
