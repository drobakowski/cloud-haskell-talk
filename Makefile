
SOURCE = CloudHaskellTalk
HS_PROG = $(SOURCE)
HC = ghc

all: compile

clean:
	@rm -f $(SOURCE).o $(SOURCE).hi $(HS_PROG)

compile:
	@$(HC) -threaded --make ${SOURCE}.hs

$(HS_PROG): compile

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