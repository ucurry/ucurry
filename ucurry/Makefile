.PHONY : all
all : ucurry

ucurry :
	@dune build
	@cp ./_build/install/default/bin/ucurry ./
	@chmod 777 ucurry

clean:
	@dune clean;\
	cd ./tests/lazy; \
	rm -f *.out *.ast *.cast *.last *.ll *.s *.exe; \
	cd ../scanparse; \
	rm -f *.ast *.cast *.last *.ll *.s *.exe *.out *.trimmed; \
	cd ../non-lazy; \
	rm -f *.ast *.cast *.last *.ll *.s *.exe *.out *.trimmed; \
	cd ../test_suite/positive_test;\
	rm -f *.out *.ll *.s *.exe ;\
	cd ../negative_test;\
	rm -f *.out *.ll *.s *.exe ;\
	rm -f ucurry
