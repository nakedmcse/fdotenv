all: fdotenv test

fdotenv: fdotenv.f90 fdotenv_tokenizer.f90
	gfortran -ffree-line-length-0 -o fdotenv_tokenizer.o -c fdotenv_tokenizer.f90
	gfortran -ffree-line-length-0 -o fdotenv.o -c fdotenv.f90
	ar rcs fdotenv.a fdotenv.o fdotenv_tokenizer.o
	rm -f *.o

test: fdotenv.a test.f90 test_tokenizer.f90
	gfortran -ffree-line-length-0 -o test test.f90 fdotenv.a
	gfortran -ffree-line-length-0 -o test_tokenizer test_tokenizer.f90 fdotenv.a

clean:
	rm -f *.a
	rm -f *.o
	rm -f *.mod
	rm -f test
	rm -f test_tokenizer