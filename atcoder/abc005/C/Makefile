.PHONY: test clean

a.out: main.ml
	ocamlfind ocamlopt -package num -linkpkg main.ml

test: a.out

	@cat in1 | ./a.out > out1.gen
	diff out1 out1.gen

	@cat in2 | ./a.out > out2.gen
	diff out2 out2.gen

	@cat in3 | ./a.out > out3.gen
	diff out3 out3.gen

	@cat in4 | ./a.out > out4.gen
	diff out4 out4.gen

	@cat in5 | ./a.out > out5.gen
	diff out5 out5.gen


clean:
	rm -f a.out
	rm -f *.gen