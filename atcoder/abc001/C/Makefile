.PHONY: test clean

a.out: main.ml
	ocamlopt main.ml

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

	@cat in6 | ./a.out > out6.gen
	diff out6 out6.gen

	@cat in7 | ./a.out > out7.gen
	diff out7 out7.gen

	@cat in8 | ./a.out > out8.gen
	diff out8 out8.gen


clean:
	rm -f a.out
	rm -f *.gen