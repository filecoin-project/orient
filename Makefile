test:
	cl -Q -sp orient -x "(asdf:test-system :orient)"

docker:
	docker build . -t ubercalc

ubercalc:
	cl -Q -sp orient --dump bin/ubercalc

