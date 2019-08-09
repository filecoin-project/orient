#FROM davazp/quicksbcl
FROM lokedhs/sbcl-quicklisp

ENV LC_TYPE=en_US.UTF-8

RUN apt-get update; apt-get upgrade -y; apt-get install -y openssl; apt-get install -y libssl1.0.0;

RUN apt-get install -y cl-launch
RUN apt-get install -y jq

COPY . /root/orient

RUN ln -s /root/orient/orient.asd /root/quicklisp/local-projects/orient.asd

# RUN cd /root/orient/bin && cl -Q -sp orient --dump /root/ubercalc
# We should be able to use `cl --dump`, but for some reason it's not working in the container.

RUN cl -Q -sp orient -x "(progn (push :docker *features*) (sb-ext:save-lisp-and-die \"/root/ubercalc\" :toplevel #'cli:main :executable t))"

ENTRYPOINT ["/root/ubercalc"]
