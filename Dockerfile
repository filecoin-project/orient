#FROM davazp/quicksbcl
FROM lokedhs/sbcl-quicklisp

ENV LC_TYPE=en_US.UTF-8

RUN apt update; apt upgrade -y; apt install -y openssl libssl1.0.0 cl-launch jq python3;

COPY . /root/orient

RUN ln -s /root/orient/orient.asd /root/quicklisp/local-projects/orient.asd

# RUN cd /root/orient/bin && cl -Q -sp orient --dump /root/orient
# We should be able to use `cl --dump`, but for some reason it's not working in the container.

RUN cl -Q -sp orient -x "(progn (push :docker *features*) (sb-ext:save-lisp-and-die \"/root/orient.image\" :toplevel #'cli:main :executable t))"

ENTRYPOINT ["/root/orient.image"]
