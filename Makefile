.PHONY: server fmt

server:
	art work
fmt:
	find . -maxdepth 5 -name "*.scm" -type f -exec guix style -f {} \;
