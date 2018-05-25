

EMACS := /home/arnold/.local/bin/emacs
TESTCOMMAND=$(EMACS) --no-desktop -q  --no-init-file --kill --batch -l

verify-emacs:
	${EMACS} --version

lal-add-import-test:
	$(TESTCOMMAND) lal-add-import-test.el

test: verify-emacs lal-add-import-test
	true
