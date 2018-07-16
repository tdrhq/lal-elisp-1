

EMACS := emacs
TESTCOMMAND=$(EMACS) --no-desktop -q  --no-init-file --kill --batch -l

verify-emacs:
	${EMACS} --version

lal-workspace-test:
	$(TESTCOMMAND) lal-workspace-test.el

lal-add-import-test:
	$(TESTCOMMAND) lal-add-import-test.el

lal-test-setup-test:
	$(TESTCOMMAND) lal-test-setup-test.el

test: verify-emacs lal-add-import-test lal-workspace-test lal-test-setup-test
	true

jenkins:
	EMACS=~arnold/.local/bin/emacs $(MAKE) test
