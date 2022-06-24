.PHONY: all clean coverage install lint test

SRC := $(shell git ls-files '*.ss')
INSTALLROOT := ~/.emacs.d/swish-lint

ifeq (Windows_NT,${OS})
EXESUFFIX:= .exe
SCHEME_LIBPATH:=$(shell cygpath -w "$$(dirname "$$(type -p scheme)")")
else
EXESUFFIX:=
SCHEME_LIBPATH:=
endif

all: swish-lint${EXESUFFIX}

swish-lint${EXESUFFIX}:: | prepare-source
swish-lint${EXESUFFIX}:: git.revision ${SRC}
	@./copy-dlls -s "${SCHEME_LIBPATH}"
	swish-build -o $@ main.ss -b petite --rtlib swish --libs-visible

test: all
	swish-test --progress test --report mat-report.html .

coverage: all
	rm -f profile.data
	swish-test --progress test --report mat-report.html --save-profile profile.data --coverage coverage.html --exclude 'testing/**' .

lint: swish-lint${EXESUFFIX} ${SRC}
	./swish-lint${EXESUFFIX} ${SRC}

# force evaluation, but use a different target from the output that
# main.ss depends on so we don't rebuild needlessly
.PHONY: prepare-source
prepare-source:
	@git describe --always --exclude='*' --abbrev=40 --dirty > git.revision.tmp
	@if cmp --quiet git.revision git.revision.tmp; then \
	  rm git.revision.tmp; \
	else \
	  mv git.revision.tmp git.revision; touch software-info.ss; echo "git.revision changed"; \
	fi

install: all
	install -d ${INSTALLROOT}
	install swish-lint${EXESUFFIX} ${INSTALLROOT}
	install -m 644 swish-lint.boot ${INSTALLROOT}
	install -m 644 lsp-swish.el ${INSTALLROOT}
ifeq (Windows_NT,${OS})
	install csv*.dll uv.dll osi.dll sqlite3.dll ${INSTALLROOT}
endif

clean:
	rm -f git.revision
	rm -f swish-lint${EXESUFFIX} swish-lint.boot
	rm -f *.{so,mo,wpo,sop,ss.html}
	rm -f testing/*.{so,mo,wpo,sop,ss.html}
	rm -f profile.data coverage.html mat-report.html
