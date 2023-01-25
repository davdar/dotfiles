# [EXAMPLE OVERLEAF CONFIG]
#
# ARGS := \
#   MAIN_SRC    = main.tex \
#   TEX_SRC_DIR = . \
#   BIB_SRC_DIR = . \
#   CLS_SRC_DIR = . \
#   BST_SRC_DIR = . \
#   IMG_SRC_DIR = figures \
#   IMG_DST_DIR = figures
#
# `make final` to build final version, or...
#
# [TO BUILD FINAL VERSION AS DEFULT]
# ARGS := \
#   STYLE = final \
#   ...

ARGS :=

.PHONY: default
default:
	make -f Makefile_darais $(ARGS)

%:
	make -f Makefile_darais $@ $(ARGS)

