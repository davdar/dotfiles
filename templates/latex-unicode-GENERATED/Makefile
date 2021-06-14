# ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
# Override variables in CONFIG here...
# ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓



# ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑
# Don't touch stuff below here if possible
# ↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑

# EXAMPLE OVERLEAF CONFIG
#
# MAIN_SRC    := <main file>
# 
# TEX_SRC_DIR := .
# STY_SRC_DIR := .
# BIB_SRC_DIR := .
# CLS_SRC_DIR := .
# BST_SRC_DIR := .
# PDF_SRC_DIR := figures
# PNG_SRC_DIR := figures
# 
# IMG_DST_DIR := figures

##########
# CONFIG #
##########

MAIN_SRC    ?= main
MAIN_OUT    ?= main

STYLE       ?= dev
ALL_STYLES  ?= dev review review-ext draft final

LATEX       ?= pdflatex # xelatex lualatex

BIBTEX      ?= bibtex # bibtex8 biber touch

TEX_SRC_DIR ?= tex
STY_SRC_DIR ?= sty
BIB_SRC_DIR ?= bib
CLS_SRC_DIR ?= cls
BST_SRC_DIR ?= bst
PDF_SRC_DIR ?= pdf
PND_SRC_DIR ?= png

IMG_DST_DIR ?= .

SED_DIR     ?= sed

SED_BIB     ?= yup # set to empty string for false

STG_DIR     ?= stg
OUT_DIR     ?= out

ifeq ($(STYLE), dev)
  INTERNAL  ?= 1
  AUTHORID  ?= 1
  EXTENDED  ?= 1
else ifeq ($(STYLE), anon)
  INTERNAL  ?= 0
  AUTHORID  ?= 0
  EXTENDED  ?= 0
else ifeq ($(STYLE), anon-ext)
  INTERNAL  ?= 0
  AUTHORID  ?= 0
  EXTENDED  ?= 1
else ifeq ($(STYLE), final)
  INTERNAL  ?= 0
  AUTHORID  ?= 1
  EXTENDED  ?= 0
else ifeq ($(STYLE), final-ext)
  INTERNAL  ?= 0
  AUTHORID  ?= 1
  EXTENDED  ?= 1
endif

INTERNAL ?= 1
AUTHORID ?= 1
EXTENDED ?= 1

LATEX_CMD := \
  \\def\\isinternal{$(INTERNAL)} \
  \\def\\isauthorid{$(AUTHORID)} \
  \\def\\isextended{$(EXTENDED)} \
  \\input{$(MAIN_SRC)}

#########
# GLOBS #
#########

TEX_SRC_FILES := $(wildcard $(TEX_SRC_DIR)/*.tex)
TEX_STG_FILES := $(subst $(TEX_SRC_DIR)/,$(STG_DIR)/,$(TEX_SRC_FILES))

STY_SRC_FILES := $(wildcard $(STY_SRC_DIR)/*.sty)
STY_STG_FILES := $(subst $(STY_SRC_DIR)/,$(STG_DIR)/,$(STY_SRC_FILES))

BIB_SRC_FILES := $(wildcard $(BIB_SRC_DIR)/*.bib)
BIB_STG_FILES := $(subst $(BIB_SRC_DIR)/,$(STG_DIR)/,$(BIB_SRC_FILES))

CLS_SRC_FILES := $(wildcard $(CLS_SRC_DIR)/*.cls)
CLS_STG_FILES := $(subst $(CLS_SRC_DIR)/,$(STG_DIR)/,$(CLS_SRC_FILES))

BST_SRC_FILES := $(wildcard $(BST_SRC_DIR)/*.bst)
BST_STG_FILES := $(subst $(BST_SRC_DIR)/,$(STG_DIR)/,$(BST_SRC_FILES))

PDF_SRC_FILES := $(wildcard $(PDF_SRC_DIR)/*.pdf)
PDF_STG_FILES := $(subst $(PDF_SRC_DIR)/,$(STG_DIR)/$(IMG_DST_DIR)/,$(PDF_SRC_FILES))

PNG_SRC_FILES := $(wildcard $(PNG_SRC_DIR)/*.png)
PNG_STG_FILES := $(subst $(PNG_SRC_DIR)/,$(STG_DIR)/$(IMG_DST_DIR)/,$(PNG_SRC_FILES))

STG_FILES := \
  $(TEX_STG_FILES) \
  $(STY_STG_FILES) \
  $(BIB_STG_FILES) \
  $(CLS_STG_FILES) \
  $(BST_STG_FILES) \
  $(PDF_STG_FILES) \
  $(PNG_STG_FILES)

SED_FILES := $(sort $(wildcard $(SED_DIR)/*.sed))

###############
# STAGE FILES #
###############

$(STG_DIR)/%.tex: $(TEX_SRC_DIR)/%.tex $(SED_FILES)
	mkdir -p $(@D)
	cp $< $@
	for s in $(SED_FILES) ; do sed -E -i.bu -f $$s $@ ; done

$(STG_DIR)/%.sty: $(STY_SRC_DIR)/%.sty
	mkdir -p $(@D)
	cp $< $@

$(STG_DIR)/%.bib: $(BIB_SRC_DIR)/%.bib
	mkdir -p $(@D)
	cp $< $@
	$(if $(SED_BIB),for s in $(SED_FILES) ; do sed -E -i.bu -f $$s $@ ; done)

$(STG_DIR)/%.cls: $(CLS_SRC_DIR)/%.cls
	mkdir -p $(@D)
	cp $< $@

$(STG_DIR)/%.bst: $(BST_SRC_DIR)/%.bst
	mkdir -p $(@D)
	cp $< $@

$(STG_DIR)/$(IMG_DST_DIR)/%.pdf: $(PDF_SRC_DIR)/%.pdf
	mkdir -p $(@D)
	cp $< $@

$(STG_DIR)/$(IMG_DST_DIR)/%.png: $(PNG_SRC_DIR)/%.png
	mkdir -p $(@D)
	cp $< $@

############
# BUILDING #
############

.PHONY: default
default: $(MAIN_OUT)-$(STYLE).pdf ;

$(OUT_DIR)/$(STYLE)/PHASE1: | $(STG_FILES)
	mkdir -p $(@D)
	cd $(STG_DIR) && $(LATEX) --output-directory=../$(@D) "$(LATEX_CMD)"
	touch $@

$(OUT_DIR)/$(STYLE)/PHASE2: $(OUT_DIR)/$(STYLE)/PHASE1
	(cp $(STG_DIR)/*.bib $(@D)/) | true
	cd $(@D) && $(BIBTEX) $(MAIN_SRC)
	touch $@

$(OUT_DIR)/$(STYLE)/PHASE3: $(OUT_DIR)/$(STYLE)/PHASE2
	cd $(STG_DIR) && $(LATEX) --output-directory=../$(@D) "$(LATEX_CMD)"
	touch $@

$(OUT_DIR)/$(STYLE)/PHASE4: $(OUT_DIR)/$(STYLE)/PHASE3 $(STG_FILES)
	cd $(STG_DIR) && $(LATEX) --output-directory=../$(@D) "$(LATEX_CMD)"
	touch $@

$(OUT_DIR)/$(STYLE)/$(MAIN_SRC).pdf: $(OUT_DIR)/$(STYLE)/PHASE4 ;

$(MAIN_OUT)-$(STYLE).pdf: $(OUT_DIR)/$(STYLE)/$(MAIN_SRC).pdf
	cp $< $@

.PHONY: again
again:
	rm -f $(OUT_DIR)/$(STYLE)/PHASE4
	make

.PHONY: rebib
rebib:
	rm -f $(OUT_DIR)/$(STYLE)/PHASE1
	make

.PHONY: twice
twice:
	make again
	make again

.PHONY: thrice
thrice:
	make twice
	make again

##########
# STYLES #
##########

.PHONY: dev
dev:
	make STYLE=dev

.PHONY: review
anon:
	make STYLE=anon

.PHONY: review-ext
anon-ext:
	make STYLE=anon-ext

.PHONY: draft
final:
	make STYLE=final

.PHONY: final
final-ext:
	make STYLE=final-ext

.PHONY: all
all:
	$(foreach S,$(ALL_STYLES),make $S;)

.PHONY: clean
clean:
	rm -rf $(OUT_DIR)/
	rm -rf $(STG_DIR)/
	rm -f $(MAIN_OUT)-*.pdf
