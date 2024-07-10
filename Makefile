.PHONY: all
all: exe

FSTAR_HOME ?= $(realpath $(dir $(shell which fstar.exe))/..)
FSTAR_EXE = $(FSTAR_HOME)/bin/fstar.exe

# Assume FSTAR_HOME points to the F* source tree
include $(FSTAR_HOME)/.common.mk
include $(FSTAR_HOME)/ulib/gmake/z3.mk    # This pins $(Z3) ...
include $(FSTAR_HOME)/ulib/gmake/fstar.mk # and $(FSTAR) for all sub-make calls
include $(FSTAR_HOME)/src/Makefile.boot.common

OUTPUT_DIRECTORY = $(CURDIR)/ocaml

FSTAR_OPTIONS= \
	$(OTHERFLAGS) --lax --MLish \
	--no_location_info --warn_error -271-272-241-319-274 \
	$(addprefix --include , $(addprefix $(FSTAR_HOME)/src/,$(INCLUDE_PATHS))) \
	--include . \
	--odir "$(OUTPUT_DIRECTORY)" \
	--cache_dir $(FSTAR_HOME)/src/.cache.boot \
	--already_cached 'Prims,FStar' \
	--cache_checked_modules
FSTAR_C=$(RUNLIM) $(FSTAR_EXE) $(SIL) $(FSTAR_OPTIONS)

EXTRACT_FILES=Mutate.ml
extract: $(addprefix $(OUTPUT_DIRECTORY)/,$(EXTRACT_FILES))

.PHONY: exe
exe: extract
	$(MAKE) -C ocaml

# And then, in a separate invocation, from each .checked.lax we
# extract an .ml file
$(OUTPUT_DIRECTORY)/%.ml: %.fst Rand.fsti
	mkdir -p $(OUTPUT_DIRECTORY)
	$(call msg, "EXTRACT", $(notdir $@))
	$(Q)$(BENCHMARK_PRE) $(FSTAR_C) $< \
		   --odir "$(OUTPUT_DIRECTORY)" \
                   --codegen OCaml \
                   --extract_module $(basename $(notdir $<))
	chmod -x $@
