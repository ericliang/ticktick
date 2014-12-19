PREFIX:=../
DEST:=$(PREFIX)$(PROJECT)

REBAR=./rebar

REL_DIR=./rel
APPID=ticktick
NODEID=$(APPID)

.PHONY: all edoc test clean build_plt dialyzer app

all:
	@$(REBAR) prepare-deps

edoc: all
	@$(REBAR) doc

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

app:
	@$(REBAR) -r create template=mochiwebapp dest=$(DEST) appid=$(PROJECT)

$(REL_DIR)/reltool.config:
	@cd $(REL_DIR) && ../$(REBAR) create-node nodeid=$(NODEID)
	@echo "==> You can now edit the file: ./$(REL_DIR)/reltool.config, and run: make rel"
	@echo "==> Note: {lib_dir, ".."} should be added to sys.app.$(NODEID) in ./$(REL_DIR)/reltool.config"

rel: all $(REL_DIR)/reltool.config
	@$(REBAR) generate

