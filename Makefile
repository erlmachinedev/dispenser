PROJECT = dispenser
PROJECT_DESCRIPTION = "Machine which brings Erlang to the cloud (AWS Lambda)"
PROJECT_VERSION = 0.0.1

DEPS = erlbox jsx gun

dep_erlbox = git git@github.com:erlmachinedev/erlbox.git

dep_gun = hex 2.0.1
dep_jsx = hex 3.1.0

TEST_DEPS = cowboy meck

DEP_PLUGINS = cowboy

dep_cowboy = hex 2.10.0

dep_meck = git git@github.com:eproxus/meck.git

include erlang.mk
