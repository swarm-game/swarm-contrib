#!/bin/bash -xe

SCRIPT_DIR=$(dirname $(realpath -s $0))

GIT_ROOT_DIR_CONTRIB=$(git rev-parse --show-toplevel)
cd $GIT_ROOT_DIR_CONTRIB

# The swarm repo should be checked out as a sibling
GIT_ROOT_DIR_SWARM=$(git -C ../swarm rev-parse --show-toplevel)

SUPPLEMENTAL_CONTENT_PATH=$(mktemp --tmpdir=. -t greenhouse-supplemental.XXXX.yaml)
FLOWERS_DATA=$SCRIPT_DIR/flowers.json
SCENARIO_TEMPLATE=$SCRIPT_DIR/scenario-template.yaml

SCENARIO_PARENT_DIR=data/scenarios/Challenges/Ranching
GREENHOUSE_AUX_FILES_DIR=$SCENARIO_PARENT_DIR/_greenhouse
SCENARIO_RELATIVE_PATH=$SCENARIO_PARENT_DIR/greenhouse.yaml
SCENARIO_DESTINATION=$GIT_ROOT_DIR_SWARM/$SCENARIO_RELATIVE_PATH

stack build --fast gen-scenario:gen-flowers
stack exec gen-flowers -- --data $FLOWERS_DATA --out $SUPPLEMENTAL_CONTENT_PATH

# NOTE: If this command does not work within VS Code console, run this parent script
# from an independent terminal app.
# See https://stackoverflow.com/questions/75659711/vscode-yq-fails-with-permission-denied
$GIT_ROOT_DIR_SWARM/scripts/merge-yaml.sh "$SUPPLEMENTAL_CONTENT_PATH $SCENARIO_TEMPLATE" > $SCENARIO_DESTINATION

stack exec gen-flowers -- --data $FLOWERS_DATA --out $GIT_ROOT_DIR_SWARM/$GREENHOUSE_AUX_FILES_DIR/planter.sw --codegen-template $SCRIPT_DIR/code/planter-template.sw.j2

SOLUTION_CHECKER_SW_PATH=$GIT_ROOT_DIR_SWARM/$GREENHOUSE_AUX_FILES_DIR/solution-checker.sw
# Place the solution checking script inline into the YAML file as an optimization
PROGRAM=$(cat $SOLUTION_CHECKER_SW_PATH | sed -e 's/[[:blank:]]\+$//') yq -i '.objectives[0].condition = strenv(PROGRAM) | .objectives[].condition style="literal"' $SCENARIO_DESTINATION

cd $GIT_ROOT_DIR_SWARM
scripts/play.sh --scenario $SCENARIO_RELATIVE_PATH --autoplay