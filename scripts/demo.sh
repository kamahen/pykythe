# Do all the setup for a demo run.

set -o xtrace
set -o verbose
set -o errexit
set -o pipefail

# Assumes that you've installed by:
# cd $SRCDIR
# git clone https://github.com/kamahen/pykythe.git

# Assumes that you've got python3.7 or later with a suitable lib2to3

SRCDIR=$HOME/src             # Change as needed
DOWNLOADDIR=$HOME/Downloads  # Change as needed
KYTHE_VERSION=v0.0.37        # Change as needed
NEED_SWIPL_VERSION="8.1.28"  # Change as needed

# The following are needed only if you don't have Python3.7 already:
# sudo add-apt-repository ppa:deadsnakes/ppa
# sudo apt install python3.7

if [ ! type swipl ]
then
    sudo apt install software-properties-common
    sudo apt-add-repository ppa:swi-prolog/devel
    sudo apt update
    sudo apt install swi-prolog
fi

SWIPL_VERSION=$(swipl --version | cut -d' ' -f 3)
if [[ "$SWIPL_VERSION" < "$NEED_SWIPL_VERSION" ]]
then
    sudo apt install swi-prolog  # upgrade
fi

if [ ! -e $DOWNLOADDIR/kythe-$KYTHE_VERSION.tar.gz ]
then
    sudo apt install wget
    cd $DOWNLOADDIR
    wget https://github.com/kythe/kythe/releases/download/$KYTHE_VERSION/kythe-$KYTHE_VERSION.tar.gz
    rm -rf kythe-$KYTHE_VERSION
    tar xzf kythe-$KYTHE_VERSION.tar.gz
fi

cd $SRC

(echo 'pack_install(edcg, [interactive(false), upgrade(true)]).' ; \
 echo 'pack_install(rdet, [interactive(false), upgrade(true)]).') | swipl

make --warn-undefined-variables -C $HOME/src/pykythe \
     ENTRYSTREAM_EXE=$DOWNLOADDIR/kythe-$KYTHE_VERSION/tools/entrystream \
     WRITE_ENTRIES_EXE=$DOWNLOADDIR/kythe-$KYTHE_VERSION/tools/write_entries \
     WRITE_TABLES_EXE=$DOWNLOADDIR/kythe-$KYTHE_VERSION/tools/write_tables \
     HTTP_SERVER_EXE=$DOWNLOADDIR/kythe-$KYTHE_VERSION/tools/http_server \
     BATCH_ID=bbb \
         show-vars \
         test \
         make-json \
         make-json-pretty \
         json-decoded-all

echo The following starts a local server
echo You can use it by:
echo 'http://localhost:9999'
echo or
echo 'http://localhost:9999/static/src_browser.html?corpus=CORPUS&root=ROOT&path=home/peter/src/pykythe/pykythe/ast_raw.py&line=81'
echo And exit by '^D'

make --warn-undefined-variables -C $HOME/src/pykythe \
     SRC_BROWSER_PORT=9999 run-src-browser
