TARGET_DIR=$(pwd)
TEMP_DIR=$(mktemp -d)
cd ${TEMP_DIR}

wget -O spec.tar.gz "https://github.com/jgm/CommonMark/archive/0.20.tar.gz"
tar xf spec.tar.gz

cd CommonMark*

python3 test/spec_tests.py -d > "${TARGET_DIR}/spec.json"

rm -r $TEMP_DIR
