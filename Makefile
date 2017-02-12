# Generating parsable version of spec (needed to build testutils)
comark-testutils/spec.json:
	./spec/test/spec_tests.py -s spec/spec.txt --dump-tests > comark-testutils/spec.json

spec: comark-testutils/spec.json

progit:
	git clone https://github.com/progit/progit.git

comark-html/progit.md: progit
	echo "" > $@
	for lang in ar az be ca cs de en eo es es-ni fa fi fr hi hu id it ja ko mk nl no-nb pl pt-br ro ru sr th tr uk vi zh zh-tw; do \
		for i in `seq 1 2`; do \
			cat progit/$$lang/*/*.markdown >> $@; \
		done; \
	done

benchfiles: comark-html/progit.md
