
ZOMPC = ../../zomp-check-syntax.sh

FILES = $(wildcard *.zomp)

all: $(FILES:.zomp=.test)

invalid_token_bof.test: EXPECTED_LINE = 1
invalid_token_not_first_line.test: EXPECTED_LINE = 3
invalid_token_single_line_comment.test: EXPECTED_LINE = 5
invalid_token_multi_line_comment.test: EXPECTED_LINE = 6

%.test: %.zomp
	@echo "Checking if compiler reports error in line $(EXPECTED_LINE) in $<"
	- ($(ZOMPC) $< 2>&1) > $(<:.zomp=.tmp)
	(cat $(<:.zomp=.tmp) | grep "$<:$(EXPECTED_LINE)" --quiet) || (cat $(<:.zomp=.tmp); exit 1)

