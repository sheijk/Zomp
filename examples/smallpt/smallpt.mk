

examples/smallpt/all: examples/smallpt/smallpt-cpp

examples/smallpt/smallpt-cpp: examples/smallpt/smallpt-cpp.cpp
	@$(ECHO) "Building smallpt (C++) ..."
	$(CXX) -o $@ $<

examples/smallpt/image-%.ppm: examples/smallpt/smallpt-%
	@$(ECHO) "Rendering image using smallpt (C++) ..."
	./$<
	$(MV) image.ppm $@

examples/smallpt/%.png: examples/smallpt/%.ppm
	@$(ECHO) "Converting $< to $@ ..."
	$(CONVERT-IMAGE) $< $@

CLEAN_SUB_TARGETS += examples/smallpt/clean
examples/smallpt/clean:
	@$(ECHO) "Cleaning examples/smallpt ..."
	$(DELETE_FILE) examples/smallpt/smallpt-cpp
	$(DELETE_FILE) examples/smallpt/image.png examples/smallpt/image.ppm

