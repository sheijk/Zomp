

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

examples/smallpt/clean:
	$(RM) -f examples/smallpt/smallpt-cpp
	$(RM) -f examples/smallpt/image.png examples/smallpt/image.ppm

