
# image magick
CONVERT_IMAGE = convert

all: smallpt-cpp

smallpt-cpp: smallpt-cpp.cpp
	$(CXX) -o $@ $<

image-%.ppm: smallpt-%
	./$<
	mv image.ppm $@

%.png: %.ppm
	$(CONVERT-IMAGE) $< $@

