.PHONY: all build generate clean resume

all: generate

generate: build
	stack run blog -- rebuild

build:
	stack build

clean:
	stack clean

# Render CV and place the PDF into site static assets
resources/static/Eric_Seidel_Resume.pdf: Eric_Seidel_CV.yaml
	mkdir -p resources/static
	rendercv render Eric_Seidel_CV.yaml
	mv -f rendercv_output/Eric_Seidel_CV.pdf $@

resume: resources/static/Eric_Seidel_Resume.pdf
