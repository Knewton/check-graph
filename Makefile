all: docker

chroot:
	@sudo -E debootstrap --arch=amd64 --variant=minbase trusty chroot

.knewton_ubuntu_trusty:
	@sudo -E tar c -C chroot . \
		| docker import - knewton/ubuntu:trusty \
		| tee .knewton_ubuntu_trusty
	@docker tag knewton/ubuntu:trusty knewton/ubuntu

.knewton_check-graph_lib:
	@docker build --rm=true --tag=knewton/check-graph:lib ./etc/docker/lib/ \
		| tee .knewton_check-graph_lib

.knewton_check-graph_dev:
	@docker build --rm=false --tag=knewton/check-graph:dev ./etc/docker/dev/ \
		| tee .knewton_check-graph_dev

.cabal-sandbox/bin/check-graph:
	@cabal sandbox init
	@cabal update
	@cabal install
	@strip .cabal-sandbox/bin/check-graph

install: | .knewton_check-graph_dev
	@docker run -v $(PWD):/usr/src/check-graph knewton/check-graph:dev \
		/usr/bin/make .cabal-sandbox/bin/check-graph

docker: | install .knewton_check-graph_lib
	@docker build --rm=true --tag=knewton/check-graph .

clean:
	@sudo rm -rf .cabal-sandbox/bin/check-graph

distclean: clean
	@sudo rm -rf *cabal*sandbox .knewton* chroot

.PHONY: \
	all \
	clean \
	distclean \
	docker \
	image \
	knewton/check-graph \
	knewton/ubuntu
