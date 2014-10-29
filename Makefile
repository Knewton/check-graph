all: docker

chroot:
	@sudo -E debootstrap --arch=amd64 --variant=minbase trusty chroot

docker.knewton.net/knewton/ubuntu: | chroot
	@sudo -E tar c -C chroot . \
		| docker import - docker.knewton.net/knewton/ubuntu:trusty
	@docker tag \
		docker.knewton.net/knewton/ubuntu:trusty \
		docker.knewton.net/knewton/ubuntu

docker.knewton.net/knewton/check-graph_lib:
	@docker build \
		--rm=true \
		--tag=docker.knewton.net/knewton/check-graph:lib \
		./etc/docker/lib/

docker.knewton.net/knewton/check-graph_dev:
	@docker build \
		--rm=false \
		--tag=docker.knewton.net/knewton/check-graph:dev \
		./etc/docker/dev/

.cabal-sandbox/bin/check-graph:
	@cabal sandbox init
	@cabal update
	@cabal install
	@strip .cabal-sandbox/bin/check-graph

install: | .knewton_check-graph_dev
	@docker run -v $(PWD):/usr/local/src/check-graph \
		docker.knewton.net/knewton/check-graph:dev \
		/usr/bin/make .cabal-sandbox/bin/check-graph

docker: | install .knewton_check-graph_lib
	@docker build --tag=docker.knewton.net/knewton/check-graph .

clean:
	@sudo rm -rf .cabal-sandbox cabal.sandbox.config dist

distclean: clean
	@sudo rm -rf chroot .knewton*

.PHONY: \
	all \
	clean \
	distclean \
	docker \
	docker.knewton.net/knewton/check-graph_dev \
	docker.knewton.net/knewton/check-graph_lib \
	docker.knewton.net/knewton/ubuntu \
	install
