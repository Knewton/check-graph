docker: docker-run

docker-chroot:
	@sudo -E debootstrap --arch=amd64 --variant=minbase trusty docker-chroot

docker-ubuntu: docker-chroot
	@sudo -E tar c -C docker-chroot . \
		| docker import - docker.knewton.net/knewton/ubuntu:trusty
	@docker tag \
		docker.knewton.net/knewton/ubuntu:trusty \
		docker.knewton.net/knewton/ubuntu

docker-lib:
	@ln -sf ./etc/docker/lib/Dockerfile . \
		&& docker build \
			--tag=docker.knewton.net/knewton/check-graph:lib \
			$(PWD)

docker-dev:
	@ln -sf ./etc/docker/dev/Dockerfile . \
		&& docker build \
			--rm=false \
			--tag=docker.knewton.net/knewton/check-graph:dev \
			$(PWD)

docker-bld:
	@ln -sf ./etc/docker/bld/Dockerfile . \
		&& docker build \
			--rm=false \
			--tag=docker.knewton.net/knewton/check-graph:bld \
			$(PWD)

docker-run: check-graph
	@ln -sf ./etc/docker/run/Dockerfile . \
		&& docker build \
			--tag=docker.knewton.net/knewton/check-graph \
			$(PWD)

check-graph: docker-bld
	@docker run \
		--volume=$(PWD):/host \
		docker.knewton.net/knewton/check-graph:bld \
		cp /usr/local/bin/check-graph /host/

clean:
	@sudo rm check-graph

distclean: clean
	@sudo rm -rf docker-chroot

.PHONY: \
	clean \
	distclean \
	docker \
	docker-bld \
	docker-dev \
	docker-lib \
	docker-run \
	docker-ubuntu
