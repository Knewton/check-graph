# +--------------------+
# |                    |      ARTIFACT
# |     BUILD IMAGE    | +-------+
# |                    |         |
# +--------------------+         |
# |                    |         |
# |                    |         |
# |                    |         |
# |                    |         |
# |      DEV IMAGE     |         |
# |                    |         v
# |                    +-------------------+
# |                    |                   |
# |                    |   RUNTIME IMAGE   |
# |                    |                   |
# +--------------------+-------------------+
# |                                        |
# |                LIB IMAGE               |
# |                                        |
# +----------------------------------------+

all: docker-run

# LIB DOCKER IMAGE: RUNTIME FOR THIS PROJECT ADDED ON TOP "UBUNTU"
docker-lib:
	@ln -sf ./etc/docker/lib/Dockerfile .
	@docker build \
		--tag=docker.knewton.net/knewton/check-graph:lib \
		$(PWD)

# DEV DOCKER IMAGE: DEV TOOLS FOR THIS PROJECT ADDED ON TOP "LIB"
docker-dev:
	@ln -sf ./etc/docker/dev/Dockerfile .
	@docker build \
		--rm=false \
		--tag=docker.knewton.net/knewton/check-graph:dev \
		$(PWD)

# BLD DOCKER IMAGE: BUILD/TEST THIS PROJECT ON TOP OUR "DEV" IMAGE
docker-bld:
	@ln -sf ./etc/docker/bld/Dockerfile .
	@docker build \
		--rm=false \
		--tag=docker.knewton.net/knewton/check-graph:bld \
		$(PWD)

# EXTRACT BINARY: COPY THE BINARY EXE FROM OUR PROJECT "BLD" IMAGE
check-graph: docker-bld
	@docker run \
		--volume=$(TMP):/host \
		docker.knewton.net/knewton/check-graph:bld \
		cp /usr/local/bin/check-graph /host/
	@cp $(TMP)/check-graph .

# RUN DOCKER IMAGE: ADD THE PROJECT'S BINARY ON TOP OUR "LIB" IMAGE
docker-run: check-graph
	@ln -sf ./etc/docker/run/Dockerfile .
	@docker build \
		--tag=docker.knewton.net/knewton/check-graph \
		$(PWD)

clean:
	@rm -f Dockerfile check-graph

.PHONY: \
	all \
	clean \
	docker-bld \
	docker-dev \
	docker-lib \
	docker-run

TMP := $(shell mktemp -d)
