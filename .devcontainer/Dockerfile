FROM rust:alpine

# Install fundamental tools
RUN apk add --no-cache bash docker-cli git tree
RUN adduser devuser --disabled-password --gecos ""
WORKDIR /workspace
USER devuser