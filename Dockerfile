FROM fpco/stack-build:lts-13
MAINTAINER Dan Goje <gojedan98@gmail.com>

# Copy code.
Add . /opt/app
WORKDIR /opt/app

# Setup project.
RUN stack setup

# Test application
CMD ["stack", "test"]