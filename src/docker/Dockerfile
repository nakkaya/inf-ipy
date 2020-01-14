# sudo docker build -t nakkaya/inf-ipy-build:latest .
# sudo docker run -it -v `pwd`:/inf-ipy/ -w /inf-ipy/ nakkaya/inf-ipy-build:latest /bin/bash -c 'service ssh start && bash'
# sudo docker push nakkaya/inf-ipy-build:latest

FROM    ubuntu:18.04
MAINTAINER Nurullah Akkaya <nurullah@nakkaya.com>

RUN apt-get update && \
    apt-get upgrade -y

# Setup Tangle Requirements

RUN apt-get install python3 python3-pip git openssh-server openssh-client -y

RUN pip3 install ipykernel

RUN ssh-keygen -q -t rsa -N '' -f /root/.ssh/id_rsa
RUN cp /root/.ssh/id_rsa.pub /root/.ssh/authorized_keys
RUN echo "Host *\n\tStrictHostKeyChecking no\n" >> /root/.ssh/config